"""
Cliente de datos macroeconomicos para el simulador de pensiones.

Fuente primaria: API REST del Banco Central de Chile (si3.bcentral.cl).
Requiere registro gratuito y variables de entorno BCENTRAL_USER/BCENTRAL_PASS:
    https://si3.bcentral.cl/siete/secure/cuadros/home.aspx

Fuente de respaldo (fallback automatico): mindicador.cl — API publica sin
autenticacion. Se usa automaticamente para UF e IPC si el BCCh no esta
configurado o la llamada falla (red, credenciales, error HTTP), para que
la aplicacion funcione sin configuracion previa. La cobertura historica de
mindicador.cl es menor que la del BCCh (consultada año por año).

Variables de entorno:
    BCENTRAL_USER  — correo de registro (opcional; sin ella se usa el fallback)
    BCENTRAL_PASS  — contrasena de la cuenta (opcional; sin ella se usa el fallback)

Datos sin endpoint publico en BCCh ni en mindicador.cl:
    Comisiones AFP, rentabilidades por fondo y PBS son valores regulados
    por la Superintendencia de Pensiones. Se mantienen como constantes
    auditadas con fuente oficial documentada y deben actualizarse manualmente.
    Fuente: https://www.spensiones.cl
"""

import logging
import os
from datetime import datetime, timedelta
from typing import Dict, List

import requests

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Configuracion API Banco Central de Chile
# ---------------------------------------------------------------------------

_BCENTRAL_BASE = "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx"
_MINDICADOR_BASE = "https://mindicador.cl/api"
_TIMEOUT = 10  # segundos

# Series BCCh — verificar vigencia en si3.bcentral.cl/siete/secure/cuadros/home.aspx
_SERIES: Dict[str, str] = {
    "uf":  "F073.UFF.PRE.Z.D",        # UF, valor diario
    "ipc": "F074.IPC.VAR.Z.EP23.C.M", # IPC General, variacion mensual, serie empalmada base 2023=100, desde 2010
    "utm": "F073.UTR.PRE.Z.M",        # Unidad Tributaria Mensual, valor mensual
}

_CREDENCIALES_INSTRUCCIONES = (
    "\nCredenciales del Banco Central de Chile no configuradas.\n"
    "Pasos:\n"
    "  1. Registrese gratis en https://si3.bcentral.cl/siete/secure/cuadros/home.aspx\n"
    "  2. Exporte las siguientes variables de entorno antes de iniciar la aplicacion:\n"
    "       export BCENTRAL_USER='su_correo@ejemplo.com'\n"
    "       export BCENTRAL_PASS='su_contrasena'\n"
    "  3. En Windows use 'set' en lugar de 'export'.\n"
)

# ---------------------------------------------------------------------------
# Constantes reguladas — fuente: Superintendencia de Pensiones
#
# Actualizacion manual requerida. Fuentes:
#   Comisiones AFP   : https://www.spensiones.cl/apps/loadEstadisticas/genEstadAFP.php
#   Rentabilidades   : https://www.spensiones.cl/apps/rentabilidad/getRentabilidad.php
#   PBS / PGU        : Decretos Supremos MIDESO (Diario Oficial)
#   Tope imponible   : Art. 16 DL 3.500 (reajustado anualmente en UF)
#   Sueldo minimo    : Ministerio del Trabajo y Prevision Social
#                      https://www.mintrab.gob.cl/remuneraciones/
#                      Valor vigente al 1 de agosto de cada año (Ley 21.248 y sucesoras)
#
# Ultima revision: mayo 2025
# ---------------------------------------------------------------------------

_COMISIONES_AFP: Dict[str, float] = {
    "Capital":   1.44,
    "Cuprum":    1.44,
    "Habitat":   1.27,
    "Modelo":    0.58,
    "Planvital": 1.16,
    "Provida":   1.45,
    "Uno":       0.49,
}

_RENTABILIDADES_FONDO: Dict[str, float] = {
    "A": 6.8,
    "B": 6.2,
    "C": 5.5,
    "D": 4.8,
    "E": 3.2,
}

_VOLATILIDAD_FONDO: Dict[str, float] = {
    "A": 12.0,
    "B":  9.0,
    "C":  6.0,
    "D":  4.0,
    "E":  2.0,
}

_PBS_HISTORICA: Dict[int, int] = {
    2025: 225_975,
    2024: 214_296,
    2023: 206_173,
    2022: 193_917,
    2021: 185_004,
    2020: 177_849,
}

_TOPE_IMPONIBLE_UF: float = 84.7

# Sueldo minimo nominal: valor vigente al 31 de diciembre de cada ano (CLP)
# Fuente: Biblioteca del Congreso Nacional — Ley Chile (leychile.bcn.cl)
# Multiples reajustes dentro del ano; se usa el valor de mayor vigencia anual.
_SUELDO_MINIMO_HISTORICO: Dict[int, int] = {
    2026: 539_000,   # desde 01-ene-2026 (Ley 21.751)
    2025: 529_000,   # desde 01-may-2025 (Ley 21.578)
    2024: 500_000,   # desde 01-jul-2024 (Ley 21.578)
    2023: 460_000,   # desde 01-sep-2023 (Ley 21.456)
    2022: 400_000,   # desde 01-ago-2022 (Ley 21.456)
    2021: 337_000,   # desde 01-may-2021 (Ley 21.360)
    2020: 326_500,   # desde 01-sep-2020 (Ley 21.283)
    2019: 301_000,   # desde 01-mar-2019 (Ley 21.112)
    2018: 288_000,   # desde 01-sep-2018 (Ley 21.112)
    2017: 270_000,   # desde 01-jul-2017 (Ley 20.935)
    2016: 257_500,   # desde 01-jul-2016 (Ley 20.935)
    2015: 241_000,   # desde 01-jul-2015 (Ley 20.763)
    2014: 225_000,   # desde 01-jul-2014 (Ley 20.689)
}


class DataFetcher:
    """Cliente exclusivo del API REST del Banco Central de Chile.

    Lanza EnvironmentError si las credenciales no estan configuradas.
    Lanza requests.HTTPError o ValueError si la API retorna un error.
    No usa fuentes alternativas ni valores estaticos para datos de mercado.
    """

    def __init__(self) -> None:
        self._cache: Dict[str, object] = {}
        self._bcentral_user = os.environ.get("BCENTRAL_USER", "")
        self._bcentral_pass = os.environ.get("BCENTRAL_PASS", "")
        self._uso_fallback = False

    # ------------------------------------------------------------------
    # UF
    # ------------------------------------------------------------------

    def obtener_uf_actual(self) -> float:
        """Valor de la UF del dia. BCCh (serie F073.UFF.PRE.Z.D) como fuente
        primaria, con fallback automatico a mindicador.cl si el BCCh no esta
        configurado o la llamada falla.

        Returns:
            UF en CLP.

        Raises:
            ValueError: Si ninguna de las dos fuentes retorna datos validos.
        """
        clave = f"uf_{datetime.now().date()}"
        if clave in self._cache:
            return self._cache[clave]  # type: ignore[return-value]

        try:
            valor = self._uf_desde_bcentral()
        except (EnvironmentError, requests.RequestException, ValueError, KeyError) as exc:
            logger.warning("BCCh no disponible para UF (%s). Usando fallback mindicador.cl.", exc)
            valor = self._uf_desde_mindicador()
            self._uso_fallback = True

        self._cache[clave] = valor
        return valor

    def _uf_desde_bcentral(self) -> float:
        self._verificar_credenciales()

        hoy = datetime.now().date()
        ayer = hoy - timedelta(days=3)
        params = {
            "user":       self._bcentral_user,
            "pass":       self._bcentral_pass,
            "function":   "GetSeries",
            "timeseries": _SERIES["uf"],
            "startdate":  ayer.strftime("%Y-%m-%d"),
            "enddate":    hoy.strftime("%Y-%m-%d"),
            "format":     "json",
        }
        data = self._get_bcentral(params)
        series = data["Series"]["Obs"]
        if not series:
            raise ValueError("BCCh no retorno observaciones para la serie UF.")
        ultimo = sorted(series, key=lambda x: x["indexDateString"], reverse=True)[0]
        valor = float(ultimo["value"].replace(",", "."))
        logger.info("UF obtenida desde BCCh: %.2f", valor)
        return valor

    def _uf_desde_mindicador(self) -> float:
        """Fallback: UF del dia desde mindicador.cl (API publica, sin autenticacion)."""
        response = requests.get(f"{_MINDICADOR_BASE}/uf", timeout=_TIMEOUT)
        response.raise_for_status()
        serie = response.json().get("serie", [])
        if not serie:
            raise ValueError("mindicador.cl no retorno observaciones para la serie UF.")
        valor = float(serie[0]["valor"])
        logger.info("UF obtenida desde mindicador.cl (fallback): %.2f", valor)
        return valor

    # ------------------------------------------------------------------
    # UTM
    # ------------------------------------------------------------------

    def obtener_utm_actual(self) -> float:
        """Valor de la UTM del mes vigente. BCCh (serie F073.UTR.PRE.Z.M,
        confirmada contra la API real) como fuente primaria, con fallback
        automatico a mindicador.cl si el BCCh no esta configurado o falla.

        Returns:
            UTM en CLP.

        Raises:
            ValueError: Si ninguna de las dos fuentes retorna datos validos.
        """
        clave = f"utm_{datetime.now().strftime('%Y-%m')}"
        if clave in self._cache:
            return self._cache[clave]  # type: ignore[return-value]

        try:
            valor = self._utm_desde_bcentral()
        except (EnvironmentError, requests.RequestException, ValueError, KeyError) as exc:
            logger.warning("BCCh no disponible para UTM (%s). Usando fallback mindicador.cl.", exc)
            valor = self._utm_desde_mindicador()
            self._uso_fallback = True

        self._cache[clave] = valor
        return valor

    def _utm_desde_bcentral(self) -> float:
        self._verificar_credenciales()

        hoy = datetime.now().date()
        inicio_mes_anterior = hoy.replace(day=1) - timedelta(days=32)
        params = {
            "user":       self._bcentral_user,
            "pass":       self._bcentral_pass,
            "function":   "GetSeries",
            "timeseries": _SERIES["utm"],
            "startdate":  inicio_mes_anterior.strftime("%Y-%m-%d"),
            "enddate":    hoy.strftime("%Y-%m-%d"),
            "format":     "json",
        }
        data = self._get_bcentral(params)
        series = data["Series"]["Obs"]
        if not series:
            raise ValueError("BCCh no retorno observaciones para la serie UTM.")
        ultimo = sorted(series, key=lambda x: x["indexDateString"], reverse=True)[0]
        valor = float(ultimo["value"].replace(",", "."))
        logger.info("UTM obtenida desde BCCh: %.2f", valor)
        return valor

    def _utm_desde_mindicador(self) -> float:
        """Fallback: UTM del mes desde mindicador.cl (API publica, sin autenticacion)."""
        response = requests.get(f"{_MINDICADOR_BASE}/utm", timeout=_TIMEOUT)
        response.raise_for_status()
        serie = response.json().get("serie", [])
        if not serie:
            raise ValueError("mindicador.cl no retorno observaciones para la serie UTM.")
        valor = float(serie[0]["valor"])
        logger.info("UTM obtenida desde mindicador.cl (fallback): %.2f", valor)
        return valor

    # ------------------------------------------------------------------
    # Inflacion (IPC)
    # ------------------------------------------------------------------

    def obtener_inflacion_anual(self, anos: int = 10) -> Dict[int, float]:
        """Inflacion anual compuesta calculada desde el IPC mensual.

        Fuente primaria: BCCh, serie F074.IPC.VAR.Z.EP23.C.M (empalmada base
        2023=100, desde 2010). Si el BCCh no esta configurado o falla, usa
        mindicador.cl como fallback (API publica, sin autenticacion; cobertura
        historica menor, consultada año por año).

        Formula: inf_anual_t = prod(1 + ipc_mes/100 for mes in t) - 1

        Args:
            anos: Ventana historica en anos.

        Returns:
            Dict {ano: inflacion_anual_porcentaje}.

        Raises:
            ValueError: Si ninguna de las dos fuentes retorna datos suficientes.
        """
        clave = f"inflacion_anual_{anos}"
        if clave in self._cache:
            return self._cache[clave]  # type: ignore[return-value]

        try:
            serie_mensual = self._ipc_mensual_desde_bcentral(anos)
        except (EnvironmentError, requests.RequestException, ValueError, KeyError) as exc:
            logger.warning("BCCh no disponible para IPC (%s). Usando fallback mindicador.cl.", exc)
            serie_mensual = self._ipc_mensual_desde_mindicador(anos)
            self._uso_fallback = True

        resultado = self._calcular_inflacion_anual(serie_mensual)

        if not resultado:
            raise ValueError(
                f"No hay datos de IPC suficientes para los ultimos {anos} anos."
            )

        self._cache[clave] = resultado
        return resultado

    def _ipc_mensual_desde_bcentral(self, anos: int) -> Dict[str, float]:
        """Descarga variaciones mensuales del IPC desde BCCh.

        Returns:
            Dict {YYYY-MM: variacion_porcentual_mensual}.

        Raises:
            EnvironmentError: Si las credenciales no estan configuradas.
            ValueError: Si la respuesta no contiene datos validos.
            requests.HTTPError: Si la API retorna un error HTTP.
        """
        self._verificar_credenciales()

        inicio = (datetime.now() - timedelta(days=anos * 366)).strftime("%Y-%m-%d")
        fin = datetime.now().strftime("%Y-%m-%d")
        params = {
            "user":       self._bcentral_user,
            "pass":       self._bcentral_pass,
            "function":   "GetSeries",
            "timeseries": _SERIES["ipc"],
            "startdate":  inicio,
            "enddate":    fin,
            "format":     "json",
        }
        data = self._get_bcentral(params)
        serie = data["Series"]["Obs"]

        resultado: Dict[str, float] = {}
        for entry in serie:
            fecha = entry["indexDateString"][:7]  # YYYY-MM
            valor_str = entry["value"].replace(",", ".")
            if valor_str not in ("", "N/E"):
                resultado[fecha] = float(valor_str)

        if not resultado:
            raise ValueError("BCCh no retorno observaciones validas para la serie IPC.")

        logger.info("IPC mensual obtenido desde BCCh: %d registros.", len(resultado))
        return resultado

    def _ipc_mensual_desde_mindicador(self, anos: int) -> Dict[str, float]:
        """Fallback: variaciones mensuales del IPC desde mindicador.cl.

        mindicador.cl no ofrece un rango de fechas arbitrario; se consulta
        año por año via /api/ipc/{año} y se combinan los resultados.

        Returns:
            Dict {YYYY-MM: variacion_porcentual_mensual}.

        Raises:
            ValueError: Si no se obtiene ningun dato valido.
        """
        ano_actual = datetime.now().year
        resultado: Dict[str, float] = {}

        for ano in range(ano_actual - anos, ano_actual + 1):
            try:
                response = requests.get(f"{_MINDICADOR_BASE}/ipc/{ano}", timeout=_TIMEOUT)
                response.raise_for_status()
                serie = response.json().get("serie", [])
            except requests.RequestException:
                continue

            for entry in serie:
                fecha = entry["fecha"][:7]  # YYYY-MM
                if entry.get("valor") is not None:
                    resultado[fecha] = float(entry["valor"])

        if not resultado:
            raise ValueError("mindicador.cl no retorno observaciones validas para la serie IPC.")

        logger.info("IPC mensual obtenido desde mindicador.cl (fallback): %d registros.", len(resultado))
        return resultado

    @staticmethod
    def _calcular_inflacion_anual(serie_mensual: Dict[str, float]) -> Dict[int, float]:
        """Agrupa variaciones mensuales por ano y calcula inflacion compuesta."""
        por_ano: Dict[int, List[float]] = {}
        for fecha_mes, variacion in serie_mensual.items():
            ano = int(fecha_mes[:4])
            por_ano.setdefault(ano, []).append(variacion)

        resultado: Dict[int, float] = {}
        ano_actual = datetime.now().year
        for ano, meses in por_ano.items():
            if len(meses) >= 12:
                factor = 1.0
                for m in meses:
                    factor *= 1.0 + m / 100.0
                resultado[ano] = round((factor - 1.0) * 100.0, 2)
            elif len(meses) >= 3 and ano == ano_actual:
                factor = 1.0
                for m in meses:
                    factor *= 1.0 + m / 100.0
                anualizado = ((factor ** (12.0 / len(meses))) - 1.0) * 100.0
                resultado[ano] = round(anualizado, 2)

        return resultado

    def obtener_inflacion_promedio(self, anos: int = 5) -> float:
        """Media aritmetica de la inflacion anual de los ultimos N anos completos.

        Args:
            anos: Ventana de calculo.

        Returns:
            Promedio en puntos porcentuales.

        Raises:
            EnvironmentError: Si las credenciales no estan configuradas.
            ValueError: Si no hay datos suficientes.
        """
        historico = self.obtener_inflacion_anual(anos + 1)
        ano_actual = datetime.now().year
        valores = [v for a, v in historico.items() if ano_actual - anos <= a < ano_actual]
        if not valores:
            raise ValueError(
                f"No hay datos de inflacion para los ultimos {anos} anos completos."
            )
        return round(sum(valores) / len(valores), 1)

    # ------------------------------------------------------------------
    # Datos regulados (constantes auditadas — fuentes oficiales)
    # El BCCh no publica sueldo minimo via API; es un valor legal fijado
    # por decreto. Fuente: Ministerio del Trabajo y Prevision Social.
    # ------------------------------------------------------------------

    def obtener_comisiones_afp(self) -> Dict[str, float]:
        """Comisiones AFP vigentes (Superintendencia de Pensiones — actualizacion manual)."""
        return dict(_COMISIONES_AFP)

    def obtener_rentabilidades_afp(self, fondo: str = "C") -> Dict[str, float]:
        """Rentabilidad y volatilidad historica por tipo de fondo (SP Pensiones)."""
        return {
            "fondo":                     fondo,
            "rentabilidad_promedio_10a": _RENTABILIDADES_FONDO.get(fondo, 5.0),
            "volatilidad_estimada":      _VOLATILIDAD_FONDO.get(fondo, 6.0),
        }

    def obtener_pension_basica_solidaria(self) -> int:
        """PBS / PGU vigente segun Decreto Supremo mas reciente."""
        ano = datetime.now().year
        return _PBS_HISTORICA.get(ano, max(_PBS_HISTORICA.values()))

    def obtener_sueldo_minimo(self, ano: int | None = None) -> int:
        """Sueldo minimo nominal vigente (Ministerio del Trabajo — actualizacion manual).

        El BCCh no publica esta serie via API. Valor fijado por ley cada agosto.

        Args:
            ano: Ano consultado. Si None, usa el ano actual.

        Returns:
            Sueldo minimo en CLP.
        """
        ano = ano or datetime.now().year
        return _SUELDO_MINIMO_HISTORICO.get(ano, max(_SUELDO_MINIMO_HISTORICO.values()))

    def obtener_sueldo_minimo_historico(self) -> Dict[int, int]:
        """Serie historica completa del sueldo minimo nominal (CLP)."""
        return dict(_SUELDO_MINIMO_HISTORICO)

    def obtener_tope_imponible(self, en_uf: bool = True) -> float:
        """Tope imponible (Art. 16 DL 3.500).

        Args:
            en_uf: Si True retorna el tope en UF; si False, en CLP.
        """
        if en_uf:
            return _TOPE_IMPONIBLE_UF
        return _TOPE_IMPONIBLE_UF * self.obtener_uf_actual()

    # ------------------------------------------------------------------
    # Punto de entrada unico
    # ------------------------------------------------------------------

    def obtener_datos_completos(self) -> Dict:
        """Agrega todos los indicadores en una sola llamada.

        Returns:
            Dict con uf, inflacion_anual, inflacion_promedio, comisiones_afp,
            rentabilidades_fondos, pbs, topes, fecha y la fuente efectivamente
            usada para UF/IPC (BCCh o el fallback mindicador.cl).
        """
        resultado = {
            "uf":                  self.obtener_uf_actual(),
            "utm":                 self.obtener_utm_actual(),
            "inflacion_anual":     self.obtener_inflacion_anual(10),
            "inflacion_promedio":  self.obtener_inflacion_promedio(5),
            "comisiones_afp":      self.obtener_comisiones_afp(),
            "rentabilidades_fondos": {
                f: self.obtener_rentabilidades_afp(f) for f in ("A", "B", "C", "D", "E")
            },
            "pbs":                  self.obtener_pension_basica_solidaria(),
            "tope_imponible_uf":    self.obtener_tope_imponible(en_uf=True),
            "tope_imponible_clp":   self.obtener_tope_imponible(en_uf=False),
            "sueldo_minimo":        self.obtener_sueldo_minimo(),
            "fecha_actualizacion":  datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        }
        resultado["fuente"] = (
            "mindicador.cl (fallback — BCCh no disponible)"
            if self._uso_fallback
            else "Banco Central de Chile — si3.bcentral.cl"
        )
        return resultado

    # ------------------------------------------------------------------
    # Utilidades internas
    # ------------------------------------------------------------------

    def _verificar_credenciales(self) -> None:
        """Lanza EnvironmentError si las credenciales BCCh no estan configuradas."""
        if not (self._bcentral_user and self._bcentral_pass):
            raise EnvironmentError(_CREDENCIALES_INSTRUCCIONES)

    def _get_bcentral(self, params: Dict) -> Dict:
        """Ejecuta una llamada GET al API del BCCh y retorna el JSON validado.

        Raises:
            requests.HTTPError: Si el servidor retorna un codigo de error.
            ValueError: Si la respuesta no contiene la clave 'Series'.
        """
        response = requests.get(_BCENTRAL_BASE, params=params, timeout=_TIMEOUT)
        response.raise_for_status()
        data: Dict = response.json()
        if "Series" not in data:
            raise ValueError(
                f"Respuesta inesperada del BCCh. Claves recibidas: {list(data.keys())}"
            )
        return data


# Instancia global compartida
data_fetcher = DataFetcher()
