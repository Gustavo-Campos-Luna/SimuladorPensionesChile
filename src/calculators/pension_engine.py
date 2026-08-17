"""
Motor de calculo del sistema de pensiones chileno (AFP).

Implementa la proyeccion actuarial año a año del saldo acumulado,
el calculo de pension bajo modalidades de Retiro Programado y Renta
Vitalicia, y un conjunto de metricas financieras avanzadas.

Arquitectura:
    PensionCalculator  -- clase principal (stateless, thread-safe)
        calcular_pension_completa()   -> Dict con todos los resultados
        _simular_acumulacion()        -> calculo vectorizado del saldo
        _distribuir_lagunas()         -> asignacion de periodos sin cotizacion
"""

from typing import Dict, List, Optional

import numpy as np
import pandas as pd

from .financial_metrics import (
    calcular_brecha_previsional,
    calcular_duracion_macaulay,
    calcular_dv01_pension,
    calcular_pension_retiro_programado,
    calcular_rentabilidad_real,
    calcular_tir,
    calcular_valor_presente_pension,
    calcular_vpn,
    clp_a_uf,
)


class PensionCalculator:
    """Calculadora actuarial de pensiones del sistema AFP chileno.

    Todos los parametros de porcentaje se reciben en puntos porcentuales
    (ej: 10.0 para 10 %) y se convierten internamente a decimales. El
    resultado se expresa en pesos chilenos nominales y reales (pesos
    constantes del año de calculo).

    Constants:
        TOPE_IMPONIBLE_UF: Tope imponible vigente en UF (Ley 19.728).
        PBS_CLP: Pension Basica Solidaria de referencia (2024, CLP). Valor de
            respaldo solo si calcular_pension_completa() no recibe el
            parametro `pbs`; se recomienda pasar el valor vigente obtenido
            dinamicamente via data_sources.obtener_pension_basica_solidaria().
        FACTOR_RENTA_VITALICIA: Porcentaje estimado de conversion RP->RV.
        TASA_RETIRO_DESCUENTO: Tasa de descuento real para calculo de PMT.
    """

    TOPE_IMPONIBLE_UF: float = 84.7
    PBS_CLP: int = 214_296  # Referencia 2024; usar el parametro `pbs` con el valor vigente.
    FACTOR_RENTA_VITALICIA: float = 0.92
    TASA_RETIRO_DESCUENTO: float = 0.03

    def calcular_pension_completa(
        self,
        edad_actual: int,
        edad_jubilacion: int,
        esperanza_vida: int,
        ingreso_mensual: float,
        aumento_salarial_anual: float,
        cotizacion_obligatoria: float,
        comision_afp: float,
        aporte_empleador: float,
        cotizacion_voluntaria: float,
        rentabilidad_nominal: float,
        inflacion_esperada: float,
        anos_lagunas: int,
        distribucion_lagunas: str,
        valor_uf: float = 37_500.0,
        pbs: Optional[float] = None,
    ) -> Dict:
        """Proyeccion completa de pension con metricas financieras avanzadas.

        Ejecuta la simulacion año a año con capitalización compuesta y
        calcula el conjunto de indicadores necesario para un analisis
        previsional profesional.

        Args:
            edad_actual: Edad actual del cotizante (anos).
            edad_jubilacion: Edad objetivo de jubilacion (anos).
            esperanza_vida: Esperanza de vida estimada (anos).
            ingreso_mensual: Remuneracion mensual imponible en CLP.
            aumento_salarial_anual: Incremento anual del sueldo (pp).
            cotizacion_obligatoria: Tasa de cotizacion obligatoria (pp); min 10.
            comision_afp: Comision de la AFP sobre remuneracion imponible (pp).
            aporte_empleador: Aporte adicional del empleador (pp).
            cotizacion_voluntaria: APV u otro ahorro voluntario (pp).
            rentabilidad_nominal: Rentabilidad nominal anual esperada del fondo (pp).
            inflacion_esperada: Inflacion anual esperada (pp).
            anos_lagunas: Anos sin cotizacion (periodos de cesantia u otro).
            distribucion_lagunas: Patron temporal de lagunas.
                Opciones: 'Aleatorio', 'Inicio de carrera',
                          'Mitad de carrera', 'Final de carrera'.
            valor_uf: Valor vigente de la UF en CLP.
            pbs: Pension Basica Solidaria vigente en CLP. Si es None, usa
                PBS_CLP (valor de referencia 2024; se recomienda pasar el
                valor vigente obtenido dinamicamente via data_sources).

        Returns:
            Diccionario con resultados, metricas financieras y DataFrame anual.
            Incluye la clave 'error' solo ante parametros invalidos.
        """
        anos_acumulacion = edad_jubilacion - edad_actual
        anos_retiro = esperanza_vida - edad_jubilacion

        if anos_acumulacion <= 0:
            return self._error("La edad de jubilacion debe ser mayor a la edad actual.")
        if anos_retiro <= 0:
            return self._error("La esperanza de vida debe superar la edad de jubilacion.")

        # Conversion a decimales
        r_nominal = rentabilidad_nominal / 100.0
        r_inflacion = inflacion_esperada / 100.0
        r_real = calcular_rentabilidad_real(r_nominal, r_inflacion)
        cot_total = (cotizacion_obligatoria + cotizacion_voluntaria) / 100.0
        comision = comision_afp / 100.0
        emp_contrib = aporte_empleador / 100.0
        aumento = aumento_salarial_anual / 100.0
        tope_clp = self.TOPE_IMPONIBLE_UF * valor_uf

        # Simulacion año a año
        df = self._simular_acumulacion(
            anos_acumulacion=anos_acumulacion,
            edad_inicial=edad_actual,
            ingreso_inicial=ingreso_mensual,
            aumento_salarial=aumento,
            cot_total=cot_total,
            comision=comision,
            emp_contrib=emp_contrib,
            r_nominal=r_nominal,
            r_inflacion=r_inflacion,
            anos_lagunas=anos_lagunas,
            distribucion_lagunas=distribucion_lagunas,
            tope_clp=tope_clp,
        )

        saldo_nominal = float(df["saldo_nominal"].iloc[-1])
        saldo_real = float(df["saldo_real"].iloc[-1])
        ultimo_sueldo = float(df["ingreso_mensual"].iloc[-1])
        meses_retiro = anos_retiro * 12

        # Pension RP con formula de anualidad (tasa real de retiro)
        pension_rp_nominal = calcular_pension_retiro_programado(
            saldo_nominal, meses_retiro, self.TASA_RETIRO_DESCUENTO
        )
        pension_rp_real = calcular_pension_retiro_programado(
            saldo_real, meses_retiro, self.TASA_RETIRO_DESCUENTO
        )

        # Pension RV
        pension_rv_nominal = pension_rp_nominal * self.FACTOR_RENTA_VITALICIA
        pension_rv_real = pension_rp_real * self.FACTOR_RENTA_VITALICIA

        # Tasas de reemplazo
        tasa_rp = pension_rp_nominal / ultimo_sueldo * 100.0 if ultimo_sueldo else 0.0
        tasa_rv = pension_rv_nominal / ultimo_sueldo * 100.0 if ultimo_sueldo else 0.0
        sueldo_real_inicio = ultimo_sueldo / (1.0 + r_inflacion) ** anos_acumulacion
        tasa_real = (
            pension_rp_real / sueldo_real_inicio * 100.0 if sueldo_real_inicio else 0.0
        )

        # Metricas financieras
        flujos = df["cotizacion_neta_anual"].tolist()
        vpn = calcular_vpn(flujos, r_real)
        tir = calcular_tir(flujos, valor_terminal=saldo_nominal)

        vp_pension = calcular_valor_presente_pension(
            pension_rp_nominal, anos_retiro, r_real, r_inflacion
        )
        duracion = calcular_duracion_macaulay(meses_retiro, self.TASA_RETIRO_DESCUENTO)
        dv01 = calcular_dv01_pension(saldo_nominal, meses_retiro, self.TASA_RETIRO_DESCUENTO)

        # Brecha previsional (objetivo OCDE: 70 % del ultimo sueldo)
        pension_objetivo = ultimo_sueldo * 0.70
        brecha = calcular_brecha_previsional(
            pension_rp_nominal,
            pension_objetivo,
            anos_acumulacion,
            r_nominal,
            anos_retiro,
        )

        # Piso PBS
        pbs_efectivo = pbs if pbs is not None else self.PBS_CLP
        pension_rp_con_pbs = max(pension_rp_nominal, pbs_efectivo)
        pension_rv_con_pbs = max(pension_rv_nominal, pbs_efectivo)

        return {
            # Saldos
            "saldo_final_nominal": saldo_nominal,
            "saldo_final_real": saldo_real,
            "saldo_final_uf": clp_a_uf(saldo_nominal, valor_uf),
            # Pensiones
            "pension_rp_nominal": pension_rp_nominal,
            "pension_rp_real": pension_rp_real,
            "pension_rv_nominal": pension_rv_nominal,
            "pension_rv_real": pension_rv_real,
            "pension_rp_con_pbs": pension_rp_con_pbs,
            "pension_rv_con_pbs": pension_rv_con_pbs,
            # Tasas de reemplazo
            "tasa_reemplazo_rp": tasa_rp,
            "tasa_reemplazo_rv": tasa_rv,
            "tasa_reemplazo_real": tasa_real,
            # Parametros derivados
            "ultimo_sueldo": ultimo_sueldo,
            "anos_cotizacion": anos_acumulacion,
            "anos_retiro": anos_retiro,
            "anos_lagunas_efectivos": anos_lagunas,
            "anos_cotizacion_efectivos": anos_acumulacion - anos_lagunas,
            # Metricas financieras
            "vpn_cotizaciones": vpn,
            "tir_cotizaciones": tir * 100.0,
            "rentabilidad_real": r_real * 100.0,
            "rentabilidad_nominal": r_nominal * 100.0,
            "vp_pension_total": vp_pension,
            # ALM
            "duracion_macaulay_anos": duracion,
            "dv01_clp": dv01,
            # Brecha previsional
            "brecha_previsional": brecha,
            # Simulacion tabular
            "simulacion_anual": df,
            # Metadata
            "factor_renta_vitalicia": self.FACTOR_RENTA_VITALICIA,
            "inflacion_esperada": r_inflacion * 100.0,
            "valor_uf_usado": valor_uf,
            "pbs_usado": pbs_efectivo,
            "tasa_descuento_retiro": self.TASA_RETIRO_DESCUENTO * 100.0,
        }

    # ------------------------------------------------------------------
    # Simulacion interna
    # ------------------------------------------------------------------

    def _simular_acumulacion(
        self,
        anos_acumulacion: int,
        edad_inicial: int,
        ingreso_inicial: float,
        aumento_salarial: float,
        cot_total: float,
        comision: float,
        emp_contrib: float,
        r_nominal: float,
        r_inflacion: float,
        anos_lagunas: int,
        distribucion_lagunas: str,
        tope_clp: float,
    ) -> pd.DataFrame:
        """Proyeccion año a año usando listas pre-asignadas (O(n) eficiente).

        Calcula el saldo nominal acumulando cotizaciones netas con
        capitalizacion compuesta. El saldo real se obtiene deflactando
        el saldo nominal al nivel de precios del año base.

        Returns:
            DataFrame con columnas: ano, edad, ingreso_mensual,
            ingreso_imponible, cotizacion_anual, aporte_empleador_anual,
            comision_anual, cotizacion_neta_anual, rentabilidad_nominal_anual,
            saldo_nominal, saldo_real, tiene_laguna.
        """
        n = anos_acumulacion
        lagunas = set(self._distribuir_lagunas(n, anos_lagunas, distribucion_lagunas))

        # Pre-asignacion con listas
        edades = list(range(edad_inicial, edad_inicial + n))
        ingresos = np.zeros(n)
        imponibles = np.zeros(n)
        cotizaciones = np.zeros(n)
        aportes_emp = np.zeros(n)
        comisiones = np.zeros(n)
        netos = np.zeros(n)
        rentabilidades = np.zeros(n)
        saldos_nominales = np.zeros(n)
        tiene_laguna = np.zeros(n, dtype=bool)

        saldo = 0.0
        ingreso = ingreso_inicial

        for i in range(n):
            if i > 0:
                ingreso = ingresos[i - 1] * (1.0 + aumento_salarial)

            imponible = min(ingreso, tope_clp)
            ingresos[i] = ingreso
            imponibles[i] = imponible
            tiene_laguna[i] = i in lagunas

            if not tiene_laguna[i]:
                cot_anual = imponible * cot_total * 12.0
                emp_anual = imponible * emp_contrib * 12.0
                com_anual = imponible * comision * 12.0
            else:
                cot_anual = emp_anual = com_anual = 0.0

            neto = cot_anual + emp_anual - com_anual
            rent = saldo * r_nominal
            saldo = saldo + rent + neto

            cotizaciones[i] = cot_anual
            aportes_emp[i] = emp_anual
            comisiones[i] = com_anual
            netos[i] = neto
            rentabilidades[i] = rent
            saldos_nominales[i] = saldo

        # Deflactar saldo nominal para obtener saldo real (pesos constantes año 0)
        indices = (1.0 + r_inflacion) ** np.arange(1, n + 1)
        saldos_reales = saldos_nominales / indices

        return pd.DataFrame({
            "ano": np.arange(1, n + 1),
            "edad": edades,
            "ingreso_mensual": ingresos,
            "ingreso_imponible": imponibles,
            "cotizacion_anual": cotizaciones,
            "aporte_empleador_anual": aportes_emp,
            "comision_anual": comisiones,
            "cotizacion_neta_anual": netos,
            "rentabilidad_nominal_anual": rentabilidades,
            "saldo_nominal": saldos_nominales,
            "saldo_real": saldos_reales,
            "tiene_laguna": tiene_laguna,
        })

    def _distribuir_lagunas(
        self, anos_totales: int, anos_lagunas: int, distribucion: str
    ) -> List[int]:
        """Indices de anos con laguna previsional segun patron elegido.

        Args:
            anos_totales: Duracion total del periodo de acumulacion.
            anos_lagunas: Cantidad de anos sin cotizacion.
            distribucion: Patron de distribucion.

        Returns:
            Lista de indices (0-based) con laguna.
        """
        if anos_lagunas <= 0 or anos_lagunas > anos_totales:
            return []

        if distribucion == "Aleatorio":
            rng = np.random.default_rng()
            return rng.choice(anos_totales, anos_lagunas, replace=False).tolist()
        if distribucion == "Inicio de carrera":
            return list(range(min(anos_lagunas, anos_totales)))
        if distribucion == "Mitad de carrera":
            inicio = max(0, (anos_totales // 2) - (anos_lagunas // 2))
            return list(range(inicio, min(anos_totales, inicio + anos_lagunas)))
        if distribucion == "Final de carrera":
            return list(range(max(0, anos_totales - anos_lagunas), anos_totales))
        return []

    # ------------------------------------------------------------------
    # Utilidades
    # ------------------------------------------------------------------

    @staticmethod
    def _error(mensaje: str) -> Dict:
        return {
            "error": True,
            "mensaje": mensaje,
            "saldo_final_nominal": 0.0,
            "pension_rp_nominal": 0.0,
        }
