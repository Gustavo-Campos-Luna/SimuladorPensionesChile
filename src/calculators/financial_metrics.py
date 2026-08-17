"""
Modulo de metricas financieras avanzadas para el sistema previsional chileno.

Implementa calculo de VPN, TIR, rentabilidad real, duracion de pasivo
previsional, beneficios tributarios APV y anualidades actuariales.
"""

from typing import Dict, List, Optional

import numpy as np
from scipy.optimize import brentq, newton

# Unidad Tributaria Mensual (UTM) vigente, en CLP.
# Fuente: Servicio de Impuestos Internos (sii.cl/valores_y_fechas/utm).
# Se reajusta el 1 de cada mes segun IPC del mes anterior.
# Valor vigente a agosto de 2026: actualizar manualmente cuando corresponda.
_UTM_CLP: float = 71_649.0


# ---------------------------------------------------------------------------
# Tasas y conversion
# ---------------------------------------------------------------------------

def calcular_rentabilidad_real(rentabilidad_nominal: float, inflacion: float) -> float:
    """Rentabilidad real mediante la ecuacion de Fisher.

    Args:
        rentabilidad_nominal: Tasa nominal anual como decimal (0.06 = 6 %).
        inflacion: Tasa de inflacion anual como decimal.

    Returns:
        Rentabilidad real anual como decimal.
    """
    return (1.0 + rentabilidad_nominal) / (1.0 + inflacion) - 1.0


def tasa_mensual_equivalente(tasa_anual: float) -> float:
    """Tasa mensual equivalente de capitalizacion compuesta.

    Args:
        tasa_anual: Tasa anual efectiva como decimal.

    Returns:
        Tasa mensual equivalente.
    """
    return (1.0 + tasa_anual) ** (1.0 / 12.0) - 1.0


# ---------------------------------------------------------------------------
# Valor presente y anualidades
# ---------------------------------------------------------------------------

def calcular_vpn(flujos: List[float], tasa_descuento: float) -> float:
    """Valor Presente Neto de una serie de flujos de caja anuales.

    Args:
        flujos: Secuencia de flujos netos anuales (cotizaciones netas).
        tasa_descuento: Tasa de descuento anual como decimal.

    Returns:
        Valor presente neto en CLP.
    """
    periodos = np.arange(1, len(flujos) + 1)
    factores = (1.0 + tasa_descuento) ** periodos
    return float(np.sum(np.array(flujos) / factores))


def calcular_valor_presente_pension(
    pension_mensual: float,
    anos_pension: int,
    tasa_descuento_anual: float,
    inflacion_anual: float,
) -> float:
    """Valor presente de la corriente de pensiones futuras ajustada por inflacion.

    Utiliza vectorizacion numpy en lugar de un bucle Python para mayor eficiencia.

    Args:
        pension_mensual: Pension mensual nominal al inicio del retiro.
        anos_pension: Periodo esperado de retiro en anos.
        tasa_descuento_anual: Tasa de descuento anual como decimal.
        inflacion_anual: Inflacion anual esperada como decimal.

    Returns:
        Valor presente total de la corriente de pensiones en CLP.
    """
    meses = np.arange(1, anos_pension * 12 + 1)
    pensiones_ajustadas = pension_mensual * (1.0 + inflacion_anual) ** (meses / 12.0)
    factores_descuento = (1.0 + tasa_descuento_anual) ** (meses / 12.0)
    return float(np.sum(pensiones_ajustadas / factores_descuento))


def calcular_pension_retiro_programado(
    saldo: float,
    meses_retiro: int,
    tasa_descuento_anual: float,
) -> float:
    """Pension mensual bajo Retiro Programado mediante formula de anualidad.

    Calcula el pago mensual constante (en terminos reales) que agota el saldo
    en exactamente meses_retiro periodos, descontado a la tasa real de retorno.
    Esta formula es mas precisa que la division simple (que implica tasa = 0).

    PMT = PV * r / (1 - (1 + r)^-n)

    Args:
        saldo: Saldo acumulado al momento de jubilar (CLP).
        meses_retiro: Meses esperados de pension (esperanza de vida - edad jubilacion).
        tasa_descuento_anual: Tasa anual efectiva para el periodo de retiro.

    Returns:
        Pension mensual en CLP.
    """
    if meses_retiro <= 0:
        return 0.0

    r = tasa_mensual_equivalente(tasa_descuento_anual)

    if r <= 0.0:
        return saldo / meses_retiro

    return saldo * r / (1.0 - (1.0 + r) ** (-meses_retiro))


# ---------------------------------------------------------------------------
# Duracion del pasivo previsional
# ---------------------------------------------------------------------------

def calcular_duracion_macaulay(
    meses_retiro: int,
    tasa_descuento_anual: float,
) -> float:
    """Duracion de Macaulay de la corriente de pensiones (en anos).

    Mide la sensibilidad del valor presente de la pension a cambios en la
    tasa de descuento. Relevante para ALM (Asset-Liability Management).

    Args:
        meses_retiro: Numero total de meses de pension.
        tasa_descuento_anual: Tasa de descuento anual como decimal.

    Returns:
        Duracion de Macaulay en anos.
    """
    r = tasa_mensual_equivalente(tasa_descuento_anual)
    meses = np.arange(1, meses_retiro + 1)

    if r <= 0.0:
        pesos = np.ones(meses_retiro) / meses_retiro
    else:
        factores = (1.0 + r) ** (-meses)
        pesos = factores / factores.sum()

    duracion_meses = float(np.dot(meses, pesos))
    return duracion_meses / 12.0


def calcular_dv01_pension(
    saldo: float,
    meses_retiro: int,
    tasa_descuento_anual: float,
) -> float:
    """Sensibilidad del valor presente a un punto base (DV01).

    Mide cuanto cambia el valor presente de la corriente de pensiones
    ante un incremento de 1 punto base (0.01 %) en la tasa de descuento,
    manteniendo la pension mensual fija al nivel calculado con la tasa base.

    DV01 = |VP(r) - VP(r + 1bp)|   con pension mensual constante.

    Args:
        saldo: Saldo acumulado al momento de jubilar.
        meses_retiro: Meses esperados de pension.
        tasa_descuento_anual: Tasa de descuento anual como decimal.

    Returns:
        DV01 en CLP por punto base.
    """
    anos_retiro = meses_retiro // 12
    if anos_retiro <= 0:
        return 0.0

    delta = 0.0001
    pension_fija = calcular_pension_retiro_programado(saldo, meses_retiro, tasa_descuento_anual)

    # VP con pension fija y tasa base
    meses = np.arange(1, meses_retiro + 1)
    vp_base = float(np.sum(pension_fija / (1.0 + tasa_descuento_anual) ** (meses / 12.0)))

    # VP con pension fija y tasa bumpeada
    vp_bump = float(np.sum(pension_fija / (1.0 + tasa_descuento_anual + delta) ** (meses / 12.0)))

    return abs(vp_bump - vp_base)


# ---------------------------------------------------------------------------
# TIR
# ---------------------------------------------------------------------------

def calcular_tir(flujos: List[float], valor_terminal: float = 0.0) -> float:
    """Tasa Interna de Retorno de la inversion previsional.

    Encuentra la tasa r que iguala el valor presente de las cotizaciones
    al valor presente del saldo acumulado terminal:

        sum( C_t / (1+r)^t ) = valor_terminal / (1+r)^n

    Usa Brent con fallback a Newton-Raphson.

    Args:
        flujos: Cotizaciones netas anuales en CLP (flujos positivos).
        valor_terminal: Saldo final acumulado en CLP al año n.
            Si es 0, la funcion intenta encontrar la TIR igualando los flujos a cero.

    Returns:
        TIR anual como decimal. Retorna 0.0 si no converge.
    """
    arr = np.array(flujos, dtype=float)
    n = len(arr)
    periodos = np.arange(1, n + 1)

    def ecuacion(r: float) -> float:
        if r <= -1.0:
            return np.inf
        with np.errstate(over="ignore", invalid="ignore"):
            factores = (1.0 + r) ** periodos
            vp_flujos = float(np.sum(arr / factores))
            vp_terminal = valor_terminal / (1.0 + r) ** n if valor_terminal else 0.0
        return vp_flujos - vp_terminal

    try:
        fa, fb = ecuacion(0.0), ecuacion(3.0)
        if np.sign(fa) != np.sign(fb):
            tir = brentq(ecuacion, 0.0, 3.0, xtol=1e-8, maxiter=500)
            return float(tir) if np.isfinite(tir) else 0.0
    except ValueError:
        pass

    try:
        tir = newton(ecuacion, x0=0.05, tol=1e-8, maxiter=300)
        if np.isfinite(tir) and abs(ecuacion(tir)) < 1e3:
            return float(tir)
    except (RuntimeError, ValueError):
        pass

    return 0.0


# ---------------------------------------------------------------------------
# Brecha previsional
# ---------------------------------------------------------------------------

def calcular_brecha_previsional(
    pension_esperada: float,
    pension_deseada: float,
    anos_faltantes: int,
    rentabilidad_anual: float,
    anos_retiro: int = 20,
) -> Dict[str, float]:
    """Capital y cotizacion adicional para cerrar la brecha previsional.

    Determina cuanto ahorro adicional mensual se requiere para alcanzar
    la pension objetivo (por defecto 70 % del ultimo sueldo, norma OCDE).

    Args:
        pension_esperada: Pension mensual proyectada con parametros actuales.
        pension_deseada: Pension mensual objetivo.
        anos_faltantes: Anos hasta la jubilacion.
        rentabilidad_anual: Rentabilidad nominal esperada como decimal.
        anos_retiro: Anos estimados de pension (para capitalizacion adicional).

    Returns:
        Diccionario con brecha_mensual, ahorro_total_necesario y flags.
    """
    if pension_esperada >= pension_deseada:
        return {
            "brecha_mensual": 0.0,
            "ahorro_total_necesario": 0.0,
            "hay_brecha": False,
            "brecha_pension_mensual": 0.0,
        }

    brecha_pension = pension_deseada - pension_esperada

    r_m = tasa_mensual_equivalente(rentabilidad_anual)
    n_retiro = anos_retiro * 12
    capital_adicional = (
        brecha_pension / r_m * (1.0 - (1.0 + r_m) ** (-n_retiro))
        if r_m > 0
        else brecha_pension * n_retiro
    )

    n_acum = anos_faltantes * 12
    if n_acum > 0 and r_m > 0:
        cotizacion_mensual = capital_adicional * r_m / ((1.0 + r_m) ** n_acum - 1.0)
    elif n_acum > 0:
        cotizacion_mensual = capital_adicional / n_acum
    else:
        cotizacion_mensual = 0.0

    return {
        "brecha_mensual": cotizacion_mensual,
        "ahorro_total_necesario": capital_adicional,
        "hay_brecha": True,
        "brecha_pension_mensual": brecha_pension,
    }


# ---------------------------------------------------------------------------
# APV - Ahorro Previsional Voluntario
# ---------------------------------------------------------------------------

def calcular_apv_beneficio_tributario(
    monto_apv_mensual: float,
    regimen: str,
    tasa_impositiva_marginal: float,
    valor_uf: float,
    anos_acumulacion: int,
    rentabilidad_anual: float,
    valor_utm: Optional[float] = None,
) -> Dict[str, float]:
    """Beneficio tributario del APV segun regimen elegido.

    Regimen A: El Fisco deposita directamente el 15 % del APV en la cuenta,
    con tope de 6 UTM anuales. El retiro tributa con tasa de 43 %.

    Regimen B: Las cotizaciones se descuentan de la base imponible (ahorro
    fiscal = monto x tasa marginal del cotizante). El retiro tributa como
    renta ordinaria.

    Para ahorradores con tasa marginal > 15 %, Regimen B es superior.

    Args:
        monto_apv_mensual: Ahorro voluntario mensual en CLP.
        regimen: 'A' o 'B'.
        tasa_impositiva_marginal: Tasa impositiva marginal actual (0.35 = 35 %).
        valor_uf: Valor vigente de la UF en CLP.
        anos_acumulacion: Anos de acumulacion hasta jubilacion.
        rentabilidad_anual: Rentabilidad nominal esperada del fondo.
        valor_utm: Valor vigente de la UTM en CLP. Si es None, usa _UTM_CLP
            (valor de referencia agosto 2026; se recomienda pasar el valor
            vigente obtenido dinamicamente via data_sources.obtener_utm_actual()).

    Returns:
        Diccionario con beneficio_anual, capital_acumulado_extra y comparacion.
    """
    utm = valor_utm if valor_utm is not None else _UTM_CLP
    tope_anual_uf = 600.0
    tope_anual_clp = tope_anual_uf * valor_uf
    aporte_anual = min(monto_apv_mensual * 12, tope_anual_clp)

    r_m = tasa_mensual_equivalente(rentabilidad_anual)
    n = anos_acumulacion * 12
    factor_acumulacion = ((1.0 + r_m) ** n - 1.0) / r_m if r_m > 0 else n

    capital_con_apv = aporte_anual / 12 * factor_acumulacion

    if regimen.upper() == "A":
        # Tope: 15% del ahorro anual, hasta un maximo de 6 UTM al ano (no 6 UTM x 12).
        subsidio_estatal_anual = min(aporte_anual * 0.15, 6 * utm)
        beneficio_anual = subsidio_estatal_anual
        capital_extra = subsidio_estatal_anual / 12 * factor_acumulacion
        descripcion = "Subsidio estatal directo del 15% sobre aportes (tope 6 UTM anuales)"
    else:
        ahorro_tributario_anual = aporte_anual * tasa_impositiva_marginal
        beneficio_anual = ahorro_tributario_anual
        capital_extra = ahorro_tributario_anual / 12 * factor_acumulacion
        descripcion = f"Deduccion base imponible a tasa marginal {tasa_impositiva_marginal*100:.0f}%"

    return {
        "regimen": regimen.upper(),
        "aporte_anual": aporte_anual,
        "beneficio_anual": beneficio_anual,
        "capital_acumulado_apv": capital_con_apv,
        "capital_extra_por_beneficio": capital_extra,
        "descripcion": descripcion,
        "conviene_regimen_b": tasa_impositiva_marginal > 0.15,
    }


def comparar_regimenes_apv(
    monto_apv_mensual: float,
    tasa_impositiva_marginal: float,
    valor_uf: float,
    anos_acumulacion: int,
    rentabilidad_anual: float,
    valor_utm: Optional[float] = None,
) -> Dict[str, Dict]:
    """Compara Regimen A y B del APV para el perfil del cotizante.

    Args:
        monto_apv_mensual: Monto mensual de APV en CLP.
        tasa_impositiva_marginal: Tasa marginal del cotizante.
        valor_uf: Valor de la UF en CLP.
        anos_acumulacion: Anos hasta jubilacion.
        rentabilidad_anual: Rentabilidad esperada del fondo APV.
        valor_utm: Valor vigente de la UTM en CLP (tope del subsidio Regimen A).
            Si es None, usa el valor de referencia interno.

    Returns:
        Diccionario con resultados de ambos regimenes y recomendacion.
    """
    resultado_a = calcular_apv_beneficio_tributario(
        monto_apv_mensual, "A", tasa_impositiva_marginal,
        valor_uf, anos_acumulacion, rentabilidad_anual, valor_utm,
    )
    resultado_b = calcular_apv_beneficio_tributario(
        monto_apv_mensual, "B", tasa_impositiva_marginal,
        valor_uf, anos_acumulacion, rentabilidad_anual, valor_utm,
    )

    recomendado = "B" if resultado_b["beneficio_anual"] > resultado_a["beneficio_anual"] else "A"

    return {
        "regimen_a": resultado_a,
        "regimen_b": resultado_b,
        "regimen_recomendado": recomendado,
        "diferencia_capital": abs(
            resultado_b["capital_extra_por_beneficio"]
            - resultado_a["capital_extra_por_beneficio"]
        ),
    }


# ---------------------------------------------------------------------------
# Utilidades de conversion
# ---------------------------------------------------------------------------

def clp_a_uf(monto_clp: float, valor_uf: float) -> float:
    """Convierte pesos chilenos a UF.

    Args:
        monto_clp: Monto en CLP.
        valor_uf: Valor vigente de la UF en CLP.

    Returns:
        Equivalente en UF. Retorna 0.0 si valor_uf es cero.
    """
    return monto_clp / valor_uf if valor_uf > 0.0 else 0.0


def deflactar_serie(valores_nominales: np.ndarray, inflacion_anual: float) -> np.ndarray:
    """Deflacta una serie nominal a pesos constantes del periodo inicial.

    Args:
        valores_nominales: Array de valores nominales (uno por ano).
        inflacion_anual: Inflacion anual constante como decimal.

    Returns:
        Array de valores en pesos constantes del ano 0.
    """
    anos = np.arange(1, len(valores_nominales) + 1)
    deflactores = (1.0 + inflacion_anual) ** anos
    return valores_nominales / deflactores
