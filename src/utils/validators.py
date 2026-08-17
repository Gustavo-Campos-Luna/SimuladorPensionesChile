"""
Validadores de parametros de entrada para el simulador previsional.

Cada funcion retorna una tupla (valido: bool, mensaje: str | None).
La funcion validar_simulacion_completa agrupa todas las validaciones
en una sola llamada para uso desde la interfaz Streamlit.
"""

from typing import Dict, Optional, Tuple

# Tipo de retorno estandar de todos los validadores
ValidationResult = Tuple[bool, Optional[str]]


# ---------------------------------------------------------------------------
# Validadores individuales
# ---------------------------------------------------------------------------

def validar_edad(
    edad: int,
    minima: int = 18,
    maxima: int = 75,
) -> ValidationResult:
    """Verifica que la edad este dentro del rango previsional valido.

    Args:
        edad: Edad en anos.
        minima: Limite inferior (default 18, inicio vida laboral).
        maxima: Limite superior (default 75).
    """
    if edad < minima:
        return False, f"La edad debe ser al menos {minima} anos."
    if edad > maxima:
        return False, f"La edad no puede superar {maxima} anos."
    return True, None


def validar_rango_jubilacion(
    edad_actual: int,
    edad_jubilacion: int,
    esperanza_vida: int,
) -> ValidationResult:
    """Verifica la coherencia cronologica de las tres edades.

    Args:
        edad_actual: Edad del cotizante hoy.
        edad_jubilacion: Edad objetivo de jubilacion.
        esperanza_vida: Esperanza de vida proyectada.
    """
    if edad_jubilacion <= edad_actual:
        return False, "La edad de jubilacion debe ser mayor a la edad actual."
    if esperanza_vida <= edad_jubilacion:
        return False, "La esperanza de vida debe superar la edad de jubilacion."
    if edad_jubilacion - edad_actual > 55:
        return False, "El horizonte de acumulacion no puede exceder 55 anos."
    return True, None


def validar_ingreso(
    ingreso: float,
    minimo: float = 350_000,
    maximo: float = 100_000_000,
) -> ValidationResult:
    """Verifica que el ingreso mensual sea economicamente plausible.

    Args:
        ingreso: Remuneracion mensual en CLP.
        minimo: Aproximacion al sueldo minimo vigente.
        maximo: Techo de referencia para entrada de datos.
    """
    if ingreso < minimo:
        return False, f"El ingreso debe ser al menos ${minimo:,.0f}.".replace(",", ".")
    if ingreso > maximo:
        return False, f"El ingreso supera el maximo permitido (${maximo:,.0f}).".replace(",", ".")
    return True, None


def validar_cotizacion(
    cotizacion_obligatoria: float,
    cotizacion_voluntaria: float = 0.0,
) -> ValidationResult:
    """Verifica que los porcentajes de cotizacion sean validos.

    La cotizacion obligatoria minima es 10 % segun la ley chilena.
    La suma total no puede superar 50 % para evitar escenarios irreales.

    Args:
        cotizacion_obligatoria: Porcentaje obligatorio (pp).
        cotizacion_voluntaria: Porcentaje voluntario adicional (pp).
    """
    if cotizacion_obligatoria < 10.0:
        return False, "La cotizacion obligatoria minima es 10 % (Decreto Ley 3.500)."
    if cotizacion_obligatoria + cotizacion_voluntaria > 50.0:
        return False, "La cotizacion total no puede superar el 50 % del ingreso."
    return True, None


def validar_rentabilidad(
    rentabilidad: float,
    minima: float = -10.0,
    maxima: float = 15.0,
) -> ValidationResult:
    """Verifica que la rentabilidad esperada sea razonable historicamente.

    Args:
        rentabilidad: Tasa anual esperada en puntos porcentuales.
        minima: Limite inferior (-10 pp, escenario muy adverso).
        maxima: Limite superior (15 pp, escenario muy optimista).
    """
    if rentabilidad < minima:
        return False, f"La rentabilidad no puede ser inferior a {minima} %."
    if rentabilidad > maxima:
        return False, f"La rentabilidad supera el maximo razonable ({maxima} %)."
    return True, None


def validar_inflacion(
    inflacion: float,
    minima: float = 0.0,
    maxima: float = 20.0,
) -> ValidationResult:
    """Verifica que la inflacion esperada sea positiva y razonable.

    Args:
        inflacion: Tasa anual en puntos porcentuales.
    """
    if inflacion < minima:
        return False, "La inflacion esperada no puede ser negativa."
    if inflacion > maxima:
        return False, f"La inflacion supera el maximo razonable ({maxima} %)."
    return True, None


def validar_lagunas(
    anos_lagunas: int,
    anos_cotizacion: int,
) -> ValidationResult:
    """Verifica que los anos de laguna sean coherentes con el horizonte.

    Se rechaza si superan el 60 % del horizonte de acumulacion para
    evitar proyecciones con saldo cero en la mayor parte del periodo.

    Args:
        anos_lagunas: Anos sin cotizacion.
        anos_cotizacion: Total de anos de acumulacion.
    """
    if anos_lagunas < 0:
        return False, "Los anos de laguna no pueden ser negativos."
    if anos_lagunas > anos_cotizacion:
        return False, "Los anos de laguna no pueden exceder el horizonte de acumulacion."
    if anos_cotizacion > 0 and anos_lagunas > anos_cotizacion * 0.6:
        return False, (
            f"Las lagunas ({anos_lagunas} anos) superan el 60 % del horizonte "
            f"({anos_cotizacion} anos). Revise los parametros."
        )
    return True, None


# ---------------------------------------------------------------------------
# Validacion agregada
# ---------------------------------------------------------------------------

def validar_simulacion_completa(parametros: Dict) -> ValidationResult:
    """Ejecuta todas las validaciones sobre el diccionario de parametros.

    Orden: edad actual, rango de edades, ingreso, cotizacion, rentabilidad,
    inflacion, lagunas. Retorna el primer error encontrado.

    Args:
        parametros: Diccionario con las claves de PensionCalculator.calcular_pension_completa().

    Returns:
        (True, None) si todos los parametros son validos; (False, mensaje) si no.
    """
    validaciones = [
        lambda: validar_edad(parametros.get("edad_actual", 0)),
        lambda: validar_rango_jubilacion(
            parametros.get("edad_actual", 0),
            parametros.get("edad_jubilacion", 0),
            parametros.get("esperanza_vida", 0),
        ),
        lambda: validar_ingreso(parametros.get("ingreso_mensual", 0)),
        lambda: validar_cotizacion(
            parametros.get("cotizacion_obligatoria", 0),
            parametros.get("cotizacion_voluntaria", 0),
        ),
        lambda: validar_rentabilidad(parametros.get("rentabilidad_nominal", 0)),
        lambda: validar_inflacion(parametros.get("inflacion_esperada", 0)),
        lambda: validar_lagunas(
            parametros.get("anos_lagunas", 0),
            parametros.get("edad_jubilacion", 0) - parametros.get("edad_actual", 0),
        ),
    ]

    for validacion in validaciones:
        valido, mensaje = validacion()
        if not valido:
            return False, mensaje

    return True, None
