"""
Modulo de simulacion Monte Carlo para analisis de riesgo previsional.

Implementa dos enfoques:
    MonteCarloSimulator -- simulacion vectorizada de alta performance
        que evita el bucle Python por escenario; util para 10 000+
        iteraciones con tiempos de respuesta menores a 2 segundos.

    El metodo analizar_sensibilidad reutiliza PensionCalculator
    para analisis parametrico determinista.

Metodologia:
    Variables estocasticas por ano:
        - Rentabilidad nominal ~ Normal(mu_r, sigma_r)
        - Inflacion           ~ Normal(mu_i, sigma_i)
        - Cesantia            ~ Bernoulli(p_desempleo) por año
    Restricciones:
        - Rentabilidad nominal >= -20 % (floor)
        - Inflacion >= 0 %
"""

from typing import Dict, List, Optional

import numpy as np
import pandas as pd

from .financial_metrics import (
    tasa_mensual_equivalente,
)
from .pension_engine import PensionCalculator


class MonteCarloSimulator:
    """Simulador Monte Carlo vectorizado para el sistema AFP chileno.

    La implementacion vectorizada calcula todos los escenarios en paralelo
    usando operaciones numpy, lo que resulta en un speedup de 50-100x
    respecto a un bucle Python por escenario.

    Args:
        n_simulaciones: Numero de trayectorias a generar.
        seed: Semilla para reproducibilidad (None = aleatorio).
    """

    TASA_RETIRO_DESCUENTO: float = PensionCalculator.TASA_RETIRO_DESCUENTO
    FACTOR_RV: float = PensionCalculator.FACTOR_RENTA_VITALICIA

    def __init__(self, n_simulaciones: int = 10_000, seed: Optional[int] = 42):
        self.n_simulaciones = n_simulaciones
        self._rng = np.random.default_rng(seed)
        self._calculator = PensionCalculator()

    def simular_escenarios(
        self,
        parametros_base: Dict,
        volatilidad_rentabilidad: float = 0.02,
        volatilidad_inflacion: float = 0.01,
        prob_desempleo_anual: float = 0.05,
    ) -> Dict:
        """Simulacion vectorizada de N escenarios de acumulacion previsional.

        Genera matrices (n_simulaciones x anos_acumulacion) de rentabilidades,
        inflaciones y estados de empleo, luego calcula el saldo final de cada
        escenario sin bucles Python.

        Args:
            parametros_base: Diccionario de parametros de PensionCalculator
                con porcentajes en puntos porcentuales.
            volatilidad_rentabilidad: Desviacion estandar de rentabilidad
                anual en pp (ej: 2.0 = 2 pp sigma).
            volatilidad_inflacion: Desviacion estandar de inflacion en pp.
            prob_desempleo_anual: Probabilidad de cesantia cada año [0, 1].

        Returns:
            Diccionario con estadisticas por percentil y distribuciones completas.
        """
        p = parametros_base
        n = p["edad_jubilacion"] - p["edad_actual"]
        n_ret = p["esperanza_vida"] - p["edad_jubilacion"]
        n_sim = self.n_simulaciones

        if n <= 0 or n_ret <= 0:
            return {}

        # --- Variables estocasticas ------------------------------------------
        r_nom_base = p["rentabilidad_nominal"] / 100.0
        r_inf_base = p["inflacion_esperada"] / 100.0
        vol_r = volatilidad_rentabilidad / 100.0
        vol_i = volatilidad_inflacion / 100.0

        # Matrices (n_sim, n_anos)
        rentabilidades = self._rng.normal(r_nom_base, vol_r, (n_sim, n))
        rentabilidades = np.maximum(rentabilidades, -0.20)

        inflaciones = self._rng.normal(r_inf_base, vol_i, (n_sim, n))
        inflaciones = np.maximum(inflaciones, 0.0)

        cesantia = self._rng.binomial(1, prob_desempleo_anual, (n_sim, n)).astype(bool)

        # --- Flujos deterministas (salario, cotizaciones) ----------------------
        cot_total = (p["cotizacion_obligatoria"] + p["cotizacion_voluntaria"]) / 100.0
        comision = p["comision_afp"] / 100.0
        emp_contrib = p["aporte_empleador"] / 100.0
        aumento = p["aumento_salarial_anual"] / 100.0
        tope_clp = PensionCalculator.TOPE_IMPONIBLE_UF * p.get("valor_uf", 37_500.0)

        anos_idx = np.arange(n)
        salarios = p["ingreso_mensual"] * (1.0 + aumento) ** anos_idx
        imponibles = np.minimum(salarios, tope_clp)

        neto_base = imponibles * (cot_total + emp_contrib - comision) * 12.0

        # Por escenario: neto = 0 si hay cesantia
        netos = neto_base[np.newaxis, :] * (~cesantia)

        # --- Calculo vectorizado de saldo -------------------------------------
        # B_n = sum_{t=0}^{n-1} C_t * prod_{s=t+1}^{n-1}(1+r_s)
        # Se usa la relacion: B_n = cumprod_total * sum(C_t / cumprod_t)

        growth = 1.0 + rentabilidades
        cumprod = np.cumprod(growth, axis=1)

        # divisor para cada t: producto de (1+r) desde 0 hasta t-1
        divisors = np.ones((n_sim, n))
        divisors[:, 1:] = cumprod[:, :-1]

        saldos_nominales = cumprod[:, -1] * np.sum(netos / divisors, axis=1)

        # Deflactar para saldo real (pesos constantes año 0)
        inflacion_acum = np.prod(1.0 + inflaciones, axis=1)
        saldos_reales = saldos_nominales / inflacion_acum

        # --- Pensiones -------------------------------------------------------
        meses_retiro = n_ret * 12
        r_ret = self.TASA_RETIRO_DESCUENTO
        r_m = tasa_mensual_equivalente(r_ret)
        factor_pmt = r_m / (1.0 - (1.0 + r_m) ** (-meses_retiro)) if r_m > 0 else 1.0 / meses_retiro

        pensiones_rp = saldos_nominales * factor_pmt
        pensiones_rv = pensiones_rp * self.FACTOR_RV

        # Tasas de reemplazo
        ultimo_sueldo = float(salarios[-1])
        tasas_reemplazo = pensiones_rp / ultimo_sueldo * 100.0 if ultimo_sueldo else np.zeros(n_sim)

        return {
            "pension_rp": self._estadisticas(pensiones_rp),
            "pension_rv": self._estadisticas(pensiones_rv),
            "saldo_final": self._estadisticas(saldos_nominales),
            "saldo_real": self._estadisticas(saldos_reales),
            "tasa_reemplazo": self._estadisticas(tasas_reemplazo),
            "distribucion_completa": {
                "pension_rp": pensiones_rp.tolist(),
                "saldo_nominal": saldos_nominales.tolist(),
                "saldo_real": saldos_reales.tolist(),
                "tasa_reemplazo": tasas_reemplazo.tolist(),
            },
            "n_simulaciones": n_sim,
        }

    def analizar_sensibilidad(
        self,
        parametros_base: Dict,
        parametro_variable: str,
        rango_valores: List[float],
    ) -> pd.DataFrame:
        """Analisis de sensibilidad determinista de un parametro.

        Ejecuta la calculadora con cada valor del rango y registra los
        resultados clave. Util para graficar el impacto de cambios en
        rentabilidad, cotizacion, edad de jubilacion, etc.

        Args:
            parametros_base: Parametros base de la simulacion.
            parametro_variable: Nombre del parametro a variar
                (clave del diccionario de parametros).
            rango_valores: Secuencia de valores a evaluar.

        Returns:
            DataFrame con columnas: valor_parametro, pension_rp,
            pension_rv, saldo_final, tasa_reemplazo, tir.
        """
        filas = []
        for valor in rango_valores:
            params = {**parametros_base, parametro_variable: valor}
            resultado = self._calculator.calcular_pension_completa(**params)
            if "error" not in resultado:
                filas.append({
                    "valor_parametro": valor,
                    "parametro": parametro_variable,
                    "pension_rp": resultado["pension_rp_nominal"],
                    "pension_rv": resultado["pension_rv_nominal"],
                    "saldo_final": resultado["saldo_final_nominal"],
                    "tasa_reemplazo": resultado["tasa_reemplazo_rp"],
                    "tir": resultado["tir_cotizaciones"],
                })
        return pd.DataFrame(filas)

    def comparar_escenarios(
        self,
        escenario_base: Dict,
        escenarios_alternativos: List[Dict],
    ) -> pd.DataFrame:
        """Comparacion de multiples escenarios deterministas.

        Args:
            escenario_base: Parametros del escenario de referencia.
            escenarios_alternativos: Lista de diccionarios con claves
                'nombre' (str) y 'parametros' (dict de overrides).

        Returns:
            DataFrame con una fila por escenario y las metricas clave.
        """
        filas = []

        res_base = self._calculator.calcular_pension_completa(**escenario_base)
        if "error" not in res_base:
            filas.append({
                "escenario": "Base",
                "pension_rp": res_base["pension_rp_nominal"],
                "pension_rv": res_base["pension_rv_nominal"],
                "saldo_final": res_base["saldo_final_nominal"],
                "tasa_reemplazo": res_base["tasa_reemplazo_rp"],
                "tir": res_base["tir_cotizaciones"],
                "duracion_anos": res_base["duracion_macaulay_anos"],
            })

        for i, escenario in enumerate(escenarios_alternativos, 1):
            params = {**escenario_base, **escenario.get("parametros", {})}
            resultado = self._calculator.calcular_pension_completa(**params)
            if "error" not in resultado:
                filas.append({
                    "escenario": escenario.get("nombre", f"Alternativa {i}"),
                    "pension_rp": resultado["pension_rp_nominal"],
                    "pension_rv": resultado["pension_rv_nominal"],
                    "saldo_final": resultado["saldo_final_nominal"],
                    "tasa_reemplazo": resultado["tasa_reemplazo_rp"],
                    "tir": resultado["tir_cotizaciones"],
                    "duracion_anos": resultado["duracion_macaulay_anos"],
                })

        return pd.DataFrame(filas)

    # ------------------------------------------------------------------
    # Utilidades internas
    # ------------------------------------------------------------------

    @staticmethod
    def _estadisticas(arr: np.ndarray) -> Dict:
        """Estadisticas descriptivas con percentiles estandar."""
        percentiles = np.percentile(arr, [5, 10, 25, 50, 75, 90, 95])
        return {
            "percentil_5": float(percentiles[0]),
            "percentil_10": float(percentiles[1]),
            "percentil_25": float(percentiles[2]),
            "mediana": float(percentiles[3]),
            "percentil_75": float(percentiles[4]),
            "percentil_90": float(percentiles[5]),
            "percentil_95": float(percentiles[6]),
            "promedio": float(np.mean(arr)),
            "desviacion_std": float(np.std(arr)),
            "minimo": float(np.min(arr)),
            "maximo": float(np.max(arr)),
            "coef_variacion": float(np.std(arr) / np.mean(arr)) if np.mean(arr) != 0 else 0.0,
        }
