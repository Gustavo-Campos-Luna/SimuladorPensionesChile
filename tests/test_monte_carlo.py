"""Tests for MonteCarloSimulator: output shape/statistics sanity,
reproducibility with a fixed seed, and the n<=0 edge case.
"""

import numpy as np
import pytest

from calculators.monte_carlo import MonteCarloSimulator

BASE_PARAMS = dict(
    edad_actual=30,
    edad_jubilacion=65,
    esperanza_vida=85,
    ingreso_mensual=1_000_000.0,
    aumento_salarial_anual=2.0,
    cotizacion_obligatoria=10.0,
    comision_afp=1.44,
    aporte_empleador=0.0,
    cotizacion_voluntaria=0.0,
    rentabilidad_nominal=6.0,
    inflacion_esperada=3.0,
    valor_uf=40_000.0,
)


class TestSimularEscenarios:
    def test_zero_accumulation_years_returns_empty_dict(self):
        sim = MonteCarloSimulator(n_simulaciones=100, seed=1)
        params = {**BASE_PARAMS, "edad_jubilacion": 30}  # same as edad_actual -> n=0
        assert sim.simular_escenarios(params) == {}

    def test_percentiles_are_monotonically_non_decreasing(self):
        sim = MonteCarloSimulator(n_simulaciones=5_000, seed=42)
        result = sim.simular_escenarios(BASE_PARAMS)

        stats = result["pension_rp"]
        ordered_keys = [
            "percentil_5",
            "percentil_10",
            "percentil_25",
            "mediana",
            "percentil_75",
            "percentil_90",
            "percentil_95",
        ]
        values = [stats[k] for k in ordered_keys]
        assert values == sorted(values)

    def test_distribution_completa_matches_n_simulaciones(self):
        sim = MonteCarloSimulator(n_simulaciones=1_000, seed=7)
        result = sim.simular_escenarios(BASE_PARAMS)
        assert len(result["distribucion_completa"]["pension_rp"]) == 1_000
        assert result["n_simulaciones"] == 1_000

    def test_same_seed_is_reproducible(self):
        params = dict(BASE_PARAMS)
        sim1 = MonteCarloSimulator(n_simulaciones=500, seed=99)
        sim2 = MonteCarloSimulator(n_simulaciones=500, seed=99)
        r1 = sim1.simular_escenarios(params)
        r2 = sim2.simular_escenarios(params)
        assert r1["pension_rp"]["mediana"] == pytest.approx(r2["pension_rp"]["mediana"])

    def test_all_saldos_are_finite_and_non_negative(self):
        sim = MonteCarloSimulator(n_simulaciones=2_000, seed=3)
        result = sim.simular_escenarios(BASE_PARAMS)
        saldos = np.array(result["distribucion_completa"]["saldo_nominal"])
        assert np.isfinite(saldos).all()
        assert (saldos >= 0).all()

    def test_higher_prob_desempleo_reduces_median_balance(self):
        sim_low = MonteCarloSimulator(n_simulaciones=3_000, seed=5)
        sim_high = MonteCarloSimulator(n_simulaciones=3_000, seed=5)
        low = sim_low.simular_escenarios(BASE_PARAMS, prob_desempleo_anual=0.0)
        high = sim_high.simular_escenarios(BASE_PARAMS, prob_desempleo_anual=0.30)
        assert high["saldo_final"]["mediana"] < low["saldo_final"]["mediana"]


class TestAnalizarSensibilidad:
    def test_returns_one_row_per_value(self):
        sim = MonteCarloSimulator(n_simulaciones=100, seed=1)
        params = {**BASE_PARAMS, "anos_lagunas": 0, "distribucion_lagunas": "Aleatorio"}
        df = sim.analizar_sensibilidad(params, "rentabilidad_nominal", [3.0, 6.0, 9.0])
        assert len(df) == 3
        assert list(df["valor_parametro"]) == [3.0, 6.0, 9.0]

    def test_higher_rentabilidad_increases_pension(self):
        sim = MonteCarloSimulator(n_simulaciones=100, seed=1)
        params = {**BASE_PARAMS, "anos_lagunas": 0, "distribucion_lagunas": "Aleatorio"}
        df = sim.analizar_sensibilidad(params, "rentabilidad_nominal", [3.0, 9.0])
        assert df.iloc[1]["pension_rp"] > df.iloc[0]["pension_rp"]


class TestCompararEscenarios:
    def test_base_plus_alternatives_produces_expected_rows(self):
        sim = MonteCarloSimulator(n_simulaciones=100, seed=1)
        base = {**BASE_PARAMS, "anos_lagunas": 0, "distribucion_lagunas": "Aleatorio"}
        df = sim.comparar_escenarios(
            base,
            [{"nombre": "Optimista", "parametros": {"rentabilidad_nominal": 8.0}}],
        )
        assert list(df["escenario"]) == ["Base", "Optimista"]
