"""Tests for PensionCalculator: parameter validation, PBS floor override
(regression for the stale-hardcoded-PBS bug), and laguna distribution logic.
"""

import pytest

from calculators.pension_engine import PensionCalculator


@pytest.fixture
def calc() -> PensionCalculator:
    return PensionCalculator()


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
    anos_lagunas=0,
    distribucion_lagunas="Inicio de carrera",
)


class TestParameterValidation:
    def test_edad_jubilacion_before_edad_actual_returns_error(self, calc):
        params = {**BASE_PARAMS, "edad_jubilacion": 25}
        result = calc.calcular_pension_completa(**params)
        assert result.get("error") is True

    def test_esperanza_vida_before_jubilacion_returns_error(self, calc):
        params = {**BASE_PARAMS, "esperanza_vida": 60}
        result = calc.calcular_pension_completa(**params)
        assert result.get("error") is True

    def test_valid_params_produce_no_error(self, calc):
        result = calc.calcular_pension_completa(**BASE_PARAMS)
        assert "error" not in result
        assert result["saldo_final_nominal"] > 0


class TestPbsFloor:
    """Regression tests: calcular_pension_completa() used to always apply
    the stale hardcoded PBS_CLP (2024 value) regardless of the real
    current value fetched from data_sources. The `pbs` parameter must
    let callers override it."""

    def test_pbs_override_is_used_when_provided(self, calc):
        # A PBS far above any realistic computed pension forces the floor to bind.
        pbs_alto = 900_000_000
        result = calc.calcular_pension_completa(**BASE_PARAMS, pbs=pbs_alto)
        assert result["pension_rp_con_pbs"] == pbs_alto
        assert result["pbs_usado"] == pbs_alto

    def test_pbs_none_falls_back_to_class_constant(self, calc):
        result = calc.calcular_pension_completa(**BASE_PARAMS, pbs=None)
        assert result["pbs_usado"] == PensionCalculator.PBS_CLP

    def test_different_pbs_values_change_the_floor(self, calc):
        low = calc.calcular_pension_completa(**BASE_PARAMS, pbs=100_000)
        high = calc.calcular_pension_completa(**BASE_PARAMS, pbs=900_000_000)
        assert low["pension_rp_con_pbs"] != high["pension_rp_con_pbs"]
        assert high["pension_rp_con_pbs"] == 900_000_000


class TestLagunaDistribution:
    def test_zero_lagunas_matches_full_contribution_years(self, calc):
        result = calc.calcular_pension_completa(**{**BASE_PARAMS, "anos_lagunas": 0})
        assert result["anos_cotizacion_efectivos"] == result["anos_cotizacion"]

    def test_lagunas_reduce_effective_contribution_years(self, calc):
        result = calc.calcular_pension_completa(**{**BASE_PARAMS, "anos_lagunas": 5})
        assert result["anos_cotizacion_efectivos"] == result["anos_cotizacion"] - 5

    @pytest.mark.parametrize(
        "distribucion",
        ["Inicio de carrera", "Mitad de carrera", "Final de carrera", "Aleatorio"],
    )
    def test_all_distribution_patterns_run_without_error(self, calc, distribucion):
        params = {**BASE_PARAMS, "anos_lagunas": 3, "distribucion_lagunas": distribucion}
        result = calc.calcular_pension_completa(**params)
        assert "error" not in result

    def test_lagunas_reduce_final_balance_vs_no_lagunas(self, calc):
        sin_lagunas = calc.calcular_pension_completa(**{**BASE_PARAMS, "anos_lagunas": 0})
        con_lagunas = calc.calcular_pension_completa(
            **{**BASE_PARAMS, "anos_lagunas": 10, "distribucion_lagunas": "Final de carrera"}
        )
        assert con_lagunas["saldo_final_nominal"] < sin_lagunas["saldo_final_nominal"]


class TestSaldoAcumulacion:
    def test_higher_contribution_rate_yields_higher_balance(self, calc):
        base = calc.calcular_pension_completa(**{**BASE_PARAMS, "cotizacion_obligatoria": 10.0})
        alto = calc.calcular_pension_completa(**{**BASE_PARAMS, "cotizacion_obligatoria": 15.0})
        assert alto["saldo_final_nominal"] > base["saldo_final_nominal"]

    def test_real_balance_never_exceeds_nominal_with_positive_inflation(self, calc):
        result = calc.calcular_pension_completa(**BASE_PARAMS)
        assert result["saldo_final_real"] <= result["saldo_final_nominal"]

    def test_zero_income_produces_zero_balance(self, calc):
        result = calc.calcular_pension_completa(**{**BASE_PARAMS, "ingreso_mensual": 0.0})
        assert result["saldo_final_nominal"] == pytest.approx(0.0)
