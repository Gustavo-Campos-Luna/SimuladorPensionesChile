"""Tests for financial_metrics.py: Fisher equation, annuity formula, TIR
convergence, APV subsidy cap, and edge cases (zero rates, no gap, zero UF).
"""

import numpy as np
import pytest

from calculators.financial_metrics import (
    calcular_apv_beneficio_tributario,
    calcular_brecha_previsional,
    calcular_duracion_macaulay,
    calcular_dv01_pension,
    calcular_pension_retiro_programado,
    calcular_rentabilidad_real,
    calcular_tir,
    calcular_vpn,
    clp_a_uf,
    comparar_regimenes_apv,
    tasa_mensual_equivalente,
)


class TestRentabilidadReal:
    def test_fisher_equation_matches_known_values(self):
        # 6% nominal, 3% inflation -> real return via exact Fisher equation.
        r_real = calcular_rentabilidad_real(0.06, 0.03)
        assert r_real == pytest.approx((1.06 / 1.03) - 1.0, rel=1e-12)

    def test_zero_inflation_equals_nominal(self):
        assert calcular_rentabilidad_real(0.05, 0.0) == pytest.approx(0.05)


class TestPensionRetiroProgramado:
    def test_zero_months_returns_zero(self):
        assert calcular_pension_retiro_programado(100_000_000, 0, 0.03) == 0.0

    def test_zero_rate_falls_back_to_simple_division(self):
        """r <= 0 must not divide by zero in the annuity formula; falls
        back to saldo / meses_retiro."""
        pension = calcular_pension_retiro_programado(120_000_000, 240, 0.0)
        assert pension == pytest.approx(120_000_000 / 240)

    def test_positive_rate_uses_annuity_formula(self):
        saldo = 100_000_000
        meses = 240
        r_anual = 0.03
        pension = calcular_pension_retiro_programado(saldo, meses, r_anual)

        r_m = tasa_mensual_equivalente(r_anual)
        expected = saldo * r_m / (1.0 - (1.0 + r_m) ** (-meses))
        assert pension == pytest.approx(expected, rel=1e-9)
        # Annuity payment must exceed the naive division (time value of money).
        assert pension > saldo / meses


class TestVPN:
    def test_vpn_of_zero_flows_is_zero(self):
        assert calcular_vpn([0.0, 0.0, 0.0], 0.05) == 0.0

    def test_vpn_discounts_future_flows(self):
        vpn = calcular_vpn([100.0], 0.10)
        assert vpn == pytest.approx(100.0 / 1.10)


class TestTIR:
    def test_recovers_known_rate_from_generated_cashflows(self):
        """Build cashflows that grow at exactly 8%/yr with a known terminal
        value, then verify calcular_tir recovers ~8%."""
        rate = 0.08
        flujos = [1_000_000.0] * 10
        # Terminal value = future value of an annuity at `rate`.
        valor_terminal = sum(f * (1 + rate) ** (10 - t) for t, f in enumerate(flujos, start=1))

        tir = calcular_tir(flujos, valor_terminal=valor_terminal)
        assert tir == pytest.approx(rate, abs=1e-4)

    def test_no_terminal_value_and_no_flows_does_not_raise(self):
        assert calcular_tir([], valor_terminal=0.0) == 0.0

    def test_non_convergent_case_returns_zero_not_raise(self):
        # Degenerate: all-zero flows and zero terminal value never cross.
        result = calcular_tir([0.0, 0.0, 0.0], valor_terminal=0.0)
        assert result == 0.0


class TestBrechaPrevisional:
    def test_no_gap_when_expected_meets_target(self):
        result = calcular_brecha_previsional(
            pension_esperada=800_000,
            pension_deseada=700_000,
            anos_faltantes=10,
            rentabilidad_anual=0.06,
        )
        assert result["hay_brecha"] is False
        assert result["brecha_mensual"] == 0.0

    def test_gap_produces_positive_required_savings(self):
        result = calcular_brecha_previsional(
            pension_esperada=400_000,
            pension_deseada=700_000,
            anos_faltantes=15,
            rentabilidad_anual=0.06,
        )
        assert result["hay_brecha"] is True
        assert result["brecha_mensual"] > 0
        assert result["ahorro_total_necesario"] > 0

    def test_zero_years_remaining_does_not_divide_by_zero(self):
        result = calcular_brecha_previsional(
            pension_esperada=400_000,
            pension_deseada=700_000,
            anos_faltantes=0,
            rentabilidad_anual=0.06,
        )
        assert result["hay_brecha"] is True
        assert result["brecha_mensual"] == 0.0


class TestApvBeneficioTributario:
    """Regression tests for the subsidy-cap bug: the code used to compute
    `6 * UTM * 12` (12x too high) instead of `6 * UTM` for the Regimen A
    annual subsidy cap."""

    def test_regimen_a_subsidy_capped_at_six_utm_not_72(self):
        utm = 71_649.0
        # A large enough monthly APV that the 15% subsidy would exceed the cap.
        resultado = calcular_apv_beneficio_tributario(
            monto_apv_mensual=1_000_000,
            regimen="A",
            tasa_impositiva_marginal=0.10,
            valor_uf=40_000,
            anos_acumulacion=20,
            rentabilidad_anual=0.06,
            valor_utm=utm,
        )
        assert resultado["beneficio_anual"] == pytest.approx(6 * utm)
        # Must NOT equal the old (buggy) 12x-inflated cap.
        assert resultado["beneficio_anual"] != pytest.approx(6 * utm * 12)

    def test_regimen_a_subsidy_below_cap_is_fifteen_percent(self):
        resultado = calcular_apv_beneficio_tributario(
            monto_apv_mensual=50_000,
            regimen="A",
            tasa_impositiva_marginal=0.10,
            valor_uf=40_000,
            anos_acumulacion=20,
            rentabilidad_anual=0.06,
            valor_utm=71_649.0,
        )
        assert resultado["beneficio_anual"] == pytest.approx(50_000 * 12 * 0.15)

    def test_regimen_b_scales_with_marginal_tax_rate(self):
        resultado = calcular_apv_beneficio_tributario(
            monto_apv_mensual=200_000,
            regimen="B",
            tasa_impositiva_marginal=0.23,
            valor_uf=40_000,
            anos_acumulacion=20,
            rentabilidad_anual=0.06,
        )
        assert resultado["beneficio_anual"] == pytest.approx(200_000 * 12 * 0.23)

    def test_default_utm_used_when_not_provided(self):
        """valor_utm=None must fall back to the internal reference constant,
        not raise or silently use 0."""
        resultado = calcular_apv_beneficio_tributario(
            monto_apv_mensual=1_000_000,
            regimen="A",
            tasa_impositiva_marginal=0.10,
            valor_uf=40_000,
            anos_acumulacion=20,
            rentabilidad_anual=0.06,
        )
        assert resultado["beneficio_anual"] > 0

    def test_comparar_regimenes_recommends_b_above_fifteen_percent_bracket(self):
        comparacion = comparar_regimenes_apv(
            monto_apv_mensual=100_000,
            tasa_impositiva_marginal=0.23,
            valor_uf=40_000,
            anos_acumulacion=20,
            rentabilidad_anual=0.06,
        )
        assert comparacion["regimen_recomendado"] == "B"


class TestClpAUf:
    def test_zero_uf_returns_zero_not_error(self):
        assert clp_a_uf(1_000_000, 0.0) == 0.0

    def test_normal_conversion(self):
        assert clp_a_uf(4_000_000, 40_000) == pytest.approx(100.0)


class TestDuracionYDv01:
    def test_duracion_macaulay_bounded_by_horizon(self):
        duracion = calcular_duracion_macaulay(240, 0.03)
        assert 0 < duracion <= 20.0

    def test_dv01_zero_when_no_retirement_years(self):
        assert calcular_dv01_pension(100_000_000, 0, 0.03) == 0.0

    def test_dv01_positive_for_normal_case(self):
        dv01 = calcular_dv01_pension(100_000_000, 240, 0.03)
        assert dv01 > 0
        assert np.isfinite(dv01)
