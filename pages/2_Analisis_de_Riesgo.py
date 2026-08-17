"""
Analisis de Riesgo — Simulacion Monte Carlo vectorizada.

Evalua la distribucion probabilistica de la pension bajo incertidumbre
en rentabilidad, inflacion y periodos de cesantia. Requiere ejecutar
primero una simulacion en la pagina Simulador.
"""

import sys
from pathlib import Path

import streamlit as st

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from calculators.monte_carlo import MonteCarloSimulator
from utils.formatters import formato_clp, formato_porcentaje
from visualizations.charts import PensionCharts

st.set_page_config(
    page_title="Analisis de Riesgo",
    page_icon=None,
    layout="wide",
)

st.title("Analisis de Riesgo")
st.caption("Evaluacion probabilistica mediante simulacion Monte Carlo — Sistema AFP Chile")

# ---------------------------------------------------------------------------
# Verificacion de estado
# ---------------------------------------------------------------------------
if "parametros" not in st.session_state:
    st.warning(
        "Para ejecutar el analisis de riesgo debe primero completar una simulacion "
        "en la seccion **Simulador**."
    )
    st.stop()

parametros_base = st.session_state["parametros"]

with st.expander("Parametros base de la simulacion", expanded=False):
    col1, col2, col3 = st.columns(3)
    with col1:
        st.markdown(f"""
        - Edad actual: {parametros_base["edad_actual"]} anos
        - Edad de jubilacion: {parametros_base["edad_jubilacion"]} anos
        - Esperanza de vida: {parametros_base["esperanza_vida"]} anos
        """)
    with col2:
        st.markdown(f"""
        - Ingreso mensual: {formato_clp(parametros_base["ingreso_mensual"])}
        - Rentabilidad nominal: {formato_porcentaje(parametros_base["rentabilidad_nominal"])}
        - Inflacion esperada: {formato_porcentaje(parametros_base["inflacion_esperada"])}
        """)
    with col3:
        st.markdown(f"""
        - AFP: {parametros_base.get("afp", "—")}
        - Cotizacion voluntaria: {formato_porcentaje(parametros_base["cotizacion_voluntaria"])}
        - Lagunas: {parametros_base["anos_lagunas"]} anos
        """)

# ---------------------------------------------------------------------------
# Sidebar — configuracion Monte Carlo
# ---------------------------------------------------------------------------
st.sidebar.header("Configuracion Monte Carlo")

n_simulaciones = st.sidebar.select_slider(
    "Numero de simulaciones",
    options=[1_000, 5_000, 10_000, 20_000],
    value=10_000,
    help="A mayor numero de simulaciones, mayor precision estadistica.",
)
volatilidad_rent = st.sidebar.slider(
    "Volatilidad rentabilidad (pp)",
    min_value=0.5, max_value=6.0, value=2.0, step=0.5,
    help="Desviacion estandar de la rentabilidad anual en puntos porcentuales.",
)
volatilidad_inf = st.sidebar.slider(
    "Volatilidad inflacion (pp)",
    min_value=0.2, max_value=4.0, value=1.0, step=0.2,
    help="Desviacion estandar de la inflacion anual en puntos porcentuales.",
)
prob_desempleo = st.sidebar.slider(
    "Probabilidad de cesantia anual (%)",
    min_value=0.0, max_value=20.0, value=5.0, step=1.0,
    help="Probabilidad de cada ano de no cotizar por desempleo.",
) / 100.0

st.sidebar.divider()
ejecutar = st.sidebar.button(
    "Ejecutar simulacion Monte Carlo",
    type="primary",
    use_container_width=True,
)

# ---------------------------------------------------------------------------
# Ejecucion y resultados
# ---------------------------------------------------------------------------
if ejecutar or "mc_resultados" in st.session_state:

    if ejecutar:
        with st.spinner(f"Ejecutando {n_simulaciones:,} simulaciones..."):
            simulator = MonteCarloSimulator(n_simulaciones=n_simulaciones, seed=None)
            resultados_mc = simulator.simular_escenarios(
                parametros_base=parametros_base,
                volatilidad_rentabilidad=volatilidad_rent,
                volatilidad_inflacion=volatilidad_inf,
                prob_desempleo_anual=prob_desempleo,
            )
            st.session_state["mc_resultados"] = resultados_mc
        st.success(f"Simulacion completada: {resultados_mc['n_simulaciones']:,} escenarios generados.")

    resultados_mc = st.session_state["mc_resultados"]
    stats_rp = resultados_mc["pension_rp"]

    st.divider()

    # -----------------------------------------------------------------------
    # KPIs principales
    # -----------------------------------------------------------------------
    st.subheader("Resultados probabilisticos — Pension Mensual (Retiro Programado)")

    c1, c2, c3, c4 = st.columns(4)
    with c1:
        st.metric(
            "Escenario adverso (P10)",
            formato_clp(stats_rp["percentil_10"]),
            help="10 % de probabilidad de obtener menos de este valor.",
        )
    with c2:
        st.metric(
            "Mediana (P50)",
            formato_clp(stats_rp["mediana"]),
            help="Valor central de la distribucion. Igual probabilidad por encima y por debajo.",
        )
    with c3:
        st.metric(
            "Escenario favorable (P90)",
            formato_clp(stats_rp["percentil_90"]),
            help="90 % de probabilidad de obtener menos de este valor.",
        )
    with c4:
        st.metric(
            "Promedio",
            formato_clp(stats_rp["promedio"]),
            f"+/- {formato_clp(stats_rp['desviacion_std'])}",
            help="Media aritmetica y desviacion estandar de la distribucion.",
        )

    st.divider()

    # -----------------------------------------------------------------------
    # Graficos
    # -----------------------------------------------------------------------
    charts = PensionCharts()

    col_hist, col_perc = st.columns([3, 2])

    with col_hist:
        st.subheader("Distribucion de resultados")
        st.plotly_chart(
            charts.grafico_monte_carlo(resultados_mc),
            use_container_width=True,
        )

    with col_perc:
        st.subheader("Tabla de percentiles")
        percentil_labels = ["P5", "P10", "P25", "P50", "P75", "P90", "P95"]
        percentil_keys = [
            "percentil_5", "percentil_10", "percentil_25", "mediana",
            "percentil_75", "percentil_90", "percentil_95",
        ]
        st.table({
            "Percentil": percentil_labels,
            "Pension RP": [formato_clp(stats_rp[k]) for k in percentil_keys],
            "Pension RV": [
                formato_clp(resultados_mc["pension_rv"].get(k, 0))
                for k in percentil_keys
            ],
        })

    # Rangos de confianza
    st.subheader("Rangos de confianza por percentil")
    st.plotly_chart(
        charts.grafico_fan_chart(resultados_mc, parametros_base["edad_jubilacion"]),
        use_container_width=True,
    )

    st.divider()

    # -----------------------------------------------------------------------
    # Interpretacion
    # -----------------------------------------------------------------------
    st.subheader("Interpretacion del analisis")

    p10 = stats_rp["percentil_10"]
    p50 = stats_rp["mediana"]
    p90 = stats_rp["percentil_90"]
    cv = stats_rp["coef_variacion"]
    rango_relativo = (p90 - p10) / p50 * 100 if p50 else 0

    st.info(
        f"Con los supuestos de volatilidad configurados, el rango intercuantil "
        f"P10-P90 es de {formato_clp(p10)} a {formato_clp(p90)}, "
        f"representando una amplitud del {rango_relativo:.1f} % respecto a la mediana.\n\n"
        f"El coeficiente de variacion es {cv:.2f}, indicando "
        f"{'alta' if cv > 0.3 else 'moderada' if cv > 0.15 else 'baja'} "
        f"dispersion relativa de los resultados.\n\n"
        f"Se recomienda planificar con el escenario P10-P25 para mantener un "
        f"margen de seguridad ante condiciones adversas de mercado."
    )

    # -----------------------------------------------------------------------
    # Probabilidad de alcanzar objetivo
    # -----------------------------------------------------------------------
    st.divider()
    st.subheader("Probabilidad de alcanzar pension objetivo")

    pension_objetivo = st.slider(
        "Pension objetivo mensual (CLP)",
        min_value=200_000,
        max_value=5_000_000,
        value=500_000,
        step=50_000,
        format="%d",
    )

    dist = resultados_mc["distribucion_completa"]["pension_rp"]
    prob_alcanzar = sum(1 for p in dist if p >= pension_objetivo) / len(dist) * 100

    if prob_alcanzar >= 75:
        st.success(
            f"Probabilidad de alcanzar o superar {formato_clp(pension_objetivo)}: "
            f"**{prob_alcanzar:.1f} %**"
        )
    elif prob_alcanzar >= 50:
        st.warning(
            f"Probabilidad de alcanzar o superar {formato_clp(pension_objetivo)}: "
            f"**{prob_alcanzar:.1f} %**"
        )
    else:
        st.error(
            f"Probabilidad de alcanzar o superar {formato_clp(pension_objetivo)}: "
            f"**{prob_alcanzar:.1f} %** — probabilidad insuficiente para planificacion solida."
        )

    st.divider()

    # -----------------------------------------------------------------------
    # Analisis de sensibilidad
    # -----------------------------------------------------------------------
    st.subheader("Analisis de sensibilidad parametrica")

    parametro_sens = st.selectbox(
        "Parametro a analizar",
        options=[
            ("rentabilidad_nominal", "Rentabilidad nominal (%)"),
            ("cotizacion_voluntaria", "Cotizacion voluntaria (%)"),
            ("inflacion_esperada", "Inflacion esperada (%)"),
            ("edad_jubilacion", "Edad de jubilacion"),
        ],
        format_func=lambda x: x[1],
    )
    clave, etiqueta = parametro_sens

    rango_map = {
        "rentabilidad_nominal": [float(v) for v in range(2, 12)],
        "cotizacion_voluntaria": [float(v) for v in range(0, 16, 2)],
        "inflacion_esperada": [float(v) for v in range(2, 9)],
        "edad_jubilacion": list(range(
            max(parametros_base["edad_actual"] + 2, 55),
            min(parametros_base["esperanza_vida"] - 1, 72),
        )),
    }

    if st.button("Ejecutar sensibilidad"):
        with st.spinner("Calculando..."):
            simulator_sens = MonteCarloSimulator(n_simulaciones=1)
            df_sens = simulator_sens.analizar_sensibilidad(
                parametros_base, clave, rango_map[clave]
            )
        st.plotly_chart(
            charts.grafico_sensibilidad(df_sens, etiqueta),
            use_container_width=True,
        )

else:
    st.info(
        "Configure los parametros en el panel lateral y presione "
        "**Ejecutar simulacion Monte Carlo** para obtener la distribucion probabilistica."
    )
