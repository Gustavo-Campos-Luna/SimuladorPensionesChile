"""
Simulador de Pensiones Chile — Pagina de inicio.

Presenta los indicadores macroeconomicos vigentes y sirve como punto de
entrada a las herramientas de simulacion y analisis de riesgo.
"""

import sys
from pathlib import Path

import streamlit as st

sys.path.insert(0, str(Path(__file__).parent / "src"))

from api.data_sources import data_fetcher

st.set_page_config(
    page_title="Simulador de Pensiones — Chile",
    page_icon=None,
    layout="wide",
    initial_sidebar_state="expanded",
)

st.markdown("""
<style>
    .block-title {
        font-size: 2.2rem;
        font-weight: 700;
        color: #1E3A8A;
        margin-bottom: 0.25rem;
    }
    .block-subtitle {
        font-size: 1.05rem;
        color: #6B7280;
        margin-bottom: 2rem;
    }
    .indicator-card {
        background: #F8FAFC;
        padding: 1.25rem 1.5rem;
        border-radius: 8px;
        border-left: 4px solid #1E3A8A;
        margin: 0.5rem 0;
    }
    .indicator-card h4 {
        margin: 0 0 0.25rem 0;
        color: #1E293B;
        font-size: 0.85rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
    }
    .indicator-card p {
        margin: 0;
        font-size: 1.5rem;
        font-weight: 700;
        color: #1E3A8A;
    }
    .section-label {
        font-size: 0.75rem;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: #9CA3AF;
        margin-bottom: 0.5rem;
    }
    .footer-bar {
        border-top: 1px solid #E5E7EB;
        padding-top: 1.5rem;
        margin-top: 3rem;
        color: #9CA3AF;
        font-size: 0.85rem;
    }
</style>
""", unsafe_allow_html=True)

# ---------------------------------------------------------------------------
# Encabezado
# ---------------------------------------------------------------------------
st.markdown(
    '<div class="block-title">Simulador de Pensiones — Chile</div>',
    unsafe_allow_html=True,
)
st.markdown(
    '<div class="block-subtitle">'
    'Herramienta de analisis previsional con metricas financieras avanzadas '
    'para el sistema AFP chileno.'
    '</div>',
    unsafe_allow_html=True,
)

st.divider()

# ---------------------------------------------------------------------------
# Indicadores macroeconomicos en tiempo real
# ---------------------------------------------------------------------------
st.markdown('<div class="section-label">Indicadores vigentes</div>', unsafe_allow_html=True)

with st.spinner("Obteniendo indicadores actualizados..."):
    try:
        datos = data_fetcher.obtener_datos_completos()
        col1, col2, col3, col4 = st.columns(4)

        with col1:
            st.metric(
                label="Unidad de Fomento (UF)",
                value=f"${datos['uf']:,.2f}",
                help="Valor diario publicado por el Banco Central de Chile.",
            )
        with col2:
            st.metric(
                label="Inflacion promedio (5 anos)",
                value=f"{datos['inflacion_promedio']} %",
                help="Media aritmetica de los ultimos 5 anos segun INE.",
            )
        with col3:
            st.metric(
                label="Pension Basica Solidaria",
                value=f"${datos['pbs']:,}".replace(",", "."),
                help="PBS 2024 — Decreto Supremo N° 14.",
            )
        with col4:
            st.metric(
                label="Tope imponible",
                value=f"{datos['tope_imponible_uf']:.1f} UF",
                help=f"Equivalente a ${datos['tope_imponible_clp']:,.0f} CLP.".replace(",", "."),
            )
        st.caption(f"Ultima actualizacion: {datos['fecha_actualizacion']}")
    except Exception:
        st.info(
            "Los indicadores no estan disponibles en este momento. "
            "Se usaran valores por defecto en la simulacion."
        )

st.divider()

# ---------------------------------------------------------------------------
# Descripcion de la herramienta
# ---------------------------------------------------------------------------
col_izq, col_der = st.columns(2)

with col_izq:
    st.markdown("#### Capacidades del simulador")
    st.markdown("""
    - Proyeccion año a año con capitalizacion compuesta
    - Calculo de pension mediante formula de anualidad actuarial
    - Valor Presente Neto (VPN) y Tasa Interna de Retorno (TIR) de cotizaciones
    - Duracion de Macaulay del pasivo previsional (ALM)
    - Brecha previsional respecto al objetivo OCDE (70 % de reemplazo)
    - Analisis de lagunas previsionales por periodo de carrera
    - Comparacion Retiro Programado vs. Renta Vitalicia
    - Beneficio tributario del APV bajo Regimen A y Regimen B
    """)

with col_der:
    st.markdown("#### Analisis de riesgo")
    st.markdown("""
    - Simulacion Monte Carlo vectorizada (hasta 20.000 escenarios)
    - Rentabilidad e inflacion como variables estocasticas independientes
    - Cesantia modelada como proceso de Bernoulli por ano
    - Distribuciones con percentiles P5, P10, P25, P50, P75, P90, P95
    - Probabilidad de alcanzar pension objetivo configurable
    - Analisis de sensibilidad parametrica determinista
    - Comparador de escenarios multiples
    """)

st.divider()

# ---------------------------------------------------------------------------
# Comisiones AFP
# ---------------------------------------------------------------------------
st.markdown("#### Comisiones AFP vigentes")

try:
    comisiones = data_fetcher.obtener_comisiones_afp()
    cols = st.columns(len(comisiones))
    for col, (afp, comision) in zip(cols, comisiones.items()):
        with col:
            st.markdown(
                f'<div class="indicator-card">'
                f'<h4>{afp}</h4>'
                f'<p>{comision} %</p>'
                f'</div>',
                unsafe_allow_html=True,
            )
    st.caption("Fuente: Superintendencia de Pensiones. Comision sobre remuneracion imponible.")
except Exception:
    st.info("Informacion de comisiones no disponible. Se usara valor por defecto.")

st.divider()

# ---------------------------------------------------------------------------
# Navegacion
# ---------------------------------------------------------------------------
st.markdown("#### Herramientas disponibles")
st.info(
    "Utilice el menu lateral para acceder a:\n\n"
    "**Simulador** — Proyeccion personalizada de pension con metricas avanzadas.\n\n"
    "**Analisis de Riesgo** — Simulacion Monte Carlo y distribuciones probabilisticas.\n\n"
    "**Metodologia** — Descripcion de formulas, supuestos y fuentes de datos."
)

# ---------------------------------------------------------------------------
# Disclaimer legal
# ---------------------------------------------------------------------------
st.divider()
st.warning(
    "**Aviso legal**\n\n"
    "Esta herramienta tiene caracter estrictamente educativo e informativo. "
    "Los resultados son estimaciones basadas en los supuestos ingresados y "
    "no constituyen asesoria financiera ni previsional. La rentabilidad pasada "
    "no garantiza rentabilidad futura. Se recomienda complementar el analisis "
    "con un asesor previsional certificado.\n\n"
    "Fuentes: Banco Central de Chile, Superintendencia de Pensiones, CMF, INE."
)

# ---------------------------------------------------------------------------
# Footer
# ---------------------------------------------------------------------------
st.markdown(
    '<div class="footer-bar">'
    'Simulador de Pensiones Chile &mdash; '
    'Desarrollado con Python y Streamlit &mdash; '
    'Datos actualizados desde APIs publicas oficiales'
    '</div>',
    unsafe_allow_html=True,
)
