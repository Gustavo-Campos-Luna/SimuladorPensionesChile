"""
Simulador principal de pensiones — Proyeccion individual con metricas avanzadas.

Permite al usuario configurar su perfil previsional y obtener una proyeccion
detallada del saldo acumulado, pension estimada, indicadores financieros
(VPN, TIR, duracion) y analisis de brecha previsional.
"""

import json
import sys
from datetime import datetime
from pathlib import Path

import pandas as pd
import streamlit as st

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from api.data_sources import data_fetcher
from calculators.financial_metrics import comparar_regimenes_apv
from calculators.pension_engine import PensionCalculator
from utils.formatters import formato_anos, formato_clp, formato_porcentaje, formato_uf
from utils.pdf_generator import PDFReportGenerator
from utils.validators import validar_simulacion_completa
from visualizations.charts import PensionCharts

# ---------------------------------------------------------------------------
# Configuracion de pagina
# ---------------------------------------------------------------------------
st.set_page_config(
    page_title="Simulador de Pensiones",
    page_icon=None,
    layout="wide",
)

st.markdown("""
<style>
    .kpi-card {
        background: linear-gradient(135deg, #1E3A8A 0%, #2563EB 100%);
        padding: 1.25rem 1.5rem;
        border-radius: 8px;
        color: #FFFFFF;
        text-align: center;
    }
    .kpi-label {
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        opacity: 0.85;
        margin-bottom: 0.5rem;
    }
    .kpi-value {
        font-size: 1.75rem;
        font-weight: 700;
        margin: 0;
        line-height: 1.1;
    }
    .kpi-sub {
        font-size: 0.8rem;
        opacity: 0.75;
        margin-top: 0.35rem;
    }
</style>
""", unsafe_allow_html=True)

# ---------------------------------------------------------------------------
# Inicializacion
# ---------------------------------------------------------------------------
calculator = PensionCalculator()
charts = PensionCharts()
pdf_generator = PDFReportGenerator()

with st.spinner("Cargando indicadores actualizados..."):
    datos = data_fetcher.obtener_datos_completos()

# ---------------------------------------------------------------------------
# Sidebar — parametros de simulacion
# ---------------------------------------------------------------------------
st.sidebar.header("Parametros de simulacion")

# Datos personales
st.sidebar.subheader("Datos personales")

edad_actual = st.sidebar.number_input(
    "Edad actual (anos)", min_value=18, max_value=70, value=30, step=1,
)
genero = st.sidebar.selectbox(
    "Genero",
    options=["Hombre", "Mujer"],
    help="La edad legal de jubilacion en Chile es 65 anos para hombres y 60 para mujeres.",
)
edad_jubilacion_default = 65 if genero == "Hombre" else 60
edad_jubilacion = st.sidebar.number_input(
    "Edad de jubilacion (anos)",
    min_value=edad_actual + 1, max_value=75,
    value=edad_jubilacion_default, step=1,
)
esperanza_vida_default = 83 if genero == "Hombre" else 89
esperanza_vida = st.sidebar.number_input(
    "Esperanza de vida (anos)",
    min_value=edad_jubilacion + 1, max_value=110,
    value=esperanza_vida_default, step=1,
    help="Tablas de mortalidad RV-2014 (SP Pensiones).",
)

# Ingresos
st.sidebar.subheader("Ingresos")

ingreso_mensual = st.sidebar.number_input(
    "Remuneracion mensual imponible (CLP)",
    min_value=int(datos["sueldo_minimo"]),
    max_value=50_000_000,
    value=1_200_000,
    step=50_000,
    help=f"Tope imponible vigente: {datos['tope_imponible_uf']:.1f} UF.",
)
aumento_salarial = st.sidebar.slider(
    "Aumento salarial anual (%)",
    min_value=0.0, max_value=8.0, value=2.0, step=0.5,
)

# Cotizaciones
st.sidebar.subheader("Cotizaciones y AFP")

cotizacion_obligatoria = st.sidebar.number_input(
    "Cotizacion obligatoria (%)",
    min_value=10.0, max_value=20.0, value=10.0, step=0.5,
    help="Minimo legal: 10 % segun DL 3.500.",
)
afps = datos["comisiones_afp"]
afp_seleccionada = st.sidebar.selectbox(
    "AFP",
    options=list(afps.keys()),
    index=list(afps.keys()).index("Modelo") if "Modelo" in afps else 0,
)
comision_afp = afps[afp_seleccionada]
st.sidebar.caption(f"Comision {afp_seleccionada}: {comision_afp} %")

cotizacion_voluntaria = st.sidebar.slider(
    "APV / Cotizacion voluntaria adicional (%)",
    min_value=0.0, max_value=20.0, value=0.0, step=0.5,
    help="Ahorro Previsional Voluntario u otro mecanismo.",
)
aporte_empleador = st.sidebar.slider(
    "Aporte adicional del empleador (%)",
    min_value=0.0, max_value=10.0, value=0.0, step=0.5,
)

# Rentabilidad e inflacion
st.sidebar.subheader("Parametros financieros")

tipo_fondo = st.sidebar.select_slider(
    "Tipo de fondo",
    options=["E", "D", "C", "B", "A"],
    value="C",
    help="A = mayor riesgo y retorno historico / E = conservador.",
)
rent_fondo = datos["rentabilidades_fondos"][tipo_fondo]
rentabilidad_sugerida = rent_fondo["rentabilidad_promedio_10a"]

rentabilidad_nominal = st.sidebar.slider(
    "Rentabilidad nominal anual (%)",
    min_value=0.0, max_value=12.0,
    value=float(rentabilidad_sugerida), step=0.5,
    help=f"Fondo {tipo_fondo}: promedio historico 10 anos = {rentabilidad_sugerida} %.",
)
inflacion_esperada = st.sidebar.slider(
    "Inflacion anual esperada (%)",
    min_value=0.0, max_value=10.0,
    value=float(datos["inflacion_promedio"]), step=0.5,
)

# Lagunas previsionales
st.sidebar.subheader("Lagunas previsionales")

anos_cotizacion_total = edad_jubilacion - edad_actual
anos_lagunas = st.sidebar.slider(
    "Anos sin cotizacion",
    min_value=0,
    max_value=min(20, anos_cotizacion_total),
    value=0, step=1,
    help="Periodos de cesantia, trabajo independiente sin cotizar, etc.",
)
distribucion_lagunas = st.sidebar.selectbox(
    "Distribucion de lagunas",
    options=["Aleatorio", "Inicio de carrera", "Mitad de carrera", "Final de carrera"],
)

st.sidebar.divider()
simular_btn = st.sidebar.button("Calcular pension", type="primary", use_container_width=True)

# ---------------------------------------------------------------------------
# Encabezado principal
# ---------------------------------------------------------------------------
st.title("Simulador de Pensiones")
st.caption("Proyeccion actuarial individualizada — Sistema AFP Chile")

# ---------------------------------------------------------------------------
# Ejecucion de la simulacion
# ---------------------------------------------------------------------------
if simular_btn or "resultados" in st.session_state:

    parametros = {
        "edad_actual": edad_actual,
        "edad_jubilacion": edad_jubilacion,
        "esperanza_vida": esperanza_vida,
        "ingreso_mensual": ingreso_mensual,
        "aumento_salarial_anual": aumento_salarial,
        "cotizacion_obligatoria": cotizacion_obligatoria,
        "comision_afp": comision_afp,
        "aporte_empleador": aporte_empleador,
        "cotizacion_voluntaria": cotizacion_voluntaria,
        "rentabilidad_nominal": rentabilidad_nominal,
        "inflacion_esperada": inflacion_esperada,
        "anos_lagunas": anos_lagunas,
        "distribucion_lagunas": distribucion_lagunas,
        "valor_uf": datos["uf"],
        "pbs": datos["pbs"],
    }

    valido, mensaje_error = validar_simulacion_completa(parametros)

    if not valido:
        st.error(f"Parametros invalidos: {mensaje_error}")
        st.stop()

    with st.spinner("Calculando proyeccion..."):
        resultados = calculator.calcular_pension_completa(**parametros)
        st.session_state["resultados"] = resultados
        st.session_state["parametros"] = parametros

    if "error" in resultados:
        st.error(resultados["mensaje"])
        st.stop()

    # -----------------------------------------------------------------------
    # KPIs principales
    # -----------------------------------------------------------------------
    st.subheader("Resultados principales")
    c1, c2, c3, c4 = st.columns(4)

    with c1:
        st.markdown(f"""
        <div class="kpi-card">
            <div class="kpi-label">Saldo final acumulado</div>
            <div class="kpi-value">{formato_clp(resultados["saldo_final_nominal"])}</div>
            <div class="kpi-sub">{formato_uf(resultados["saldo_final_uf"])} al tipo de cambio actual</div>
        </div>""", unsafe_allow_html=True)

    with c2:
        st.markdown(f"""
        <div class="kpi-card">
            <div class="kpi-label">Pension mensual (Retiro Programado)</div>
            <div class="kpi-value">{formato_clp(resultados["pension_rp_nominal"])}</div>
            <div class="kpi-sub">Real: {formato_clp(resultados["pension_rp_real"])} (CLP constantes)</div>
        </div>""", unsafe_allow_html=True)

    with c3:
        st.markdown(f"""
        <div class="kpi-card">
            <div class="kpi-label">Tasa de reemplazo</div>
            <div class="kpi-value">{formato_porcentaje(resultados["tasa_reemplazo_rp"])}</div>
            <div class="kpi-sub">Objetivo OCDE: 70 %</div>
        </div>""", unsafe_allow_html=True)

    with c4:
        st.markdown(f"""
        <div class="kpi-card">
            <div class="kpi-label">TIR de cotizaciones</div>
            <div class="kpi-value">{formato_porcentaje(resultados["tir_cotizaciones"])}</div>
            <div class="kpi-sub">Retorno efectivo de la inversion previsional</div>
        </div>""", unsafe_allow_html=True)

    st.divider()

    # -----------------------------------------------------------------------
    # Tabs de contenido
    # -----------------------------------------------------------------------
    tab_graf, tab_modal, tab_metr, tab_apv, tab_tabla, tab_export = st.tabs([
        "Graficos",
        "Modalidades de pension",
        "Metricas financieras",
        "APV y beneficio tributario",
        "Detalle anual",
        "Exportar",
    ])

    # --- Graficos -----------------------------------------------------------
    with tab_graf:
        st.subheader("Proyeccion del saldo acumulado")
        st.plotly_chart(
            charts.grafico_evolucion_saldo(resultados["simulacion_anual"]),
            use_container_width=True,
        )
        col_a, col_b = st.columns(2)
        with col_a:
            st.subheader("Composicion del saldo final")
            st.plotly_chart(
                charts.grafico_composicion_saldo(resultados["simulacion_anual"]),
                use_container_width=True,
            )
        with col_b:
            st.subheader("Flujo de caja anual")
            st.plotly_chart(
                charts.grafico_flujo_caja(resultados["simulacion_anual"]),
                use_container_width=True,
            )

    # --- Modalidades --------------------------------------------------------
    with tab_modal:
        col_izq, col_der = st.columns(2)
        with col_izq:
            st.plotly_chart(
                charts.grafico_comparacion_modalidades(
                    resultados["pension_rp_nominal"],
                    resultados["pension_rv_nominal"],
                    datos["pbs"],
                ),
                use_container_width=True,
            )
        with col_der:
            st.markdown("#### Retiro Programado")
            st.markdown(f"""
            | Concepto | Valor |
            |---|---|
            | Pension mensual nominal | {formato_clp(resultados["pension_rp_nominal"])} |
            | Pension mensual real | {formato_clp(resultados["pension_rp_real"])} |
            | Tasa de reemplazo | {formato_porcentaje(resultados["tasa_reemplazo_rp"])} |
            | Periodo de pago | {formato_anos(resultados["anos_retiro"])} |
            | Con PBS (piso estado) | {formato_clp(resultados["pension_rp_con_pbs"])} |
            """)

            st.markdown("#### Renta Vitalicia")
            st.markdown(f"""
            | Concepto | Valor |
            |---|---|
            | Pension mensual nominal | {formato_clp(resultados["pension_rv_nominal"])} |
            | Pension mensual real | {formato_clp(resultados["pension_rv_real"])} |
            | Tasa de reemplazo | {formato_porcentaje(resultados["tasa_reemplazo_rv"])} |
            | Factor conversion RV/RP | {resultados["factor_renta_vitalicia"]:.0%} |
            | Con PBS (piso estado) | {formato_clp(resultados["pension_rv_con_pbs"])} |
            """)

            pbs = datos["pbs"]
            if resultados["pension_rp_nominal"] < pbs:
                st.warning(
                    f"La pension proyectada ({formato_clp(resultados['pension_rp_nominal'])}) "
                    f"es inferior a la PBS ({formato_clp(pbs)}). "
                    "El Estado puede complementar hasta dicho monto segun Ley 20.255."
                )

    # --- Metricas -----------------------------------------------------------
    with tab_metr:
        st.subheader("Indicadores financieros avanzados")

        col1, col2, col3 = st.columns(3)
        with col1:
            st.metric(
                "VPN cotizaciones",
                formato_clp(resultados["vpn_cotizaciones"]),
                help="Valor presente neto de la corriente de cotizaciones netas.",
            )
            st.metric(
                "TIR cotizaciones",
                formato_porcentaje(resultados["tir_cotizaciones"]),
                help="Tasa interna de retorno efectiva de la inversion previsional.",
            )
            st.metric(
                "Rentabilidad real (Fisher)",
                formato_porcentaje(resultados["rentabilidad_real"]),
                help="Rentabilidad real segun ecuacion de Fisher.",
            )
        with col2:
            st.metric(
                "Valor presente pensiones",
                formato_clp(resultados["vp_pension_total"]),
                help="Valor presente de toda la corriente de pensiones futuras.",
            )
            st.metric(
                "Duracion de Macaulay",
                f"{resultados['duracion_macaulay_anos']:.1f} anos",
                help="Sensibilidad del pasivo previsional a cambios de tasa.",
            )
            st.metric(
                "DV01 (por pb)",
                formato_clp(resultados["dv01_clp"]),
                help="Cambio en VP de la pension ante 1 punto base de variacion en la tasa.",
            )
        with col3:
            st.metric(
                "Ultimo sueldo proyectado",
                formato_clp(resultados["ultimo_sueldo"]),
                help="Remuneracion estimada al momento de la jubilacion.",
            )
            st.metric(
                "Anos de cotizacion efectivos",
                f"{resultados['anos_cotizacion_efectivos']} / {resultados['anos_cotizacion']}",
                help="Anos con cotizacion activa sobre el total del horizonte.",
            )
            st.metric(
                "Saldo en UF",
                formato_uf(resultados["saldo_final_uf"]),
                help="Equivalente en UF al tipo de cambio utilizado.",
            )

        st.divider()
        st.subheader("Analisis de brecha previsional")

        brecha = resultados["brecha_previsional"]
        pension_objetivo = resultados["ultimo_sueldo"] * 0.70

        if brecha["hay_brecha"]:
            st.warning(
                f"Existe una brecha respecto al objetivo del 70 % del ultimo sueldo "
                f"({formato_clp(pension_objetivo)}).\n\n"
                f"- Pension proyectada: {formato_clp(resultados['pension_rp_nominal'])}\n"
                f"- Deficit mensual: {formato_clp(brecha['brecha_pension_mensual'])}\n"
                f"- Ahorro adicional mensual requerido: {formato_clp(brecha['brecha_mensual'])}\n"
                f"- Capital adicional necesario: {formato_clp(brecha['ahorro_total_necesario'])}"
            )
        else:
            st.success(
                f"La pension proyectada ({formato_clp(resultados['pension_rp_nominal'])}) "
                f"supera el objetivo del 70 % del ultimo sueldo ({formato_clp(pension_objetivo)}). "
                "El plan previsional es suficiente bajo los supuestos actuales."
            )

    # --- APV ----------------------------------------------------------------
    with tab_apv:
        st.subheader("Ahorro Previsional Voluntario — Beneficio tributario")
        st.markdown(
            "El APV ofrece dos regimenes tributarios. El optimo depende de la "
            "tasa marginal del cotizante: Regimen A conviene con tasas menores a 15 %; "
            "Regimen B es superior para tasas marginales mas altas."
        )

        col_a, col_b = st.columns(2)
        with col_a:
            monto_apv = st.number_input(
                "Aporte APV mensual (CLP)",
                min_value=0,
                max_value=5_000_000,
                value=100_000,
                step=10_000,
            )
        with col_b:
            tasa_marginal = st.slider(
                "Tasa impositiva marginal actual (%)",
                min_value=0, max_value=40, value=10, step=1,
                help="Tramo del impuesto global complementario del cotizante.",
            ) / 100.0

        if monto_apv > 0:
            comparacion = comparar_regimenes_apv(
                monto_apv_mensual=monto_apv,
                tasa_impositiva_marginal=tasa_marginal,
                valor_uf=datos["uf"],
                anos_acumulacion=resultados["anos_cotizacion"],
                rentabilidad_anual=resultados["rentabilidad_nominal"] / 100.0,
                valor_utm=datos["utm"],
            )

            reg_a = comparacion["regimen_a"]
            reg_b = comparacion["regimen_b"]
            recomendado = comparacion["regimen_recomendado"]

            st.markdown(f"**Regimen recomendado para este perfil: Regimen {recomendado}**")
            st.caption(f"Diferencia estimada en capital: {formato_clp(comparacion['diferencia_capital'])}")

            col1, col2 = st.columns(2)
            with col1:
                st.markdown("##### Regimen A")
                st.markdown(f"""
                - Beneficio anual: {formato_clp(reg_a["beneficio_anual"])}
                - Capital APV acumulado: {formato_clp(reg_a["capital_acumulado_apv"])}
                - Capital extra por subsidio: {formato_clp(reg_a["capital_extra_por_beneficio"])}
                - *{reg_a["descripcion"]}*
                """)
            with col2:
                st.markdown("##### Regimen B")
                st.markdown(f"""
                - Beneficio anual: {formato_clp(reg_b["beneficio_anual"])}
                - Capital APV acumulado: {formato_clp(reg_b["capital_acumulado_apv"])}
                - Capital extra por deduccion: {formato_clp(reg_b["capital_extra_por_beneficio"])}
                - *{reg_b["descripcion"]}*
                """)

            st.plotly_chart(
                charts.grafico_apv_comparacion(
                    reg_a, reg_b, resultados["saldo_final_nominal"]
                ),
                use_container_width=True,
            )

    # --- Detalle anual -------------------------------------------------------
    with tab_tabla:
        st.subheader("Proyeccion año a año")

        df_raw = resultados["simulacion_anual"].copy()
        df_display = pd.DataFrame({
            "Ano": df_raw["ano"],
            "Edad": df_raw["edad"],
            "Ingreso mensual": df_raw["ingreso_mensual"].apply(formato_clp),
            "Cotizacion anual": df_raw["cotizacion_anual"].apply(formato_clp),
            "Comision AFP": df_raw["comision_anual"].apply(formato_clp),
            "Aporte empleador": df_raw["aporte_empleador_anual"].apply(formato_clp),
            "Rentabilidad": df_raw["rentabilidad_nominal_anual"].apply(formato_clp),
            "Saldo nominal": df_raw["saldo_nominal"].apply(formato_clp),
            "Saldo real": df_raw["saldo_real"].apply(formato_clp),
            "Laguna": df_raw["tiene_laguna"].apply(lambda x: "Si" if x else "No"),
        })

        st.dataframe(df_display, use_container_width=True, height=420)

        @st.cache_data
        def _df_to_excel(df: pd.DataFrame) -> bytes:
            from io import BytesIO
            buf = BytesIO()
            with pd.ExcelWriter(buf, engine="openpyxl") as writer:
                df.to_excel(writer, index=False, sheet_name="Simulacion")
            return buf.getvalue()

        st.download_button(
            label="Descargar Excel",
            data=_df_to_excel(df_raw),
            file_name=f"simulacion_pension_{datetime.now():%Y%m%d}.xlsx",
            mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        )

    # --- Exportar -----------------------------------------------------------
    with tab_export:
        col_pdf, col_json = st.columns(2)

        with col_pdf:
            st.markdown("#### Reporte PDF")
            st.markdown(
                "Genera un informe profesional con resumen ejecutivo, "
                "tabla de resultados, metricas financieras y aviso legal."
            )
            if st.button("Generar PDF", type="primary"):
                with st.spinner("Generando reporte..."):
                    try:
                        pdf_buffer = pdf_generator.generar_reporte(resultados, parametros)
                        st.download_button(
                            label="Descargar PDF",
                            data=pdf_buffer,
                            file_name=f"reporte_pension_{datetime.now():%Y%m%d}.pdf",
                            mime="application/pdf",
                        )
                    except Exception as e:
                        st.error(f"No se pudo generar el PDF: {e}")

        with col_json:
            st.markdown("#### Guardar simulacion (JSON)")
            st.markdown(
                "Exporta los parametros y el resumen de resultados en formato "
                "JSON para reutilizar en sesiones posteriores."
            )
            payload = {
                "parametros": parametros,
                "fecha": datetime.now().isoformat(),
                "resumen": {
                    "saldo_final_nominal": resultados["saldo_final_nominal"],
                    "pension_rp_nominal": resultados["pension_rp_nominal"],
                    "tasa_reemplazo_rp": resultados["tasa_reemplazo_rp"],
                    "tir_cotizaciones": resultados["tir_cotizaciones"],
                    "vpn_cotizaciones": resultados["vpn_cotizaciones"],
                    "duracion_macaulay_anos": resultados["duracion_macaulay_anos"],
                },
            }
            st.download_button(
                label="Descargar JSON",
                data=json.dumps(payload, indent=2, default=str),
                file_name=f"simulacion_{datetime.now():%Y%m%d_%H%M%S}.json",
                mime="application/json",
            )

else:
    # Estado inicial antes de calcular
    st.info(
        "Configure los parametros en el panel lateral y presione **Calcular pension** "
        "para obtener la proyeccion personalizada."
    )
    st.markdown("#### El simulador calcula:")
    col1, col2, col3 = st.columns(3)
    with col1:
        st.markdown("""
        **Proyeccion actuarial**
        - Saldo acumulado nominal y real
        - Pension RP mediante formula de anualidad
        - Tasa de reemplazo nominal y real
        - Comparacion Renta Vitalicia
        """)
    with col2:
        st.markdown("""
        **Metricas financieras**
        - Valor Presente Neto (VPN)
        - Tasa Interna de Retorno (TIR)
        - Duracion de Macaulay
        - DV01 del pasivo previsional
        """)
    with col3:
        st.markdown("""
        **Herramientas adicionales**
        - Analisis APV Regimen A vs B
        - Brecha previsional (objetivo OCDE)
        - Detalle año a año descargable
        - Reporte PDF y exportacion JSON
        """)
