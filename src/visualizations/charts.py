"""
Modulo de visualizaciones para el simulador de pensiones.

Todas las figuras se generan con Plotly y siguen la paleta corporativa
definida en PensionCharts.PALETTE. El template base es 'plotly_white'
para garantizar legibilidad en contextos de presentacion profesional.
"""

from typing import Dict

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots


class PensionCharts:
    """Generador de graficos profesionales para el simulador previsional.

    Todos los metodos retornan objetos go.Figure listos para ser
    renderizados con st.plotly_chart() en Streamlit.
    """

    PALETTE = {
        "primary": "#1E3A8A",
        "secondary": "#3B82F6",
        "success": "#10B981",
        "warning": "#F59E0B",
        "danger": "#EF4444",
        "info": "#06B6D4",
        "gray": "#6B7280",
        "light_primary": "rgba(30, 58, 138, 0.12)",
        "light_success": "rgba(16, 185, 129, 0.12)",
    }
    TEMPLATE = "plotly_white"
    FONT = "Arial, sans-serif"
    HEIGHT_STD = 480
    HEIGHT_TALL = 680

    def grafico_evolucion_saldo(
        self,
        df: pd.DataFrame,
        mostrar_real: bool = True,
    ) -> go.Figure:
        """Evolucion del saldo acumulado nominal y real en el tiempo.

        Args:
            df: DataFrame de simulacion con columnas saldo_nominal,
                saldo_real, edad, tiene_laguna.
            mostrar_real: Incluir curva de saldo real deflactado.

        Returns:
            Figura Plotly.
        """
        fig = go.Figure()

        fig.add_trace(go.Scatter(
            x=df["edad"],
            y=df["saldo_nominal"],
            name="Saldo Nominal",
            mode="lines",
            line=dict(color=self.PALETTE["primary"], width=2.5),
            hovertemplate="Edad %{x}<br>Saldo: $%{y:,.0f}<extra></extra>",
        ))

        if mostrar_real:
            fig.add_trace(go.Scatter(
                x=df["edad"],
                y=df["saldo_real"],
                name="Saldo Real (CLP constantes)",
                mode="lines",
                line=dict(color=self.PALETTE["success"], width=2.5, dash="dot"),
                hovertemplate="Edad %{x}<br>Saldo Real: $%{y:,.0f}<extra></extra>",
            ))

        lagunas = df[df["tiene_laguna"]]
        if not lagunas.empty:
            fig.add_trace(go.Scatter(
                x=lagunas["edad"],
                y=lagunas["saldo_nominal"],
                name="Laguna previsional",
                mode="markers",
                marker=dict(
                    size=10,
                    color=self.PALETTE["danger"],
                    symbol="x",
                    line=dict(width=2),
                ),
                hovertemplate="Laguna — Edad %{x}<extra></extra>",
            ))

        self._layout(
            fig,
            title="Proyeccion del Saldo Previsional Acumulado",
            xaxis_title="Edad",
            yaxis_title="Saldo (CLP)",
            yaxis_format="$,.0f",
            height=self.HEIGHT_STD,
        )
        return fig

    def grafico_comparacion_modalidades(
        self,
        pension_rp: float,
        pension_rv: float,
        pension_pbs: float = 0.0,
    ) -> go.Figure:
        """Comparacion de pensiones por modalidad de retiro.

        Args:
            pension_rp: Pension mensual Retiro Programado en CLP.
            pension_rv: Pension mensual Renta Vitalicia en CLP.
            pension_pbs: Pension Basica Solidaria de referencia en CLP.

        Returns:
            Figura Plotly.
        """
        etiquetas = ["Retiro Programado", "Renta Vitalicia"]
        valores = [pension_rp, pension_rv]
        colores = [self.PALETTE["primary"], self.PALETTE["success"]]

        if pension_pbs > 0:
            etiquetas.append("PBS (referencia)")
            valores.append(pension_pbs)
            colores.append(self.PALETTE["warning"])

        fig = go.Figure(go.Bar(
            x=etiquetas,
            y=valores,
            marker=dict(color=colores, line=dict(color="white", width=2)),
            text=[f"${v:,.0f}" for v in valores],
            textposition="outside",
            textfont=dict(size=13, family=self.FONT),
            hovertemplate="<b>%{x}</b><br>Pension: $%{y:,.0f}<extra></extra>",
        ))

        self._layout(
            fig,
            title="Pension Mensual por Modalidad de Retiro",
            yaxis_title="Pension Mensual (CLP)",
            yaxis_format="$,.0f",
            height=self.HEIGHT_STD,
            show_legend=False,
        )
        return fig

    def grafico_composicion_saldo(self, df: pd.DataFrame) -> go.Figure:
        """Composicion del saldo final: cotizaciones, rentabilidad y comisiones.

        Args:
            df: DataFrame de simulacion.

        Returns:
            Figura Plotly de tipo donut.
        """
        total_cot = df["cotizacion_neta_anual"].sum()
        total_com = df["comision_anual"].sum()
        total_rent = df["rentabilidad_nominal_anual"].sum()
        saldo_final = float(df["saldo_nominal"].iloc[-1])

        labels = ["Cotizaciones netas", "Rentabilidad generada", "Comisiones AFP"]
        values = [total_cot, total_rent, total_com]
        colors = [self.PALETTE["primary"], self.PALETTE["success"], self.PALETTE["danger"]]

        fig = go.Figure(go.Pie(
            labels=labels,
            values=values,
            marker=dict(colors=colors, line=dict(color="white", width=2)),
            hole=0.45,
            textinfo="label+percent",
            textfont=dict(size=12, family=self.FONT),
            hovertemplate="<b>%{label}</b><br>$%{value:,.0f}<br>%{percent}<extra></extra>",
        ))

        fig.update_layout(
            title=dict(
                text="<b>Composicion del Saldo Final</b>",
                font=dict(size=17, family=self.FONT),
            ),
            annotations=[dict(
                text=f"<b>Total</b><br>${saldo_final:,.0f}",
                x=0.5, y=0.5,
                font=dict(size=14, family=self.FONT),
                showarrow=False,
            )],
            template=self.TEMPLATE,
            font=dict(family=self.FONT),
            height=self.HEIGHT_STD,
        )
        return fig

    def grafico_flujo_caja(self, df: pd.DataFrame) -> go.Figure:
        """Flujo de caja anual: cotizaciones, comisiones y rentabilidad.

        Utiliza un eje secundario para la rentabilidad (escala diferente).

        Args:
            df: DataFrame de simulacion.

        Returns:
            Figura Plotly con doble eje.
        """
        fig = make_subplots(specs=[[{"secondary_y": True}]])

        fig.add_trace(go.Bar(
            x=df["edad"],
            y=df["cotizacion_neta_anual"],
            name="Cotizacion neta",
            marker=dict(color=self.PALETTE["success"]),
            hovertemplate="Edad %{x}<br>Cotizacion: $%{y:,.0f}<extra></extra>",
        ), secondary_y=False)

        fig.add_trace(go.Bar(
            x=df["edad"],
            y=-df["comision_anual"],
            name="Comision AFP",
            marker=dict(color=self.PALETTE["danger"]),
            hovertemplate="Edad %{x}<br>Comision: $%{y:,.0f}<extra></extra>",
        ), secondary_y=False)

        fig.add_trace(go.Scatter(
            x=df["edad"],
            y=df["rentabilidad_nominal_anual"],
            name="Rentabilidad anual",
            mode="lines",
            line=dict(color=self.PALETTE["primary"], width=2.5),
            hovertemplate="Edad %{x}<br>Rentabilidad: $%{y:,.0f}<extra></extra>",
        ), secondary_y=True)

        fig.update_layout(
            title=dict(
                text="<b>Flujo de Caja Previsional Anual</b>",
                font=dict(size=17, family=self.FONT),
            ),
            xaxis=dict(title="Edad", showgrid=True, gridcolor="#E5E7EB"),
            barmode="relative",
            template=self.TEMPLATE,
            font=dict(family=self.FONT),
            height=self.HEIGHT_STD,
            legend=dict(orientation="h", y=1.08, x=0.5, xanchor="center"),
        )
        fig.update_yaxes(title_text="CLP (cotizaciones / comisiones)", tickformat="$,.0f", secondary_y=False)
        fig.update_yaxes(title_text="CLP (rentabilidad)", tickformat="$,.0f", secondary_y=True)
        return fig

    def grafico_monte_carlo(self, resultados_mc: Dict) -> go.Figure:
        """Distribucion de resultados del analisis Monte Carlo.

        Muestra el histograma de pensiones con lineas de percentiles clave.

        Args:
            resultados_mc: Resultado de MonteCarloSimulator.simular_escenarios().

        Returns:
            Figura Plotly.
        """
        dist = resultados_mc["distribucion_completa"]["pension_rp"]
        stats = resultados_mc["pension_rp"]

        fig = go.Figure()

        fig.add_trace(go.Histogram(
            x=dist,
            nbinsx=60,
            name="Distribucion",
            marker=dict(color=self.PALETTE["primary"], line=dict(color="white", width=0.5)),
            opacity=0.75,
            hovertemplate="Pension: $%{x:,.0f}<br>Frecuencia: %{y}<extra></extra>",
        ))

        marcas = [
            (stats["percentil_10"], "P10", self.PALETTE["danger"]),
            (stats["mediana"], "P50", self.PALETTE["success"]),
            (stats["percentil_90"], "P90", self.PALETTE["info"]),
        ]
        for valor, nombre, color in marcas:
            fig.add_vline(
                x=valor,
                line_dash="dash",
                line_color=color,
                line_width=2,
                annotation_text=f"{nombre}  ${valor:,.0f}",
                annotation_position="top",
                annotation_font=dict(size=11, family=self.FONT),
            )

        n_sim = resultados_mc["n_simulaciones"]
        self._layout(
            fig,
            title=f"Distribucion Probabilistica de Pension — {n_sim:,} simulaciones",
            xaxis_title="Pension Mensual (CLP)",
            yaxis_title="Frecuencia",
            xaxis_format="$,.0f",
            height=self.HEIGHT_STD,
            show_legend=False,
        )
        return fig

    def grafico_fan_chart(self, resultados_mc: Dict, edad_jubilacion: int) -> go.Figure:
        """Fan chart de rangos de confianza de la pension proyectada.

        Representa la incertidumbre en la estimacion mediante bandas de
        probabilidad superpuestas (area chart con transparencia).

        Args:
            resultados_mc: Resultado de MonteCarloSimulator.simular_escenarios().
            edad_jubilacion: Edad de referencia para el eje x.

        Returns:
            Figura Plotly.
        """
        stats = resultados_mc["pension_rp"]
        labels = ["P5", "P10", "P25", "P50", "P75", "P90", "P95"]
        valores = [
            stats["percentil_5"], stats["percentil_10"], stats["percentil_25"],
            stats["mediana"],
            stats["percentil_75"], stats["percentil_90"], stats["percentil_95"],
        ]
        categorias = ["Muy pesimista", "Pesimista", "Conservador", "Mediana",
                      "Moderado", "Optimista", "Muy optimista"]

        fig = go.Figure()

        colores = [
            self.PALETTE["danger"], self.PALETTE["warning"], self.PALETTE["info"],
            self.PALETTE["primary"],
            self.PALETTE["info"], self.PALETTE["success"], self.PALETTE["success"],
        ]
        for lbl, val, cat, color in zip(labels, valores, categorias, colores):
            fig.add_trace(go.Bar(
                x=[lbl],
                y=[val],
                name=f"{lbl} — {cat}",
                marker=dict(color=color),
                text=[f"${val:,.0f}"],
                textposition="outside",
                textfont=dict(size=11),
                hovertemplate=f"<b>{cat}</b><br>Pension: ${{y:,.0f}}<extra></extra>",
            ))

        self._layout(
            fig,
            title="Rangos de Confianza — Pension Mensual Proyectada",
            xaxis_title="Percentil",
            yaxis_title="Pension Mensual (CLP)",
            yaxis_format="$,.0f",
            height=self.HEIGHT_STD,
            show_legend=False,
        )
        return fig

    def grafico_sensibilidad(
        self,
        df_sensibilidad: pd.DataFrame,
        parametro: str,
    ) -> go.Figure:
        """Analisis de sensibilidad parametrica (pension y tasa de reemplazo).

        Args:
            df_sensibilidad: DataFrame de MonteCarloSimulator.analizar_sensibilidad().
            parametro: Nombre legible del parametro analizado.

        Returns:
            Figura Plotly con dos subplots.
        """
        fig = make_subplots(
            rows=2, cols=1,
            subplot_titles=("Impacto en Pension Mensual", "Impacto en Tasa de Reemplazo"),
            vertical_spacing=0.18,
        )

        for nombre, col, color in [
            ("Retiro Programado", "pension_rp", self.PALETTE["primary"]),
            ("Renta Vitalicia", "pension_rv", self.PALETTE["success"]),
        ]:
            fig.add_trace(go.Scatter(
                x=df_sensibilidad["valor_parametro"],
                y=df_sensibilidad[col],
                mode="lines+markers",
                name=nombre,
                line=dict(color=color, width=2.5),
                marker=dict(size=7),
                hovertemplate=f"<b>{nombre}</b><br>%{{x}}<br>$%{{y:,.0f}}<extra></extra>",
            ), row=1, col=1)

        fig.add_trace(go.Scatter(
            x=df_sensibilidad["valor_parametro"],
            y=df_sensibilidad["tasa_reemplazo"],
            mode="lines+markers",
            name="Tasa de reemplazo",
            line=dict(color=self.PALETTE["info"], width=2.5),
            marker=dict(size=7),
            fill="tozeroy",
            fillcolor=self.PALETTE["light_primary"],
            hovertemplate="%{x}<br>Tasa: %{y:.1f}%<extra></extra>",
        ), row=2, col=1)

        etiqueta = parametro.replace("_", " ").title()
        fig.update_xaxes(title_text=etiqueta, row=2, col=1)
        fig.update_yaxes(title_text="Pension (CLP)", tickformat="$,.0f", row=1, col=1)
        fig.update_yaxes(title_text="Tasa de Reemplazo (%)", row=2, col=1)

        fig.update_layout(
            title=dict(
                text=f"<b>Sensibilidad: {etiqueta}</b>",
                font=dict(size=17, family=self.FONT),
            ),
            template=self.TEMPLATE,
            font=dict(family=self.FONT),
            height=self.HEIGHT_TALL,
            hovermode="x unified",
            legend=dict(orientation="h", y=1.06, x=0.5, xanchor="center"),
        )
        return fig

    def grafico_comparador_escenarios(self, df_comparacion: pd.DataFrame) -> go.Figure:
        """Comparacion de saldo, pension y TIR entre multiples escenarios.

        Args:
            df_comparacion: DataFrame de MonteCarloSimulator.comparar_escenarios().

        Returns:
            Figura Plotly de barras agrupadas.
        """
        fig = go.Figure()

        for nombre, col, color in [
            ("Retiro Programado", "pension_rp", self.PALETTE["primary"]),
            ("Renta Vitalicia", "pension_rv", self.PALETTE["success"]),
        ]:
            fig.add_trace(go.Bar(
                name=nombre,
                x=df_comparacion["escenario"],
                y=df_comparacion[col],
                marker=dict(color=color),
                text=[f"${v:,.0f}" for v in df_comparacion[col]],
                textposition="outside",
                hovertemplate=f"<b>%{{x}}</b><br>{nombre}: $%{{y:,.0f}}<extra></extra>",
            ))

        self._layout(
            fig,
            title="Comparacion de Escenarios — Pension Mensual",
            xaxis_title="Escenario",
            yaxis_title="Pension Mensual (CLP)",
            yaxis_format="$,.0f",
            height=self.HEIGHT_STD,
            barmode="group",
        )
        return fig

    def grafico_apv_comparacion(
        self,
        resultado_a: Dict,
        resultado_b: Dict,
        saldo_base: float,
    ) -> go.Figure:
        """Comparacion del impacto del APV en el saldo acumulado.

        Args:
            resultado_a: Resultado de calcular_apv_beneficio_tributario para Regimen A.
            resultado_b: Resultado de calcular_apv_beneficio_tributario para Regimen B.
            saldo_base: Saldo proyectado sin APV.

        Returns:
            Figura Plotly de barras horizontales.
        """
        escenarios = ["Sin APV", "APV Regimen A", "APV Regimen B"]
        saldos = [
            saldo_base,
            saldo_base + resultado_a["capital_acumulado_apv"] + resultado_a["capital_extra_por_beneficio"],
            saldo_base + resultado_b["capital_acumulado_apv"] + resultado_b["capital_extra_por_beneficio"],
        ]
        colores = [self.PALETTE["gray"], self.PALETTE["secondary"], self.PALETTE["success"]]

        fig = go.Figure(go.Bar(
            x=saldos,
            y=escenarios,
            orientation="h",
            marker=dict(color=colores),
            text=[f"${s:,.0f}" for s in saldos],
            textposition="outside",
            hovertemplate="<b>%{y}</b><br>Saldo: $%{x:,.0f}<extra></extra>",
        ))

        self._layout(
            fig,
            title="Impacto del APV en el Saldo Acumulado",
            xaxis_title="Saldo Proyectado (CLP)",
            xaxis_format="$,.0f",
            height=350,
            show_legend=False,
        )
        return fig

    # ------------------------------------------------------------------
    # Utilidad privada de layout
    # ------------------------------------------------------------------

    def _layout(
        self,
        fig: go.Figure,
        title: str,
        xaxis_title: str = "",
        yaxis_title: str = "",
        xaxis_format: str = "",
        yaxis_format: str = "",
        height: int = 480,
        show_legend: bool = True,
        barmode: str = "stack",
    ) -> None:
        """Aplica configuracion de layout estandar a una figura."""
        fig.update_layout(
            title=dict(text=f"<b>{title}</b>", font=dict(size=17, family=self.FONT)),
            xaxis=dict(
                title=xaxis_title,
                showgrid=True,
                gridcolor="#E5E7EB",
                tickformat=xaxis_format if xaxis_format else None,
            ),
            yaxis=dict(
                title=yaxis_title,
                showgrid=True,
                gridcolor="#E5E7EB",
                tickformat=yaxis_format if yaxis_format else None,
            ),
            template=self.TEMPLATE,
            font=dict(family=self.FONT, size=12),
            height=height,
            showlegend=show_legend,
            barmode=barmode,
            hovermode="x unified",
            legend=dict(orientation="h", y=1.08, x=0.5, xanchor="center"),
            margin=dict(t=80, b=50, l=60, r=30),
        )
