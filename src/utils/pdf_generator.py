"""
Generador de reportes PDF para el simulador de pensiones.

Produce un informe estructurado con resumen ejecutivo, resultados
principales, metricas financieras avanzadas, tabla de simulacion
y aviso legal. Utiliza ReportLab para la composicion de paginas.
"""

from datetime import datetime
from io import BytesIO
from typing import Dict

import pandas as pd
from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_JUSTIFY
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import inch
from reportlab.platypus import (
    PageBreak,
    Paragraph,
    SimpleDocTemplate,
    Spacer,
    Table,
    TableStyle,
)

# ---------------------------------------------------------------------------
# Paleta corporativa
# ---------------------------------------------------------------------------
_AZUL = colors.HexColor("#1E3A8A")
_AZUL_CLARO = colors.HexColor("#EFF6FF")
_VERDE = colors.HexColor("#10B981")
_VERDE_CLARO = colors.HexColor("#F0FDF4")
_GRIS = colors.HexColor("#6B7280")
_GRIS_CLARO = colors.HexColor("#F9FAFB")
_BLANCO = colors.white


class PDFReportGenerator:
    """Generador de reportes PDF profesionales para simulaciones previsionales.

    Genera un informe de pagina completa en formato carta con:
    - Encabezado institucional con fecha de generacion
    - Resumen de parametros de entrada
    - Resultados de pension por modalidad
    - Indicadores financieros avanzados (VPN, TIR, duracion, DV01)
    - Muestra de la proyeccion año a año
    - Aviso legal estandar
    """

    def __init__(self):
        self._estilos = getSampleStyleSheet()
        self._registrar_estilos()

    def generar_reporte(self, resultados: Dict, parametros: Dict) -> BytesIO:
        """Genera el reporte PDF completo y lo retorna en un buffer de bytes.

        Args:
            resultados: Diccionario de salida de PensionCalculator.calcular_pension_completa().
            parametros: Diccionario de parametros de entrada.

        Returns:
            BytesIO posicionado en el inicio del PDF generado.
        """
        buffer = BytesIO()
        doc = SimpleDocTemplate(
            buffer,
            pagesize=letter,
            rightMargin=0.85 * inch,
            leftMargin=0.85 * inch,
            topMargin=0.85 * inch,
            bottomMargin=0.6 * inch,
        )

        contenido = []
        contenido += self._seccion_encabezado()
        contenido += self._seccion_parametros(parametros, resultados)
        contenido += self._seccion_resultados(resultados)
        contenido += self._seccion_metricas(resultados)
        contenido += self._seccion_tabla(resultados)
        contenido.append(PageBreak())
        contenido += self._seccion_aviso_legal()

        doc.build(contenido)
        buffer.seek(0)
        return buffer

    # ------------------------------------------------------------------
    # Secciones del reporte
    # ------------------------------------------------------------------

    def _seccion_encabezado(self) -> list:
        elementos = []
        elementos.append(Paragraph(
            "Reporte de Simulacion Previsional",
            self._estilos["Titulo"],
        ))
        elementos.append(Paragraph(
            f"Generado el {datetime.now().strftime('%d/%m/%Y a las %H:%M')} "
            "— Herramienta de analisis educativo",
            self._estilos["Subtitulo"],
        ))
        elementos.append(Spacer(1, 16))
        return elementos

    def _seccion_parametros(self, parametros: Dict, resultados: Dict) -> list:
        elementos = [Paragraph("Parametros de la simulacion", self._estilos["Seccion"])]

        filas = [
            ["Parametro", "Valor"],
            ["Edad actual", f"{parametros.get('edad_actual', '—')} anos"],
            ["Edad de jubilacion", f"{parametros.get('edad_jubilacion', '—')} anos"],
            ["Esperanza de vida", f"{parametros.get('esperanza_vida', '—')} anos"],
            ["Anos de acumulacion", f"{resultados.get('anos_cotizacion', '—')} anos"],
            ["Anos efectivos (sin lagunas)", f"{resultados.get('anos_cotizacion_efectivos', '—')} anos"],
            ["Ingreso mensual inicial", f"${parametros.get('ingreso_mensual', 0):,.0f}".replace(",", ".")],
            ["Rentabilidad nominal anual", f"{parametros.get('rentabilidad_nominal', 0):.1f} %"],
            ["Inflacion anual esperada", f"{parametros.get('inflacion_esperada', 0):.1f} %"],
            ["AFP seleccionada", parametros.get("afp", "—")],
        ]

        elementos.append(self._tabla(filas, col_widths=[3.2 * inch, 2.5 * inch], header_color=_AZUL))
        elementos.append(Spacer(1, 16))
        return elementos

    def _seccion_resultados(self, resultados: Dict) -> list:
        elementos = [Paragraph("Resultados principales", self._estilos["Seccion"])]

        filas = [
            ["Metrica", "Valor"],
            ["Saldo final acumulado (nominal)", f"${resultados.get('saldo_final_nominal', 0):,.0f}".replace(",", ".")],
            ["Saldo final acumulado (real CLP constantes)", f"${resultados.get('saldo_final_real', 0):,.0f}".replace(",", ".")],
            ["Pension mensual — Retiro Programado", f"${resultados.get('pension_rp_nominal', 0):,.0f}/mes".replace(",", ".")],
            ["Pension mensual — Renta Vitalicia", f"${resultados.get('pension_rv_nominal', 0):,.0f}/mes".replace(",", ".")],
            ["Pension con PBS (RP)", f"${resultados.get('pension_rp_con_pbs', 0):,.0f}/mes".replace(",", ".")],
            ["Tasa de reemplazo — Retiro Programado", f"{resultados.get('tasa_reemplazo_rp', 0):.1f} %"],
            ["Tasa de reemplazo — Renta Vitalicia", f"{resultados.get('tasa_reemplazo_rv', 0):.1f} %"],
            ["Tasa de reemplazo real", f"{resultados.get('tasa_reemplazo_real', 0):.1f} %"],
            ["Ultimo sueldo proyectado", f"${resultados.get('ultimo_sueldo', 0):,.0f}/mes".replace(",", ".")],
        ]

        elementos.append(self._tabla(filas, col_widths=[3.8 * inch, 2.2 * inch], header_color=_VERDE))

        tasa = resultados.get("tasa_reemplazo_rp", 0)
        if tasa >= 70:
            evaluacion = "La pension proyectada supera el objetivo del 70 % recomendado por la OCDE."
        elif tasa >= 50:
            evaluacion = "La pension proyectada es moderada. Se recomienda evaluar ahorro adicional (APV)."
        else:
            evaluacion = (
                "La pension proyectada es significativamente inferior al ultimo sueldo. "
                "Se sugiere incrementar cotizaciones voluntarias o extender el periodo de acumulacion."
            )

        elementos.append(Spacer(1, 8))
        elementos.append(Paragraph(evaluacion, self._estilos["Cuerpo"]))
        elementos.append(Spacer(1, 16))
        return elementos

    def _seccion_metricas(self, resultados: Dict) -> list:
        elementos = [Paragraph("Metricas financieras avanzadas", self._estilos["Seccion"])]

        filas = [
            ["Metrica", "Valor", "Descripcion"],
            [
                "VPN cotizaciones",
                f"${resultados.get('vpn_cotizaciones', 0):,.0f}".replace(",", "."),
                "Valor presente de la corriente de cotizaciones",
            ],
            [
                "TIR cotizaciones",
                f"{resultados.get('tir_cotizaciones', 0):.2f} %",
                "Retorno efectivo de la inversion previsional",
            ],
            [
                "Rentabilidad real (Fisher)",
                f"{resultados.get('rentabilidad_real', 0):.2f} %",
                "Tasa real = (1+nom)/(1+inf) - 1",
            ],
            [
                "VP total de pensiones",
                f"${resultados.get('vp_pension_total', 0):,.0f}".replace(",", "."),
                "Valor presente de la corriente de pensiones futuras",
            ],
            [
                "Duracion de Macaulay",
                f"{resultados.get('duracion_macaulay_anos', 0):.1f} anos",
                "Sensibilidad del pasivo a variaciones de tasa (ALM)",
            ],
            [
                "DV01 del pasivo",
                f"${resultados.get('dv01_clp', 0):,.0f}".replace(",", "."),
                "Cambio en VP ante 1 punto base de variacion en tasa",
            ],
        ]

        elementos.append(
            self._tabla(
                filas,
                col_widths=[1.8 * inch, 1.6 * inch, 2.9 * inch],
                header_color=_GRIS,
            )
        )
        elementos.append(Spacer(1, 16))
        return elementos

    def _seccion_tabla(self, resultados: Dict) -> list:
        elementos = [Paragraph("Proyeccion año a año (muestra)", self._estilos["Seccion"])]

        df: pd.DataFrame = resultados.get("simulacion_anual")
        if df is None or df.empty:
            return elementos

        muestra = pd.concat([df.head(5), df.tail(5)]).drop_duplicates()

        encabezado = ["Ano", "Edad", "Ingreso mensual", "Cotizacion anual", "Saldo nominal", "Laguna"]
        filas = [encabezado]
        for _, row in muestra.iterrows():
            filas.append([
                str(int(row["ano"])),
                str(int(row["edad"])),
                f"${row['ingreso_mensual']:,.0f}".replace(",", "."),
                f"${row['cotizacion_anual']:,.0f}".replace(",", "."),
                f"${row['saldo_nominal']:,.0f}".replace(",", "."),
                "Si" if row["tiene_laguna"] else "No",
            ])

        elementos.append(
            self._tabla(
                filas,
                col_widths=[0.6 * inch, 0.6 * inch, 1.4 * inch, 1.4 * inch, 1.5 * inch, 0.7 * inch],
                header_color=_GRIS,
                font_size_body=8,
            )
        )
        return elementos

    def _seccion_aviso_legal(self) -> list:
        elementos = [Paragraph("Aviso legal", self._estilos["Seccion"])]

        texto = (
            "Este reporte tiene caracter estrictamente educativo e informativo. "
            "Los resultados son estimaciones basadas en los supuestos ingresados y "
            "no constituyen asesoria financiera, previsional ni recomendacion de inversion. "
            "Esta herramienta no se encuentra registrada ni regulada por la Comision para "
            "el Mercado Financiero (CMF).<br/><br/>"
            "La rentabilidad pasada no garantiza rentabilidad futura. Las proyecciones "
            "asumen condiciones de mercado y legislativas estables, lo cual no puede "
            "garantizarse. Se recomienda complementar el analisis con un asesor previsional "
            "certificado antes de tomar decisiones de inversion o planificacion.<br/><br/>"
            "Fuentes: Banco Central de Chile, Superintendencia de Pensiones, CMF, INE."
        )
        elementos.append(Paragraph(texto, self._estilos["Legal"]))
        return elementos

    # ------------------------------------------------------------------
    # Utilidades de composicion
    # ------------------------------------------------------------------

    def _tabla(
        self,
        filas: list,
        col_widths: list,
        header_color: colors.Color = _AZUL,
        font_size_body: int = 9,
    ) -> Table:
        """Crea una tabla con estilo corporativo estandar."""
        paragrafos = []
        for i, fila in enumerate(filas):
            estilo = "TablaEncabezado" if i == 0 else "TablaCuerpo"
            paragrafos.append([Paragraph(str(celda), self._estilos[estilo]) for celda in fila])

        tabla = Table(paragrafos, colWidths=col_widths)
        tabla.setStyle(TableStyle([
            ("BACKGROUND", (0, 0), (-1, 0), header_color),
            ("TEXTCOLOR", (0, 0), (-1, 0), _BLANCO),
            ("FONTNAME", (0, 0), (-1, 0), "Helvetica-Bold"),
            ("FONTSIZE", (0, 0), (-1, 0), 10),
            ("BOTTOMPADDING", (0, 0), (-1, 0), 8),
            ("TOPPADDING", (0, 0), (-1, 0), 8),
            ("ALIGN", (0, 0), (-1, -1), "LEFT"),
            ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
            ("FONTSIZE", (0, 1), (-1, -1), font_size_body),
            ("GRID", (0, 0), (-1, -1), 0.5, colors.HexColor("#D1D5DB")),
            ("ROWBACKGROUNDS", (0, 1), (-1, -1), [_BLANCO, _GRIS_CLARO]),
            ("LEFTPADDING", (0, 0), (-1, -1), 6),
            ("RIGHTPADDING", (0, 0), (-1, -1), 6),
        ]))
        return tabla

    def _registrar_estilos(self) -> None:
        """Registra los estilos personalizados del reporte."""
        self._estilos.add(ParagraphStyle(
            "Titulo",
            parent=self._estilos["Heading1"],
            fontSize=20,
            textColor=_AZUL,
            alignment=TA_CENTER,
            spaceAfter=6,
            fontName="Helvetica-Bold",
        ))
        self._estilos.add(ParagraphStyle(
            "Subtitulo",
            parent=self._estilos["Normal"],
            fontSize=9,
            textColor=_GRIS,
            alignment=TA_CENTER,
            spaceAfter=12,
        ))
        self._estilos.add(ParagraphStyle(
            "Seccion",
            parent=self._estilos["Heading2"],
            fontSize=13,
            textColor=_AZUL,
            spaceAfter=8,
            spaceBefore=14,
            fontName="Helvetica-Bold",
        ))
        self._estilos.add(ParagraphStyle(
            "Cuerpo",
            parent=self._estilos["Normal"],
            fontSize=10,
            alignment=TA_JUSTIFY,
            spaceAfter=8,
        ))
        self._estilos.add(ParagraphStyle(
            "Legal",
            parent=self._estilos["Normal"],
            fontSize=8,
            textColor=_GRIS,
            alignment=TA_JUSTIFY,
            leftIndent=12,
            rightIndent=12,
            spaceAfter=8,
        ))
        self._estilos.add(ParagraphStyle(
            "TablaEncabezado",
            parent=self._estilos["Normal"],
            fontSize=10,
            textColor=_BLANCO,
            fontName="Helvetica-Bold",
        ))
        self._estilos.add(ParagraphStyle(
            "TablaCuerpo",
            parent=self._estilos["Normal"],
            fontSize=9,
        ))
