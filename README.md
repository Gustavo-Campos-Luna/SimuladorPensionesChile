# Simulador de Pensiones Chile

Herramienta de analisis previsional para el sistema AFP chileno. Proyecta el
saldo acumulado y la pension estimada mediante metodos actuariales estandar,
incorpora metricas financieras avanzadas y ofrece analisis de riesgo mediante
simulacion Monte Carlo vectorizada.

---

## Descripcion

El simulador implementa un motor de calculo basado en capitalizacion individual
año a año, coherente con la estructura del sistema AFP establecido por el
Decreto Ley 3.500. Los resultados incluyen indicadores de uso habitual en
gestion de patrimonio y planificacion previsional: VPN, TIR, duracion de
Macaulay del pasivo, DV01, brecha previsional y comparacion de regimenes APV.

El modulo de analisis de riesgo ejecuta hasta 20.000 escenarios Monte Carlo
mediante operaciones vectoriales numpy, sin bucles Python por escenario,
con tiempos de respuesta inferiores a 2 segundos.

---

## Funcionalidades principales

### Proyeccion actuarial
- Acumulacion año a año con capitalizacion compuesta nominal
- Saldo real deflactado a pesos constantes del año de calculo
- Tope imponible dinamico (84,7 UF segun Ley 19.728)
- Modelado de lagunas previsionales por patron de carrera
- Aumento salarial configurable con tope imponible

### Calculo de pension
- Formula de anualidad actuarial para Retiro Programado:
  `PMT = PV x r / (1 - (1+r)^-n)`
- Comparacion Retiro Programado vs. Renta Vitalicia
- Piso de Pension Basica Solidaria (PBS)
- Ajuste por tasa de descuento durante el periodo de retiro

### Metricas financieras
- Valor Presente Neto (VPN) de cotizaciones
- Tasa Interna de Retorno (TIR) — metodo Brent con fallback Newton-Raphson
- Rentabilidad real mediante ecuacion de Fisher exacta
- Valor presente de la corriente de pensiones futuras
- Duracion de Macaulay del pasivo previsional (ALM)
- DV01 por punto base de variacion en la tasa de descuento
- Brecha previsional respecto al objetivo OCDE (70 % de reemplazo)

### APV — Beneficio tributario
- Regimen A: subsidio estatal directo del 15 % (tope 600 UF/año)
- Regimen B: deduccion de base imponible segun tasa marginal
- Recomendacion automatica segun tasa marginal del cotizante
- Proyeccion del capital adicional acumulado por beneficio tributario

### Analisis de riesgo (Monte Carlo)
- Rentabilidad nominal: variable aleatoria normal truncada
- Inflacion: variable aleatoria normal con piso en cero
- Cesantia: proceso de Bernoulli por año
- Implementacion vectorizada (numpy): 50-100x mas rapido que bucle Python
- Estadisticas por percentil: P5, P10, P25, P50, P75, P90, P95
- Coeficiente de variacion y amplitud intercuantil
- Probabilidad de alcanzar pension objetivo configurable
- Analisis de sensibilidad parametrica determinista

### Visualizaciones
- Evolucion del saldo nominal y real
- Composicion del saldo (cotizaciones, rentabilidad, comisiones)
- Flujo de caja previsional anual con eje doble
- Comparacion de modalidades de pension
- Histograma de distribucion Monte Carlo con lineas de percentil
- Fan chart de rangos de confianza
- Analisis de sensibilidad parametrica (2 subplots)
- Comparacion de impacto APV por regimen

### Exportacion
- Reporte PDF con resumen ejecutivo y aviso legal (ReportLab)
- Tabla año a año en Excel (openpyxl)
- Parametros y resumen en JSON (reutilizable entre sesiones)

---

## Estructura del proyecto

```
Simulador-pensiones-Chile/
|
|-- Home.py                          # Pagina de inicio con indicadores macroeconomicos
|
|-- pages/
|   |-- 1_Simulador.py              # Proyeccion individual con metricas avanzadas
|   |-- 2_Analisis_de_Riesgo.py     # Monte Carlo y analisis de sensibilidad
|   |-- 3_Metodologia.py            # Documentacion tecnica y aviso legal
|
|-- src/
|   |-- calculators/
|   |   |-- pension_engine.py       # Motor de calculo (PensionCalculator)
|   |   |-- financial_metrics.py    # VPN, TIR, Fisher, APV, duracion, DV01
|   |   |-- monte_carlo.py          # Monte Carlo vectorizado (MonteCarloSimulator)
|   |
|   |-- visualizations/
|   |   |-- charts.py               # Graficos Plotly (PensionCharts)
|   |
|   |-- utils/
|   |   |-- formatters.py           # Formateo CLP, UF, porcentajes
|   |   |-- validators.py           # Validacion de parametros de entrada
|   |   |-- pdf_generator.py        # Generacion de reportes PDF (ReportLab)
|   |
|   |-- api/
|       |-- data_sources.py         # Cliente APIs publicas (UF, AFP, inflacion)
|
|-- .streamlit/
|   |-- config.toml                 # Tema y configuracion del servidor
|
|-- tests/                          # pytest: motor de calculo, metricas, Monte Carlo
|-- pyproject.toml                  # Config de ruff y pytest
|-- requirements.txt                # Dependencias Python con versiones fijadas
|-- requirements-dev.txt            # pytest, ruff (no requeridas para correr la app)
|-- .python-version                 # Version de Python (3.9)
|-- .gitignore
|-- README.md
```

---

## Instalacion

### Requisitos del sistema
- Python 3.9 o superior
- pip
- Conexion a internet (para APIs de datos en tiempo real)

### Instalacion local

```bash
# Clonar el repositorio
git clone https://github.com/gustavo-campos-luna/simulador-pensiones-chile.git
cd simulador-pensiones-chile

# Crear entorno virtual
python -m venv .venv
source .venv/bin/activate        # Linux / macOS
.venv\Scripts\activate           # Windows

# Instalar dependencias
pip install -r requirements.txt

# Ejecutar la aplicacion
streamlit run Home.py
```

La aplicacion estara disponible en `http://localhost:8501`. Funciona sin
configuracion adicional: usa mindicador.cl (API publica) para UF e inflacion.

### Configuracion opcional: Banco Central de Chile

Por defecto la app obtiene UF e IPC desde mindicador.cl (API publica, sin
registro). Para usar la fuente primaria del Banco Central de Chile (mismos
indicadores, mayor profundidad historica), registrate gratis en
[si3.bcentral.cl](https://si3.bcentral.cl/siete/secure/cuadros/home.aspx) y
define las siguientes variables de entorno antes de ejecutar la app:

```bash
export BCENTRAL_USER='tu_correo@ejemplo.com'   # PowerShell: $env:BCENTRAL_USER = '...'
export BCENTRAL_PASS='tu_contrasena'
```

Si las credenciales no estan configuradas o la API del BCCh falla, la
aplicacion recurre automaticamente a mindicador.cl sin interrumpir el uso.

### Tests

La suite cubre la logica cuantitativa (motor de calculo, metricas financieras,
Monte Carlo): validacion de parametros, formula de anualidad, ecuacion de
Fisher, convergencia de TIR, tope del subsidio APV, y distribucion de lagunas
previsionales. No cubre las paginas Streamlit ni las llamadas HTTP en vivo.

```bash
pip install -r requirements-dev.txt
pytest
ruff check .
```

### Despliegue en Streamlit Cloud

1. Subir el repositorio a GitHub (publico o privado con acceso).
2. Acceder a [share.streamlit.io](https://share.streamlit.io).
3. Configurar: repositorio, rama `main`, archivo principal `Home.py`.
4. Streamlit Cloud detecta `requirements.txt` automaticamente.

---

## Dependencias

| Libreria | Version minima | Uso |
|---|---|---|
| streamlit | 1.28 | Framework web interactivo |
| numpy | 1.24 | Calculo vectorizado y Monte Carlo |
| pandas | 2.0 | Manipulacion de datos tabulares |
| scipy | 1.11 | Optimizacion numerica (TIR via Brent) |
| plotly | 5.17 | Visualizaciones interactivas |
| reportlab | 4.0 | Generacion de reportes PDF |
| requests | 2.31 | Consumo de APIs externas |
| openpyxl | 3.1 | Exportacion a Excel |
| pillow | 10.0 | Procesamiento de imagenes (PDF) |

---

## Fuentes de datos

| Indicador | Fuente primaria | Fallback automatico | Frecuencia |
|---|---|---|---|
| Valor UF | Banco Central de Chile (API SI3, requiere cuenta gratuita) | mindicador.cl (API publica, sin cuenta) | Diaria |
| Inflacion historica (IPC) | Banco Central de Chile (API SI3) | mindicador.cl (API publica, sin cuenta) | Mensual |
| Comisiones AFP | Superintendencia de Pensiones | — | Mensual (manual) |
| Rentabilidades AFP | Superintendencia de Pensiones | — | Mensual (manual) |
| Pension Basica Solidaria | Decreto Supremo | — | Anual (manual) |
| Tope imponible | Ley 19.728 | — | Anual (manual) |

**Nota:** la API del Banco Central (SI3) exige registro gratuito y credenciales
(`BCENTRAL_USER`/`BCENTRAL_PASS`); sin ellas, la aplicacion usa automaticamente
mindicador.cl (sin necesidad de configuracion) para UF e IPC. La cobertura
historica de mindicador.cl es menor que la del BCCh. Ver seccion
[Configuracion opcional](#configuracion-opcional-banco-central-de-chile).

---

## Metodologia resumida

### Acumulacion

```
Saldo_t = Saldo_{t-1} x (1 + r_nominal) + Cotizacion_neta_t
Saldo_real_t = Saldo_nominal_t / (1 + inflacion)^t
```

### Pension — Formula de anualidad

```
PMT = PV x r_m / (1 - (1 + r_m)^-n)
r_m = (1 + r_anual)^(1/12) - 1
```

### Rentabilidad real (Fisher)

```
r_real = (1 + r_nominal) / (1 + inflacion) - 1
```

### TIR (metodo Brent)

```
0 = sum_{t=1}^{T} [C_t / (1 + TIR)^t] - Saldo_Final
```

### Monte Carlo vectorizado

```
G_t = prod_{s=0}^{t} (1 + r_s)
Saldo_Final = G_n x sum_{t=0}^{n-1} [C_t / G_t]
```

---

## Normativa de referencia

- Decreto Ley 3.500 (1980) — Sistema de capitalizacion individual AFP
- Ley 19.728 (2001) — Seguro de Cesantia
- Ley 20.255 (2008) — Reforma Previsional (PBS y APS)
- Ley 21.563 (2024) — Pension Garantizada Universal (PGU)
- Circular SP N° 1.723 — Comisiones AFP vigentes

---

## Notas de desarrollo

Este proyecto fue construido con asistencia de IA (Claude): la implementacion
inicial, un refactor posterior hacia la arquitectura modular actual, y una
auditoria que corrigio dos errores de calculo reales (el tope del subsidio
APV Regimen A estaba inflado 12x por una multiplicacion de mas, y el piso de
Pension Basica Solidaria usaba un valor fijo desactualizado de 2024 en vez
del valor vigente) y agrego un fallback automatico a mindicador.cl para que
la aplicacion funcione sin necesidad de credenciales del Banco Central. El
codigo de la serie UTM del BCCh (`F073.UTR.PRE.Z.M`) fue verificado contra la
API real.

---

## Aviso legal

Esta herramienta tiene caracter estrictamente educativo e informativo.
Los resultados son estimaciones basadas en supuestos simplificadores y
no constituyen asesoria financiera ni recomendacion de inversion.
La rentabilidad pasada no garantiza rentabilidad futura.
Se recomienda complementar el analisis con un asesor previsional certificado.

---

## Autor

Gustavo Felipe Campos Luna

- LinkedIn: [linkedin.com/in/gustavo-campos-luna](https://www.linkedin.com/in/gustavo-campos-luna)
- GitHub: [github.com/gustavo-campos-luna](https://github.com/gustavo-campos-luna)
- Contacto: camposluna@uc.cl
