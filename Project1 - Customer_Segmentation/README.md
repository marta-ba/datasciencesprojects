# Project 1: Análisis de Personalidad y Consumo

## Objetivo
Identificar el tipo de perfil de cliente asociado con cada producto, analizando patrones de comportamiento y características de personalidad.

## Dataset
- **Archivo:** `marketing_campaign.parquet`
- Contiene datos sobre características demográficas, comportamiento de compra y preferencias de productos de los clientes.

## Metodología
1. **Exploratory Data Analysis (EDA):**
   - Análisis inicial para comprender las variables y sus relaciones.
   - Identificación de correlaciones entre características de los clientes y productos consumidos.
2. **Modelado:**
   - Uso de técnicas estadísticas y visualizaciones para encontrar patrones en los datos.
   - Identificación de las variables más influyentes en la elección de productos.
3. **Visualización:**
   - Representación gráfica de los perfiles de clientes y los productos más consumidos.

## Resultados
- Identificados los productos principales asociados con distintos perfiles:
  - **Perfil 1:** Los clientes con un nivel de ingresos superior a la media tienden a consumir más vino y carne.
  - **Perfil 2:** Los clientes con hijos suelen comprar mucho menos dado que los clientes sin hijos compran en más de un 50% más de 16 compras.
  - **Perfil 3:** En el grupo de "Non University Degree", más de un 60% de los clientes realizan menos de 8 compras.
- Insights clave sobre las características que determinan las preferencias de consumo:
  - Las ofertas que se lanzan en las diferntes campañas tienden a aumentar la venta de los productos.

## Archivos
- `Project1 - Análisis de Personalidad y Consumo.Rmd`: Código en RMarkdown para reproducir el análisis.
- `Project1---Analisis-de-Personalidad.html`: Reporte generado en formato HTML.
- `description.xlsx`: Diccionario de datos del dataset.
- `marketing_campaign.parquet`: Dataset utilizado para el análisis.

## Herramientas y Librerías
- **Lenguaje:** R
- **Librerías:** `ggplot2`, `arrow`, `formattable`, `corrplot`, `dplyr`, `skimr`

## Cómo ejecutar el proyecto
1. Descarga el archivo `Project1 - Análisis de Personalidad y Consumo.Rmd`.
2. Abre el archivo en RStudio.
3. Instala las librerías requeridas ejecutando:
   ```R
   install.packages(c("ggplot2", "dplyr", "psych", "corrplot"))
