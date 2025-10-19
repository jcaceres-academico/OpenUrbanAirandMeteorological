# ===============================
# Fig. 1 – Mapa de estaciones de Madrid
# ===============================

# 1. Cargar librerías
library(ggplot2)

setwd("C:\\Users\\cacerestj\\Desktop\\00_FORMACION\\00 UNIVERSIDAD COMPLUTENSE\\05 DOCTORADO\\02 ARTICULOS\\2025_26\\4_R5EDI 2025\\datos\\Estaciones_Meteorologicas")


# 2. Cargar datos con el formato correcto
estaciones <- read.csv("estaciones.csv", sep = ";")

# 3. Aseguramos que LONGITUD y LATITUD sean numéricos
estaciones$LONGITUD <- as.numeric(estaciones$LONGITUD)
estaciones$LATITUD  <- as.numeric(estaciones$LATITUD)

# 4. Filtramos filas con coordenadas válidas
estaciones_clean <- estaciones[!is.na(estaciones$LONGITUD) & !is.na(estaciones$LATITUD), ]

# 5. Graficamos
ggplot(estaciones_clean, aes(x = LONGITUD, y = LATITUD, color = NOM_TIPO)) +
  geom_point(size = 5, alpha = 0.9) +
  scale_color_manual(values = c("Urbana tráfico" = "red",
                                "Urbana fondo"   = "blue",
                                "Suburbana"      = "green")) +
  theme_minimal(base_size = 14) +
  coord_cartesian(xlim = c(-3.9, -3.5), ylim = c(40.3, 40.6)) +
  labs(title = "Red de estaciones de calidad del aire - Madrid",
       x = "Longitud", y = "Latitud", color = "Tipo de estación")


# ==========================================
# UNIFICACIÓN ANUAL DE DATOS DE CALIDAD DEL AIRE (2020–2024)
# ==========================================

library(dplyr)
library(lubridate)
library(tidyr)

# 1. Ruta base donde están las carpetas 2020, 2021, etc.
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"

# 2. Años a procesar
anios <- 2020:2024

# 3. Bucle por cada año
for (anio in anios) {
  
  carpeta_anio <- file.path(ruta_base, as.character(anio))
  
  # Buscar todos los CSV dentro de la carpeta del año
  archivos_csv <- list.files(path = carpeta_anio, pattern = "\\.csv$", 
                             full.names = TRUE, recursive = FALSE)
  
  message("\n📁 Año ", anio, " → ", length(archivos_csv), " archivos encontrados.")
  
  # Saltar si no hay CSV
  if (length(archivos_csv) == 0) {
    warning("⚠️ No se encontraron CSV en ", carpeta_anio)
    next
  }
  
  # Leer y unir todos los CSV de ese año
  datos_raw <- do.call(rbind, lapply(archivos_csv, function(fichero) {
    message("   Leyendo: ", basename(fichero))
    read.csv(fichero, sep = ";", header = TRUE, fileEncoding = "ISO-8859-1")
  }))
  
  # Normalizar nombres
  names(datos_raw) <- toupper(names(datos_raw))
  
  # Crear columna FECHA
  datos_raw <- datos_raw %>%
    mutate(FECHA = make_date(year = ANO, month = MES, day = DIA))
  
  # Pasar a formato largo (una fila por hora)
  datos_largo <- datos_raw %>%
    pivot_longer(
      cols = starts_with("H"),
      names_to = "HORA_TXT",
      values_to = "VALOR"
    ) %>%
    mutate(
      HORA = as.numeric(sub("H", "", HORA_TXT)),
      FECHA_HORA = make_datetime(year = ANO, month = MES, day = DIA, hour = HORA - 1)
    ) %>%
    select(PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD, PUNTO_MUESTREO,
           ANO, MES, DIA, HORA, FECHA_HORA, VALOR)
  
  # Guardar el resultado en la carpeta principal
  salida <- file.path(ruta_base, paste0("datos_calidad_unificado_", anio, ".csv"))
  write.csv(datos_largo, salida, row.names = FALSE)
  
  message("✅ Archivo creado: ", salida)
}

cat("\n🎯 Proceso completado. Archivos anuales creados en la carpeta principal.\n")


# ==========================================
# Fig. 4 – Annual Variability of NO₂ and O₃ (2020–2024)
# ==========================================

library(tidyverse)
library(lubridate)

# 1. Ruta base donde están los CSV unificados por año
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"

# 2. Leer y combinar todos los ficheros anuales
archivos_csv <- list.files(path = ruta_base,
                           pattern = "datos_calidad_unificado_\\d{4}\\.csv$",
                           full.names = TRUE)

datos_all <- archivos_csv %>%
  map_dfr(read_csv, show_col_types = FALSE)

# 3. Crear columna FECHA a partir de FECHA_HORA
datos_all <- datos_all %>%
  mutate(FECHA = as.Date(FECHA_HORA))

# 4. Filtrar magnitudes de interés
# Códigos: NO₂ = 8 | O₃ = 14
datos_filtrados <- datos_all %>%
  filter(MAGNITUD %in% c(8, 14)) %>%
  mutate(
    MAGNITUD = factor(MAGNITUD,
                      levels = c(8, 14),
                      labels = c("NO₂", "O₃")),
    ANO = as.factor(ANO)
  )

# 5. Calcular media diaria por contaminante y año
datos_diarios <- datos_filtrados %>%
  group_by(ANO, FECHA, MAGNITUD) %>%
  summarise(valor_medio = mean(VALOR, na.rm = TRUE), .groups = "drop")

# 6. Crear boxplot anual
fig4 <- ggplot(datos_diarios, aes(x = ANO, y = valor_medio, fill = MAGNITUD)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(
    name = "Pollutant",
    values = c("NO₂" = "steelblue3", "O₃" = "darkorange2"),
    labels = c(expression(NO[2]), expression(O[3]))
  ) +
  labs(
    title = expression("Annual Variability of " * NO[2] * " and " * O[3] * " (2020–2024)"),
    x = "Year",
    y = expression("Mean daily concentration (" * mu * "g/m"^3 * ")"),
    fill = "Pollutant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# 7. Mostrar la figura
print(fig4)

# 8. Guardar en alta resolución (MDPI requirement)
ggsave(filename = file.path(ruta_base, "Fig4_Annual_Variability_NO2_O3.tiff"),
       plot = fig4, dpi = 300, width = 14, height = 10, units = "cm")

cat("\n✅ Fig.4 generada y guardada correctamente en la carpeta de datos.\n")



# =====================================================
# Fig. 5a – Prophet-based forecasting of daily NO₂ concentrations (2020–2024)
# =====================================================

library(tidyverse)
library(lubridate)
library(prophet)

# --- 1. Ruta base donde están los CSV anuales unificados
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"

# --- 2. Cargar y combinar todos los años
archivos_csv <- list.files(path = ruta_base, 
                           pattern = "datos_calidad_unificado_\\d{4}\\.csv$", 
                           full.names = TRUE)

datos_all <- archivos_csv %>%
  map_dfr(read_csv, show_col_types = FALSE)

# --- 3. Filtrar NO₂ (MAGNITUD = 8) y calcular promedio diario
datos_no2 <- datos_all %>%
  filter(MAGNITUD == 8) %>%
  mutate(FECHA = as.Date(FECHA_HORA)) %>%
  group_by(FECHA) %>%
  summarise(y = mean(VALOR, na.rm = TRUE)) %>%
  rename(ds = FECHA) %>%
  arrange(ds)

# --- 4. Dividir en entrenamiento (2020–2023) y validación (enero–marzo 2024)
train <- datos_no2 %>% filter(ds < as.Date("2024-04-01"))
test  <- datos_no2 %>% filter(ds >= as.Date("2024-04-01"))

# --- 5. Ajustar el modelo Prophet
m <- prophet(train,
             yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             changepoint.prior.scale = 0.4)

# --- 6. Crear dataframe futuro (90 días más allá del último registro)
future <- make_future_dataframe(m, periods = 90)
forecast <- predict(m, future)

# --- 7. Combinar observaciones y predicciones
resultados <- forecast %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  left_join(datos_no2, by = "ds")

# --- 8. Calcular métricas de rendimiento (MAE y RMSE)
MAE <- mean(abs(resultados$y - resultados$yhat), na.rm = TRUE)
RMSE <- sqrt(mean((resultados$y - resultados$yhat)^2, na.rm = TRUE))

# --- 9. Crear gráfico (Fig. 5)
fig5 <- ggplot(resultados, aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper),
              fill = "grey80", alpha = 0.4) +
  geom_line(aes(y = y, colour = "Observed"), linewidth = 0.6) +
  geom_line(aes(y = yhat, colour = "Predicted"), linewidth = 0.8) +
  scale_colour_manual(
    name = NULL,
    values = c("Observed" = "steelblue3", "Predicted" = "darkorange2"),
    labels = c(expression("Observed  " * NO[2]), expression("Predicted  " * NO[2]))
  ) +
  labs(
    title = expression("Observed vs Predicted Daily Concentrations of " * NO[2] * " (2020–2024)"),
    x = "Date",
    y = expression("Concentration (" * mu * "g/m"^3 * ")"),
    caption = paste0("MAE = ", round(MAE, 2), "   RMSE = ", round(RMSE, 2),
                     "   Data source: Madrid Open Data Portal")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text.align = 0,
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- 10. Mostrar gráfico
print(fig5)

# --- 11. Guardar en alta resolución (formato MDPI)
ggsave(file.path(ruta_base, "Fig5_Prophet_NO2_Forecast.tiff"),
       plot = fig5, dpi = 300, width = 16, height = 10, units = "cm")

cat("\n✅ Fig.5 generada correctamente. MAE =", round(MAE, 2), "RMSE =", round(RMSE, 2), "\n")



# =====================================================
# Fig. 5b – Prophet-based forecasting of daily O₃ concentrations (2020–2024)
# =====================================================

library(tidyverse)
library(lubridate)
library(prophet)

# 1. Ruta base
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"

# 2. Leer y combinar archivos anuales
archivos_csv <- list.files(path = ruta_base,
                           pattern = "datos_calidad_unificado_\\d{4}\\.csv$",
                           full.names = TRUE)

datos_all <- archivos_csv %>%
  map_dfr(read_csv, show_col_types = FALSE)

# 3. Filtrar O₃ (MAGNITUD = 14) y calcular promedio diario
datos_o3 <- datos_all %>%
  filter(MAGNITUD == 14) %>%
  mutate(FECHA = as.Date(FECHA_HORA)) %>%
  group_by(FECHA) %>%
  summarise(y = mean(VALOR, na.rm = TRUE)) %>%
  rename(ds = FECHA) %>%
  arrange(ds)

# 4. Dividir en entrenamiento (2020–2023) y validación (enero–marzo 2024)
train_o3 <- datos_o3 %>% filter(ds < as.Date("2024-04-01"))
test_o3  <- datos_o3 %>% filter(ds >= as.Date("2024-04-01"))

# 5. Ajustar modelo Prophet (misma configuración que NO₂)
m_o3 <- prophet(train_o3,
                yearly.seasonality = TRUE,
                weekly.seasonality = TRUE,
                changepoint.prior.scale = 0.3)

# 6. Generar pronóstico (90 días adicionales)
future_o3 <- make_future_dataframe(m_o3, periods = 90)
forecast_o3 <- predict(m_o3, future_o3)

# 7. Combinar observaciones y predicciones
resultados_o3 <- forecast_o3 %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  left_join(datos_o3, by = "ds")

# 8. Calcular métricas
MAE_o3  <- mean(abs(resultados_o3$y - resultados_o3$yhat), na.rm = TRUE)
RMSE_o3 <- sqrt(mean((resultados_o3$y - resultados_o3$yhat)^2, na.rm = TRUE))

# 9. Crear gráfico Fig. 5b
fig5b <- ggplot(resultados_o3, aes(x = ds)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper),
              fill = "grey80", alpha = 0.4) +
  geom_line(aes(y = y, colour = "Observed"), linewidth = 0.6) +
  geom_line(aes(y = yhat, colour = "Predicted"), linewidth = 0.8) +
  scale_colour_manual(
    name = NULL,
    values = c("Observed" = "steelblue3", "Predicted" = "darkorange2"),
    labels = c(expression("Observed  " * O[3]), expression("Predicted  " * O[3]))
  ) +
  labs(
    title = expression("Observed vs Predicted Daily Concentrations of " * O[3] * " (2020–2024)"),
    x = "Date",
    y = expression("Concentration (" * mu * "g/m"^3 * ")"),
    caption = paste0("MAE = ", round(MAE_o3, 2), "   RMSE = ", round(RMSE_o3, 2),
                     "   Data source: Madrid Open Data Portal")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text.align = 0,
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(fig5b)

# 10. Guardar la figura (formato MDPI)
ggsave(file.path(ruta_base, "Fig5b_Prophet_O3_Forecast.tiff"),
       plot = fig5b, dpi = 300, width = 16, height = 10, units = "cm")

cat("\n✅ Fig.5b (O₃) generada correctamente. MAE =", round(MAE_o3, 2), "RMSE =", round(RMSE_o3, 2), "\n")



# ==========================================
# Fig. 6 – Learning and Reproducibility Ecosystem
# ==========================================
# Author: Jesús Cáceres Tello & J.J. Galán Hernández
# Journal: Applied Sciences (MDPI)
# ==========================================

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# 1. Ruta base
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"
# Crear gráfico con DiagrammeR
gr <- grViz("
digraph learning_reproducibility {
  
  graph [layout = dot, rankdir = LR,
         bgcolor = 'transparent',
         fontsize = 20,
         fontname = 'Helvetica']

  node [shape = rectangle,
        style = filled,
        color = '#4A6FA5',
        fontcolor = black,
        fontname = 'Helvetica',
        penwidth = 1.2,
        width = 2.8,
        height = 0.9,
        fixedsize = false]

  # Nodos
  A [label = 'Open Data Access\\n(Environmental datasets)',
     fillcolor = '#CFE2F3']
  B [label = 'Reproducible Analysis\\n(R + tidyverse + Prophet)',
     fillcolor = '#D9EAD3']
  C [label = 'Exploratory & Forecasting Results\\n(Visualisation, Prediction)',
     fillcolor = '#EAD1DC']
  D [label = 'Quarto Documentation\\n(Code, Text, Results)',
     fillcolor = '#FCE5CD']
  E [label = 'STEM & Citizen Learning\\n(Awareness, Digital Literacy)',
     fillcolor = '#FFF2CC']
  
  # Conexiones
  A -> B [color = '#4A6FA5', penwidth = 1.4]
  B -> C [color = '#4A6FA5', penwidth = 1.4]
  C -> D [color = '#4A6FA5', penwidth = 1.4]
  D -> E [color = '#4A6FA5', penwidth = 1.4]
  
  labelloc = t
}
")

# Mostrar en visor RStudio
gr

# Exportar a PNG (alta resolución)
ruta_salida <- file.path(ruta_base, "Fig6_Learning_Reproducibility_Ecosystem.png")
export_svg(gr) |> charToRaw() |> rsvg_png(ruta_salida, width = 2000, height = 300)

cat("✅ Figura guardada en:", ruta_salida, "\n")




library(readr)

ruta <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia/2020/ene_meteo20.csv"

df <- read_csv2(ruta, n_max = 5, locale = locale(encoding = "ISO-8859-1"))
names(df)
str(df[, 1:15])


# ============================================================
# UNIFICACIÓN ANUAL DE DATOS METEOROLÓGICOS HORARIOS (2020–2024)
# Formato: H01–H24 con códigos de validación V01–V24
# ============================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia"
anios <- 2020:2024

for (anio in anios) {
  
  carpeta_anio <- file.path(ruta_base, as.character(anio))
  archivos_csv <- list.files(path = carpeta_anio, pattern = "\\.csv$", full.names = TRUE)
  
  message("\n📁 Año ", anio, " → ", length(archivos_csv), " archivos meteorológicos encontrados.")
  if (length(archivos_csv) == 0) next
  
  datos_anio <- lapply(archivos_csv, function(fichero) {
    message("   Leyendo: ", basename(fichero))
    
    df <- read.csv(fichero, sep = ";", header = TRUE, fileEncoding = "ISO-8859-1")
    names(df) <- toupper(trimws(names(df)))
    
    # Detectar columnas horarias Hxx y validaciones Vxx
    col_horas <- grep("^H\\d{2}$", names(df), value = TRUE)
    col_valid <- grep("^V\\d{2}$", names(df), value = TRUE)
    
    # Emparejar solo las que tengan correspondencia exacta
    n <- min(length(col_horas), length(col_valid))
    col_horas <- col_horas[1:n]
    col_valid <- col_valid[1:n]
    
    # Construir registros hora a hora
    registros <- map_dfr(seq_len(n), function(i) {
      tibble(
        PROVINCIA = df$PROVINCIA,
        MUNICIPIO = df$MUNICIPIO,
        ESTACION = df$ESTACION,
        MAGNITUD = df$MAGNITUD,
        PUNTO_MUESTREO = df$PUNTO_MUESTREO,
        ANO = as.numeric(df$ANO),
        MES = as.numeric(df$MES),
        DIA = as.numeric(df$DIA),
        HORA = i,
        VALOR = df[[col_horas[i]]],
        VALID = df[[col_valid[i]]]
      )
    })
    
    # Filtrar solo valores validados
    registros %>% filter(VALID == "V")
  })
  
  datos_limpios <- bind_rows(datos_anio) %>%
    mutate(
      FECHA_HORA = make_datetime(year = ANO, month = MES, day = DIA, hour = HORA - 1),
      VALOR = suppressWarnings(as.numeric(VALOR))
    ) %>%
    select(PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD, FECHA_HORA, VALOR)
  
  salida <- file.path(ruta_base, paste0("meteo_unificado_", anio, ".csv"))
  write.csv(datos_limpios, salida, row.names = FALSE)
  
  message("✅ Archivo creado: ", basename(salida), " (", nrow(datos_limpios), " filas)")
}

cat("\n🎯 Proceso completado. Archivos meteorológicos anuales creados correctamente.\n")

# ============================================================
# Códigos de magnitud (según Ayuntamiento de Madrid):
# 81 = Velocidad viento (m/s)
# 82 = Dirección viento (°)
# 83 = Temperatura (°C)
# 86 = Humedad relativa (%)
# 88 = Radiación solar (W/m²)
# 89 = Precipitación (l/m²)
# ============================================================



# ============================================================
# Fig. 7 — Workflow for harmonising meteorological data (R–Quarto)
# ============================================================

# Instalar si no están
# install.packages(c("ggplot2", "grid", "cowplot", "magick"))

library(ggplot2)
library(grid)
library(cowplot)
library(magick)

# Paleta coherente con Figs. previas
col_inputs  <- "#CFE9D4"  # verde suave
col_process <- "#CFE2F3"  # azul claro
col_output  <- "#FCE5CD"  # naranja claro

# ---- Contenidos de los tres bloques ----
text_inputs <- "
**Inputs**

**Meteorological data (2020–2024)**
Temperature (°C), relative humidity (%),
wind speed (m s⁻¹), wind direction (°),
solar radiation (W m⁻²), precipitation (mm)

**Station metadata**
Station ID, coordinates (ETRS89),
altitude, typology (traffic / background / suburban)
"

text_processing <- "
**Processing in R–Quarto**

1. Import and merge monthly CSV files  
   *(readr / vroom)*
2. Reshape hourly fields (H01–H24) → tidy long format  
   *(pivot_longer)*
3. Filter validated observations (V01–V24)
4. Aggregate to daily means  
   (T, RH, WS, SR, P)
5. Derive dynamic covariates  
   • Wind components (u,v)  
   • Calm (WS < 1 m s⁻¹)  
   • High-insolation days (SR > P₇₅)
"

text_output <- "
**Integration and Output**

• Join with pollutant data (NO₂, O₃)
  by station–date  
• Generate harmonised dataset (2020–2024)  
  Ready for correlation and forecasting analyses
"

# ---- Función auxiliar para crear cada bloque ----
make_block <- function(text, fill_col){
  ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = text,
             size = 3.2, family = "sans", lineheight = 1.1,
             hjust = 0, vjust = 1, parse = FALSE) +
    theme(
      plot.background = element_rect(fill = fill_col, colour = "grey50", linewidth = 0.4),
      plot.margin = margin(10,10,10,10)
    )
}

g1 <- make_block(text_inputs,  col_inputs)
g2 <- make_block(text_processing, col_process)
g3 <- make_block(text_output, col_output)

# ---- Flechas ----
arrow_right <- grid::linesGrob(arrow = grid::arrow(type = "closed", length = unit(0.25, "cm")),
                               gp = grid::gpar(col = "grey40", lwd = 2),
                               x = unit(c(0,1), "npc"), y = unit(0.5, "npc"))

# ---- Combinar bloques y flechas ----
fig7 <- plot_grid(
  g1, NULL, g2, NULL, g3,
  nrow = 1, rel_widths = c(1, 0.05, 1.2, 0.05, 1),
  labels = NULL
)

# Añadir flechas visuales
fig7_final <- ggdraw(fig7) +
  draw_grob(arrow_right, x = 0.33, y = 0.5, width = 0.04, height = 0.02) +
  draw_grob(arrow_right, x = 0.66, y = 0.5, width = 0.04, height = 0.02)

# ---- Exportar en formato TIFF (MDPI) ----
ruta_base <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI_2025/datos/Meteorologia"

if(!dir.exists(ruta_base)) dir.create(ruta_base, recursive = TRUE)

ggsave(
  file.path(ruta_base, "Fig7_Integracion_Meteorologica_2020_2024.tiff"),
  plot = fig7_final,
  dpi = 300, width = 16, height = 10, units = "cm",
  compression = "lzw"
)

fig7_final





# ==========================================================================================
# FUNCIÓN: LIMPIEZA 1 MEJORADA
# CORRIGE ERRORES DE FORMATO EN DIA ('01 → 1), MUNICIPIO ('079 → 79), ETC.
# CONVIERTE COLUMNAS CLAVE A ENTERO Y ELIMINA PUNTO_MUESTREO.
# GUARDA VERSIONES *_validado.csv EN NUEVA CARPETA METEOROLOGIA_VALIDADOS.
# ==========================================================================================

library(tidyverse)

validar_csvs_meteo_anio <- function(ruta_origen_anio, ruta_destino_anio) {
  dir.create(ruta_destino_anio, showWarnings = FALSE, recursive = TRUE)
  
  archivos <- list.files(ruta_origen_anio, pattern = "\\.csv$", full.names = TRUE)
  
  for (f in archivos) {
    message("Procesando: ", basename(f))
    
    df <- read_delim(
      f,
      delim = ";",
      locale = locale(encoding = "latin1"),
      show_col_types = FALSE
    )
    
    names(df) <- toupper(names(df))
    
    # --- Eliminar columna redundante ---
    if ("PUNTO_MUESTREO" %in% names(df)) {
      df <- select(df, -PUNTO_MUESTREO)
    }
    
    # --- Columnas numéricas susceptibles de errores de formato ---
    columnas_a_limpiar <- c("PROVINCIA", "MUNICIPIO", "ESTACION", "ANO", "MES", "DIA")
    
    for (col in columnas_a_limpiar) {
      if (col %in% names(df)) {
        # Elimina cualquier carácter no numérico y convierte a entero
        df[[col]] <- str_replace_all(df[[col]], "[^0-9]", "")
        df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      }
    }
    
    # --- Guardar archivo validado ---
    nombre_salida <- str_replace(basename(f), "\\.csv$", "_validado.csv")
    ruta_salida <- file.path(ruta_destino_anio, nombre_salida)
    
    write_delim(df, ruta_salida, delim = ";", na = "", append = FALSE)
    message("✓ Guardado: ", nombre_salida)
  }
  
  message("✔️  Año completado: ", basename(ruta_origen_anio))
}

# ============================================================
# FUNCIÓN PRINCIPAL: recorrer todos los años
# ============================================================

validar_todos_los_anios <- function(ruta_base_origen, ruta_base_destino, anios = 2020:2024) {
  for (anio in anios) {
    message("\n===== Año ", anio, " =====")
    ruta_origen_anio  <- file.path(ruta_base_origen,  as.character(anio))
    ruta_destino_anio <- file.path(ruta_base_destino, as.character(anio))
    
    validar_csvs_meteo_anio(ruta_origen_anio, ruta_destino_anio)
  }
  message("\n✅ Validación completa de todos los años.")
}

# ============================================================
# RUTAS BASE Y EJECUCIÓN
# ============================================================

ruta_base_origen  <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia"
ruta_base_destino <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia_Validados"

# Ejecutar para todos los años 2020–2024
validar_todos_los_anios(ruta_base_origen, ruta_base_destino)



# ==========================================================================================
# FUNCIÓN: LIMPIEZA 2: PIVOTAR HORAS, CREAR FECHA_HORA, FILTRAR POR VALORES VALIDADOS.
#         CREA ARCHIVOS .parquet EN UNA SOLA CARPETA
# ==========================================================================================

library(tidyverse)
library(lubridate)
library(arrow)

procesar_meteo_validado <- function(ruta_archivo) {
  message("→ Procesando: ", basename(ruta_archivo))
  
  df <- read_delim(ruta_archivo, delim = ";",
                   locale = locale(encoding = "latin1"), show_col_types = FALSE)
  names(df) <- toupper(names(df))
  
  # --- Identificar columnas Hxx / Vxx ---
  h_cols <- grep("^H\\d{2}$", names(df), value = TRUE)
  v_cols <- grep("^V\\d{2}$", names(df), value = TRUE)
  if (length(h_cols) == 0 || length(v_cols) == 0) {
    warning("Saltado (sin H/V): ", basename(ruta_archivo))
    return(NULL)
  }
  
  # --- Pivotar valores horarios ---
  h_long <- df %>%
    pivot_longer(
      cols = all_of(h_cols),
      names_to = "HORA_STR",
      names_pattern = "H(\\d{2})",
      values_to = "VALOR"
    ) %>%
    mutate(HORA = as.integer(HORA_STR)) %>%
    select(-HORA_STR)
  
  # --- Pivotar validaciones ---
  v_long <- df %>%
    pivot_longer(
      cols = all_of(v_cols),
      names_to = "HORA_STR",
      names_pattern = "V(\\d{2})",
      values_to = "VALIDACION"
    ) %>%
    mutate(HORA = as.integer(HORA_STR)) %>%
    select(ANO, MES, DIA, ESTACION, MAGNITUD, HORA, VALIDACION)
  
  # --- Unir valores y validaciones ---
  df_long <- left_join(
    h_long, v_long,
    by = c("ANO", "MES", "DIA", "ESTACION", "MAGNITUD", "HORA")
  ) %>%
    filter(VALIDACION == "V") %>%
    mutate(
      VALOR = suppressWarnings(as.numeric(str_replace(VALOR, ",", "."))),
      FECHA_HORA = make_datetime(ANO, MES, DIA, HORA, 0, 0, tz = "Europe/Madrid")
    ) %>%
    select(PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD,
           ANO, MES, DIA, FECHA_HORA, HORA, VALOR)
  
  return(df_long)
}

# ============================================================
# PROCESAR TODOS LOS ARCHIVOS DE UN AÑO (SIN SUBCARPETAS)
# ============================================================

procesar_meteo_anio <- function(ruta_anio_validados, ruta_destino) {
  dir.create(ruta_destino, showWarnings = FALSE, recursive = TRUE)
  
  archivos <- list.files(ruta_anio_validados, pattern = "_validado\\.csv$", full.names = TRUE)
  if (length(archivos) == 0) {
    warning("No hay archivos validados en ", ruta_anio_validados)
    return(NULL)
  }
  
  lista_meses <- lapply(archivos, function(f) {
    tryCatch(procesar_meteo_validado(f), error = function(e) {
      message("⚠️ Error en ", basename(f), ": ", e$message)
      NULL
    })
  })
  
  meteo_anual <- bind_rows(lista_meses) %>% arrange(FECHA_HORA)
  
  # --- Guardar Parquet anual en carpeta raíz ---
  anio <- basename(ruta_anio_validados)
  nombre_parquet <- paste0("meteo_validados_", anio, ".parquet")
  ruta_parquet <- file.path(ruta_destino, nombre_parquet)
  
  write_parquet(meteo_anual, ruta_parquet)
  message("✓ Parquet anual guardado: ", ruta_parquet)
  
  return(meteo_anual)
}

# ============================================================
# BUCLE GENERAL PARA TODOS LOS AÑOS (2020–2024)
# ============================================================

procesar_todos_los_anios <- function(ruta_base_validados, ruta_base_parquet, anios = 2020:2024) {
  for (anio in anios) {
    message("\n===== AÑO ", anio, " =====")
    ruta_validados_anio <- file.path(ruta_base_validados, as.character(anio))
    procesar_meteo_anio(ruta_validados_anio, ruta_base_parquet)
  }
  message("\n✅ Procesamiento completo de todos los años.")
}

# ============================================================
# EJECUCIÓN GLOBAL
# ============================================================

ruta_base_validados <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia_Validados"
ruta_base_parquet   <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia_Parquet"

procesar_todos_los_anios(ruta_base_validados, ruta_base_parquet)

# ============================================================
# COMPROBACIÓN
# ============================================================

library(tidyverse)
library(arrow)

# Ruta base donde se guardaron los archivos parquet
ruta_base_parquet <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Meteorologia_Parquet"

# Elegimos uno al azar (por ejemplo, 2022)
ruta_archivo <- file.path(ruta_base_parquet, "meteo_validados_2022.parquet")

# Cargar el parquet
meteo2022 <- read_parquet(ruta_archivo)

# Mostrar estructura general
glimpse(meteo2022)

# Mostrar 100 primeras filas con columnas relevantes
meteo2022 %>%
  arrange(FECHA_HORA) %>%
  select(PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD, FECHA_HORA, HORA, VALOR) %>%
  print(n = 100)




# ==========================================================================================
# ==========================================================================================
# FUNCIÓN: LIMPIEZA 1 (MEJORADA) PARA CALIDAD DEL AIRE
# Corrige errores en columnas de fecha e identificadores y limpia valores horarios (Hxx)
# ==========================================================================================
# ==========================================================================================

library(tidyverse)

validar_csvs_aire_anio <- function(ruta_origen_anio, ruta_destino_anio) {
  dir.create(ruta_destino_anio, showWarnings = FALSE, recursive = TRUE)
  
  archivos <- list.files(ruta_origen_anio, pattern = "\\.csv$", full.names = TRUE)
  
  for (f in archivos) {
    message("Procesando: ", basename(f))
    
    df <- read_delim(
      f,
      delim = ";",
      locale = locale(encoding = "latin1"),
      show_col_types = FALSE
    )
    
    names(df) <- toupper(names(df))
    
    # --- Eliminar columna redundante ---
    if ("PUNTO_MUESTREO" %in% names(df)) {
      df <- select(df, -PUNTO_MUESTREO)
    }
    
    # --- Limpieza de columnas de identificación / fecha ---
    columnas_a_limpiar <- c("PROVINCIA", "MUNICIPIO", "ESTACION", "ANO", "MES", "DIA")
    for (col in columnas_a_limpiar) {
      if (col %in% names(df)) {
        df[[col]] <- str_replace_all(df[[col]], "[^0-9]", "")
        df[[col]] <- suppressWarnings(as.integer(df[[col]]))
      }
    }
    
    # --- Limpieza y conversión de valores horarios H01–H24 ---
    h_cols <- grep("^H\\d{2}$", names(df), value = TRUE)
    if (length(h_cols) > 0) {
      df <- df %>%
        mutate(across(all_of(h_cols), ~ {
          x <- as.character(.x)
          x <- str_replace_all(x, ",", ".")         # sustituir comas por puntos
          x <- str_replace_all(x, "^0+([0-9])", "\\1")  # eliminar ceros iniciales
          suppressWarnings(as.numeric(x))           # convertir a número
        }))
    }
    
    # --- Guardar archivo validado ---
    nombre_salida <- str_replace(basename(f), "\\.csv$", "_validado.csv")
    ruta_salida <- file.path(ruta_destino_anio, nombre_salida)
    
    write_delim(df, ruta_salida, delim = ";", na = "", append = FALSE)
    message("✓ Guardado: ", nombre_salida)
  }
  
  message("✔️  Año completado: ", basename(ruta_origen_anio))
}

# ============================================================
# FUNCIÓN PRINCIPAL PARA TODOS LOS AÑOS
# ============================================================

validar_todos_los_anios_aire <- function(ruta_base_origen, ruta_base_destino, anios = 2020:2024) {
  for (anio in anios) {
    message("\n===== Año ", anio, " =====")
    ruta_origen_anio  <- file.path(ruta_base_origen,  as.character(anio))
    ruta_destino_anio <- file.path(ruta_base_destino, as.character(anio))
    
    validar_csvs_aire_anio(ruta_origen_anio, ruta_destino_anio)
  }
  message("\n✅ Validación completa de todos los años (Calidad del Aire).")
}

# ============================================================
# RUTAS BASE Y EJECUCIÓN
# ============================================================

ruta_base_origen_aire  <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire"
ruta_base_destino_aire <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Validados"

# Ejecutar para todos los años 2020–2024
validar_todos_los_anios_aire(ruta_base_origen_aire, ruta_base_destino_aire)




# ==========================================================================================
# FUNCIÓN: LIMPIEZA 2 (CALIDAD DEL AIRE)
# Pivotar horas, crear FECHA_HORA, filtrar por valores validados ('V')
# y guardar archivos .parquet en una única carpeta
# ==========================================================================================

library(tidyverse)
library(lubridate)
library(arrow)

procesar_aire_validado <- function(ruta_archivo) {
  message("→ Procesando: ", basename(ruta_archivo))
  
  df <- read_delim(ruta_archivo, delim = ";",
                   locale = locale(encoding = "latin1"), show_col_types = FALSE)
  names(df) <- toupper(names(df))
  
  # --- Identificar columnas Hxx (valores) y Vxx (validaciones) ---
  h_cols <- grep("^H\\d{2}$", names(df), value = TRUE)
  v_cols <- grep("^V\\d{2}$", names(df), value = TRUE)
  if (length(h_cols) == 0 || length(v_cols) == 0) {
    warning("Saltado (sin H/V): ", basename(ruta_archivo))
    return(NULL)
  }
  
  # --- Pivotar valores horarios ---
  h_long <- df %>%
    pivot_longer(
      cols = all_of(h_cols),
      names_to = "HORA_STR",
      names_pattern = "H(\\d{2})",
      values_to = "VALOR"
    ) %>%
    mutate(HORA = as.integer(HORA_STR)) %>%
    select(-HORA_STR)
  
  # --- Pivotar validaciones ---
  v_long <- df %>%
    pivot_longer(
      cols = all_of(v_cols),
      names_to = "HORA_STR",
      names_pattern = "V(\\d{2})",
      values_to = "VALIDACION"
    ) %>%
    mutate(HORA = as.integer(HORA_STR)) %>%
    select(ANO, MES, DIA, ESTACION, MAGNITUD, HORA, VALIDACION)
  
  # --- Unir valores y validaciones ---
  df_long <- left_join(
    h_long, v_long,
    by = c("ANO", "MES", "DIA", "ESTACION", "MAGNITUD", "HORA")
  ) %>%
    filter(VALIDACION == "V") %>%
    mutate(
      VALOR = suppressWarnings(as.numeric(str_replace(VALOR, ",", "."))),
      FECHA_HORA = make_datetime(ANO, MES, DIA, HORA, 0, 0, tz = "Europe/Madrid")
    ) %>%
    select(PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD,
           ANO, MES, DIA, FECHA_HORA, HORA, VALOR) %>%
    arrange(FECHA_HORA)
  
  return(df_long)
}

# ============================================================
# PROCESAR TODOS LOS ARCHIVOS DE UN AÑO (SIN SUBCARPETAS)
# ============================================================

procesar_aire_anio <- function(ruta_anio_validados, ruta_destino) {
  dir.create(ruta_destino, showWarnings = FALSE, recursive = TRUE)
  
  archivos <- list.files(ruta_anio_validados, pattern = "_validado\\.csv$", full.names = TRUE)
  if (length(archivos) == 0) {
    warning("No hay archivos validados en ", ruta_anio_validados)
    return(NULL)
  }
  
  lista_meses <- lapply(archivos, function(f) {
    tryCatch(procesar_aire_validado(f), error = function(e) {
      message("⚠️ Error en ", basename(f), ": ", e$message)
      NULL
    })
  })
  
  aire_anual <- bind_rows(lista_meses) %>% arrange(FECHA_HORA)
  
  # --- Guardar Parquet anual ---
  anio <- basename(ruta_anio_validados)
  nombre_parquet <- paste0("aire_validados_", anio, ".parquet")
  ruta_parquet <- file.path(ruta_destino, nombre_parquet)
  
  write_parquet(aire_anual, ruta_parquet)
  message("✓ Parquet anual guardado: ", ruta_parquet)
  
  return(aire_anual)
}

# ============================================================
# BUCLE GENERAL PARA TODOS LOS AÑOS (2020–2024)
# ============================================================

procesar_todos_los_anios_aire <- function(ruta_base_validados, ruta_base_parquet, anios = 2020:2024) {
  for (anio in anios) {
    message("\n===== AÑO ", anio, " =====")
    ruta_validados_anio <- file.path(ruta_base_validados, as.character(anio))
    procesar_aire_anio(ruta_validados_anio, ruta_base_parquet)
  }
  message("\n✅ Procesamiento completo de todos los años (Calidad del Aire).")
}

# ============================================================
# EJECUCIÓN GLOBAL
# ============================================================

ruta_base_validados_aire <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Validados"
ruta_base_parquet_aire   <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Parquet"

procesar_todos_los_anios_aire(ruta_base_validados_aire, ruta_base_parquet_aire)


# ============================================================
# COMPROBACIÓN
# ============================================================
library(arrow)
ruta_base_parquet_aire   <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Parquet"

aire2023 <- read_parquet(file.path(ruta_base_parquet_aire, "aire_validados_2023.parquet"))
glimpse(aire2023)

aire2023 %>%
  arrange(FECHA_HORA) %>%
  select(ESTACION, MAGNITUD, FECHA_HORA, HORA, VALOR) %>%
  print(n = 200)



