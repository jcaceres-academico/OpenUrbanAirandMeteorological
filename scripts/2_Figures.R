


# ======================================================================================
# FIGURA 2a — Versión 7.3 (proyección Web Mercator EPSG:3857 + mapa OSM centrado)
# ======================================================================================

library(tidyverse)
library(sf)
library(ggspatial)
library(ggrepel)
library(ggtext)

# --- Crear objeto sf en WGS84 ---
est_sf <- estaciones %>%
  mutate(
    NOM_TIPO_EN = case_when(
      COD_TIPO == "UT" ~ "Urban Traffic",
      COD_TIPO == "UF" ~ "Urban Background",
      COD_TIPO == "S"  ~ "Suburban",
      TRUE ~ "Other"
    ),
    N_CONTAMINANTES = rowSums(select(., NO2, SO2, CO, PM10, PM2_5, O3, BTX) == "X", na.rm = TRUE)
  ) %>%
  st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)   # ✅ Convertir a Web Mercator

# --- Bounding box ampliado (en metros ahora) ---
bb <- st_bbox(est_sf)
xlim_vals <- c(bb["xmin"] - 2000, bb["xmax"] + 2000)  # 2 km de margen
ylim_vals <- c(bb["ymin"] - 2000, bb["ymax"] + 2000)

# --- Mapa base con proyección correcta ---
mapa_estaciones <- ggplot() +
  annotation_map_tile(
    type = "cartolight",   # osm
    zoomin = 0
  ) +
  geom_sf(
    data = est_sf,
    aes(colour = NOM_TIPO_EN, size = N_CONTAMINANTES),
    alpha = 0.95
  ) +
  suppressWarnings(
    geom_text_repel(
      data = est_sf,
      aes(label = ESTACION, geometry = geometry),
      stat = "sf_coordinates",
      size = 2.8, colour = "black", max.overlaps = 12
    )
  ) +
  annotation_scale(location = "br", width_hint = 0.35, text_cex = 0.8,
                   bar_cols = c("black", "white")) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.25, "cm"), pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering(text_col = "grey20")
  ) +
  scale_colour_manual(
    values = c(
      "Urban Traffic" = "#80B1D3",
      "Urban Background" = "#FB8072",
      "Suburban" = "#B3DE69"
    ),
    name = "Monitoring site type",
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  scale_size_continuous(name = "Nº of pollutants", range = c(3, 7)) +
  labs(
   # title = "Fig. 2a — Spatial distribution and measurement scope\nof Madrid’s air-quality monitoring stations (2020–2024)",
    x = "Longitude (°)", y = "Latitude (°)"
  ) +
  coord_sf(xlim = xlim_vals, ylim = ylim_vals, expand = FALSE, crs = 3857) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, lineheight = 1.1),
    legend.position = "right",
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 9)
  )

# --- Mostrar mapa ---
mapa_estaciones

# --- Guardar TIFF ---
ggsave(
  filename = "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/Fig2a_Mapa_Madrid_Final_v7_3.tiff",
  plot = mapa_estaciones,
  width = 220, height = 155, units = "mm",
  dpi = 300, compression = "lzw"
)





# ===============================================================
# FIGURA 2b — Option 1 : Horizontal bar plot of pollutant coverage
# ===============================================================

library(tidyverse)
library(ggtext)

# ------------------------------------------------------------------
# Leer el archivo CSV corregido
# ------------------------------------------------------------------
estaciones <- read_delim(
  "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Estaciones_Meteorologicas/estaciones.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8"),  # 💡 usa UTF-8 si lo guardaste así
  show_col_types = FALSE
)

# ------------------------------------------------------------------
#  Verificar que se lean correctamente los nombres con acentos
# ------------------------------------------------------------------
estaciones %>%
  filter(str_detect(ESTACION, "M")) %>%
  select(ESTACION)

fig2b_bar <- est_sf %>%
  st_drop_geometry() %>%
  arrange(desc(N_CONTAMINANTES)) %>%
  mutate(ESTACION = fct_reorder(ESTACION, N_CONTAMINANTES))

fig2b <- ggplot(fig2b_bar, aes(x = N_CONTAMINANTES, y = ESTACION, fill = NOM_TIPO_EN)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Urban Traffic" = "#80B1D3",
      "Urban Background" = "#FB8072",
      "Suburban" = "#B3DE69"
    ),
    name = ""
  ) +
  labs(
    x = "Number of pollutants measured",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.text.y = element_text(size = 9, colour = "black"),
    axis.text.x = element_text(size = 9, colour = "black"),
    plot.margin = margin(10, 10, 10, 10)
  )


# Mostrar el gráfico en la ventana de RStudio
print(fig2b)

# Guardar
ggsave( filename = "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/Fig2b_Bar_Coverage.tiff",
       width = 180, height = 130, units = "mm", dpi = 300, compression = "lzw")


# ==========================================================================================
# FIGURE 3 — Data harmonisation and quality-control pipeline (readable text + TIFF 300 dpi)
# ==========================================================================================

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magick)

fig3_code <- "
digraph pipeline {

  graph [layout = dot, rankdir = LR, nodesep = 0.4, ranksep = 0.25]

  node [shape = rectangle, style = filled, color = '#6BAED6',
        fontname = 'Cambria', fontsize = 12, fixedsize = false,
        width = 2.4, height = 0.9, margin = '0.1,0.1', fillcolor = '#E9ECEF']

  edge [color = '#6BAED6', arrowsize = 0.6]

  a [label = 'Raw CSV Files']
  b [label = 'Data Import &\\nFormat Standardisation']
  c [label = 'Quality Filtering\\n(V Validation)']
  d [label = 'Daily Aggregation &\\nOutlier Control']
  e [label = 'Dataset Integration\\n(Air Quality + Meteorology)']
  f [label = 'Tidy Dataset for Analysis', fillcolor = '#9FD76F']

  a -> b -> c -> d -> e -> f
}
"

# --- Export paths ---
svg_path  <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/Fig3_Data_Pipeline_Final.svg"
tiff_path <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/Fig3_Data_Pipeline_Final.tiff"

# --- Export to SVG ---
svg_code <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(fig3_code))
writeLines(svg_code, svg_path)

# --- Convert to high-quality TIFF (≈300 dpi) ---
img <- magick::image_read_svg(svg_path)
img_resized <- magick::image_resize(img, "2400x700")  # buena proporción
magick::image_write(img_resized, path = tiff_path, format = "tiff")

# --- Open automatically ---
browseURL(tiff_path)




# ==========================================================================================
# FIGURE 4 — Annual Variability of NO₂ and O₃ (2020–2024)
# Using aire_validados_YYYY.parquet · TIFF 300 dpi
# ==========================================================================================

library(tidyverse)
library(arrow)
library(lubridate)
library(ggtext)

# -----------------------------------------------------------
# 1️⃣ Directorio con los ficheros parquet de calidad del aire
# -----------------------------------------------------------
ruta_aire <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Parquet"

# Leer y combinar todos los años disponibles
ficheros <- list.files(ruta_aire, pattern = "aire_validados_\\d{4}\\.parquet$", full.names = TRUE)
df_aire <- map_dfr(ficheros, arrow::read_parquet)

# -----------------------------------------------------------
# 2️⃣ Selección y limpieza
# -----------------------------------------------------------
# Códigos oficiales del Ayuntamiento de Madrid:
# 8 = NO₂, 14 = O₃
pollutants <- c(8, 14)

df_clean <- df_aire %>%
  filter(MAGNITUD %in% pollutants) %>%
  mutate(
    FECHA = as.Date(FECHA_HORA),
    YEAR = year(FECHA),
    POLLUTANT = recode(MAGNITUD,
                       `8` = "NO2",
                       `14` = "O3"),
    VALUE = as.numeric(VALOR)
  ) %>%
  filter(between(YEAR, 2020, 2024))

# -----------------------------------------------------------
# 3️⃣ Crear el gráfico
# -----------------------------------------------------------
fig4 <- ggplot(df_clean, aes(x = factor(YEAR), y = VALUE, fill = POLLUTANT)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = 16, outlier.size = 1.3) +
  scale_fill_manual(
    values = c("NO2" = "#1F78B4", "O3" = "#E66101"),
    name = "Pollutant",
    labels = expression(NO[2], O[3])
  ) +
  labs(
    x = "Year",
    y = expression("Mean daily concentration (" * µ * "g/m"^3 * ")")
  ) +
  coord_cartesian(ylim = c(0, 120)) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9, colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# -----------------------------------------------------------
# 4️⃣ Guardar como TIFF (300 dpi, estándar editorial MDPI)
# -----------------------------------------------------------
tiff_path <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/Fig4_NO2_O3_Annual_Variability_Final.tiff"

ggsave(
  filename = tiff_path,
  plot = fig4,
  width = 150, height = 110, units = "mm",
  dpi = 300, compression = "lzw"
)

browseURL(tiff_path)



# =====================================================================
# FINAL CAMERA-READY VERSION — Prophet Forecasting of NO₂ and O₃
# Figures 5a & 5b (2020–2024 observed; 2025 forecast)
# =====================================================================

library(prophet)
library(dplyr)
library(ggplot2)
library(scales)

# ===============================================================
# Función genérica para graficar Prophet con estilo editorial
# ===============================================================

plot_prophet_forecast <- function(df, forecast, pollutant_expression, mae, rmse,
                                  end_obs = "2024-12-31", end_pred = "2025-12-31") {
  
  df_obs <- df %>% filter(as.Date(ds) <= as.Date(end_obs))
  forecast_ext <- forecast %>% filter(as.Date(ds) <= as.Date(end_pred))
  
  # --- Detectar contaminante ---
  pollutant_str <- if (deparse(substitute(pollutant_expression)) == "expression(NO[2])") "NO2" else "O3"
  
  # --- Etiquetas con subíndices ---
  if (pollutant_str == "NO2") {
    legend_labels <- c(expression("Observed NO"[2]), expression("Predicted NO"[2]))
  } else {
    legend_labels <- c(expression("Observed O"[3]), expression("Predicted O"[3]))
  }
  
  # --- Gráfico ---
  ggplot() +
    # Observed
    geom_line(data = df_obs, aes(x = as.Date(ds), y = y, colour = "Observed"), linewidth = 0.5) +
    # Predicted
    geom_line(data = forecast_ext, aes(x = as.Date(ds), y = yhat, colour = "Predicted"), linewidth = 0.8) +
    # Prediction Interval (95%)
    geom_ribbon(
      data = forecast_ext,
      aes(x = as.Date(ds), ymin = yhat_lower, ymax = yhat_upper, fill = "Prediction Interval"),
      alpha = 0.25
    ) +
    # Colores y leyendas
    scale_colour_manual(
      name = NULL,
      values = c("Observed" = "#1F78B4", "Predicted" = "#E66101"),
      breaks = c("Observed", "Predicted"),
      labels = legend_labels
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("Prediction Interval" = "#E66101"),
      breaks = c("Prediction Interval"),
      labels = c("95% Prediction Interval")
    ) +
    guides(
      colour = guide_legend(order = 1, override.aes = list(fill = NA)),
      fill = guide_legend(order = 3)
    ) +
    # Ejes y formato temporal
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = as.Date(c("2020-01-01", end_pred)),
      expand = c(0, 0)
    ) +
    labs(
      x = "Year",
      y = expression("Concentration (" ~ µ * "g/m"^3 * ")"),
      caption = paste0("MAE = ", round(mae, 2), "   RMSE = ", round(rmse, 2))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.text = element_text(size = 14),
      axis.text = element_text(size = 12, colour = "black", face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      plot.caption = element_text(size = 12, face = "bold", colour = "#1F4E79", hjust = 1, margin = margin(t = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.3),
      plot.margin = margin(5, 5, 10, 5)
    )
}

# ===============================================================
# Crear figuras (NO₂ y O₃)
# ===============================================================

fig5a <- plot_prophet_forecast(
  df = fc_NO2$df, forecast = forecast_NO2,
  pollutant_expression = expression(NO[2]),
  mae = fc_NO2$MAE, rmse = fc_NO2$RMSE
)

fig5b <- plot_prophet_forecast(
  df = fc_O3$df, forecast = forecast_O3,
  pollutant_expression = expression(O[3]),
  mae = fc_O3$MAE, rmse = fc_O3$RMSE
)

# ===============================================================
# Guardar como TIFF (300 dpi, formato editorial)
# ===============================================================

#path_out <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/"

#ggsave(paste0(path_out, "Fig5a_Prophet_NO2_CameraReady.tiff"), fig5a,
#       width = 160, height = 110, units = "mm", dpi = 300, compression = "lzw")

#ggsave(paste0(path_out, "Fig5b_Prophet_O3_CameraReady.tiff"), fig5b,
#       width = 160, height = 110, units = "mm", dpi = 300, compression = "lzw")

# Visualizar en RStudio
print(fig5b)
print(fig5a)




# ────────────────────────────────────────────────
# Figure 8 — Boxplots of NO₂ and O₃ (2020–2024)
# Corrected for Windows / ggplot2 expression issue
# ────────────────────────────────────────────────

library(tidyverse)
library(arrow)
library(lubridate)

# ── Directorios ─────────────────────────────────
path_in  <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Parquet/"
path_out <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/"

# ── Cargar y combinar archivos anuales ──────────
files <- list.files(path_in, pattern = "^aire_validados_\\d{4}\\.parquet$", full.names = TRUE)

df <- files %>%
  map_dfr(~ {
    temp <- read_parquet(.x)
    temp <- temp %>%
      mutate(
        FECHA_HORA = as.POSIXct(FECHA_HORA, tz = "Europe/Madrid"),
        date = as.Date(FECHA_HORA),
        year = year(date)
      )
    temp$source_file <- basename(.x)
    temp
  })

# ── Crear variable de contaminante ──────────────
df <- df %>%
  mutate(
    pollutant = case_when(
      MAGNITUD == 8 ~ "NO₂",
      MAGNITUD == 14 ~ "O₃",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(pollutant))

# ── Agregar a nivel diario (por estación) ───────
df_daily <- df %>%
  group_by(date, year, pollutant) %>%
  summarise(value = mean(VALOR, na.rm = TRUE), .groups = "drop")

# ── Paleta MDPI ─────────────────────────────────
cols <- c("NO₂" = "#6BAED6", "O₃" = "#FD8D3C")

# ── Figura 8 ────────────────────────────────────
fig8 <- ggplot(df_daily, aes(x = factor(year), y = value, fill = pollutant)) +
  geom_boxplot(
    outlier.shape = 21, outlier.size = 0.8,
    alpha = 0.8, colour = "grey40", width = 0.6
  ) +
  scale_fill_manual(values = cols) +
  facet_wrap(~ pollutant, scales = "free_y") +
  labs(
    x = "Year",
    y = expression("Concentration (" * mu * "g·m"^-3 * ")"),
    title = ""
  ) +
  theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )

# ── Mostrar en visor RStudio ────────────────────
print(fig8)

# ── Exportar a TIFF (300 dpi, formato MDPI) ─────
tiff_filename <- paste0(path_out, "Figure8_Boxplots_NO2_O3_2020_2024.tiff")

ggsave(
  filename = tiff_filename,
  plot = fig8,
  device = "tiff",
  width = 16, height = 8, units = "cm",
  dpi = 300, compression = "lzw"
)

message("✅ Figure 8 exported successfully as TIFF (300 dpi): ", tiff_filename)




# ────────────────────────────────────────────────
# Figure 9 — Temporal and Spatial Variability of NO₂ and O₃
# Author: Jesús Cáceres Tello (2025)
# ────────────────────────────────────────────────

library(tidyverse)
library(arrow)
library(lubridate)

# ── Directorios ─────────────────────────────────
path_in  <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Calidad del Aire_Parquet/"
path_out <- "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/5_JCR_AppliedScience/images/"

# ── Cargar y combinar archivos anuales ──────────
files <- list.files(path_in, pattern = "^aire_validados_\\d{4}\\.parquet$", full.names = TRUE)

df <- files %>%
  map_dfr(~ read_parquet(.x) %>%
            mutate(
              FECHA_HORA = as.POSIXct(FECHA_HORA, tz = "Europe/Madrid"),
              date = as.Date(FECHA_HORA),
              year = year(date),
              month = month(date, label = TRUE, abbr = TRUE),
              pollutant = case_when(
                MAGNITUD == 8 ~ "NO₂",
                MAGNITUD == 14 ~ "O₃",
                TRUE ~ NA_character_
              )
            )) %>%
  filter(!is.na(pollutant)) %>%
  drop_na(VALOR)

# ── Etiquetas de estaciones (simplificadas) ─────
# Supón que tienes un diccionario de tipología por ESTACION
# Crea uno manual o con una tabla externa si es necesario:
station_type <- tibble(
  ESTACION = c(4, 8, 11, 16, 17, 18, 24, 27, 35, 36),
  type = c("Traffic", "Urban", "Urban", "Urban", "Suburban",
           "Suburban", "Urban", "Urban", "Suburban", "Suburban")
)

df <- df %>%
  left_join(station_type, by = "ESTACION")

# ── Datos temporales (mensuales) ────────────────
df_monthly <- df %>%
  group_by(year, month, pollutant) %>%
  summarise(value = mean(VALOR, na.rm = TRUE), .groups = "drop")

# Excluir estaciones no clasificadas (NA real o texto "NA")
df_plot <- df %>%
  filter(!is.na(type), type != "", type != "NA")

# ── Figura 9a: Temporal variability ─────────────
fig9a <- ggplot(df_monthly, aes(x = interaction(year, month, sep = "-"), y = value, colour = pollutant, group = pollutant)) +
  geom_line(linewidth = 0.6) +
  geom_smooth(se = FALSE, linewidth = 0.7, alpha = 0.6) +
  scale_colour_manual(values = c("NO₂" = "#6BAED6", "O₃" = "#FD8D3C")) +
  labs(x = "Month–Year", y = expression("Concentration (" * mu * "g·m"^-3 * ")"),
       title = "(a) Temporal variability of NO₂ and O₃ (2020–2024)") +
  theme_minimal(base_family = "Arial", base_size = 9) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none")

# ── Figura 9b: Spatial variability ──────────────
df_spatial <- df %>%
  group_by(type, pollutant) %>%
  summarise(value = mean(VALOR, na.rm = TRUE), .groups = "drop")

fig9b <- ggplot(df_plot, aes(x = type, y = VALOR, fill = pollutant)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 0.7, colour = "grey40") +
  scale_fill_manual(values = c("NO₂" = "#6BAED6", "O₃" = "#FD8D3C")) +
  labs(x = "Station type", y = expression("Concentration (" * mu * "g·m"^-3 * ")"),
       title = "(b) Spatial variability by station type") +
  theme_minimal(base_family = "Arial", base_size = 9) +
  theme(legend.position = "none")

fig9b

# ── Exportar figuras ────────────────────────────
tiff_filename <- paste0(path_out, "Figure9_Temporal_Spatial_Variability.tiff")

tiff(tiff_filename, width = 17, height = 10, units = "cm", res = 300, compression = "lzw")
gridExtra::grid.arrange(fig9a, fig9b, ncol = 1)
dev.off()

message("✅ Figure 9 exported successfully (TIFF, 300 dpi): ", tiff_filename)



# ===========================================================
# FIGURE 10 — NO₂ and O₃ vs Meteorological Variables (2020–2024)
# Only meteorological variable changes colour; NO₂ and O₃ fixed
# ===========================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)

# Paleta refinada
col_no2 <- "#0072B2"   # azul intenso
col_o3  <- "#009E73"   # verde turquesa
col_met <- c(
  temp = "#E69F00",    # naranja
  wind = "#56B4E9",    # azul claro
  hum  = "#D55E00",    # marrón rojizo
  rad  = "#F0E442",    # amarillo
  pres = "#CC79A7",    # magenta
  precip = "#999999"   # gris neutro
)

# --- Función para subgráficas
plot_compare <- function(df, met_var, met_label, met_color) {
  ggplot(df, aes(x = month)) +
    # NO₂ y O₃
    geom_line(aes(y = NO2, colour = "NO₂"), linewidth = 0.9, alpha = 0.8) +
    geom_line(aes(y = O3, colour = "O₃"), linewidth = 0.9, alpha = 0.8) +
    # Variable meteorológica (normalizada para visualización)
    geom_line(
      aes(y = (!!sym(met_var)) / max(!!sym(met_var), na.rm = TRUE) * 100),
      colour = met_color,
      linewidth = 1.6,    # más gruesa que las anteriores
      alpha = 0.9
    ) +
    # Colores de NO₂ y O₃ + subíndices
    scale_colour_manual(
      name = NULL,
      values = c("NO₂" = col_no2, "O₃" = col_o3),
      labels = c(expression(NO[2]), expression(O[3])),
      guide = guide_legend(
        override.aes = list(linewidth = 1.6),   # grosor en leyenda
        label.theme = element_text(size = 10)
      )
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(title = met_label, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text = element_text(colour = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# --- Crear subgráficas
p_temp   <- plot_compare(df_monthly, "temp", "Temperature (°C)", col_met["temp"])
p_wind   <- plot_compare(df_monthly, "wind", "Wind speed (m·s⁻¹)", col_met["wind"])
p_hum    <- plot_compare(df_monthly, "hum", "Relative humidity (%)", col_met["hum"])
p_rad    <- plot_compare(df_monthly, "rad_solar", "Solar radiation (W·m⁻²)", col_met["rad"])
p_pres   <- plot_compare(df_monthly, "presion", "Pressure (hPa)", col_met["pres"])
p_precip <- plot_compare(df_monthly, "precip", "Precipitation (mm)", col_met["precip"])

# --- Montar figura 3x2 sin título global
final_plot <- (p_temp + p_wind + p_hum) /
  (p_rad + p_pres + p_precip) &
  theme(plot.margin = margin(5, 10, 5, 10))

# --- Mostrar en visor o guardar
print(final_plot)
# ggsave("Fig10_NO2_O3_vs_Meteorology_Final.png", final_plot, width = 12, height = 7, dpi = 300)
