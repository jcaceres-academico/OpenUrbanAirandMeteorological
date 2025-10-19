# ============================================================
# ============================================================
# Figure 2. Mapa de estaciones
# ============================================================
# ============================================================
# ==========================================================================================
# FIGURA 2a — Mapa con fondo real (OpenStreetMap) de estaciones de calidad del aire
# ==========================================================================================

library(tidyverse)
library(ggmap)
library(ggrepel)
library(ggtext)

# -----------------------------------------------------------
# 1️⃣ Cargar y preparar datos
# -----------------------------------------------------------

estaciones <- read_delim(
  "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Estaciones_Meteorologicas/estaciones.csv",
  delim = ";",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

names(estaciones) <- toupper(names(estaciones))

contaminantes <- c("NO2","SO2","CO","PM10","PM2_5","O3","BTX")

estaciones <- estaciones %>%
  mutate(
    LONGITUD = as.numeric(LONGITUD),
    LATITUD  = as.numeric(LATITUD),
    N_CONTAMINANTES = rowSums(select(., all_of(contaminantes)) == "X", na.rm = TRUE)
  ) %>%
  filter(!is.na(LONGITUD) & !is.na(LATITUD))

# ============================================================
# 2️⃣ Descargar mapa base (Stadia Maps — reemplazo de Stamen)
# ============================================================

# Definir área de Madrid
bbox_madrid <- make_bbox(lon = estaciones$LONGITUD, lat = estaciones$LATITUD, f = 0.05)

# Descargar mapa base de Stadia (requiere ggmap >= 3.0)
# Puedes elegir entre: "stamen_toner_lite", "stamen_terrain", "stamen_watercolor"
mapa_base <- get_stadiamap(
  bbox = bbox_madrid,
  zoom = 12,
  maptype = "stamen_terrain",
  crop = TRUE
)


# -----------------------------------------------------------
# 3️⃣ Crear mapa con estaciones sobre el fondo OSM
# -----------------------------------------------------------

mapa_estaciones <- ggmap(mapa_base) +
  geom_point(data = estaciones,
             aes(x = LONGITUD, y = LATITUD,
                 colour = NOM_TIPO,
                 size = N_CONTAMINANTES),
             alpha = 0.9) +
  geom_text_repel(data = estaciones,
                  aes(x = LONGITUD, y = LATITUD, label = ESTACION),
                  size = 2.8,
                  colour = "black",
                  max.overlaps = 10) +
  scale_colour_brewer(palette = "Dark2", name = "Station type") +
  scale_size_continuous(name = "No. of pollutants", range = c(2,6)) +
  labs(
    title = "Fig. 2a — Spatial distribution and measurement scope of Madrid’s air-quality stations (2020–2024)",
    subtitle = "Colour = station type · Size = no. of pollutants · Labels = station names",
    x = "Longitude (°)", y = "Latitude (°)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_markdown(size = 9),
    legend.position = "right",
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 9)
  )

# Mostrar en visor
mapa_estaciones

# -----------------------------------------------------------
# 4️⃣ Guardar como TIFF (300 dpi, 180×120 mm)
# -----------------------------------------------------------

tiff_path <- "C:/Users/cacerestj/Desktop/Fig2a_Mapa_Madrid_OSM.tiff"

ggsave(
  filename = tiff_path,
  plot = mapa_estaciones,
  width = 180, height = 120, units = "mm",
  dpi = 300, compression = "lzw"
)

# Abrir para revisión
browseURL(tiff_path)








library(tidyverse)
library(ggrepel)
library(ggtext)

# ============================================================
# 1. Cargar y preparar datos
# ============================================================

estaciones <- read_delim(
  "C:/Users/cacerestj/Desktop/00_FORMACION/00 UNIVERSIDAD COMPLUTENSE/05 DOCTORADO/02 ARTICULOS/2025_26/4_R5EDI 2025/datos/Estaciones_Meteorologicas/estaciones.csv",
  delim = ";",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

names(estaciones) <- toupper(names(estaciones))

# --- Calcular número de contaminantes medidos ---
contaminantes <- c("NO2","SO2","CO","PM10","PM2_5","O3","BTX")

estaciones <- estaciones %>%
  mutate(
    LONGITUD = as.numeric(LONGITUD),
    LATITUD  = as.numeric(LATITUD),
    N_CONTAMINANTES = rowSums(select(., all_of(contaminantes)) == "X", na.rm = TRUE),
    COMPUESTOS = apply(select(., all_of(contaminantes)), 1, function(x) {
      paste(contaminantes[which(x == "X")], collapse = ", ")
    })
  ) %>%
  filter(!is.na(LONGITUD) & !is.na(LATITUD))

# ============================================================
# 2. Crear mapa
# ============================================================

mapa_estaciones <- ggplot(estaciones, aes(x = LONGITUD, y = LATITUD)) +
  geom_point(aes(
    colour = NOM_TIPO,
    size = N_CONTAMINANTES
  ), alpha = 0.9) +
  geom_text_repel(
    aes(label = ESTACION),
    size = 2.5,
    max.overlaps = 10,
    colour = "grey25"
  ) +
  scale_colour_brewer(palette = "Dark2", name = "Station type") +
  scale_size_continuous(name = "No. of pollutants") +
  labs(
    title = "Fig. 2 — Spatial distribution and measurement scope of Madrid’s air-quality stations (2020–2024)",
    subtitle = "Colour = station type · Size = no. of pollutants · Labels = station names",
    x = "Longitude (°)", y = "Latitude (°)"
  ) +
  coord_equal() +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_markdown(size = 9),
    axis.text = element_text(size = 8, colour = "black"),
    axis.title = element_text(size = 9)
  )

# Mostrar el mapa en RStudio
mapa_estaciones

# ============================================================
# 3. Exportar como TIFF (300 dpi)
# ============================================================

tiff_path <- "C:/Users/cacerestj/Desktop/Fig2_Estaciones_Madrid_Final.tiff"
ggsave(
  filename = tiff_path,
  plot = mapa_estaciones,
  width = 180, height = 120, units = "mm",
  dpi = 300, compression = "lzw"
)

# Abrir el TIFF para revisar
browseURL(tiff_path)

