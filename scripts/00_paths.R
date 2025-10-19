# 00_paths.R — centraliza todas las rutas del proyecto
root_dir  <- here::here()
data_dir  <- file.path(root_dir, "data")
scripts_dir <- file.path(root_dir, "scripts")
figs_dir  <- file.path(root_dir, "figures")
docs_dir  <- file.path(root_dir, "docs")

# Ejemplo de uso
list.files(file.path(data_dir, "Calidad_del_Aire_Parquet"))
