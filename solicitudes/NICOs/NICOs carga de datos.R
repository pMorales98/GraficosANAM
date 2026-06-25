# SETUP -------------

setwd("../")

source("R/setup.R")

setwd("solicitudes/NICOs/")

# CARGAR DATOS -------------
# Carpeta
ruta <- "data/"

# Listar CSVs
archivos <- list.files(
  path = ruta,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Nombres de objetos (sin extensión)
nombres_df <- tools::file_path_sans_ext(basename(archivos))

# Leer archivos y limpiar nombres de columnas
dfs <- purrr::map(
  archivos,
  ~ readr::read_csv(.x, show_col_types = FALSE) %>%
    janitor::clean_names()
)

# Asignar nombres a la lista
names(dfs) <- nombres_df

# (Opcional) Crear objetos individuales en el entorno
list2env(dfs, envir = .GlobalEnv)

# Data frame resumen
resumen_archivos <- tibble(
  archivo = basename(archivos),
  nombre_df = nombres_df
)

resumen_archivos
