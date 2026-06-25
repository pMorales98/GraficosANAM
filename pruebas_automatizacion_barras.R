
importaciones =
  readxl::read_excel("data/comercio_camarones_langostinos.xlsx",
                     sheet = "importaciones") %>% 
  janitor::clean_names()

gg_histogram <- function(
    df,
    variable_nominal,
    variable_numerica,
    orientacion = c("vertical", "horizontal")) {
  
  orientacion <- match.arg(orientacion)
  
  variable_nominal <- rlang::ensym(variable_nominal)
  
  variable_numerica = rlang::ensym(variable_numerica)
  
  ggplot(df) +
    geom_col(aes(x = !!variable_nominal,
                 y = !!variable_numerica))
  
}

gg_histogram(df = importaciones,
             variable_nominal = aduana,
             variable_numerica = porcentaje_importaciones)
