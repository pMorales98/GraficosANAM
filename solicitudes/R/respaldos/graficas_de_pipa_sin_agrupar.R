source("R/setup.R")

tendencias_rec =
readxl::read_excel("../data/tendencias_recaudacion.xlsx") %>% 
  janitor::clean_names()




color_fondo_gg = "#EBF7FF"

color_linea = "#136159"

color_barras = "#3B948B"

color_leyenda_promedio = "#611232"

color_linea_promedio = "#611232"

tendencias_rec %>% 
  names()

# Recaudación ----------------
# 
# variable_y <- "recaudacion_mdp"
# 
# frecuencia_breaks = 1000
# 
# titulo_eje_y = "Recaudación (MDP)\n"
# 
# formato_labels_y <- function(x) {
#   paste0("$", scales::comma(x))
# }

# Pedimentos ---------

# variable_y <- "pedimentos_importacion"
# 
# frecuencia_breaks = 5000
# 
# titulo_eje_y = "Pedimentos\n"
# 
# formato_labels_y <- function(x) {
#   scales::comma(x)
# }

# Mercancía ---------

tendencias_rec %>% 
  names()

variable_y <- "valor_mercancia_entre_un_millon_mdp"

titulo_eje_y = "Valo de mercancía (MDP)\n"

frecuencia_breaks = 20000

titulo_eje_y = "Recaudación (MDP)\n"

formato_labels_y <- function(x) {
  paste0("$", scales::comma(x))
}

# FUNCIÓN ---------------

## PREPARACIÓN DE DATOS ---------

df <- tendencias_rec %>%
  mutate(
    fecha = as.Date(
      parse_date_time(
        etiquetas_de_fila,
        orders = "A, d B Y",
        locale = "es_ES.UTF-8"
      )
    ),
    mes = floor_date(fecha, "month")
  ) %>%
  arrange(fecha)

ultimo_punto <- df %>%
  slice_tail(n = 1)


# Promedio de pedimentos ----------
promedio <- mean(df[[variable_y]], na.rm = TRUE)

label_promedio <- paste0(
  "Promedio: ",
  "$",
  scales::comma(round(promedio, 0))
)

## GRÁFICA -----------------------------

bandas <- df %>%
  mutate(
    mes = floor_date(fecha, unit = "month")
  ) %>%
  distinct(mes) %>%
  arrange(mes) %>%
  mutate(
    xmin = mes,
    xmax = pmin(
      ceiling_date(mes, "month") - 1,
      max(df$fecha)
    ),
    
    xmin = ifelse(
      row_number() == 1,
      min(df$fecha),
      xmin
    ),
    
    fill = ifelse(
      row_number() %% 2 == 0,
      "white",
      "#EBF7FF"   # azul claro
    )
  )

grafica = ggplot(df,
       aes(x = as.numeric(fecha),
           y = .data[[variable_y]])) +
  
  geom_rect(
    data = bandas,
    aes(
      xmin = xmin - 0.5,
      xmax = xmax + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = bandas$fill,
    alpha = 0.55
  ) +
  
  geom_step(
    direction = "mid",   # <- clave
    color = color_linea,
    linewidth = 2.7,
    alpha = .6
  ) +
  
  geom_point(
    color = color_linea,
    size = 5.7
  ) +
  
  geom_hline(
    yintercept = promedio,
    linetype = "dashed",
    color = color_linea_promedio,
    linewidth = 1.8
  ) +
  
  # Negra
  geom_vline(
    xintercept = as.Date("2026-03-30"),
    color = "black",
    linetype = "dashed",
    linewidth = 1.4
  ) +
  
  scale_x_continuous(
    breaks = as.numeric(df$fecha),
    labels = format(df$fecha, "%d de %b")
  ) +
  # scale_y_continuous(
  #   breaks = seq(0, max(df[[variable_y]], na.rm = TRUE), by = frecuencia_breaks),
  #   labels = scales::comma
  # ) +
  
  scale_y_continuous(
    breaks = seq(
      0,
      max(df[[variable_y]], na.rm = TRUE)+1000,
      by = frecuencia_breaks
    ),
    labels = formato_labels_y
  ) +
  
  labs(
    y = titulo_eje_y
  ) +
  
  # geom_label(
  #   data = ultimo_punto,
  #   aes(
  #     label = paste0("$", scales::comma(round(.data[[variable_y]], 0)))
  #   ),
  #   fill = "#136159",
  #   color = "white",
  #   family = "Montserrat",
  #   fontface = "bold",
  #   size = 14,
  #   label.padding = unit(0.20, "lines"),
  #   label.r = unit(0.15, "lines"),
  #   nudge_x = -10,
  #   nudge_y = -0.05 * max(df[[variable_y]], na.rm = TRUE)
  # ) +
  # geom_vline(
  #   xintercept = as.Date(c("2026-02-02", "2026-03-16")),
  #   color = "#B08D57",
  #   linetype = "dashed",
  #   linewidth = 1
  # ) +

  
  # annotate(
  #   "label",
  #   x =  min(as.numeric(df$fecha)),
  #   y = promedio,
  #   label = label_promedio,
  #   fill = "#611232",
  #   color = "white",
  #   label.size = 0,
  #   alpha = 1,
  #   hjust = 0,
  #   vjust = -0.5,
  #   size = 14,
  #   fontface = "bold"
  # ) +
  
  theme_minimal() +
  
  theme(
    text = element_text(
      family = "Montserrat",
      size = 18
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
    panel.grid.major.y = element_line(
      color = "gray",
      linewidth = 0.8,
      linetype = "dashed"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold",
                                size = 20)
  ) 

grafica 

# guardar
ggsave(
  "../Desktop/graficas/grafica_pedimentos.png",
  plot = grafica,
  width = 25,
  height = 10,
  units = "cm",
  dpi = 300
)
