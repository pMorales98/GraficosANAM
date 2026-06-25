
# SETUP -------------

# setwd("../Git/anam/")

source("R/setup.R")

# setwd("solicitudes/NICOs/")


# LEER DATOS ------

df_uso_ventanilla <-
  readxl::read_excel("solicitudes/data/chatbot/chatbot_data.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    mes = factor(
      mes,
      levels = c(
        "Enero",
        "Febrero",
        "Marzo",
        "Abril",
        "Mayo",
        "Junio",
        "Julio",
        "Agosto",
        "Septiembre",
        "Octubre",
        "Noviembre",
        "Diciembre"
      ),
      ordered = TRUE
    )
  )
  



library(ggplot2)

# Factor de escala
escala <- max(df_uso_ventanilla$cantidad_de_usuarios) / max(df_uso_ventanilla$cantidad_de_mensajes)
options(scipen = 999)

tamano_letra_grafica <- 5

grafica <- ggplot(df_uso_ventanilla, aes(x = mes)) +
  
  # Barras de usuarios
  geom_col(
    aes(
      y = cantidad_de_usuarios,
      fill = "Cantidad de usuarios"
    )
  ) +
  
  # Etiquetas dentro de las barras
  geom_text(
    aes(
      y = cantidad_de_usuarios,
      label = scales::comma(cantidad_de_usuarios)
    ),
    vjust = 1.2,
    color = "white",
    size = tamano_letra_grafica,
    fontface = "bold",
    family = "Poppins"
  ) +
  
  # Línea de mensajes
  geom_line(
    aes(
      y = cantidad_de_mensajes,
      color = "Cantidad de mensajes",
      group = 1
    ),
    linewidth = 1.2
  ) +
  
  # Puntos de mensajes
  geom_point(
    aes(
      y = cantidad_de_mensajes,
      color = "Cantidad de mensajes"
    ),
    size = 3
  ) +
  
  # Etiquetas de mensajes
  
  geom_label(
    aes(
      y = cantidad_de_mensajes,
      label = scales::comma(cantidad_de_mensajes),
      color = "Cantidad de mensajes"
    ),
    family = "Poppins",
    fontface = "bold",
    size = tamano_letra_grafica,
    fill = "white",
    label.size = 0,        # quita borde
    label.padding = unit(0.05, "lines"),  # casi pegado
    vjust = -0.2,
    show.legend = FALSE
  ) +
  # geom_text(
  #   aes(
  #     y = cantidad_de_mensajes,
  #     label = scales::comma(cantidad_de_mensajes),
  #     color = "Cantidad de mensajes"
  #   ),
  #   vjust = -0.8,
  #   size = tamano_letra_grafica,
  #   fontface = "bold",
  #   show.legend = FALSE,
  #   family = "Poppins"
  # ) +
  
  # Colores
  scale_fill_manual(
    values = c(
      "Cantidad de usuarios" = "#0f5146"
    ),
    breaks = c("Cantidad de usuarios"),
    name = NULL
  ) +
  
  scale_color_manual(
    values = c(
      "Cantidad de mensajes" = "#9DB337"
    ),
    breaks = c("Cantidad de mensajes"),
    name = NULL
  ) +
  
  # Eje Y con separadores de miles
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, max(df_uso_ventanilla$cantidad_de_mensajes) * 1.15)
  ) +
  
  # Leyenda primero usuarios y luego mensajes
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  
  tema_anam(factor_escala = 1,
            fuente = "Poppins",
            mostrar_ejes = F) +
  
  theme(
    legend.position = "top",
    axis.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(hjust = 0.5)
)
grafica


# GUARDAR ARCHIVO ---------------------

# Cierra el dispositivo de manera segura solo si está abierto
if (dev.cur() > 1) dev.off()

ggsave(
  "graficas/usuarios_mensajes.png",
  grafica,
  width = 17.17,   # <-- 6850 px originales menos 10 píxeles
  height = 11.57,  # <-- 2598 px originales menos 10 píxeles
  units = "cm",   # Control absoluto de píxeles
  dpi = 600,
  scale = 1       # 1 asegura que los tamaños asignados (15pt, 18pt) no se escalen de más
)

