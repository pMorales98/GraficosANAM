source("R/setup.R")



grafica


## GUARDAR ARCHIVO ---------------------
# Cierra el dispositivo de manera segura solo si está abierto
if (dev.cur() > 1) dev.off()

ggsave(
  "graficas/volumetria.png",
  grafica,
  width = 21.27,   # <-- 6850 px originales menos 10 píxeles
  height = 10.66,  # <-- 2598 px originales menos 10 píxeles
  units = "cm",   # Control absoluto de píxeles
  dpi = 600,
  scale = 1       # 1 asegura que los tamaños asignados (15pt, 18pt) no se escalen de más
)

# FUNCIÓN ---------------

# Promedio de pedimentos ----------
promedio <- mean(df[[variable_y]], na.rm = TRUE)

label_promedio <- paste0(
  "Promedio: ",
  "$",
  scales::comma(round(promedio, 0))
)

# GRÁFICA -----------------------------

## Construcción de bandas de fondo ------------

(grafica = 
   ggplot(df, aes(x = fecha, y = .data[[variable_y]])) +
   
   # Línea escalonada
   geom_step(
     aes(group = !!rlang::sym(grupo),
         color = !!rlang::sym(grupo)),
     direction = "mid",
     linewidth = ancho_linea,
     alpha = .6
   ) +
   
   # Puntos intermedios
   geom_point(
     aes(color = !!rlang::sym(grupo)),
     size = tamano_punto
   ) +
   
   # Línea de Promedio Horizontal
   # geom_hline(
   #   yintercept = promedio,
   #   linetype = "dashed",
   #   color = color_linea_promedio,
   #   linewidth = ancho_linea_horizontal
   # ) +
   
   # Línea Vertical de Control (Línea Negra)
   
   # geom_vline(
   #   xintercept = as.Date("2026-03-30"),
   #   color = "black",
   #   linetype = "dashed",
   #   linewidth = ancho_linea_vertical
   # ) +
   # 
   
   scale_x_continuous(
     breaks = as.numeric(df$fecha),
     labels = format(df$fecha, "%d de %b")
   ) +
   
   # Escalas de los Ejes
   scale_x_date(
     breaks = df$fecha,
     labels = format(df$fecha, "%d de %b")
   ) +
   
   scale_y_continuous(
     limits = limites_eje_y, 
     breaks = seq(
       0,
       max(df[[variable_y]], na.rm = TRUE) + 1000,
       by = frecuencia_breaks
     ),
     labels = formato_labels_y,
     expand = expansion(mult = c(0, 0))
   ) +
   
   labs(
     y = titulo_eje_y
   ) +
   
   geom_label(
     # data = ultimo_punto,
     aes(
       label = scales::percent(.data[[variable_y]]),
       fill = .data[[grupo]]
     ),
     color = "white",
     family = "Montserrat",
     fontface = "bold",
     size = 20,
     label.padding = unit(0.04, "lines"),
     label.r = unit(0.05, "lines"),
     label.size = 0,
     nudge_x = 0,
     nudge_y = +0.07 * max(df[[variable_y]], na.rm = TRUE)
   ) +
   scale_color_manual(
     values = colores_recinto,
     name = "Recinto",
     labels = c("wtc" = "GWTC",
                "fedex" = "FEDEX",
                "cla" = "CLA")
   ) +
   
   scale_fill_manual(
     values = colores_recinto,
     guide = "none"
   )+
   tema_anam(
     tamano_letra = 12,
     tamano_titulo = 16,
     angulo_texto_eje_x = 45,
     ancho_linea_grid = 0.5
   )
)


grafica 



