
datos_diesel = 
  datos_diesel %>% 
  mutate(etiqueta_leyenda = case_when(str_detect(etiqueta_leyenda, "(?i)\\(3\\)") ~ "Diésel < 15 ppm",
                                      str_detect(etiqueta_leyenda, "(?i)\\(4\\)") ~ "Diésel > 15 ppm",
                                      T ~ str_to_sentence(etiqueta_leyenda)))

# Escalar datos -------


factor_escala <- max(datos_diesel$valor_corregido, na.rm = TRUE) / 
  max(datos_ieps_gg$promedio_estimulo_diesel, na.rm = TRUE)


datos_ieps_gg <- datos_ieps_2 %>% 
  estandarizar_fecha(fecha_mes, 
                     nuevo_nombre_fecha = fecha,
                     crear_mes = T) 

datos_diesel_gg =
  datos_diesel %>%
  estandarizar_fecha(fecha_mes,
                     nuevo_nombre_fecha = fecha,
                     crear_mes = T)

# Función para labels ------

breaks <- seq(
  min(datos_ieps_gg$mes),
  max(datos_ieps_gg$mes),
  by = "2 months"
)

labels <- stringr::str_to_title(format(breaks, "%b %y"))

# BANDAS -

anios <- seq(
  as.Date(paste0(format(min(datos_ieps_gg$mes), "%Y"), "-01-01")),
  as.Date(paste0(format(max(datos_ieps_gg$mes), "%Y"), "-12-31")),
  by = "year"
)

bandas <- data.frame(
  xmin = anios,
  xmax = c(anios[-1], max(datos_ieps_gg$mes) + 1),
  fill = rep(c(TRUE, FALSE), length.out = length(anios))
)

# ESTIMULO Diésel ---------
(
  grafica =
    ggplot() +
    # geom_rect(
    #   data = bandas,
    #   aes(
    #     xmin = xmin,
    #     xmax = xmax,
    #     ymin = -Inf,
    #     ymax = Inf,
    #     fill = fill
    #   ),
    #   alpha = 0.10,
    #   inherit.aes = FALSE
    # ) +
    # scale_fill_manual(
    #   values = c(
    #     "TRUE" = "skyblue",
    #     "FALSE" = "white"
    #   ),
    #   guide = "none"
    #   
    # )+
    geom_ribbon(
      data = datos_ieps_gg,
      aes(
        x = mes,
        ymin = 0,
        ymax = promedio_estimulo_diesel * factor_escala,
        fill = "Estímulo IEPS"
      ),
      alpha = 0.3
    ) +
    
    geom_line(
      data = datos_diesel_gg,
      aes(
        x = mes,
        y = valor_corregido,
        color = etiqueta_leyenda,
        group = etiqueta_leyenda
      ),
      linewidth = .8,
      alpha = 0.3
    ) +
    
    geom_point(
      data = datos_diesel_gg,
      aes(
        x = mes,
        y = valor_corregido,
        color = etiqueta_leyenda
      ),
      size = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2021-01-01"),
      linetype = "dashed",
      color = "#002F2A",
      linewidth = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2023-11-01"),
      linetype = "dashed",
      color = "#611232",
      linewidth = .5
    ) +
    
    scale_color_manual(values = c(
      "Diésel < 15 ppm" = "#332F27",
      "Diésel > 15 ppm" = "#374F64",
      "Similar a diésel" = "#A57F2C"
    )) +
    
    scale_x_date(
      breaks = breaks,
      labels = labels
    ) +
    
    scale_y_continuous(
      name = "Valor aduana",
      labels = scales::label_number(
        scale = 1e-6,
        suffix = "M",
        big.mark = ","
      )
    ) +
    scale_fill_manual(
      values = c(
        "Estímulo IEPS" = "grey20"
      )
    ) +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )+
    tema_anam(
      angulo_texto_eje_x = 45,
      tamano_letra = 8,
      tamano_titulo = 9,
      margen_titulo_y = 3,
      margen_plot = c(.5, 2, .5, 2)
    ) +
    
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center",
      legend.box = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      axis.text.x = element_text(margin = margin(b = -5, 
                                                 t = 1))
      
    ) +
    theme(
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.8, "cm")
    ) )


# GUARDAR ARCHIVO ---------------------

# Cierra el dispositivo de manera segura solo si está abierto
if (dev.cur() > 1) dev.off()

ggsave(
  "../../graficas/diesel.png",
  grafica,
  width = 15.76,   # <-- 6850 px originales menos 10 píxeles
  height = 8.68,  # <-- 2598 px originales menos 10 píxeles
  units = "cm",   # Control absoluto de píxeles
  dpi = 600,
  scale = 1       # 1 asegura que los tamaños asignados (15pt, 18pt) no se escalen de más
)

