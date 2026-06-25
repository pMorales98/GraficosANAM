
datos_brasil = 
  datos_brasil %>% 
  mutate(etiqueta_leyenda = case_when(str_detect(etiqueta_leyenda, "(?i)Aceites distintos a petroq") ~ "Aceites distintos a petroquímicos",
                                      str_detect(etiqueta_leyenda, "(?i)Gasolinas \\(Otros medios\\)") ~ "Gasolina por otros medios",
                                      str_detect(etiqueta_leyenda, "(?i)queroseno.+?avi") ~ "Turbosina",
                                      str_detect(etiqueta_leyenda, "(?i)aceite.+?el.ctric") ~ "Aceites dieléctricos",
                                      str_detect(etiqueta_leyenda, "(?i)otros$") ~ "Otros aceites",
                                      T ~ str_to_sentence(etiqueta_leyenda)))

datos_brasil %>% 
  count(etiqueta_leyenda)

# Escalar datos -------

# 
# factor_escala <- max(datos_brasil$total_mensual, na.rm = TRUE) / 
#   max(datos_ieps_gg$total_mensual, na.rm = TRUE)
# 

# datos_ieps_gg <- datos_ieps_2 %>% 
#   estandarizar_fecha(fecha_mes, 
#                      nuevo_nombre_fecha = fecha,
#                      crear_mes = T) 

datos_brasil_gg =
  datos_brasil %>%
  estandarizar_fecha(fecha,
                     nuevo_nombre_fecha = fecha,
                     crear_mes = T)

# Función para labels ------

breaks <- seq(
  min(datos_brasil_gg$mes),
  max(datos_brasil_gg$mes),
  by = "2 months"
)

labels <- stringr::str_to_title(format(breaks, "%b %y"))


# BANDAS -

anios <- seq(
  as.Date(paste0(format(min(datos_brasil_gg$mes), "%Y"), "-01-01")),
  as.Date(paste0(format(max(datos_brasil_gg$mes), "%Y"), "-12-31")),
  by = "year"
)

bandas <- data.frame(
  xmin = anios,
  xmax = c(anios[-1], max(datos_brasil_gg$mes) + 1),
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
    #   alpha = 0.15,
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
    # geom_ribbon(
    #   data = datos_ieps_gg,
    #   aes(
    #     x = mes,
    #     ymin = 0,
    #     ymax = total_mensual * factor_escala
    #   ),
    #   fill = "#155B4E",
    #   alpha = 0.3
    # ) +
    
    geom_line(
      data = datos_brasil_gg,
      aes(
        x = mes,
        y = total_mensual,
        color = etiqueta_leyenda,
        group = etiqueta_leyenda
      ),
      linewidth = .8,
      alpha = 0.5
    ) +
    
    geom_point(
      data = datos_brasil_gg,
      aes(
        x = mes,
        y = total_mensual,
        color = etiqueta_leyenda
      ),
      size = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2022-03-01"),
      linetype = "dashed",
      color = "#002F2A",
      linewidth = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2022-07-01"),
      linetype = "dashed",
      color = "#002F2A",
      linewidth = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2023-04-01"),
      linetype = "dashed",
      color = "#002F2A",
      linewidth = .5
    ) +
    
    geom_vline(
      xintercept = as.Date("2023-06-01"),
      linetype = "dashed",
      color = "#002F2A",
      linewidth = .5
    ) +
    
    scale_color_manual(values = c(
        "Aceites distintos a petroquímicos" = "#A33657",
        "Gasolina por otros medios"         = "#346B60",
        "Turbosina"                         = "#A57F2C",
        "Aceites dieléctricos"             = "#E6D194",
        "Otros aceites"                    = "#A0A0A0"
    )) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),  label.theme = element_text(
      hjust = 0.5
    )) +
    
    scale_x_date(
      breaks = breaks,
      labels = labels
    ) +
    
    scale_y_continuous(
      name = "Volúmen (litros)",
      labels = scales::label_number(
        scale = 1e-6,
        suffix = "M",
        big.mark = ","
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "Estímulo IEPS" = "#155B4E"
      )
    ) +
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
  "../../graficas/brasil.png",
  grafica,
  width = 15.76,   # <-- 6850 px originales menos 10 píxeles
  height = 8.4,  # <-- 2598 px originales menos 10 píxeles
  units = "cm",   # Control absoluto de píxeles
  dpi = 600,
  scale = 1       # 1 asegura que los tamaños asignados (15pt, 18pt) no se escalen de más
)

