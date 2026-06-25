source("R/setup.R")

readxl::excel_sheets("data/comercio_camarones_langostinos.xlsx") 

importaciones =
readxl::read_excel("data/comercio_camarones_langostinos.xlsx",
sheet = "importaciones") %>% 
  janitor::clean_names()

revisiones =
readxl::read_excel("data/comercio_camarones_langostinos.xlsx",
sheet = "revisiones_aduanas") %>% 
  janitor::clean_names()

detalles_importaciones =
readxl::read_excel("data/comercio_camarones_langostinos.xlsx",
sheet = "detalles_importaciones") %>% 
  janitor::clean_names()

importaciones |> names()

revisiones |> names()

detalles_importaciones |> names()


grafica_barras <- function(
    df,
    variable_x = names(df)[1],
    tipo = c("auto", "simple", "stack", "dodge"),
    orientacion = c("vertical", "horizontal"),
    ordenar = c("ninguno", "asc", "desc"),
    ordenar_por = NULL,
    colores = c(
      "#136159",
      "#3B948B",
      "#611232",
      "#B08D57",
      "#BC955C"
    ),
    titulo = NULL,
    titulo_y = NULL,
    titulo_leyenda = NULL
){

  tipo <- match.arg(tipo)
  orientacion <- match.arg(orientacion)
  ordenar <- match.arg(ordenar)

  variables_y <- setdiff(names(df), variable_x)

  #----------------------------------
  # DETERMINAR TIPO AUTOMÁTICAMENTE
  #----------------------------------

  if(tipo == "auto"){
    tipo <- ifelse(
      length(variables_y) == 1,
      "simple",
      "stack"
    )
  }

  #----------------------------------
  # ORDENAMIENTO
  #----------------------------------

  if(ordenar != "ninguno"){

    if(length(variables_y) == 1){

      columna_orden <- variables_y

    } else {

      if(!is.null(ordenar_por)){

        columna_orden <- ordenar_por

      } else {

        df <- df %>%
          mutate(
            total_orden = rowSums(
              across(all_of(variables_y)),
              na.rm = TRUE
            )
          )

        columna_orden <- "total_orden"
      }
    }

    if(ordenar == "asc"){

      df <- df %>%
        arrange(.data[[columna_orden]])

    }

    if(ordenar == "desc"){

      df <- df %>%
        arrange(desc(.data[[columna_orden]]))

    }

    df[[variable_x]] <- factor(
      df[[variable_x]],
      levels = df[[variable_x]]
    )

    if("total_orden" %in% names(df)){
      df <- select(df, -total_orden)
    }
  }

  #----------------------------------
  # BARRA SIMPLE
  #----------------------------------

  if(tipo == "simple"){

    variable_y <- variables_y[1]

    g <- ggplot(
      df,
      aes(
        x = .data[[variable_x]],
        y = .data[[variable_y]]
      )
    ) +
      geom_col(
        fill = colores[1],
        width = .7
      )

  } else {

    #----------------------------------
    # STACK O DODGE
    #----------------------------------

    df_long <- df %>%
      pivot_longer(
        cols = all_of(variables_y),
        names_to = "categoria",
        values_to = "valor"
      )

    posicion <- if(tipo == "stack"){
      "stack"
    } else {
      position_dodge(width = .8)
    }

    g <- ggplot(
      df_long,
      aes(
        x = .data[[variable_x]],
        y = valor,
        fill = categoria
      )
    ) +
      geom_col(
        position = posicion,
        width = .7
      ) +
      scale_fill_manual(
        values = colores[
          seq_len(length(unique(df_long$categoria)))
        ]
      )
  }

  #----------------------------------
  # ORIENTACIÓN
  #----------------------------------

  if(orientacion == "horizontal"){
    g <- g + coord_flip()
  }

  #----------------------------------
  # TEMA
  #----------------------------------

  g +
    labs(
      title = titulo,
      x = NULL,
      y = titulo_y,
      fill = titulo_leyenda
    ) +
    theme_minimal() +
    theme(
      text = element_text(
        family = "Montserrat",
        size = 16
      ),
      plot.title = element_text(
        face = "bold",
        hjust = .5
      ),
      axis.text = element_text(
        face = "bold"
      ),
      axis.title.y = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(
        face = "bold"
      )
    )
}

grafica_barras(
  importaciones,
  tipo = "simple",
  orientacion = "horizontal",
  ordenar = "asc",
  titulo_y = "%"
)

grafica_barras(
  revisiones,
  tipo = "stack",
  orientacion = "vertical",
  ordenar = "desc",
  colores = c("#C62828", "#2E7D32")
)

grafica_barras(
  detalles_importaciones,
  tipo = "dodge",
  orientacion = "horizontal",
  ordenar = "asc",
  colores = c(
    "#136159",
    "#3B948B",
    "#611232"
  )
)

color_fondo_gg = "#EBF7FF"

color_linea = "#136159"

color_barras = "#3B948B"

color_leyenda_promedio = "#611232"

color_linea_promedio = "#611232"

variable_y <- "recaudacion_mdp"

frecuencia_breaks = 1000

# titulo_eje_y = "Pedimentos\n"
# 
# titulo_eje_y = "Valo de mercancía (MDP)\n"

titulo_eje_y = "Recaudación (MDP)\n"

formato_labels_y <- function(x) {
  paste0("$", scales::comma(x))
}

# formato_labels_y <- function(x) {
#   scales::comma(x)
# }

# -----------------------------
# PREPARACIÓN DE DATOS
# -----------------------------

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

# Promedio de pedimentos
promedio_pedimentos <- mean(df[[variable_y]], na.rm = TRUE)

label_promedio <- paste0(
  "Promedio: ",
  "$",
  scales::comma(round(promedio_pedimentos, 0))
)

# -----------------------------
# GRÁFICA
# -----------------------------

######### step


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
    linewidth = 2,
    alpha = .6
  ) +
  
  geom_point(
    color = color_linea,
    size = 5
  ) +
  
  geom_hline(
    yintercept = promedio_pedimentos,
    linetype = "dashed",
    color = color_linea_promedio,
    linewidth = 1.5
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
  
  # Negra
  geom_vline(
    xintercept = as.Date("2026-03-30"),
    color = "black",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # annotate(
  #   "label",
  #   x =  min(as.numeric(df$fecha)),
  #   y = promedio_pedimentos,
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
      size = 23
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
      linewidth = 0.7,
      linetype = "dashed"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold",
                                size = 24)
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
