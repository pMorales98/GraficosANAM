 
source("R/setup.R")

grafica_barras <- function(
    df,
    variable_x = NULL,
    
    # Tipo de gráfica
    tipo = c("auto", "simple", "stack", "dodge"),
    
    # Orientación
    orientacion = c("vertical", "horizontal"),
    
    # Ordenamiento
    ordenar = c("ninguno", "asc", "desc"),
    ordenar_por = NULL,
    
    # Colores
    colores = c(
      "#136159",
      "#3B948B",
      "#611232",
      "#B08D57",
      "#BC955C"
    ),
    
    # Títulos
    titulo = NULL,
    titulo_y = NULL,
    titulo_leyenda = NULL,
    
    # Labels
    mostrar_labels = FALSE,
    posicion_label = 0.5,
    color_label = "white",
    size_label = 5,
    formato_label = scales::comma
) {
  
  #--------------------------------------------------
  # Validaciones iniciales
  #--------------------------------------------------
  
  if (is.null(variable_x)) {
    variable_x <- names(df)[1]
  }
  
  tipo <- match.arg(tipo)
  orientacion <- match.arg(orientacion)
  ordenar <- match.arg(ordenar)
  
  # Variables a graficar
  variables_y <- setdiff(names(df), variable_x)
  
  if (length(variables_y) == 0) {
    stop("No se encontraron variables numéricas para graficar.")
  }
  
  #--------------------------------------------------
  # DETECCIÓN AUTOMÁTICA DE TIPO
  #--------------------------------------------------
  
  if (tipo == "auto") {
    
    tipo <- ifelse(
      length(variables_y) == 1,
      "simple",
      "stack"
    )
    
  }
  
  #--------------------------------------------------
  # ORDENAMIENTO
  #--------------------------------------------------
  
  if (ordenar != "ninguno") {
    
    if (length(variables_y) == 1) {
      
      columna_orden <- variables_y
      
    } else {
      
      if (!is.null(ordenar_por)) {
        
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
    
    if (ordenar == "asc") {
      
      df <- df %>%
        arrange(.data[[columna_orden]])
      
    }
    
    if (ordenar == "desc") {
      
      df <- df %>%
        arrange(desc(.data[[columna_orden]]))
      
    }
    
    df[[variable_x]] <- factor(
      df[[variable_x]],
      levels = unique(df[[variable_x]])
    )
    
    if ("total_orden" %in% names(df)) {
      
      df <- df %>%
        select(-total_orden)
      
    }
    
  }
  
  #--------------------------------------------------
  # BARRA SIMPLE
  #--------------------------------------------------
  
  if (tipo == "simple") {
    
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
    
    if (mostrar_labels) {
      
      g <- g +
        geom_text(
          aes(
            y = .data[[variable_y]] * posicion_label,
            label = formato_label(.data[[variable_y]])
          ),
          color = color_label,
          family = "Montserrat",
          fontface = "bold",
          size = size_label
        )
      
    }
    
  } else {
    
    #--------------------------------------------------
    # STACK / DODGE
    #--------------------------------------------------
    
    df_long <- df %>%
      pivot_longer(
        cols = all_of(variables_y),
        names_to = "categoria",
        values_to = "valor"
      )
    
    #--------------------------------------------------
    # STACK
    #--------------------------------------------------
    
    if (tipo == "stack") {
      
      df_long <- df_long %>%
        group_by(.data[[variable_x]]) %>%
        mutate(
          acumulado = cumsum(valor),
          inicio = acumulado - valor,
          posicion_texto = inicio + valor * posicion_label
        ) %>%
        ungroup()
      
      g <- ggplot(
        df_long,
        aes(
          x = .data[[variable_x]],
          y = valor,
          fill = categoria
        )
      ) +
        geom_col(
          position = "stack",
          width = .7
        ) +
        scale_fill_manual(
          values = colores[
            seq_len(length(unique(df_long$categoria)))
          ]
        )
      
      if (mostrar_labels) {
        
        g <- g +
          geom_text(
            aes(
              y = posicion_texto,
              label = formato_label(valor)
            ),
            color = color_label,
            fontface = "bold",
            size = size_label
          )
        
      }
      
    }
    
    #--------------------------------------------------
    # DODGE
    #--------------------------------------------------
    
    if (tipo == "dodge") {
      
      g <- ggplot(
        df_long,
        aes(
          x = .data[[variable_x]],
          y = valor,
          fill = categoria
        )
      ) +
        geom_col(
          position = position_dodge(width = .8),
          width = .7
        ) +
        scale_fill_manual(
          values = colores[
            seq_len(length(unique(df_long$categoria)))
          ]
        )
      
      if (mostrar_labels) {
        
        g <- g +
          geom_text(
            aes(
              y = valor * posicion_label,
              label = formato_label(valor)
            ),
            position = position_dodge(width = .8),
            color = color_label,
            fontface = "bold",
            size = size_label
          )
        
      }
      
    }
    
  }
  
  #--------------------------------------------------
  # ORIENTACIÓN
  #--------------------------------------------------
  
  if (orientacion == "horizontal") {
    
    g <- g +
      coord_flip()
    
  }
  
  #--------------------------------------------------
  # TEMA
  #--------------------------------------------------
  
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


grafica_1 = grafica_barras(
  importaciones,
  tipo = "simple",
  mostrar_labels = TRUE,
  posicion_label = 0.1,
  colores = c("#943a5e"),
  orientacion = "horizontal",
  ordenar = "asc"
) 
  ggsave(
  "grafica.png",
  grafica_1,
  width = 27,
  height = 11,
  units = "cm",
  dpi = 300
)


grafica_2 = grafica_barras(
  revisiones,
  tipo = "stack",
  orientacion = "vertical",
  ordenar = "desc",
  colores = c("#9b2247", "#136159"),
   mostrar_labels = TRUE,
  posicion_label = 0.1
)

  ggsave(
  "grafica.png",
  grafica_2,
  width = 27,
  height = 11,
  units = "cm",
  dpi = 300
)
