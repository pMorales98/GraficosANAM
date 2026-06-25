##Grafica4
# Datos aproximados de la gráfica
df <- data.frame(
  seccion = c(5, 12, 11, 8, 19, 20, 9, 13, 7, 4, 6, 15, 10, 17, 18, 22, 14, 3, 16, 21, 2, 1),
  porcentaje = c(
    11.1, 8.4, 6.1, 4.9, 4.6, 4.0, 3.7, 3.7,
    3.3, 3.2, 3.2, 3.0, 2.8, 2.7, 1.9, 1.8,
    1.5, 1.2, 0.9, 0.8, 0.3, 0.1
  )
)

# Mantener el orden mostrado
df$seccion <- factor(df$seccion, levels = df$seccion)


factor_letra = 5
tamano_letra_2_grafica = 20

# Gráfica
p <- ggplot(df, aes(x = seccion, y = porcentaje)) +
  geom_col(
    fill = "#335955",
    width = 0.62
  ) +
  scale_y_continuous(
    limits = c(0, 12.5),
    breaks = c(0, 2.5, 5.0, 7.5, 10.0, 12.5),
    labels = function(x) paste0(format(x, nsmall = 1), "%"),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    # title = "Porcentaje de recaudación respecto al valor en aduana por Sección de Fracción Arancelaria",
    title = "",
    x = "Sección",
    y = "Porcentaje"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    # Título
    plot.title = element_text(
      size = 13*factor_letra,
      face = "bold",
      color = "#666666",
      hjust = 0
    ),
    
    # Títulos de ejes
    axis.title.x = element_text(
      size = 14*factor_letra,
      face = "bold",
      color = "#333333",
      margin = margin(t = 15)
    ),
    
    axis.title.y = element_text(
      size = 14*factor_letra,
      face = "bold",
      color = "#333333",
      margin = margin(r = 15)
    ),
    
    # Etiquetas de ejes
    axis.text.x = element_text(
      size = 12*factor_letra,
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 12*factor_letra,
      color = "#333333"
    ),
    
    # Gridlines horizontales
    panel.grid.major.y = element_line(
      color = "#D9D9D9",
      linewidth = 0.6
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    
    # Ejes
    axis.line.x = element_line(
      color = "#BDBDBD",
      linewidth = 0.5
    ),
    
    axis.line.y = element_blank(),
    
    axis.ticks = element_blank(),
    
    # Fondos
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Márgenes
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)



ggsave(
  "graficas/fracciones_por_seccion.png",
  p,
  width = 16.36,  
  height = 8.08,
  units = "cm",  
  dpi = 600,
  scale = 1      
)

