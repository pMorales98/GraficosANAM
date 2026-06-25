
##Grafica 2
# Datos (sustituye por los reales si los tienes)
df <- data.frame(
  seccion = c(5, 16, 17, 15, 6, 7, 11, 22, 20, 18, 4, 10, 12, 13, 8, 9, 14, 2, 3, 1, 19, 21),
  recaudacion = c(
    140000, 107000, 51000, 45000, 40000, 35000, 25000, 16000,
    12000, 9000, 8500, 7500, 6000, 4000, 2500, 1800,
    1000, 500, 300, 200, 100, 50
  )
)

factor_letra = 3.5

# Mantener el orden mostrado
df$seccion <- factor(df$seccion, levels = df$seccion)

# Gráfica
p <- ggplot(df, aes(x = seccion, y = recaudacion)) +
  geom_col(
    fill = "#335955",
    width = 0.62
  ) +
  scale_y_continuous(
    limits = c(0, 150000),
    breaks = c(0, 50000, 100000, 150000),
    labels = scales::label_dollar(
      prefix = "$",
      suffix = "",
      big.mark = ",",
      accuracy = 1
    ),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "",
    # title = "Recaudación por Sección de Fracción Arancelaria",
    x = "Sección",
    y = "Millones de pesos"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    # Título
    plot.title = element_text(
      size = 18*factor_letra,
      face = "bold",
      color = "#666666",
      hjust = 0
    ),
    
    # Títulos de ejes
    axis.title.x = element_text(
      size = 15*factor_letra,
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
      size = 15*factor_letra,
      face = "plain",
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 12*factor_letra,
      face = "plain",
      color = "#333333"
    ),
    
    # Gridlines horizontales
    panel.grid.major.y = element_line(
      color = "#D9D9D9",
      linewidth = 0.6
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Ejes
    axis.line.x = element_line(
      color = "#BDBDBD",
      linewidth = 0.5
    ),
    
    axis.line.y = element_blank(),
    
    axis.ticks = element_blank(),
    
    # Fondos
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    # Márgenes
    plot.margin = margin(5, 5, 5, 5)
  )

print(p)

# Exportar exactamente a 1025 x 520 px

if (dev.cur() > 1) dev.off()

ggsave(
  "graficas/fracciones_por_seccion.png",
  p,
  width = 12.5,  
  height = 5.45,
  units = "cm",  
  dpi = 600,
  scale = 1      
)


# # Exportar igual que la anterior
# ggsave(
#   "recaudacion_por_seccion.png",
#   p,
#   width = 1025,
#   height = 520,
#   units = "px",
#   dpi = 100,
#   bg = "white"
# )

