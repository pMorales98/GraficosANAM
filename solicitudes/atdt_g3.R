
##Grafica3
# Datos aproximados de la gráfica
df <- data.frame(
  seccion = c(16, 17, 15, 6, 5, 7, 22, 18, 11, 20, 4, 10, 2, 1, 13, 14, 12, 9, 8, 3, 19, 21),
  valor = c(
    120000,
    18000,
    14000,
    12000,
    11000,
    10000,
    9000,
    4500,
    3500,
    3000,
    2500,
    2200,
    1800,
    1500,
    1200,
    900,
    700,
    500,
    350,
    250,
    150,
    100
  )
)

# Mantener el orden mostrado en la gráfica
df$seccion <- factor(df$seccion, levels = df$seccion)
factor_letra = 3.5

# Gráfica
p <- ggplot(df, aes(x = seccion, y = valor)) +
  geom_col(
    fill = "#335955",
    width = 0.62
  ) +
  scale_y_continuous(
    limits = c(0, 125000),
    breaks = c(0, 25000, 50000, 75000, 100000, 125000),
    labels = function(x) {
      paste0("$", format(x, big.mark = ",", scientific = FALSE))
    },
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    # title = "Valor en aduana por Sección de Fracción Arancelaria",
    title = "",
    x = "Sección",
    y = "Miles de millones de pesos"
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
    
    # Fondo
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



# GUARDAR----------- 

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
