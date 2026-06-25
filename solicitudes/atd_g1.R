##Grafica PPT
library(ggplot2)
library(showtext)

# Fuente Poppins
font_add_google("Poppins", "Poppins")
showtext_auto()
font_add_google("Poppins", "Poppins")
font_add_google("Poppins", "Poppins Bold", regular.wt = 700)
library(scales)
# Datos
df <- data.frame(
  seccion = c(16, 6, 11, 15, 7, 18, 17, 2, 4, 20, 1, 13, 10, 9, 5, 12, 8, 3, 14, 22, 21, 19),
  fracciones = c(1385, 1204, 866, 737, 360, 309, 308, 287, 244, 228, 212, 191, 182, 150, 140, 80, 70, 55, 55, 50, 23, 18)
)

# Mantener orden original
df$seccion <- factor(df$seccion, levels = df$seccion)

factor_letra = 5
tamano_letra_2_grafica = 20

# Gráfica
p <- ggplot(df, aes(x = seccion, y = fracciones)) +
  geom_col(
    fill = "#335955",
    width = 0.62
  ) +
  geom_text(
    aes(label = comma(fracciones)),
    vjust = -0.5,
    family = "Poppins",
    fontface = "bold",
    size = tamano_letra_2_grafica,
    color = "#335955"
  ) +
  scale_y_continuous(
    limits = c(0, 1500),
    breaks = c(0, 500, 1000, 1500),
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "",
    # title = "Fracciones arancelarias por sección",
    x = "Sección",
    y = "No. Fracciones"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    # Título
    plot.title = element_text(
      size = 18 *factor_letra , 
      face = "bold",
      color = "#666666",
      hjust = 0
    ),
    
    # Títulos de ejes
    axis.title.x = element_text(
      size = 18  *factor_letra,
      face = "bold",
      color = "#333333",
      margin = margin(t = 15)
    ),
    
    axis.title.y = element_text(
      size = 18  *factor_letra,
      face = "bold",
      color = "#333333",
      margin = margin(r = 15)
    ),
    
    # Etiquetas de ejes (normales)
    axis.text.x = element_text(
      size = 15  *factor_letra,
      face = "plain",
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 15 *factor_letra,
      face = "plain",
      color = "#333333"
    ),
    
    # # Sin gridlines
    # panel.grid = element_blank(),
    # Gridlines horizontales
    panel.grid.major.y = element_line(
      color = "#D9D9D9",
      linewidth = 0.6
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Ejes visibles
    axis.line.x = element_line(
      color = "#BDBDBD",
      linewidth = 0.5
    ),
    
    axis.line.y = element_line(
      color = "#BDBDBD",
      linewidth = 0.5
    ),
    
    # Sin ticks
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
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)



# Exportar exactamente a 1025 x 520 px

if (dev.cur() > 1) dev.off()

ggsave(
  "graficas/fracciones_por_seccion.png",
  p,
  width = 21.74,  
  height = 10.13,
  units = "cm",  
  dpi = 600,
  scale = 1      
)

# ggsave(
#   "fracciones_por_seccion.png",
#   p,
#   width = 1025,
#   height = 520,
#   units = "px",
#   dpi = 100,
#   bg = "white"
# )

browseURL(getwd())
