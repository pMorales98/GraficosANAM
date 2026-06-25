##Area
library(ggplot2)
library(showtext)

# Fuente Poppins
font_add_google("Poppins", "Poppins")
showtext_auto()

factor_letra = 3

# Datos
df <- data.frame(
  mes = c("Enero","Febrero","Marzo","Abril","Mayo","Junio"),
  asesor = c(26, 31, 45, 44, 40, 42)
)

df$bot <- 100 - df$asesor
df$x <- 1:nrow(df)

# Gráfica
p <- ggplot(df, aes(x = x)) +
  
  # Área superior (BOT)
  geom_ribbon(
    aes(ymin = asesor, ymax = 100),
    fill = "#ACCAB2"
  ) +
  
  # Área inferior (ASESOR)
  geom_ribbon(
    aes(ymin = 0, ymax = asesor),
    fill = "#BCD5D6"
  ) +
  
  # Línea divisoria
  geom_line(
    aes(y = asesor),
    color = "#7AA8AA",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = df$x,
    labels = df$mes,
    expand = c(0.05, 0.05)
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_family = "Poppins") +
  
  theme(
    axis.text.x = element_text(
      size = 24*factor_letra,
      face = "bold",
      color = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 24*factor_letra,
      face = "bold",
      color = "#333333"
    ),
    
    panel.grid.major.y = element_line(
      color = "#D9D9D9",
      linewidth = 0.6
    ),
    
    panel.grid = element_blank(),
    
    axis.line.x = element_line(
      color = "#BDBDBD",
      linewidth = 0.5
    ),
    
    axis.line.y = element_blank(),
    
    axis.ticks = element_blank(),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)

ggsave(
  "graficas/sombras.png",
  p,
  width = 17.45,  
  height = 11.52,
  units = "cm",  
  dpi = 600,
  scale = 1      
)
