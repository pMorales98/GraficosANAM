
# Datos
df <- data.frame(
  mes = factor(
    c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"),
    levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")
  ),
  chatbot = c(1612, 4845, 6993, 6921, 8629, 1417),
  asesor = c(51175, 89962, 138249, 124642, 252111, 24518)
)

factor_letra = 4
tamano_letra_2_grafica = 30

# Gráfica
p <- ggplot(df, aes(x = mes)) +
  
  # Línea Solicitan asesor desde chatbot
  geom_line(
    aes(
      y = chatbot,
      color = "Solicitan asesor desde el chatbot",
      group = 1
    ),
    linewidth = 2
  ) +
  
  geom_point(
    aes(
      y = chatbot,
      color = "Solicitan asesor desde el chatbot"
    ),
    size = 4.5
  ) +
  
  # Línea Chats con asesor
  geom_line(
    aes(
      y = asesor,
      color = "Chats con asesor",
      group = 1
    ),
    linewidth = 2
  ) +
  
  geom_point(
    aes(
      y = asesor,
      color = "Chats con asesor"
    ),
    size = 4.5
  ) +
  
  # Etiquetas chatbot
  geom_label(
    aes(
      y = chatbot,
      label = scales::comma(chatbot)
    ),
    family = "Poppins",
    fontface = "bold",
    size = tamano_letra_2_grafica,
    fill = "white",
    color = "#ACCAB2",
    label.size = 0,
    label.padding = unit(0.02, "lines"),
    vjust = -0.5,
    show.legend = FALSE
  ) +
  
  # Etiquetas asesor
  geom_label(
    aes(
      y = asesor,
      label = scales::comma(asesor)
    ),
    family = "Poppins",
    fontface = "bold",
    size = tamano_letra_2_grafica,
    fill = "white",
    color = "#BCD5D6",
    label.size = 0,
    label.padding = unit(0.02, "lines"),
    vjust = -0.5,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Chats con asesor" = "#BCD5D6",
      "Solicitan asesor desde el chatbot" = "#ACCAB2"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 320000),
    breaks = c(0, 100000, 200000, 300000),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  
  guides(
    fill = "none",
    color = "none"
  ) +
  theme_minimal(base_family = "Poppins") +
  
  theme(
    # Leyenda
    legend.position = "top",
    
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 18*factor_letra,
      face = "plain",
      color = "#333333"
    ),
    
    # Eje X
    axis.text.x = element_text(
      size = 18*factor_letra,
      face = "bold",
      color = "#333333"
    ),
    
    # Eje Y
    axis.text.y = element_text(
      size = 18*factor_letra,
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
    
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)


ggsave(
  "graficas/gg5.png",
  p,
  width = 17.45,  
  height = 11.52,
  units = "cm",  
  dpi = 600,
  scale = 1      
)

