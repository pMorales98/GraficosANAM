pacman::p_load(
  tidyverse,
  stringr,
  lubridate,
  janitor,
  data.table,
  readxl,
  sysfonts,
  showtext,
  ggrepel,
  ggfittext
)



# ── CONFIGURACIÓN GLOBAL ──
fuente_base <- "Montserrat"
tam_base <- 23

font_add_google("Montserrat", "Montserrat")
showtext_auto()


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


# Importaciones -----------

df <- importaciones %>%
  mutate(
    label = paste0(round(porcentaje_importaciones, 2), "%"),
    fuera = porcentaje_importaciones < quantile(porcentaje_importaciones, 0.6, na.rm = TRUE)
  )

ggplot(
  df,
  aes(
    x = reorder(aduana, porcentaje_importaciones),
    y = porcentaje_importaciones
  )
) +
  geom_col(fill = "#763043") +
  coord_flip() +
  
  geom_text(
    aes(
      label = label,
      hjust = ifelse(fuera, -0.1, 1.1),
      color = ifelse(fuera, "black", "white")
    ),
    size = 5,
    fontface = "bold",
    family = fuente_base   # 👈 SOLO CAMBIAS AQUÍ
  ) +
  
  scale_color_identity() +
  
  expand_limits(y = max(df$porcentaje_importaciones, na.rm = TRUE) * 1.15) +
  
  labs(x = NULL, y = NULL) +
  
  theme_minimal() +
  
  theme(
    text = element_text(
      family = fuente_base,   # 👈 GLOBAL
      size = tam_base
    ),
    
    plot.title = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    axis.title = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    axis.text = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "gray",
      linewidth = 0.7,
      linetype = "dashed"
    ),
    
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )


# Revisiones -----------

df <- revisiones %>%
  pivot_longer(
    cols = c("rojo", "verde"),
    names_to = "grupo",
    values_to = "valor"
  ) %>%
  group_by(aduana) %>%
  mutate(
    total = sum(valor, na.rm = TRUE),
    fuera = valor < quantile(valor, 0.2, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(grupo = case_when(grupo == "verde" ~ "Verde",
                           grupo == "rojo" ~ "Rojo")) %>% 
  mutate(
    grupo = factor(grupo, levels = c("Verde", "Rojo"))
  ) 

ggplot(
  df,
  aes(
    x = reorder(aduana, -total),
    y = valor,
    fill = grupo
  )
) +
  
  geom_col() +
  
  scale_fill_manual(values = c("Rojo" = "#A42145", "Verde" = "#0e713A")) +
  
  scale_y_continuous(
    breaks = seq(0, 130, by = 25),
    limits = c(0, 140)
  ) +
  
  expand_limits(y = max(df$porcentaje_importaciones, na.rm = TRUE) * 1.15) +
  
  labs(x = NULL, y = NULL) +
  
  theme_minimal() +
  
  theme(
    text = element_text(
      family = fuente_base,
      size = tam_base
    ),
    
    plot.title = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    axis.title = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    axis.text = element_text(
      family = fuente_base,
      face = "bold"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "gray",
      linewidth = 0.7,
      linetype = "dashed"
    ),
    
    axis.title.x = element_blank(),

    axis.text.x = element_text(size = 13),
    
    axis.text.y = element_text(size = 14),
    
    legend.text = element_text(size = 14),
    
    
    legend.title = element_blank(), legend.position = "top"
  )
