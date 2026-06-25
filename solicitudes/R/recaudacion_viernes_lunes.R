# SETUP -------------
archivos <- list.files(
  path = "R/",
  pattern = "\\.R$",
  full.names = TRUE
)

lapply(archivos, source)

# CARGAR DATOS -------------

readxl::excel_sheets("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx")

tbl_recaudacion_lunes =
  readxl::read_excel("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx",
                     sheet = "recaudacion-lunes")

tbl_pedimentos =
  readxl::read_excel("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx",
                     sheet = "pedimentos viernes")

tbl_estimulos =
  readxl::read_excel("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx",
                     sheet = "estimulos_viernes_lunes") %>% 
  janitor::clean_names() %>% 
  mutate(viernes = substr(start = nchar(etiquetas_de_fila)-9, 
                        stop = nchar(etiquetas_de_fila), 
                        x = etiquetas_de_fila),
         .after = lunes) %>% 
  mutate(viernes = as.Date(format(as.Date(viernes, format = "%d/%m/%Y"), "%Y-%m-%d")),
         lunes = as.Date(lunes)) %>% 
  filter(promedio_de_diesel_pct != 0) %>% 
  filter(!str_detect(etiquetas_de_fila, "02/06/2026"))

# DF SOMBRAS -----------


factor <- max(tbl_estimulos$promedio_de_recaudacion_mpd_viernes, na.rm = TRUE) /
  max(tbl_estimulos$promedio_de_diesel_pct, na.rm = TRUE)

df_sombras <- tbl_estimulos %>%
  mutate(
    centro = viernes + (viernes - lunes)/2,
    etiqueta_rango = paste0(
      format(viernes, "%d %b"),
      " - ",
      format(lunes, "%d %b")
    )
  ) %>% 
  mutate(etiqueta_rango = case_when(str_detect(etiqueta_rango,"02 jun. - 08") ~ "02 jun.",
                                    T  ~ etiqueta_rango)) %>% 
  mutate(centro = case_when(str_detect(etiqueta_rango,"02 jun.") ~ as.Date("2026-06-03"),
                            T  ~ centro)) %>% 
  mutate(
    diesel_scaled = promedio_de_diesel_pct * factor,
    magna_scaled  = promedio_de_magna_pct * factor
  )



df_barras = 
  df_sombras %>%
 
  select(viernes,
         lunes,
         centro,
         etiqueta_rango,
         promedio_de_recaudacion_mpd_viernes,
         recaudacion_mdp_lunes) %>% 
  pivot_longer(c("promedio_de_recaudacion_mpd_viernes","recaudacion_mdp_lunes"),
               names_to = "dia",
               values_to = "promedio_de_recaudacion_mpd") %>% 
  mutate(dia = gsub("(?i)promedio_de_recaudacion_mpd_|recaudacion_mdp_",
                    "",
                    dia)) %>% 
  mutate(dia = factor(dia, c("viernes", "lunes"))) %>% 
  arrange(viernes, dia)




grafica <- 
  ggplot(df_sombras, aes(x = centro)) +
  
  # 🌊 sombras (eje izquierdo escalado)

  geom_ribbon(
    aes(
      ymin = 0,
      ymax = magna_scaled,
      fill = "Magna"
    ),
    alpha = 0.80
  ) +
  
  # geom_line(
  #   aes(y = magna_scaled),
  #   color = "#161a1d",
  #   linewidth = 1
  # ) +
  
  geom_ribbon(
    aes(
      ymin = 0,
      ymax = diesel_scaled,
      fill = "Diésel"
    ),
    alpha = 0.25
  ) +
  
  # 
  # geom_line(
  #   aes(y = diesel_scaled),
  #   color = "#0E713A",
  #   linewidth = 1
  # ) +
  
  # 📊 barras (recaudación real)
  geom_col(data = df_barras,
    aes(y = promedio_de_recaudacion_mpd,
    fill = dia),
    width = 4.5,
    position = "dodge"
  ) +
  
  # 📈 eje secundario
  scale_y_continuous(
    name = "Recaudación (MPD)\n",
    labels = function(x) paste0("$", scales::comma(x)),
    sec.axis = sec_axis(
      ~ . / factor,
      name = NULL,
      breaks = NULL,
      labels = NULL
    )
  ) +
  scale_x_date(
    breaks = df_sombras$centro,
    labels = df_sombras$etiqueta_rango
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c(
      "Diésel" = "#161a1d",
      "Magna" = "#A1B5A1",
      "viernes" = "#2f4b66",
      "lunes" = "#a9b9cc"
    ),
    labels = c(
      "Diésel" = "Estímulo a diésel",
      "Magna" = "Estímulo a magna",
      "viernes" = "Viernes",
      "lunes" = "Lunes"
    )
  ) +
  tema_anam(tamano_letra = 2, tamano_titulo = 2, angulo_texto_eje_x = 45) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
  

grafica

