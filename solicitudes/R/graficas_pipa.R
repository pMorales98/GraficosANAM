source("R/setup.R")

# Recaudación ----------------

tbl_recaudacion_lunes =
  readxl::read_excel("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx",
                     sheet = "estimulos_viernes_lunes") %>% 
  janitor::clean_names()


df_recaudacion_lunes =
  estandarizar_fecha(
    df = tbl_recaudacion_lunes,
    variable_fecha = lunes,
    nuevo_nombre_fecha = fecha,
    crear_mes = TRUE
  )

tbl_recaudacion_lunes %>% names()

(grafica = gg_pipes(
  df = df_recaudacion_lunes,
  variable_y = "recaudacion_mdp_lunes",
  titulo_eje_y = "Recaudación (MDP)",
  frecuencia_breaks = 1000,
  tipo_eje_y = "dinero",
  mostrar_bandas = TRUE,
  mostrar_promedio = TRUE,
  fecha_linea_vertical = "2026-03-30"
) )

gg_guardar = 
  grafica +
  tema_anam(
    tamano_letra = 12,
    tamano_titulo = 16,
    angulo_texto_eje_x = 45,
    ancho_linea_grid = 0.5
  )


# Pedimentos ---------

tbl_pedimentos_viernes =
  readxl::read_excel("solicitudes/data/recaudacion desgaregada ypedimentos viernes.xlsx",
                     sheet = "pedimentos viernes") %>% 
  janitor::clean_names() %>% 
  filter(fecha >= "2026-01-01")

tbl_pedimentos_viernes %>% names()

df_pedimentos_viernes =
  estandarizar_fecha(
    df = tbl_pedimentos_viernes,
    variable_fecha = fecha,
    nuevo_nombre_fecha = fecha,
    crear_mes = TRUE
  )

(grafica = gg_pipes(
  df = df_pedimentos_viernes,
  variable_y = "pedimentos",
  titulo_eje_y = "Pedimentos",
  frecuencia_breaks = 5000,
  tipo_eje_y = "conteo",
  mostrar_bandas = TRUE,
  mostrar_promedio = TRUE,
  fecha_linea_vertical = "2026-03-30"
) )

# Mercancía ---------

tendencias_rec =
  readxl::read_excel("solicitudes/data/tendencias_recaudacion.xlsx") %>% 
  janitor::clean_names()

tendencias_rec %>% names()

(df_tendencias_rec =
  estandarizar_fecha(
    df = tendencias_rec,
    variable_fecha = etiquetas_de_fila,
    nuevo_nombre_fecha = fecha,
    crear_mes = TRUE
  ))

(grafica = gg_pipes(
  df = df_pedimentos_viernes,
  variable_y = "valor_mercancia_mdp",
  titulo_eje_y = "Valor de pedimentos (MDP)",
  frecuencia_breaks = 20000,
  tipo_eje_y = "dinero",
  mostrar_bandas = TRUE,
  mostrar_promedio = TRUE,
  fecha_linea_vertical = "2026-03-30"
) )

gg_guardar = 
grafica +
  tema_anam(
    tamano_letra = 12,
    tamano_titulo = 16,
    angulo_texto_eje_x = 45,
    ancho_linea_grid = 0.5
  )

# Volumetría ----------------

df_volumetria =
  readxl::read_excel("solicitudes/data/volumetria_abandono_070626.xlsx") %>% 
  janitor::clean_names() %>% 
  rename("gwtc" = "wtc") %>% 
  pivot_longer(c("gwtc", "cla", "fedex"),
               names_to = "recinto",
               values_to = "volumetria") %>% 
  mutate(fecha = as.Date(fecha))

df <- estandarizar_fecha(
  df = df_volumetria,
  variable_fecha = fecha,
  crear_mes = TRUE
)


colores_recinto <- c(
  "gwtc" = "#90435F",
  "cla" = "#687383",
  "fedex" = "#A0B2A7"
)

(grafica = 
  gg_pipes(
    df = df,
    variable_y = "volumetria",
    grupo = "recinto",
    colores = colores_recinto,
    titulo_eje_y = "Volumetría",
    frecuencia_breaks = .2,
    tipo_eje_y = "porcentaje", 
    limites_eje_y = c(0,1),
    incluir_etiquetas = "todas",
    nombre_etiquetas = "Recinto"
  ) )

gg_guardar = 
  grafica +
  tema_anam(
    tamano_letra = 12,
    tamano_titulo = 16,
    angulo_texto_eje_x = 45,
    ancho_linea_grid = 0.5
  )


# GUARDAR ARCHIVO ---------------------

# Cierra el dispositivo de manera segura solo si está abierto
if (dev.cur() > 1) dev.off()

ggsave(
  "graficas/grafica_recaudacion_lunes.png",
  gg_guardar,
  width = 29.76,   # <-- 6850 px originales menos 10 píxeles
  height = 10.42,  # <-- 2598 px originales menos 10 píxeles
  units = "cm",   # Control absoluto de píxeles
  dpi = 600,
  scale = 1       # 1 asegura que los tamaños asignados (15pt, 18pt) no se escalen de más
)


