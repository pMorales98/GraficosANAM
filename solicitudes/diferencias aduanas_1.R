source("R/setup.R")

library(tidyverse)
library(scales)

# =========================================================
# 1. BASE LIMPIA
# =========================================================
datos_base <- datos_senda %>% 
  rename(
    Real = recaudado,
    Meta = meta_mes
  ) %>% 
  group_by(aduana, mes) %>% 
  reframe(
    Real = sum(Real, na.rm = TRUE),
    Meta = sum(Meta, na.rm = TRUE)
  ) %>% 
  mutate(
    Mes = factor(
      mes,
      levels = 1:12,
      labels = c("Ene","Feb","Mar","Abr","May","Jun",
                 "Jul","Ago","Sep","Oct","Nov","Dic")
    )
  ) %>% 
  select(aduana, Mes, Real, Meta)

for (a in unique(datos_base$aduana)) {
  
  datos =  datos_base %>% 
    filter(aduana == a)
  # -------------------------
  # INDICADOR DE DESEMPEÑO
  # -------------------------
  datos$Faltante <- pmax(datos$Meta - datos$Real, 0)
  
  ## Variable para que label no este debajo del eje
  
  datos$y_label <- pmax(datos$Real, max(datos$Real) * .15)
  
  # -------------------------
  # PASAR A FORMATO LARGO
  # -------------------------
  datos_long <- datos %>%
    select(Mes, Meta, Real, Faltante) %>%
    mutate(mayor_100 = if_else( Real / Meta >= 1, "Mayor_100", "ok")) %>% 
    pivot_longer(
      cols = c(Real, Faltante),
      names_to = "Tipo",
      values_to = "Valor"
    ) %>%
    
    # Crear variable de color condicional
    mutate(
      Tipo_color = case_when(
        Tipo == "Faltante" ~ "Faltante",
        Tipo == "Real" & mayor_100 == "ok" ~ "Real",
        Tipo == "Real" & mayor_100 != "ok" ~ "mayor_100",
      )
    )
  
  # -------------------------
  # GRÁFICO
  # -------------------------
  
  factor_tamano_letra = 4
  
  (grafica <- ggplot(datos_long,
                     aes(x = Mes, y = Valor, fill = Tipo_color)) +
      
      # Barras apiladas
      geom_col(width = 1) +
      
      # -------------------------
    # COLORES
    # -------------------------
    scale_fill_manual(
      values = c(
        "Real" = "#9b2247",            # rojo: no cumple
        "mayor_100" = "#75B7A0",         # verde: supera meta
        "Faltante" = alpha("gray20", 0.20)  # sombra objetivo
      ),
      labels = c(
        "Real" = "Recaudado",
        "mayor_100" = "Alcanza o supera meta",
        "Faltante" = "Meta mensual"
      ),
      guide = "none"
    ) +
      
      # -------------------------
    # FORMATO EJE Y
    # -------------------------
    scale_y_continuous(
      labels = label_dollar(prefix = "$"),
      expand = expansion(mult = c(0, 0.1))
    ) +
      
      # -------------------------
    # % dentro del bloque REAL
    # -------------------------
    
    geom_text(
      data = subset(datos, Real > 0),
      aes(
        x = Mes,
        y = ifelse(Real/Meta >= 0.05,
                   Real / 2,  # dentro de la barra
                   Real + max(datos$Meta) * 0.02),
        label = ifelse(
          Real / Meta < 0.005,
          "<1%",
          scales::percent(Real / Meta, accuracy = 1)
        ),
        color = ifelse(Real/Meta >= 0.05, "normal", "low")
      ),
      inherit.aes = FALSE,
      family = "noto",
      fontface = "bold",
      size = 4 * factor_tamano_letra,
      vjust = 0
    ) +
      # -------------------------
    # META arriba de cada barra
    # -------------------------
    geom_text(
      data = datos,
      aes(
        x = Mes,
        y = ifelse(
          Real/Meta >= 1,
          Real + max(Real) * 0.03,
          Meta + max(Meta) * 0.03
        ),
        label = ifelse(
          Real/Meta >= 1,
          paste0("$", scales::comma(Real, accuracy = 1)),
          paste0("$", scales::comma(Meta, accuracy = 1))
        ),
        color = ifelse(Real/Meta >= 1, "mayor_100", "menor_100")
      ),
      inherit.aes = FALSE,
      family = "noto",
      fontface = "bold",
      size = 4 * factor_tamano_letra
    ) +
      
      scale_color_manual(
        values = c(
          normal = "white",
          low = "black",
          mayor_100 = "#75B7A0",
          menor_100 = "black"
        ),
        guide = "none"
      )+
      # -------------------------
    # ETIQUETAS
    # -------------------------
    labs(
      x = NULL,
      y = "Recaudación (MDP)"
    ) +
      
      # -------------------------
    # TEMA PERSONALIZADO
    # -------------------------
    tema_anam(factor_escala = 1,
              mostrar_ejes = F,
              face_texto_ejes = "bold",
              face_titulo_ejes = "bold",
              margen_plot = "compacto") 
  
  )

# -------------------------
# GUARDAR OBJETO
# -------------------------
grafica


# =========================================================
# 9. GUARDAR
# =========================================================
filename <- paste0(
  "datos_senda/recaudacion_",
  gsub(" ", "_", a),
  ".png"
)

ggsave(
  filename,
  grafica,
  width = 6.11,
  height = 2.36,
  dpi = 600
)

}

