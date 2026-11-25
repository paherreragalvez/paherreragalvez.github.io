#https://gemini.google.com/app/4218fe2ab1d50518
#install.packages("imfweo")

rm(list=ls())
setwd("C:/00 Pablo/Programacion/R/Análisis macro internacional")

library(imfweo)
library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)

################################################################################
# # Descargar la base completa de la publicación WEO más reciente
# datos_weo <- weo_get()
# weo_get_entities() %>% View()
# weo_get_series() %>% write.xlsx("Series IMF.xlsx")
# paste(weo_get_series()$series_id, weo_get_series()$series_name, sep = " - ")
# series <- weo_get_series()
# 
# aux_search <- "Gross domestic product"
# 
# search_results <- series %>%
#   mutate(across(everything(), as.character)) %>%
#   filter(
#     str_detect(series_id, regex(aux_search, ignore_case = TRUE)) |
#       str_detect(series_name, regex(aux_search, ignore_case = TRUE))
#   )
# 
# print(search_results)
################################################################################

# Crear tabla de consulta (Lookup Table) con todas las series del WEO
df_series_lookup <- weo_get_series() %>%
  select(series_id, series_name, units) %>%
  distinct(series_id, .keep_all = TRUE)

# Definir variables clave
vars_macro <- c("NGDPRPPPPC", "NGDP_RPCH", "GGXWDG_NGDP", "PCPIPCH", "BCA_NGDPD")

# 1. Latinoamérica (Referencia Regional)
g1_latam <- c("CHL", "ARG", "BRA", "COL", "MEX", "PER", "URY")
g1_latamb <- c("CHL", "BRA", "COL", "MEX", "PER", "URY")

# 2. Potencias Top Mundiales (G7 y Motores Globales)
g2_top_world <- c("CHL", "USA", "DEU", "JPN", "GBR", "FRA", "KOR")

# 3. Referentes OCDE / Primer Mundo con Perfil Exportador/Commodities
g3_ocde_peers <- c("CHL", "AUS", "NZL", "CAN", "ESP", "PRT")

# 4. Países en Desarrollo Comparables (Emergentes de Alto Ingreso / Círculo OCDE)
g4_comparables <- c("CHL", "POL", "CZE", "HUN", "MYS", "CRI", "PAN")

# Grupo de Gigantes Emergentes / BRICS+
g_brics <- c("CHL", "CHN", "IND", "BRA", "RUS", "ZAF")

grupo_paises <- g4_comparables


# Descargar datos para Chile vs Referentes OCDE y Comparables
df_comparativo <- weo_get(
  entities   = grupo_paises,
  series     = vars_macro,
  start_year = 2010,
  end_year   = 2026
)

df <- df_comparativo %>% select(-c(series_name, units))

serie_seleccion <- "NGDPRPPPPC"

# NGDPRPPPPC (PIB Real Per Cápita en Paridad de Poder Adquisitivo - USD Int.)
# NGDP_RPCH (Crecimiento Real del PIB - % Anual)
# GGXWDG_NGDP (Deuda Bruta del Gobierno General - % del PIB)
# PCPIPCH (Inflación Promedio del Periodo - % Anual)
# BCA_NGDPD (Cuenta Corriente - % del PIB)


# 1. Filtrar los datos para obtener solo la tasa de crecimiento (NGDP_RPCH)
df_select <- df %>%
  filter(series_id == serie_seleccion)

# 2. Definir la lista de países y asignar colores/formas
paises <- unique(df_select$entity_id)

# Asignar un color rojo/destacado a Chile y tonos de gris al resto
colores_paises <- setNames(
  ifelse(paises == "CHL", "#D62728", "gray60"), 
  paises
)

#variable_titulo <- as.character(df_series_lookup$series_name[df_series_lookup$series_id == serie_seleccion])
variable_titulo <- serie_seleccion

# 1. Extraer unidades (si viene NA o vacío, asigna "")
unidades_sel <- df_series_lookup$units[df_series_lookup$series_id == serie_seleccion]
unidades_sel <- ifelse(is.na(unidades_sel) | unidades_sel == "", "", as.character(unidades_sel))



# 1. Extraer unidades (si viene NA o vacío, asigna "")
unidades_sel <- df_series_lookup$units[df_series_lookup$series_id == serie_seleccion]
unidades_sel <- ifelse(is.na(unidades_sel) | unidades_sel == "", "", as.character(unidades_sel))

jpeg(paste0("figuras/",serie_seleccion,".jpg"), width = 600, height = 450, quality = 200)

# 2. Gráfico
ggplot(df_select, aes(x = year, y = value, color = entity_id, group = entity_id)) +
  # Línea de referencia en 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.6) +
  # Capa de líneas
  geom_line(aes(linewidth = ifelse(entity_id == "CHL", 1.4, 0.6))) +
  # Capa de puntos
  geom_point(aes(shape = entity_id, size = ifelse(entity_id == "CHL", 2.5, 1.5))) +
  # Escalas manuales
  scale_color_manual(values = colores_paises) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4, 1, 2, 0)) +
  scale_linewidth_identity() +
  scale_size_identity() +
  # Configuración del eje X
  scale_x_continuous(breaks = seq(min(df_select$year), max(df_select$year), by = 2)) +
  # Títulos y etiquetas dinámicas
  labs(
    title    = paste0("Evolución ", variable_titulo, ": Chile vs. Países Seleccionados"),
    subtitle = "Evolución desde 2010",
    x        = "Año",
    y        = unidades_sel,
    color    = "País",
    shape    = "País",
    caption  = paste0("Fuente: IMF World Economic Outlook Database (Serie: ", serie_seleccion, ")")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title     = element_text(face = "bold", size = 12),
    plot.subtitle  = element_text(color = "gray30", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


dev.off()















