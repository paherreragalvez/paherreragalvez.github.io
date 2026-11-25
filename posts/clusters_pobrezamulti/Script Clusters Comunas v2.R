################################################################################
# Inicio del código

rm(list = ls())
setwd("C:/00 Pablo/Programacion/")

# Generales manejo de datos
library(dplyr)
library(tidyr) # Para funciones de limpieza como replace_na
library(arrow) # Manejo en formato Parquet
library(haven) # Formato dta.
library(scales) # Para normalizar variables (para clusters)

library(ggplot2) # Visualizaciones
library(skimr) # Compact and Flexible Summaries of Data
# Clusters
library(cluster)
# Para Mapas
library(sf) #Estructura y manipulación de datos espaciales
library(geodata) # Para importar políg
# Para importar Maestro Comunas
library(readxl)


################################################################################
# Carga de datos y exploración inicial

base_maestro_multidim <- read_parquet("R/Proyecto comunas/data/base_maestro_multidim.parquet")
base2024 <- base_maestro_multidim %>% filter(anio == 2024)
rm(base_maestro_multidim)
base2024 <- base2024 %>% select(-anio)
#names(base2024)
#summary(base2024)
#View(base2024)

sum((base2024 |> filter(comuna < 14000 & comuna >=13000))$poblacion_comunal)

dim(base2024)

################################################################################
# Normalización

base2024 <- base2024 %>% mutate(n_educ  = scales::rescale(prop_educacion),
                                n_salud = scales::rescale(prop_salud),
                                n_trab  = scales::rescale(prop_trabajo),
                                n_viv   = scales::rescale(prop_vivienda))

################################################################################
# Revisión de normalización

base2024 %>% select(starts_with("n_")) %>% summary()

################################################################################
# Método 1. Codo (suma cuadrática de las distancias).
# Método 2. Silueta (Cohesión y separación de cada clúster).

# Crear una base limpia sin NA solo con las variables normalizadas
datos_norm <- base2024 %>% 
  select(starts_with("n_"))

# Para reproducibilidad
set.seed(123) # semilla para reproducibilidad

#################################################################################
# K-means con el k seleccionado (i.e. K=3)

modelo_km <- kmeans(datos_norm, centers = 3, nstart = 25)

# Agregar la columna del cluster a la base de datos
datos_norm <- datos_norm %>% 
  mutate(cluster = modelo_km$cluster)

# Agregar a la base original el cluster asignado
base2024$cluster <- datos_norm$cluster

#################################################################################
# Exploración estadística de clusters

base2024 %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), mean))

base2024 %>% 
  group_by(cluster) %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), mean))

#base2024 %>% 
#  group_by(cluster) %>% 
#  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), c(mean, sd))) |> View()

#################################################################################
# Exploración visual de los resultados

# Un boxplot por cada variable según el clúster (Datos originales)
par(mfrow = c(1, 4), mar = c(4, 4, 3, 1))
boxplot(prop_educacion ~ cluster, data = base2024, main = "Educación", col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_salud ~ cluster, data = base2024, main = "Salud",     col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_trabajo ~ cluster,  data = base2024, main = "Trabajo",   col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_vivienda ~ cluster,   data = base2024, main = "Vivienda",  col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
par(mfrow = c(1, 1))

#################################################################################
# Scatter Plots de los Resultados EDUCACIÓN - SALUD

colores <- c("darkgoldenrod1", "red", "blue")

#colors()

plot(base2024$prop_educacion, base2024$prop_salud, 
     type = "p", 
     pch = 24,                                                     # Triángulo con contorno
     col = "black",                                                # Color del contorno
     bg = colores[as.numeric(as.factor(base2024$cluster))],      # Color del relleno
#     col = colores[as.numeric(as.factor(datos_norm$cluster))],
#     pch = 17, # pch = 19 hace que los puntos sean sólidos y más visibles
     cex = 1.3, # <-- Controla el tamaño de los puntos en el gráfico
     xlab = "Dimensión Educación", 
     ylab = "Dimensión Salud", 
     main = "Vulnerabilidad Educación vs Salud (Clusters de comunas chilenas)")

mtext("Clusters de comunas chilenas", side = 3, line = 0, cex = 0.9)

# Agregar leyenda para identificar los clusters
legend("bottomleft", 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"), 
       col = colores, 
       pch = 19)


# Scatter Plots de los Resultados TRABAJO - VIVIENDA


# comuna
# poblacion_comunal
# prop_educacion
# prop_salud
# prop_trabajo
# prop_vivienda
# anio

###############################################
## Para hacer mapa de comunas

# IMPORTAR MAPAS PAÍSES ########################################################

chile_gd3 <- gadm("Chile", level = 3, path = tempdir()) |> st_as_sf()
View(chile_gd3)
names(chile_gd3)
chile_gd3 <- chile_gd3 |> select(NAME_3, geometry)
chile_gd3 <- chile_gd3 |> rename(nombre_comuna = NAME_3)

# COMUNAS CASEN
maestro_comunas <- read_excel("C:/00 Pablo/Programacion/BBDD/CUT-2018/CUT_2018_v04.xls")
View(maestro_comunas)
names(maestro_comunas)
maestro_comunas <- maestro_comunas |> select("Código Comuna 2018", "Nombre Comuna")
maestro_comunas <- maestro_comunas |> rename(codigo_comuna = "Código Comuna 2018", nombre_comuna = "Nombre Comuna")

# AGREGAR CLUSTER A MAESTRO

base2024 <- base2024 |> rename(codigo_comuna = comuna)
maestro_comunas$codigo_comuna <- as.double(maestro_comunas$codigo_comuna)

View(base2024)

dim(base2024)
dim(maestro_comunas)
maestro_comunas <- maestro_comunas |> left_join(base2024, by = "codigo_comuna")
View(maestro_comunas)

names(maestro_comunas)

summary(maestro_comunas)

maestro_comunas <- maestro_comunas |> select(codigo_comuna, nombre_comuna, cluster)

View(maestro_comunas)


chile_gd3_cod <- chile_gd3 |> left_join(maestro_comunas, by = "nombre_comuna")

names(chile_gd3_cod)
names(chile_gd3_cod)

dim(chile_gd3_cod)
dim(chile_gd3)

View(chile_gd3_cod)

summary(chile_gd3_cod)


str(chile_gd3_cod)

names(chile_gd3_cod)

ggplot(chile_gd3) +
  geom_sf()




# MAPA

ggplot(data = chile_gd3_cod) +
  geom_sf(aes(fill = factor(cluster)), color = "gray30", linewidth = 0.1) +
    coord_sf(expand = FALSE,
           xlim = c(-79, -62),
           ylim = c(-56.2, -17.3)) +

  scale_fill_manual(
    values = colores,
    na.value = "gray85",          # Color gris para los NA
    labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Sin datos"),
    name = "Cluster"
  ) +
  theme_void() +                  # Elimina ejes y cuadrícula de coordenadas
  labs(
    title = "Vulnerabilidad Educación vs Salud",
    subtitle = "Clusters de comunas chilenas"
  )

# MAPA RM


chile_gd3_cod_rm <- chile_gd3_cod |> filter(codigo_comuna >= 13000 & codigo_comuna < 14000 )

ggplot(data = chile_gd3_cod_rm) +
  geom_sf(aes(fill = factor(cluster)), color = "gray30", linewidth = 0.1) +
    coord_sf(expand = FALSE) +
  scale_fill_manual(
    values = colores,
    na.value = "gray85",          # Color gris para los NA
    labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Sin datos"),
    name = "Cluster"
  ) +
  theme_void() +                  # Elimina ejes y cuadrícula de coordenadas
  labs(
    title = "Vulnerabilidad Educación vs Salud",
    subtitle = "Clusters de comunas chilenas"
  )




# MAPA 2

#https://bastianolea.rbind.io/blog/mapa_chile_triple/

mapa_base <- mapa |> 
  # reproyectar
  st_transform(crs = 5360) |> 
  # visualizar
  ggplot() +
  geom_sf() +
  theme_minimal(base_size = 8)

limites_continental <- c(-78, -64)



##########3

mapa_norte <- mapa_base +
  coord_sf(expand = FALSE,
           xlim = limites_continental,
           ylim = c(cortes$norte[1], cortes$sur[1]))

mapa_centro <- mapa_base +
  coord_sf(expand = FALSE,
           xlim = limites_continental,
           ylim = c(cortes$norte[2], cortes$sur[2]))

mapa_sur <- mapa_base +
  coord_sf(expand = FALSE,
           xlim = limites_continental,
           ylim = c(cortes$norte[3], cortes$sur[3]))

##############

library(patchwork)

mapa_norte + mapa_centro + mapa_sur + 
  plot_annotation(title = "Mapa de Chile",
       subtitle = "dividido en tres secciones") &
  theme(axis.text.x = element_blank())

























#################################################################################
# Violin plot

# 1. Transformar las 4 variables a formato largo y ordenar los títulos
base_larga <- base2024 %>%
  select(cluster, prop_educacion, prop_salud, prop_trabajo, prop_vivienda) %>%
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "dimension",
    values_to = "valor"
  ) %>%
  mutate(
    dimension = factor(
      dimension,
      levels = c("prop_educacion", "prop_salud", "prop_trabajo", "prop_vivienda"),
      labels = c("Educación", "Salud", "Trabajo", "Vivienda")
    )
  )

# 2. Graficar los 4 violin plots en una sola fila (1 x 4)
ggplot(base_larga, aes(x = factor(cluster), y = valor, fill = factor(cluster))) +
  geom_violin(trim = FALSE, alpha = 0.85) +
  #geom_boxplot(width = 0.12, fill = "white", outlier.size = 1) + # Boxplot interior
  facet_wrap(~ dimension, nrow = 1) +                            # Simula par(mfrow = c(1, 4))
  scale_fill_manual(values = c("gold", "tomato", "skyblue")) +
  coord_cartesian(ylim = c(0, 0.7)) +                            # Fija eje Y sin recortar densidades
  labs(
    x = "Cluster",
    y = "Proporción de vulnerabilidad"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",                                    # Oculta leyenda redundante
    strip.text = element_text(face = "bold", size = 11),         # Títulos de cada panel en negrita
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank()
  )
