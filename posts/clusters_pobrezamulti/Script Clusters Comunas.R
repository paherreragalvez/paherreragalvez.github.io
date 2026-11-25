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

################################################################################
# Normalización

base2024 <- base2024 %>% mutate(n_educ  = scales::rescale(prop_educacion),
                                n_salud = scales::rescale(prop_salud),
                                n_trab  = scales::rescale(prop_trabajo),
                                n_viv   = scales::rescale(prop_vivienda))

################################################################################
# Revisión de normalización

base2024 %>% select(starts_with("n_")) %>% summary()

par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
# Primera fila: distribuciones originales
plot(density(base2024$prop_educacion, na.rm = TRUE), main = "Original: Educacion", xlab = "prop_educacion", col = "blue", lwd = 2)
plot(density(base2024$prop_salud,     na.rm = TRUE), main = "Original: Salud",     xlab = "prop_salud",     col = "blue", lwd = 2)
plot(density(base2024$prop_trabajo,   na.rm = TRUE), main = "Original: Trabajo",   xlab = "prop_trabajo",   col = "blue", lwd = 2)
plot(density(base2024$prop_vivienda,  na.rm = TRUE), main = "Original: Vivienda",  xlab = "prop_vivienda",  col = "blue", lwd = 2)

# Primera fila: distribuciones normalizadas
plot(density(base2024$n_educ,  na.rm = TRUE), main = "Escalada: Educacion", xlab = "n_educ",  col = "darkgreen", lwd = 2)
plot(density(base2024$n_salud, na.rm = TRUE), main = "Escalada: Salud",     xlab = "n_salud", col = "darkgreen", lwd = 2)
plot(density(base2024$n_trab,  na.rm = TRUE), main = "Escalada: Trabajo",   xlab = "n_trab",  col = "darkgreen", lwd = 2)
plot(density(base2024$n_viv,   na.rm = TRUE), main = "Escalada: Vivienda",  xlab = "n_viv",   col = "darkgreen", lwd = 2)

par(mfrow = c(1, 1))

################################################################################
# Elección del valor K - Método 1. Codo (suma cuadrática de las distancias).

# Fórmula en formato latex
# $$\text{SSD}_{i} = \displaystyle\sum_{j \in \mathcal{C}_{i}}\left( x_{j} - \overline{x}_{i}\right)^{2}$$

# Crear una base limpia sin NA solo con las variables normalizadas
datos_norm <- base2024 %>% 
  select(starts_with("n_"))

# Para reproducibilidad
set.seed(123) # semilla para reproducibilidad

# Loop para cálculo del codo

clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(datos_norm,
  centers = k,
  nstart = 25)$tot.withinss}

plot(clusters, codo,
type = "l",
main = "Método del Codo. Suma cuadrática de distancias según valor k.",
xlab = "Valores de k",
ylab = "SSD")

################################################################################
# Elección del valor K - Método 2. Silueta (Cohesión y separación de cada clúster).

# Loop para cálculo de silueta (se utiliza la misma base datos_norm creada para el codo)
# Silueta mide qué tan bien separada está una observación de otro clúster vecino.
# qué tan bien "encaja" cada observación (cada comuna en este caso) dentro del cluster

silueta <- c(0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux <- kmeans(datos_norm,
                       centers = k,
                       nstart = 25)
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm))
  silueta[k] <- mean(silueta_aux[, 3])}

# plot(clusters, silueta,
# type = "b",
# main = "Método de la Silueta. Coeficiente según valor k.",
# xlab = "Valores de k",
# ylab = "Silueta promedio")

plot(2:10, silueta[2:10],
type = "b",
main = "Método de la Silueta. Coeficiente según valor k.",
xlab = "Valores de k",
ylab = "Silueta promedio")

#################################################################################
# K-means con el k seleccionado (i.e. K=3)

modelo_km <- kmeans(datos_norm, centers = 3, nstart = 25)

# Agregar la columna del cluster a la base de datos
datos_norm <- datos_norm %>% 
  mutate(cluster = modelo_km$cluster)

# datos_norm$comuna <- base2024$comuna

# Agregar a la base original el cluster asignado
base2024$cluster <- datos_norm$cluster

#################################################################################
# Exploración estadística de clusters

base2024 %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), mean))

base2024 %>% 
  group_by(cluster) %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), mean))

base2024 %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), c(mean, sd))) |> View()

base2024 %>% 
  group_by(cluster) %>% 
  summarise(across(c(prop_educacion, prop_salud, prop_trabajo, prop_vivienda), c(mean, sd))) |> View()

#################################################################################
# Exploración visual de los resultados

# Un boxplot por cada variable según el clúster (Datos normalizados)
par(mfrow = c(1, 4), mar = c(4, 4, 3, 1))
boxplot(n_educ ~ cluster, data = datos_norm, main = "Educación", col = c("gold", "tomato", "skyblue"), xlab = "Cluster")
boxplot(n_salud ~ cluster, data = datos_norm, main = "Salud",     col = c("gold", "tomato", "skyblue"), xlab = "Cluster")
boxplot(n_trab ~ cluster,  data = datos_norm, main = "Trabajo",   col = c("gold", "tomato", "skyblue"), xlab = "Cluster")
boxplot(n_viv ~ cluster,   data = datos_norm, main = "Vivienda",  col = c("gold", "tomato", "skyblue"), xlab = "Cluster")
par(mfrow = c(1, 1))

# Un boxplot por cada variable según el clúster (Datos originales)
par(mfrow = c(1, 4), mar = c(4, 4, 3, 1))
boxplot(prop_educacion ~ cluster, data = base2024, main = "Educación", col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_salud ~ cluster, data = base2024, main = "Salud",     col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_trabajo ~ cluster,  data = base2024, main = "Trabajo",   col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
boxplot(prop_vivienda ~ cluster,   data = base2024, main = "Vivienda",  col = c("gold", "tomato", "skyblue"), xlab = "Cluster", ylim = c(0, 0.7))
par(mfrow = c(1, 1))

#################################################################################
# Un boxplot por cada variable según el clúster (Datos originales)

# 4 columnas y sus nombres cortos para el eje X
vars <- c("prop_educacion", "prop_salud", "prop_trabajo", "prop_vivienda")
nombres <- c("Educ.", "Salud", "Trab.", "Viv.")

# 1 fila por 3 columnas (uno por cada clúster)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
boxplot(base2024[base2024$cluster == 1, vars], main = "Cluster 1", col = "gold",    names = nombres, ylim = c(0, 0.7))
boxplot(base2024[base2024$cluster == 2, vars], main = "Cluster 2", col = "tomato",  names = nombres, ylim = c(0, 0.7))
boxplot(base2024[base2024$cluster == 3, vars], main = "Cluster 3", col = "skyblue", names = nombres, ylim = c(0, 0.7))
par(mfrow = c(1, 1))

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
  coord_cartesian(ylim = c(0, 0.6)) +                            # Fija eje Y sin recortar densidades
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

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################









#################################################################################
# Medias por cluster desde datos_norm
medias <- datos_norm %>% 
  group_by(cluster) %>% 
  summarise(across(c(n_educ, n_salud, n_trab, n_viv), mean))

# 2. Convertir a matriz y trasponer
matriz_medias <- as.matrix(medias[, -1])

# 3. Graficar las 4 dimensiones lado a lado por cada clúster
barplot(t(matriz_medias), 
        beside = TRUE, 
        col = c("skyblue", "lightgreen", "orange", "pink"),
        main = "Promedio de Dimensiones Agrupado por Clúster",
        xlab = "Clúster", 
        ylab = "Promedio (0 a 1)",
        names.arg = paste("Cluster", medias$cluster),
        ylim = c(0, 1))

# Leyenda para identificar las variables
legend("topright", 
       legend = c("n_educ", "n_salud", "n_trab", "n_viv"), 
       fill = c("skyblue", "lightgreen", "orange", "pink"))


##################################################3

par(mfrow = c(1, 3), mar = c(5, 4, 3, 1))

# Un boxplot por cada clúster mostrando sus 4 dimensiones
for (k in 1:3) {
  datos_k <- datos_norm %>% 
    filter(cluster == k) %>% 
    select(n_educ, n_salud, n_trab, n_viv)
  
  boxplot(datos_k, 
          main = paste("Cluster", k), 
          col = c("skyblue", "lightgreen", "orange", "pink"),
          ylim = c(0, 1),
          las = 2) # las = 2 rota las etiquetas verticales para leerlas fácil
}

par(mfrow = c(1, 1))





plot(datos_norm$n_educ, datos_norm$n_salud, type = "p")

##############################################################################

plot(datos_norm$n_educ, datos_norm$n_salud, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 17, # pch = 19 hace que los puntos sean sólidos y más visibles
     cex = 1.3, # <-- Controla el tamaño de los puntos en el gráfico
     xlab = "Dimensión Educación", 
     ylab = "Dimensión Salud", 
     main = "Vulnerabilidad Educación vs Salud (Clusters de comunas chilenas)")

mtext("Clusters de comunas chilenas", side = 3, line = 0, cex = 0.9)

# Agregar leyenda para identificar los clusters
legend("bottomleft", 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"), 
       col = 1:3, 
       pch = 19)

##############################################################################

plot(datos_norm$n_educ, datos_norm$n_trab, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 19, # pch = 19 hace que los puntos sean sólidos y más visibles
     xlab = "Educación", 
     ylab = "Trabajo", 
     main = "Relación Educación vs Trabajo por Clúster")

##############################################################################
plot(datos_norm$n_educ, datos_norm$n_viv, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 19, # pch = 19 hace que los puntos sean sólidos y más visibles
     xlab = "Educación", 
     ylab = "Vivienda", 
     main = "Relación Educación vs Vivienda por Clúster")

##############################################################################
plot(datos_norm$n_salud, datos_norm$n_trab, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 19, # pch = 19 hace que los puntos sean sólidos y más visibles
     xlab = "Salud", 
     ylab = "Trabajo", 
     main = "Relación Salud vs Trabajo por Clúster")
##############################################################################
plot(datos_norm$n_salud, datos_norm$n_viv, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 19, # pch = 19 hace que los puntos sean sólidos y más visibles
     xlab = "Salud", 
     ylab = "Vivienda", 
     main = "Relación Salud vs Vivienda por Clúster")
##############################################################################
plot(datos_norm$n_trab, datos_norm$n_viv, 
     type = "p", 
     col = datos_norm$cluster, 
     pch = 19, # pch = 19 hace que los puntos sean sólidos y más visibles
     xlab = "Trabajo", 
     ylab = "Vivienda", 
     main = "Relación Trabajo vs Vivienda por Clúster")





















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


# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN
# JOIN

chile_gd3_cod <- chile_gd3 |> left_join(maestro_comunas, by = "nombre_comuna")

names(chile_gd3_cod)
names(chile_gd3_cod)

dim(chile_gd3_cod)
dim(chile_gd3)

View(chile_gd3_cod)

summary(chile_gd3_cod$codigo_comuna)


chile_gd3_cod |> filter(codigo_comuna=="") |> View()


distinct(chile_gd3_cod, codigo_comuna)|> View()




ggplot(chile_gd3) +
  geom_sf()

print("listo")  




rm(list = ls())

base_maestro_multidim <- read_parquet("R/Proyecto comunas/data/base_maestro_multidim.parquet")

base2024 <- base_maestro_multidim %>% filter(anio == 2024)

base2024 <- base2024 %>% select(-anio)

summary(base2024)

names(base2024)

View(base2024)
View(chile_gd3)

