###########################################
#####MERGE Y ANALISIS CON DATOS REALES#####
###########################################

#################
####LIBRERIAS####
#################
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(lubridate)
library(fastDummies)
library(expss)
library(car)
library(corrplot)

install.packages("corrplot")

#############################
####DIRECTORIO DE TRABAJO####
#############################
setwd("C:/SEGUNDO SEMESTRE/Taller de Estadística/SEGUNDO PARCIAL")


#####################################
###ABRAMOS NUESTRAS BASES DE DATOS###
#####################################
#Deforestacion
BD1 <- read_excel("deforestacion.xlsx")

#Encuesta de hogares
BD2 <- read_sav("EH2021_Persona.sav")

#Vinos
BD3 <- read.csv("winequality-red.csv",
                sep = ";")

#Indicadores de desarrollo
BD4 <- read.csv("world_bank_2024.csv")

#Cambiamos los nombres de BD1
bd1_var <- c('year', 'defor_ha')
names(BD1) <- bd1_var

###################################
####AJUSTES DE LA BASE DE DATOS####
###################################
#Primero volvamos los nombres en algo mas normal
anhos <- seq(1960, 2023, by = 1)
anhos <- as.character(anhos)
nombres <- c("nombre_pais", "cod_pais", "var", "cod_var", anhos)
names(BD4) <- nombres

#Tenemos que realizar un reshape
#Quiero que las variables sean las variables
#Y que los anhos sean las observaciones
#Primero anhos a observaciones
BD4_r <- pivot_longer(data = BD4,
                      cols = anhos)
names(BD4_r)[names(BD4_r) == "name"] = "anho"

#Veamos los valores vacios
sum(BD4_r$cod_pais == "")
sum(BD4_r$cod_var == "")

#Eliminemos los vacios
BD4_r <- BD4_r %>%
  filter(cod_pais != "")

#Ahora variables a variables
BD4_r <- pivot_wider(data = BD4_r,
                     id_cols = c("cod_pais", "anho"),
                     names_from = "cod_var",
                     values_from = "value")

#Para poder observar los nombres de las variables siempre podemos volver a la lista original
unique(BD4$var)
unique(BD4$cod_var)

#Podemos ver que todos los tipos de variables son en texto
#Volvamos todo numero
BD4_r <- BD4_r %>%
  mutate_at(dplyr::vars(-cod_pais), as.numeric)

#############################
####ANALISIS MULTIVARIADO####
#############################
#Primero veremos como se entiende un analisis multivariado
#Hagamos un analisis de la relación entre el PIB y la superficie
#Primero cortamos nuestra base de datos para el 2020
#Digamos que hemos creado una base de datos para Chile
BD4_2020 <- BD4_r %>%
  filter(anho == 2020)

#Hagamos un gráfico de puntos de dispersión entre las dos variables
ggplot(
  BD4_2020,
  aes(
    x = NY.GDP.MKTP.CD/1000000000,
    y = AG.SRF.TOTL.K2/1000
  )
) +
  geom_point(
    col = "#990000"
  ) +
  labs(
    x = "PIB (valor corriente en miles de millones de USD)",
    y = "Superficie (en miles de Km2)",
    title = "Relacion entre tamaño del PIB y superficie"
  ) +
  theme_minimal()

#Podemos observar una clara relacion entre estas variables
#Analicemos entonces la matriz de correlacion
#Creamos una nueva sub-base solo con los numericos
BD4_num <- BD4_2020%>%
  select_if(is.numeric)

#Eliminamos las variables que solo tienen NA
BD4_num <- BD4_num %>%
  select_if(~ !all(is.na(.)))

#Ahora hagamos la matriz
cor(
  BD4_num,
  method = "pearson",
  use = "pairwise.complete.obs")

#Existe otra manera de visualizarlo
BD4_cor <- cor(
  BD4_num,
  method = "pearson",
  use = "pairwise.complete.obs")

#Ponemos como 0 aquellos valores de correlacion igual a 0
BD4_cor[is.na(BD4_cor)] <- 0

#Graficamos la correlación con colores
corrplot(
  BD4_cor,
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.1)

#Finalmente tenemos el mapa de calor
corrplot(BD4_cor,
         method = c("color"),
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.2,
         col = colorRampPalette(c("blue",
                                  "white",
                                  "red"))(100))

#Hagamos otro caso de analisis multivariado
#Esta vez analizaremos el salario con algunas otras caracteristicas
#Primero con el sexo, hagamos una media condicionada
BD2 %>%
  group_by(s01a_02) %>%
  summarise(mean(ylab, na.rm = TRUE),
            sd(ylab, na.rm = TRUE))

#Hagamos un histograma para cada caso
ggplot(
  BD2,
  aes(
    x = ylab,
    fill = factor(s01a_02)
  )
) +
  geom_histogram(
    col = "black",
    alpha = 0.7
  ) +
  labs(
    x = "Ingreso",
    y = "Casos"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombres",
                               "2" = "Mujeres"))

#Veamos un boxplot por sexo
ggplot(
  BD2,
  aes(x = factor(s01a_02),
      y = ylab,
      group = factor(s01a_02),
      fill = factor(s01a_02))
  )+
  geom_boxplot(color = "black") +
  labs(title = "Boxplot del valor",
       x = "Sexo",
       y = "Ingreso laboral (en bolivianos por mes)") +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombre",
                               "2" = "Mujer"))

#Hagamos una base para cada caso
BD2_h <- BD2 %>%
  filter(s01a_02 == 1)

BD2_m <- BD2 %>%
  filter(s01a_02 == 2)

#Veamos los valores maximos en los bigotes
boxplot_vh <- boxplot.stats(BD2_h$ylab)
boxplot_vm <- boxplot.stats(BD2_m$ylab)

#En este caso voy a eliminar aquellos ingresos que sean superiores al bigote mas alto de las mujeres
limite_alto <- boxplot_vm$stats[5]
BD2_c <- BD2 %>%
  filter(ylab <= limite_alto)

#Hagamos un histograma de nuevo
ggplot(
  BD2_c,
  aes(
    x = ylab,
    fill = factor(s01a_02)
  )
) +
  geom_histogram(
    col = "black",
    alpha = 0.7
  ) +
  labs(
    x = "Ingreso",
    y = "Casos"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombres",
                               "2" = "Mujeres"))

#Para ilustrar mejor la diferencia puedo anhadir un par de linea en los promedio de cada grupo
ggplot(
  BD2_c,
  aes(
    x = ylab,
    fill = factor(s01a_02)
  )
) +
  geom_histogram(
    col = "black",
    alpha = 0.7
  ) +
  geom_vline(
    xintercept = mean(BD2_h$ylab, na.rm = TRUE),
    linetype = "dashed",
    color = "blue",
    size = 1
  ) +
  geom_vline(
    xintercept = mean(BD2_m$ylab, na.rm = TRUE),
    linetype = "dashed",
    color = "#ff007f",
    size = 1
  ) +
  labs(
    x = "Ingreso",
    y = "Casos"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombres",
                               "2" = "Mujeres"))

#Y de nuevo un boxplot
ggplot(
  BD2_c,
  aes(x = factor(s01a_02),
      y = ylab,
      group = factor(s01a_02),
      fill = factor(s01a_02))
)+
  geom_boxplot(color = "black") +
  labs(title = "Boxplot del valor",
       x = "Sexo",
       y = "Ingreso laboral (en bolivianos por mes)") +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombre",
                               "2" = "Mujer"))

#Pero sabemos que eliminar variables es toca la base de datos y no siempre es adecuado
#Para evitar outliers y analizar el comportamiento de dos grupos tenemos diferentes alternativas
#Logaritmos
ggplot(
  BD2_c,
  aes(x = factor(s01a_02),
      y = log(ylab),
      group = factor(s01a_02),
      fill = factor(s01a_02))
)+
  geom_boxplot(color = "black") +
  labs(title = "Boxplot del valor",
       x = "Sexo",
       y = "Ingreso laboral (en bolivianos por mes)") +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombre",
                               "2" = "Mujer"))

#Estandarización
BD2_c <- BD2_c %>%
  mutate(ylab_norm = ((ylab-mean(ylab, na.rm = TRUE))/
                         sd(ylab, na.rm = TRUE)))
ggplot(
  BD2_c,
  aes(x = factor(s01a_02),
      y = ylab_norm,
      group = factor(s01a_02),
      fill = factor(s01a_02))
)+
  geom_boxplot(color = "black") +
  labs(title = "Boxplot del valor",
       x = "Sexo",
       y = "Ingreso laboral (en bolivianos por mes)") +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "lightblue",
                               "2" = "lightpink"),
                    name = "Sexo",
                    labels = c("1" = "Hombre",
                               "2" = "Mujer")) 

#################
####EJERCICIO####
#################
#1. Hagan un analisis multivariado entre el ingreso y el area
#GRAFICO DE DISPERSION
ggplot(
  BD2,
  aes(
    x = s0
    y = AG.SRF.TOTL.K2/1000
  )
) +
  geom_point(
    col = "#990000"
  ) +
  labs(
    x = "PIB (valor corriente en miles de millones de USD)",
    y = "Superficie (en miles de Km2)",
    title = "Relacion entre tamaño del PIB y superficie"
  ) +
  theme_minimal()

#2. Hagan un analisis multivariado entre el gasto en servicios medicos por consulta externa y
#si el agua para beber y cocinar proviene principalmente de canheria de red dentro de la vivienda o no
#3. Hagan una matriz de correlacion y un mapa de calor entre la superficie, poblacion, PIB, esperanza de vida y consumo de energia electrica