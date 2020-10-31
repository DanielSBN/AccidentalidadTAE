# PRIMERA PARTE: Preprocesamiento de los datos

# Carga de datos
datos2014 <- read.csv('Incidentes_georreferenciados_2014.csv')
datos2015 <- read.csv('Incidentes_georreferenciados_2015.csv')
datos2016 <- read.csv('Incidentes_georreferenciados__2016.csv')
datos2017 <- read.csv('Incidentes_georreferenciados_2017.csv')
datos2018 <- read.csv('Incidentes_georreferenciados_2018.csv')

# Juntamos datos de 2014 a 2017
tdatos <- rbind(datos2014, datos2015, datos2016, datos2017)

# Funciones utiles
dateString <- function(ano, mes, dia) {
  return(paste(ano, mes, dia, sep='-'))
}
dateFromString <- function(ano, mes, dia) {
  return(as.Date(dateString(ano, mes, dia)))
}

# Filtrando con expresiones regulares
library(tidyverse)

filtroClase <- function(clase) {
  if (str_detect(clase, regex('Ca.*da[:space:](de[:space:])?Ocupante'))) {
    return('CaidaOcupante')
  } else if (str_detect(clase, regex('Choque[:space:]?$'))) {
    return('Choque')
  } else {
    return(clase)
  }
}
tdatos$CLASE <- lapply(tdatos$CLASE, filtroClase)

# Agrupamiento por tipo de accidente
library(dplyr)
tdatos %>%
  group_by(PERIODO, MES, DIA, COMUNA, BARRIO) %>%
  summarise(ACCIDENTES = n()) -> train
tdatos$CLASE

comunas <- c('Popular', 'Santa Cruz', 'Manrique', 'Aranjuez', 'Castilla', 'Doce de Octubre',
             'Robledo', 'Villa Hermosa', 'Buenos Aires', 'La Candelaria', 'Laureles Estadio',
             'La AmÃ©rica', 'San Javier', 'El Poblado', 'Guayabal', 'BelÃ©n')
train2 <- transform(train,
                    COMUNA = ifelse(COMUNA %in% comunas | str_detect(COMUNA, regex('Corregimiento.*')),
                                    COMUNA, BARRIO),
                    BARRIO = ifelse(COMUNA %in% comunas | str_detect(COMUNA, regex('Corregimiento.*')),
                                    BARRIO, COMUNA))

# Funcion para verificar quincena
quincena <- function(ano, mes, dia) {
  dias31 <- c(1, 3, 5, 7, 8, 10, 12)
  dias30 <- c(4, 6, 9, 11)
  sem <- weekdays(dateFromString(ano, mes, dia))
  if ((sem == 'sábado' & dia == 14) | (sem != 'domingo' & dia == 15)) {
    return(1)
  } else if (mes %in% dias31 & ((sem == 'sábado' & dia == 30) | (sem != 'domingo' & dia == 31))) {
    return(1)
  } else if (mes %in% dias30 & ((sem == 'sábado' & dia == 29) | (sem != 'domingo' & dia == 30))) {
    return(1)
  } else if (mes == 2 & ano %% 4 == 0 & ((sem == 'sábado' & dia == 28) | (sem != 'domingo' & dia == 29))) {
    return(1)
  } else if (mes == 2 & ano %% 4 != 0 & ((sem == 'sábado' & dia == 27) | (sem != 'domingo' & dia == 28))) {
    return(1)
  } else {
    return(0)
  }
}

train$QUINCENA <- mapply(quincena, train$PERIODO, train$MES, train$DIA)

train %>%
  group_by(COMUNA) %>%
  summarise(n = n()) -> test

library(RcppBDT)
fechaEspecial <- function(esp, dia, mes, ano) {
  fecha <- switch(esp,
                  'padre'=getNthDayOfWeek(third, Sun, Jun, ano),
                  'madre'=getNthDayOfWeek(second, Sun, May, ano),
                  'ano_nuevo'=dateFromString(ano, '01', '01'),
                  'navidad'=dateFromString(ano, '12', '24'),
                  'trabajo'=dateFromString(ano, '05', '01'),
                  'amor_amistad'=getNthDayOfWeek(third, Sat, Sep, ano),
                  'mujer'=dateFromString(ano, '03', '08'),
                  'halloween'=dateFromString(ano, '10', '31'),
                  'velitas_7'=dateFromString(ano, '12', '07'),
                  'velitas_8'=dateFromString(ano, '12', '08'))
  if (dateString(ano, mes, dia) == fecha) {
    return(1)
  }
  return(0)
}
fechasEspeciales = c('padre', 'madre', 'ano_nuevo', 'navidad', 'trabajo',
                     'amor_amistad', 'mujer', 'halloween', 'velitas_7',
                     'velitas_8')
train$ANO_NUEVO <- mapply(fechaEspecial, 'ano_nuevo', train$DIA, train$MES, train$PERIODO)
write.csv(train, 'train.csv')
