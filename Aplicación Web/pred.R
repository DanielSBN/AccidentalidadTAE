# PRIMERA PARTE: Preprocesamiento de los datos

# Librerias
library(dplyr)
library(RcppBDT)
library(timeDate)
library(stringr)
library(hash)
library(caret)
library(ranger)

# Funciones utiles
dateString <- function(ano, mes, dia) {
  return(paste(ano, mes, dia, sep='-'))
}
dateFromString <- function(ano, mes, dia) {
  return(as.Date(dateString(ano, mes, dia)))
}

# Funciones para fechas especiales
quincena <- function(ano, mes, dia) {
  dias31 <- c(1, 3, 5, 7, 8, 10, 12)
  dias30 <- c(4, 6, 9, 11)
  sem <- weekdays(dateFromString(ano, mes, dia))
  return(if((sem == 'sábado' & dia == 14)
            | (sem != 'domingo' & dia == 15)
            | (mes %in% dias31 & ((sem == 'sábado' & dia == 30) | (sem != 'domingo' & dia == 31)))
            | (mes %in% dias30 & ((sem == 'sábado' & dia == 29) | (sem != 'domingo' & dia == 30)))
            | (mes == 2 & ano %% 4 == 0 & ((sem == 'sábado' & dia == 28) | (sem != 'domingo' & dia == 29)))
            | (mes == 2 & ano %% 4 != 0 & ((sem == 'sábado' & dia == 27) | (sem != 'domingo' & dia == 28))))
    1 else 0)
}
fechaEspecial <- function(esp, dia, mes, ano) {
  fecha <- switch(esp,
                  'padre'=getNthDayOfWeek(third, Sun, Jun, ano),
                  'madre'=getNthDayOfWeek(second, Sun, May, ano),
                  'ano_nuevo'=dateFromString(ano, '01', '01'),
                  'navidad'=dateFromString(ano, '12', '24'),
                  'trabajo'=dateFromString(ano, '05', '01'),
                  'amor_amistad'=getNthDayOfWeek(third, Sat, Sep, ano),
                  'mujer'=dateFromString(ano, '03', '08'),
                  'halloween'=dateFromString(ano, '10', '31'))
  if (dateString(ano, mes, dia) == fecha) {
    return(1)
  }
  return(0)
}
velitas <- function(dia, mes, ano) {
  return(if(dateFromString(ano, mes, dia) %in% c(dateFromString(ano, '12', '07'),
                                                 dateFromString(ano, '12', '08')))
    1 else 0)
}
semanaSanta <- function(dia, mes, ano) {
  fecha <- dateFromString(ano, mes, dia)
  pascua <- as.Date(Easter(ano))
  ss <- c(pascua, pascua - 1, pascua - 2, pascua - 3, pascua - 4, pascua - 5,
          pascua - 6, pascua - 7, pascua - 8, pascua - 9)
  return(if(fecha %in% ss) 1 else 0)
}

# Funcion para dia de la semana
diaSemana <- function(ano, mes, dia) {
  return(as.POSIXlt(dateFromString(ano, mes, dia))$wday)
}

# Funcion para pre-procesamiento
preprocesar <- function(datos, prediccion) {
  if (prediccion) {
    # Copia de los datos
    train <- data.frame(datos)
  } else {
    # Agrupamiento y limpieza por comunas
    datos %>%
      group_by(PERIODO, MES, DIA, NOMBRE_COM) %>%
      summarise(ACCIDENTES = n()) -> train
    colnames(train) <- c('ANO', 'MES', 'DIA', 'COMUNA', 'ACCIDENTES')
    train <- train[!(train$COMUNA %in% c('', ' ') | str_detect(train$COMUNA, 'Corregimiento')),]
  }
  
  # Aplicacion de fechas especiales
  train$QUINCENA <- mapply(quincena, train$ANO, train$MES, train$DIA)
  train$PADRE <- mapply(fechaEspecial, 'padre', train$DIA, train$MES, train$ANO)
  train$MADRE <- mapply(fechaEspecial, 'madre', train$DIA, train$MES, train$ANO)
  train$ANO_NUEVO <- mapply(fechaEspecial, 'ano_nuevo', train$DIA, train$MES, train$ANO)
  train$NAVIDAD <- mapply(fechaEspecial, 'navidad', train$DIA, train$MES, train$ANO)
  train$TRABAJO <- mapply(fechaEspecial, 'trabajo', train$DIA, train$MES, train$ANO)
  train$AMOR_AMISTAD <- mapply(fechaEspecial, 'amor_amistad', train$DIA, train$MES, train$ANO)
  train$MUJER <- mapply(fechaEspecial, 'mujer', train$DIA, train$MES, train$ANO)
  train$HALLOWEEN <- mapply(fechaEspecial, 'halloween', train$DIA, train$MES, train$ANO)
  train$VELITAS <- mapply(velitas, train$DIA, train$MES, train$ANO)
  train$SEMANA_SANTA <- mapply(semanaSanta, train$DIA, train$MES, train$ANO)
  train$DIA_SEMANA <- mapply(diaSemana, train$ANO, train$MES, train$DIA)
  
  return(train)
}

# Funciones para separar por comunas
sub_comuna <- function(train, comuna) {
  return(as.data.frame(subset(train[(train$COMUNA == comuna),], select=-c(ANO, COMUNA))))
}
comunas <- function(train) {
  datos <- hash()
  
  datos[['Aranjuez']] <- sub_comuna(train, 'Aranjuez')
  datos[['Belen']] <- sub_comuna(train, 'BelÃ©n')
  datos[['Buenos Aires']] <- sub_comuna(train, 'Buenos Aires')
  datos[['Castilla']] <- sub_comuna(train, 'Castilla')
  datos[['Doce de Octubre']] <- sub_comuna(train, 'Doce de Octubre')
  datos[['El Poblado']] <- sub_comuna(train, 'El Poblado')
  datos[['Guayabal']] <- sub_comuna(train, 'Guayabal')
  datos[['La America']] <- sub_comuna(train, 'La AmÃ©rica')
  datos[['La Candelaria']] <- sub_comuna(train, 'La Candelaria')
  datos[['Laureles Estadio']] <- sub_comuna(train, 'Laureles Estadio')
  datos[['Manrique']] <- sub_comuna(train, 'Manrique')
  datos[['Popular']] <- sub_comuna(train, 'Popular')
  datos[['Robledo']] <- sub_comuna(train, 'Robledo')
  datos[['San Javier']] <- sub_comuna(train, 'San Javier')
  datos[['Santa Cruz']] <- sub_comuna(train, 'Santa Cruz')
  datos[['Villa Hermosa']] <- sub_comuna(train, 'Villa Hermosa')
  
  return(datos)
}

# Funcion para entrenamiento y seleccion de modelos
modelo <- function(entrenamiento, validacion) {
  set.seed(123456789)
  
  grid <- expand.grid(
    ntrees = seq(10, 300, by=10)#,
    #mtry = seq(2, 5, by=1)
  )
  
  min_mse <- 500
  mejor_rf <- NULL
  
  for (i in 1:nrow(grid)) {
    rf <- ranger(
      formula = ACCIDENTES ~ .,
      data = entrenamiento,
      num.trees = grid$ntrees[i]#,
      #mtry = grid$mtry[i]
    )
    
    pred <- predict(rf, data=validacion)
    val_err <- with(validacion, mean((ACCIDENTES - pred$predictions)^2))
    if (val_err < min_mse) {
      min_mse <- val_err
      mejor_rf <- rf
    }
  }
  
  return(mejor_rf)
}

# Funcion para evaluar el desempeno de un modelo
errores <- function(modelo, entrenamiento, validacion) {
  # Error cuadratico medio en datos de entrenamiento
  train_pred <- predict(modelo, entrenamiento)
  train_err <- with(entrenamiento, mean((ACCIDENTES - train_pred$predictions)^2))
  
  # Error cuadratico medio en datos de validacion
  val_pred <- predict(modelo, validacion)
  val_err <- with(validacion, mean((ACCIDENTES - val_pred$predictions)^2))
  
  # Porcentaje de variacion entre errores de entrenamiento y validacion
  variacion <- (abs(val_err - train_err) / train_err) * 100
  
  return(c(train_err, val_err, variacion))
}

## Carga de datos
#tr <- read.csv('uni.csv', sep=';')
#val <- read.csv('validacion_uni.csv', sep=';')

## Pre-procesamiento
#entr <- preprocesar(tr, FALSE)
#valid <- preprocesar(val, FALSE)

## Separacion por comunas
#entrenamiento <- comunas(entr)
#validacion <- comunas(valid)

## Entrenamiento de modelos
#bosques <- hash()
#errs <- hash()
#for (comuna in keys(entrenamiento)) {
#  bosques[[comuna]] <- modelo(entrenamiento[[comuna]], validacion[[comuna]])
#  errs[[comuna]] <- errores(bosques[[comuna]], entrenamiento[[comuna]], validacion[[comuna]])
#}

## Guardado de los modelos en un archivo
#save(bosques, file='bosques.RData')

#fec <- F[1]
#dat <- preprocesar(fec, TRUE)
#aran <- bosques[['Aranjuez']]
#pred <- predict(aran, data=preprocesar(fec, TRUE))

