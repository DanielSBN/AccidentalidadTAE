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
preprocesar <- function(datos) {
  # Agrupamiento y limpieza por comunas
  datos %>%
    group_by(PERIODO, MES, DIA, NOMBRE_COM) %>%
    summarise(ACCIDENTES = n()) -> train
  colnames(train) <- c('ANO', 'MES', 'DIA', 'COMUNA', 'ACCIDENTES')
  train <- train[!(train$COMUNA %in% c('', ' ') | str_detect(train$COMUNA, 'Corregimiento')),]
  
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

# Funcion para entrenamiento y seleccion de modelos
modelo <- function(entrenamiento, validacion) {
  set.seed(123456789)
  
  grid <- expand.grid(
    ntrees = seq(10, 150, by=10),
    mtry = seq(4, 10, by=2),
    max_depth = seq(10, 100, 10)
  )
  
  min_mse <- 500
  mejor_rf <- NULL
  
  for (i in 1:nrow(grid)) {
    rf <- ranger(
      formula = ACCIDENTES ~ .,
      data = entrenamiento,
      num.trees = grid$ntrees[1],
      mtry = grid$mtry[1]
    )
    
    pred <- predict(rf, validacion)
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

# Carga de datos
tr <- read.csv('uni.csv', sep=';')
val <- read.csv('validacion_uni.csv', sep=';')

# Pre-procesamiento
train <- preprocesar(tr)
entrenamiento <- subset(train[(train$COMUNA == 'Doce de Octubre'),], select=-c(ANO, COMUNA))
valid <- preprocesar(val)
validacion <- subset(valid[(valid$COMUNA == 'Doce de Octubre'),], select=-c(ANO, COMUNA))

# Seleccion de hiper-parametros
bosque <- modelo(entrenamiento, validacion)
errs <- errores(bosque, entrenamiento, validacion)

# Separacion por comunas
# entrenamiento <- hash()
# entrenamiento[['Aranjuez']] <- subset(train[(train$COMUNA == 'Aranjuez'),], select=-c(ANO, COMUNA))
# entrenamiento[['Belen']] <- subset(train[(train$COMUNA == 'BelÃ©n'),], select=-c(ANO, COMUNA))
# entrenamiento[['Buenos Aires']] <- subset(train[(train$COMUNA == 'Buenos Aires'),], select=-c(ANO, COMUNA))
# entrenamiento[['Castilla']] <- subset(train[(train$COMUNA == 'Castilla'),], select=-c(ANO, COMUNA))
# entrenamiento[['Doce de Octubre']] <- subset(train[(train$COMUNA == 'Doce de Octubre'),], select=-c(ANO, COMUNA))
# entrenamiento[['El Poblado']] <- subset(train[(train$COMUNA == 'El Poblado'),], select=-c(ANO, COMUNA))
# entrenamiento[['Guayabal']] <- subset(train[(train$COMUNA == 'Guayabal'),], select=-c(ANO, COMUNA))
# entrenamiento[['La America']] <- subset(train[(train$COMUNA == 'La AmÃ©rica'),], select=-c(ANO, COMUNA))
# entrenamiento[['La Candelaria']] <- subset(train[(train$COMUNA == 'La Candelaria'),], select=-c(ANO, COMUNA))
# entrenamiento[['Laureles Estadio']] <- subset(train[(train$COMUNA == 'Laureles Estadio'),], select=-c(ANO, COMUNA))
# entrenamiento[['Manrique']] <- subset(train[(train$COMUNA == 'Manrique'),], select=-c(ANO, COMUNA))
# entrenamiento[['Popular']] <- subset(train[(train$COMUNA == 'Popular'),], select=-c(ANO, COMUNA))
# entrenamiento[['Robledo']] <- subset(train[(train$COMUNA == 'Robledo'),], select=-c(ANO, COMUNA))
# entrenamiento[['San Javier']] <- subset(train[(train$COMUNA == 'San Javier'),], select=-c(ANO, COMUNA))
# entrenamiento[['Santa Cruz']] <- subset(train[(train$COMUNA == 'Santa Cruz'),], select=-c(ANO, COMUNA))
# entrenamiento[['Villa Hermosa']] <- subset(train[(train$COMUNA == 'Villa Hermosa'),], select=-c(ANO, COMUNA))
