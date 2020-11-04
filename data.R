# PRIMERA PARTE: Preprocesamiento de los datos

# Librerias
library(dplyr)
library(RcppBDT)
library(timeDate)

# Funciones utiles
arreglar <- function(num) {
  return(as.numeric(gsub(',', '.', num)))
}
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
                  'halloween'=dateFromString(ano, '10', '31'),
                  'velitas_7'=dateFromString(ano, '12', '07'),
                  'velitas_8'=dateFromString(ano, '12', '08'))
  if (dateString(ano, mes, dia) == fecha) {
    return(1)
  }
  return(0)
}
semanaSanta <- function(dia, mes, ano) {
  fecha <- dateFromString(ano, mes, dia)
  pascua <- as.Date(Easter(ano))
  ss <- c(pascua, pascua - 1, pascua - 2, pascua - 3, pascua - 4, pascua - 5,
          pascua - 6, pascua - 7, pascua - 8, pascua - 9)
  return(if(fecha %in% ss) 1 else 0)
}

# Carga de datos y arreglo de numeros
datos <- read.csv('uni.csv', sep=';')
datos$DIA <- lapply(datos$DIA, arreglar)
datos$PERIODO <- lapply(datos$PERIODO, arreglar)
datos$MES <- lapply(datos$MES, arreglar)

# Agrupamiento y limpieza por comunas
datos %>%
  group_by(PERIODO, MES, DIA, NOMBRE_COM) %>%
  summarise(ACCIDENTES = n()) -> train
colnames(train) <- c('ANO', 'MES', 'DIA', 'COMUNA', 'ACCIDENTES')
train <- train[!(train$COMUNA %in% c('', ' ') | str_detect(train$COMUNA, 'Corregimiento')),]


