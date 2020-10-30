datos2014 <- read.csv('Incidentes_georreferenciados_2014.csv')
datos2015 <- read.csv('Incidentes_georreferenciados_2015.csv')
datos2016 <- read.csv('Incidentes_georreferenciados__2016.csv')
datos2017 <- read.csv('Incidentes_georreferenciados_2017.csv')
datos2018 <- read.csv('Incidentes_georreferenciados_2018.csv')

tdatos <- rbind(datos2014, datos2015, datos2016, datos2017)
train <- data.frame(tdatos$DIA, tdatos$PERIODO, tdatos$CLASE, tdatos$GRAVEDAD,
                    tdatos$COMUNA, tdatos$DISENO, tdatos$DIA_NOMBRE, tdatos$MES)
names(train) <- c("DIA", "PERIODO", "CLASE", "GRAVEDAD", "COMUNA", "DISENO",
                  "DIA_NOMBRE", "MES")

library(RcppBDT)
dateString <- function(ano, mes, dia) {
  return(as.Date(paste(ano, mes, dia, sep='-')))
}

train$FECHA <- mapply(dateString, train$DIA, train$MES, train$PERIODO)

fechaEspecial <- function(esp, dia, mes, ano) {
  fecha <- switch(esp,
                  'padre'=getNthDayOfWeek(third, Sun, Jun, ano),
                  'madre'=getNthDayOfWeek(second, Sun, May, ano),
                  'ano_nuevo'=dateString(ano, '01', '01'),
                  'navidad'=dateString(ano, '12', '24'),
                  'trabajo'=dateString(ano, '05', '01'),
                  'amor_amistad'=getNthDayOfWeek(third, Sat, Sep, ano),
                  'mujer'=dateString(ano, '03', '08'),
                  'halloween'=dateString(ano, '10', '31'),
                  'velitas_7'=dateString(ano, '12', '07'),
                  'velitas_8'=dateString(ano, '12', '08'))
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
