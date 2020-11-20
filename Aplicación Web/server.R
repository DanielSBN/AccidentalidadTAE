#TRABAJO TAE
library(shiny)
library(hash)
library(caret)
library(ranger)
library(dplyr)
source("codigoDANIEL.R")
load("bosques.RData")
shinyServer(function(input, output) {
  
  
  
  
  output$comuna <- renderText({
    Numero_comuna <-input$var
    if(Numero_comuna==1) {
      Nombre_comuna="Aranjuez"
    } else if (Numero_comuna==2) {
      Nombre_comuna="Belen"
    } else if (Numero_comuna==3) {
      Nombre_comuna="Buenos Aires"
    }else if (Numero_comuna==4) {
      Nombre_comuna="Castilla"
    }else if (Numero_comuna==5) {
      Nombre_comuna="Doce de Octubre"
    }else if (Numero_comuna==6) {
      Nombre_comuna="El Poblado"
    }else if (Numero_comuna==7) {
      Nombre_comuna="Guayabal"
    }else if (Numero_comuna==8) {
      Nombre_comuna="La America"
    }else if (Numero_comuna==9) {
      Nombre_comuna="La Candelaria"
    }else if (Numero_comuna==10) {
      Nombre_comuna="Laureles Estadio"
    }else if (Numero_comuna==11) {
      Nombre_comuna="Manrique"
    }else if (Numero_comuna==12) {
      Nombre_comuna="Popular"
    }else if (Numero_comuna==13) {
      Nombre_comuna="Robledo"
    }else if (Numero_comuna==14) {
      Nombre_comuna="San Javier"
    }else if (Numero_comuna==15) {
      Nombre_comuna="Santa Cruz"
    }else {
      Nombre_comuna="Villa Hermosa"
    }
      paste("La comuna seleccionada es:  ", Nombre_comuna)
      
    })
   
  output$data <- renderTable({

    datos<- read.csv(file="FECHITAS.csv",header=T, sep=";", dec=",")
    fechainicial <-input$Datosfecha_inicial
    fechafinal <-input$Datosfecha_final
    indice_inicial<-as.numeric(fechainicial-as.Date("2014-01-01"))+1
    indice_final<-as.numeric(fechafinal-as.Date("2014-01-01"))+1
    Mnueva<-datos[indice_inicial:indice_final,1:17]
    Mnueva
  })
  
  
  
  output$data2 <- renderTable(rownames = FALSE, colnames = TRUE,{
    
    Numero_comuna <-input$var
    if(Numero_comuna==1) {
      Nombre_comuna="Aranjuez"
    } else if (Numero_comuna==2) {
      Nombre_comuna="Belen"
    } else if (Numero_comuna==3) {
      Nombre_comuna="Buenos Aires"
    }else if (Numero_comuna==4) {
      Nombre_comuna="Castilla"
    }else if (Numero_comuna==5) {
      Nombre_comuna="Doce de Octubre"
    }else if (Numero_comuna==6) {
      Nombre_comuna="El Poblado"
    }else if (Numero_comuna==7) {
      Nombre_comuna="Guayabal"
    }else if (Numero_comuna==8) {
      Nombre_comuna="La America"
    }else if (Numero_comuna==9) {
      Nombre_comuna="La Candelaria"
    }else if (Numero_comuna==10) {
      Nombre_comuna="Laureles Estadio"
    }else if (Numero_comuna==11) {
      Nombre_comuna="Manrique"
    }else if (Numero_comuna==12) {
      Nombre_comuna="Popular"
    }else if (Numero_comuna==13) {
      Nombre_comuna="Robledo"
    }else if (Numero_comuna==14) {
      Nombre_comuna="San Javier"
    }else if (Numero_comuna==15) {
      Nombre_comuna="Santa Cruz"
    }else {
      Nombre_comuna="Villa Hermosa"
    }
    
    
    
    datos<- read.csv(file="FECHITAS.csv",header=T, sep=";", dec=",")
    fechainicial2 <-input$Datosfecha_inicial2
    fechafinal2 <-input$Datosfecha_final2
    indice_inicial2<-as.numeric(fechainicial2-as.Date("2014-01-01")+1)
    indice_final2<-as.numeric(fechafinal2-as.Date("2014-01-01")+1)
    
    
    rango <- seq(fechainicial2, fechafinal2, by='days')
    
    an <- mapply(format, rango, format='%Y')
    me <- mapply(format, rango, format='%m')
    di <- mapply(format, rango, format='%d')
    anos <- as.integer(an)
    meses <- as.integer(me)
    dias <- as.integer(di)
    
    

    
    fechas <- data.frame(ANO=anos, MES=meses, DIA=dias)
    dat <- preprocesar(fechas, TRUE)
    aran <- bosques[[Nombre_comuna]]
    pred <- predict(aran, data=dat)
    
    
    Agrupacion <-input$Agrupamiento
    PRE<-mapply(round, pred$predictions, 0)
    fechas$ACCIDENTES <- as.integer(PRE)
    
    
    if(Agrupacion=='Diario'){
      fechas
    }else if(Agrupacion=='Mensual'){
      
      fechas %>%
        group_by(ANO, MES) %>%
        summarise(ACCIDENTES_MENSUALES = sum(ACCIDENTES)) -> mensual
    }else if(Agrupacion=='Semanal'){
      fechasc <- mapply(dateString, fechas$ANO, fechas$MES, fechas$DIA)
      semanas <- mapply(strftime,fechasc,format="%V")
      fechas$SEMANA <- as.integer(semanas)
      fechas %>%
        group_by(ANO, SEMANA) %>%
      summarise(ACCIDENTES_SEMANALES = sum(ACCIDENTES)) -> semanal
      
    }
    
  })
  
})