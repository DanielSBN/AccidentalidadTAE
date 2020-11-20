#TRABAJO TAE
library(shiny)
library(hash)
library(caret)
library(ranger)
library(dplyr)
shinyUI(pageWithSidebar(
  
  
    headerPanel("Accidentalidad en Medellin"),
    sidebarPanel(
      
      
      conditionalPanel(condition="input.tabselected==1",
                       h4("Haga click en el siguiente enlace para ver el mapa interactivo"),a(href="https://arcg.is/LOvKm","Ver mapa")
                       
                       ),
      conditionalPanel(condition="input.tabselected==2",
                      
                       dateInput(
                         "Datosfecha_inicial","FECHA INICAL",value = NULL,min = "2014-01-01",max = "2017-12-31",
                         format = "yyyy-mm-dd",startview = "month",weekstart = 0,language = "en",width = NULL,
                         autoclose = TRUE,datesdisabled = NULL,daysofweekdisabled = NULL),
                       
                       dateInput(
                         "Datosfecha_final","FECHA FINAL",value = NULL,min = "2014-01-01",max = "2017-12-31",
                         format = "yyyy-mm-dd",startview = "month",weekstart = 0,language = "en",width = NULL,
                         autoclose = TRUE,datesdisabled = NULL,daysofweekdisabled = NULL),
                      

           ),
      
      conditionalPanel(condition="input.tabselected==3",
                       selectInput("var", label="Seleccione la Columa en la que quiera saber cuantos accidente hubo", 
                                   choices=c("Aranjuez"=1, "Belen"=2, "Buenos Aires"=3, "Castilla"=4,
                                             "Doce de Octubre"=5,"El Poblado"=6,"Guayabal"=7,"La America"=8,
                                             "La Candelaria"=9,"Laureles Estadio"=10,"Manrique"=11,"Popular"=12,
                                             "Robledo"=13,"San Javier"=14,"Santa Cruz"=15,"Villa Hermosa"=16), 
                                   selected=NULL),
                       dateInput(
                         "Datosfecha_inicial2","FECHA INICAL",value = NULL,min = "2019-01-01",max = "2025-12-31",
                         format = "yyyy-mm-dd",startview = "month",weekstart = 0,language = "en",width = NULL,
                         autoclose = TRUE,datesdisabled = NULL,daysofweekdisabled = NULL),
                       
                       dateInput(
                         "Datosfecha_final2","FECHA FINAL",value = NULL,min = "2019-01-01",max = "2025-12-31",
                         format = "yyyy-mm-dd",startview = "month",weekstart = 0,language = "en",width = NULL,
                         autoclose = TRUE,datesdisabled = NULL,daysofweekdisabled = NULL),
                       
                       radioButtons("Agrupamiento","Seleccione la agrupacion",list("Diario","Semanal","Mensual"),"")
                       
                       ),
                       
                       
            
      
      
      ),
      
    
  
  
    
    
    
    
    
    
    mainPanel(
      
      tabsetPanel(
        
            tabPanel("Segmentacion", value=1, 
                     tags$img(src="IMAGEN_TAE.png",width="860px",height="250px"),
                     h3("Vista previa del mapa interactivo"),
                     tags$img(src="IMAGEN2_TAE.png",width="500px",height="250px")
                     ),
            tabPanel("Visualizacion", value=2, 
                      tableOutput("data")
                     ),
            tabPanel("Prediccion", value=3,
                     textOutput("comuna"),
                     tableOutput("data2")
            
                     ),
            id = "tabselected"
        )
      
      

    )
  )
)
