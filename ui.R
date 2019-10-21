library(shinydashboard)
library(tidyverse)
library(plotly)
library(flexdashboard)
library(shinyhelper)
#library(tmap,tmaptools)
library(leaflet)
load("datos/gauges.RData")
load("datos/promedio.RData")
load("datos/programas_dist.RData")
outlier<-function(variable){outlier <- ifelse(abs(variable) > 1.5*IQR(variable), NA, variable)
return(outlier)}
#e6f2ff
columnn_color <-"background-color:#cce6ff; border-style: solid; border-color: #e6f2ff;"


navbarPage(img(src = "fodesaf.jpg", witdh=70, height = 60), theme = shinythemes::shinytheme(theme = "readable") ,
           tabPanel("Ejecución",
              shinydashboard::dashboardPage(skin = 'blue',header = dashboardHeader(disable = T), sidebar = dashboardSidebar(collapsed = T),
                  body = dashboardBody(
                      
                               fluidRow(
                                 column(4,selectInput("programas", "Seleccione un programa", unique(gauges$programa), selected = "Banhvi")),
                                 column(4, selectInput("trimestre", label = "Seleccione un trimestre o semestre", choices = unique(gauges$Acumulado), selected = "IT")), 
                                 column(width = 1, radioButtons("ano", label = "Seleccione un año",choices = unique(gauges$Ano), selected = "2018")),
                                 column(width = 1,  div("Consultas o comentarios",a(icon("envelope", "fa-3x") ,  target="_blank", href =  "mailto:direccion.desaf@mtss.go.cr ")))
                                 ),
                               fluidRow( 
                                 column( style = columnn_color,
                                          gaugeOutput("gauge1", width = "100%", height = "auto")%>% helper(type = "markdown", content = "gauge2"),
                                          uiOutput("infobox_1", width =4 ), p(style="color:blue", "Fuente: Sistema de Indicadores Desaf"), width = 4
                                          #gaugeOutput("gauge2",width = "100%", height = "auto")
                                ),
                                 column(style = columnn_color,
                                          gaugeOutput("gauge3",width = "100%", height = "auto") %>% helper(type = "markdown",content = "gauge2"),
                                          uiOutput("infobox_2"), width = 4, p(style="color:#cce6ff", "v ") 
                                          #gaugeOutput("gauge4",width = "100%", height = "100px")
                                ),
                                 column(style = columnn_color,
                                          gaugeOutput("gauge5",width = "100%", height = "auto")%>% helper(type = "markdown",content = "gauge3"),
                                          uiOutput("infobox_3"), width = 4, p(style="color:#cce6ff", "v ")
                                          #gaugeOutput("gauge6",width = "100%", height = "auto")
                                          
                                   )
                               ),br(),
                              
                                   column(6, plotlyOutput(outputId = "promedio", height = 200),p(style="color:#4da6ff", "Fuente: Sistema de Indicadores Desaf "), br(), plotlyOutput("boxplot", height = 300)%>% helper(type = "markdown",content = "boxplot"), p(style="color:#4da6ff", "Fuente: Sinirube: 2018-2019")) ,
                               
                                   column(6, leafletOutput(outputId = "mapa1", height = 500)),
                               tags$head(tags$style(HTML('
                                

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
                                
                                /*ajuste*/
                                section.content { overflow-y: hidden; }
                                
                                .html-widget.gauge svg {
                                                  height: 300px;
                                                  width: 500px;
                                                  size: 10;
                                                  aling: center;
                                }
                                                
                                .shiny-output-error-validation {
                                                    color: green;
                                                    font-size: 30px;
                                }
                                                  
                                .box.box-solid.box-primary>.box-header {

                                }
                
                                .box.box-solid.box-primary{
                
                                background:#222d32
                                }

                                ')))
                                   
                               
                       
                       
                       
                   
               )
             
    )
  ),

tabPanel("Beneficiarios", 
         dashboardPage(header = dashboardHeader(disable = T), sidebar = dashboardSidebar(disable = T), 
                       body = dashboardBody(
                         
                         fluidRow(column(6, leafletOutput(outputId = "mapa", height = 500)),
                                  column(3),
                                  column(3))
                         
                         
                       ))
         
         
        
         
         
         
         )
)
