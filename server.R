library(shiny)
library(plotly)
library(RColorBrewer)
library(shinyhelper)
#library(ggwordcloud)
library(ggpubr)
library(shinyhelper)
library(sf)
load("datos/gauges.RData")
load("datos/promedio.RData")
load("datos/programas_dist.RData")
outlier<-function(variable){outlier <- ifelse(abs(variable) > 1.5*IQR(variable), NA, variable)
return(outlier)}
not_sinirube <- function(input) {
    if (input == 0) {
        "La informacion de este programa aun no se reporta al SINIRUBE"
    } else if (input == "") {
        FALSE
    } else {
        NULL
    }
}

colores <- brewer.pal(name = "Blues", n = 9)[c(3, 6, 9)]
decimales <- 0
style_info <- "font-size: 150%;"
fondo <-"white"

shinyServer(function(input, output, session) {
    
    observe_helpers(withMathJax = TRUE)

    output$gauge1<-renderGauge({
        gauge(gauges[gauges$programa == paste(input$programas) & str_detect(gauges$indicadores, "avance.+ficiarios") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]], abbreviateDecimals = 0,
              min = 0, 
              max = 100,
              sectors = gaugeSectors(danger = c(50, 100), 
                                     warning = c(30, 50),
                                     success = c(0, 30), colors = colores), label = '% Avance Beneficiarios')
    })
    
    output$infobox_1<-renderInfoBox({
        infoBox(tags$p("Beneficiarios atendidos", style = style_info), value = tags$p(round(gauges[gauges$programa == input$programas & str_detect(gauges$indicadores, "Benef") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]]%>% as.numeric(), decimales)%>% format(big.mark = " "), style = style_info), icon = icon("user"),fill = TRUE,color = "yellow")
    })
    
    output$gauge3<-renderGauge({
        gauge(gauges[gauges$programa == paste(input$programas) & str_detect(gauges$indicadores, "avance.+cursos") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]], 
              min = 0, 
              max = 100, 
              sectors = gaugeSectors(danger = c(50, 100), 
                                     warning = c(30, 50),
                                     success = c(0, 30), colors = colores),label = '% Avance Recursos')
    })
    
    output$infobox_2<-renderInfoBox({
        infoBox(tags$p("Recursos girados (millones)", style = style_info), tags$p(round(gauges[gauges$programa == input$programas & str_detect(gauges$indicadores, "girados") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]]%>% as.numeric()/1000000, decimales)%>% format(big.mark = " "), style = style_info), icon = shiny::icon("hand-holding-usd"),color = "fuchsia",width = 4,fill = TRUE)
    })
    
    output$gauge5<-renderGauge({
        gauge(gauges[gauges$programa == paste(input$programas) & str_detect(gauges$indicadores, "Cobert") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]], 
              min = 0, 
              max = 100, 
              sectors = gaugeSectors(danger = c(50, 100), 
                                     warning = c(30, 50),
                                     success = c(0, 30), colors = colores),label = 'Cobertura Efectiva')
    })
    
    output$infobox_3<-renderInfoBox({
        #validate(not_sinirube(programas_dist %>% filter(programa_ind == input$programas) %>% nrow() != 0))
        
        
        infoBox(
            tags$p("Población objetivo", style = style_info), value = tags$p(round(gauges[gauges$programa == input$programas & str_detect(gauges$indicadores, "Poblac") & gauges$Acumulado == input$trimestre & gauges$Ano == input$ano, "valor"][[1,]]%>% as.numeric(), decimales) %>% format(big.mark = " "), style = style_info), icon = icon("user-tag"), color = "green", fill = TRUE)
    })
    
   output$promedio <-renderPlotly({
       promedio <- promedio[promedio$programa == input$programas & promedio$Acumulado == "ANUAL",]
       # ay <- list(
       #     tickfont = list(color = "red"),
       #     overlaying = "y",
       #     side = "right",
       #     title = " Por obra"
       # )
       
       
       plot_ly(x = promedio$Ano)  %>%
           add_lines(y = promedio$`Gasto efectivo por beneficiario (Promedio)`, name = "Gasto por beneficiario", showlegend = T ) %>% layout(title = "Evolución del beneficio promedio anual (millones de colones)",  xaxis = list(title="Año")) %>% layout(plot_bgcolor=fondo) %>% 
           layout(paper_bgcolor=fondo)
           # add_lines(y = promedio$`Gasto efectivo por obra (Promedio)`,  yaxis = "y2"  ) %>% 
           # layout(
           #     title = "Double Y Axis", yaxis2 = ay,
           #     xaxis = list(title="x")
           # )
       
   })
   filteredData <- reactive({
     validate(not_sinirube(programas_dist %>% filter(programa_ind == input$programas & programas_dist$año == input$ano) %>% nrow() != 0))
     programas_dist[programas_dist$programa_ind == input$programas & programas_dist$año == input$ano & str_detect(programas_dist$componente, "Prev"), ]%>% st_transform(crs = "+init=epsg:4326") %>% filter(!is.na(monto))
   
     })
   output$mapa <- renderLeaflet( {  
       validate(not_sinirube(programas_dist %>% filter(programa_ind == input$programas & programas_dist$año == input$ano) %>% nrow() != 0))
       
      
     leaflet() %>%
       addTiles() %>% 
       setView(lat = 	9.934739, lng = -84.087502 ,zoom = 8)

   
    
        
   })
   
   observe({
     validate(not_sinirube(programas_dist %>% filter(programa_ind == input$programas) %>% nrow() != 0))
     mpal <- colorNumeric(palette = "Blues", domain = programas_dist[programas_dist$programa_ind == input$programas & programas_dist$año == input$ano & str_detect(programas_dist$componente, "Prev"),][["monto"]])
     
     leafletProxy("mapa", data = filteredData()) %>%
       # clearShapes()%>%
       addPolygons(popup = ~ Distrito, fillColor = ~mpal(monto),
                   fillOpacity = 0.8, stroke = FALSE)%>%
       addLegend("bottomright", pal = mpal, values = ~monto,title = "Beneficiarios",opacity = 0.8 ) 
   })
   
   
   
   output$boxplot <- renderPlotly({
       validate(not_sinirube(programas_dist %>% filter(programa_ind == input$programas & programas_dist$año == input$ano) %>% nrow() != 0))

       plot_ly(programas_dist %>% as.tibble()  %>% filter(programa_ind == input$programas & programas_dist$año == input$ano & !is.na(presencia) ) , y = ~componente, x = ~outlier(monto), color = ~factor(presencia), type = "box", ymax = 100) %>% layout(boxmode = "group", title = "Asociacion entre inversión y componentes de Puente",  xaxis = list(title="monto invertido por distrito (millones de colones)")) %>% layout(plot_bgcolor=fondo) %>% 
           layout(paper_bgcolor=fondo)

       })

})
