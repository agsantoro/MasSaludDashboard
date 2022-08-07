library(leaflet)
library(sf)
library(stringr)
library(dplyr)
library(rgdal)
library(tmap)
library(readxl)
library(RColorBrewer)
library(shiny)
library(shinyWidgets)
library(plotly)
library(rgeos)
library(shinyjs)
library(flexdashboard)
library(maptools)
library(gridExtra)
library(generator)
library(shinyBS)
library(lubridate)
library(DT)
library(shinydashboard)
library(shinythemes)


# source functions
source('functions/functions.R', encoding = "UTF-8")

# load data
load('shp/radios/radios.RData')
load('shp/centros-de-salud/cesacs.RData')
load("data/context/comuna7_hist.RData")
load("data/context/hospitales.RData")
load("data/people/pacientes.RData")



ui <- bootstrapPage(
  titlePanel(windowTitle = "+APS Dashboard", title = ""),
  theme = shinytheme("simplex"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  tags$style("
        #AP {
          background-color: #ddd;
          opacity: 0.7;
        }
        "),
  tags$style("
        #AP2 {
          background-color: #deebf7;
          opacity: 0.7;
        }
        "),
  tags$style("
        #AP3 {
          background-color: #e0f3db;
          opacity: 0.85;
        }
        "),
  tags$style("
        #AP6 {
          
          opacity: 0.9;
        }
        "),
  tags$style("
        #FP {
          background-color: #f0f0f0;
          opacity: 0.8;
        }
        "),


  

  fixedPanel(id="AP",
    top = 20, left = 15, bottom= 30, draggable = F, width = "20%", style = "z-index:500; min-width: 300px;",
    column(12,
           fluidRow(
             br(),
             br(),
             column(6,
                    br(),
                    img(src='IECS_20a-OK-esp.png', align = "center", height="100%", width="100%"),
                    align="center"),
             column(6,
                    img(src='CIPS_fondo-transparente.png', align = "center", height="75%", width="75%"),
                    align="center")
           ),
           tags$h3(strong("+APS")),
           h4("Primary Care Dashboard"),
           h5("Comuna 7 - Ciudad Autónoma de Buenos Aires"),
           br(),
           tabsetPanel(id="TSP", type="pills",
                       tabPanel("Contextual Data",
                                br(),
                                br(),
                                pickerInput("selectVariable",
                                            "Select factor",
                                            choices = c("Deprivation factor" = "Deprivation factor",
                                                        "Unemployment rate" = "Desocupación",
                                                        "Illiteracy" = "Analfabetismo",
                                                        "Population under 15 years" = "Jovenes",
                                                        "Inadequate access to services" = "Acceso a servicios insuficiente",
                                                        "Insufficient housing construction quality" = "Calidad constructiva insuficiente",
                                                        "Inadequate housing" = "Vivienda inadecuada"
                                                        ))),
                                            #choices=rev(colnames(radios@data)[18:24]))),
                       tabPanel("Catchment Area"),
                       tabPanel("Primary Care Center Info"),
                       tabPanel("Patients"))
           )
    ,
    br()
  ),
  uiOutput("AP2"),
  uiOutput("AP3"),
  uiOutput("AP4"),
  uiOutput("AP6"),
  uiOutput("AP8"),
  uiOutput("AP9"),
  uiOutput("AP10"),
  uiOutput("AP11"),
  uiOutput("AP12"),
  fixedPanel(id= "FP",
             top=20,
             left=50,
             right=50,
             bottom=80,
    fluidPage(
      fluidRow(
        column(6,
               br(),
               uiOutput("AP7"),
               br(),
               actionButton("show_indicators", "Show indicators"),
               br(),
               br(),
               actionButton("show_patients", "Show patients")
               
        ),
        column(3),
        column(3, 
               br(),
               div(style = "border-style: solid; border-color: #bdbdbd;",
                   leafletOutput("mini_map", width = "100%",height = "125px")),
               br(),
               align="center"
        )
        
      )
    )
  ),
    
    fixedPanel(id= "IND",
               top=20,
               left=50,
               right=50,
               bottom=80,
               tabsetPanel(type = "pills",
                 tabPanel("Health Indicators",
                                    fluidPage(
                                      br(),
                                      br(),
                                      h4(strong("Selected Key Performance indicators: Clinical (%)")),
                                      h5("Last year"),
                                      br(),
                                      
                                      fluidRow(tags$div(id="add"))
                                    )
                           ),
                  tabPanel("Management",
                                fluidPage(
                                br(),
                                br(),
                                h4(strong("Selected Key Performance indicators: Management")),
                                h5("Last year"),
                                br(),
                                fluidRow(tags$div(id="add2")))
                           )
               )
  ),
  fixedPanel(id= "PAT",
             top=20,
             left=50,
             right=50,
             bottom=80,
             fluidPage(
               fluidRow(
                 DT::dataTableOutput("patients_records")
             )
             
      )
  )

    

)

server <- function (input,output,session) {
  shinyjs::hideElement("AP2") 
  shinyjs::hideElement("AP6")
  shinyjs::hideElement("FP")
  shinyjs::hideElement("AP9")
  shinyjs::hideElement("IND")
  shinyjs::hideElement("AP10")
  shinyjs::hideElement("AP11")
  shinyjs::hideElement("PAT")
  
  observeEvent(input$back, {
    shinyjs::hideElement("FP")
    shinyjs::hideElement("IND")
    shinyjs::hide("back")
    shinyjs::show("map", animType = "fade",10)
    shinyjs::showElement("AP", animType = "fade",10)
    shinyjs::show("show_hosp", animType = "fade",10) 
  })
  
  output$AP7 <- renderText({
    getCenterReport("",input,output,cesacs)
  })
  
  output$map <- renderLeaflet({
    getMapa(radios,
            cesacs,
            input$selectVariable,
            hospitales,
            pacientes)

  })
  proxy <- leafletProxy("map")
  selected <- reactiveValues(groups = vector())
  
  observeEvent(input$map_shape_click, {
    if (input$TSP=="Catchment Area") {
      if(input$map_shape_click$group == "radios"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        centroid <- gCentroid(subset(radios,radios@data$LINK==input$map_shape_click$id),byid=TRUE)
        proxy %>% showGroup(group = as.character(input$map_shape_click$id)) %>%
                  setView(zoom = 14.5,
                          lat=centroid@coords[1,2], 
                          lng=centroid@coords[1,1])
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
    }

  })
  
  observeEvent(input$TSP, {
    if (input$TSP=="Contextual Data") {
      proxy %>% hideGroup(group = radios@data$LINK) %>% hideGroup(group = "pacientes") %>% 
        setView(zoom = 13.50,
                lat=-34.634610, 
                lng=-58.442854 )

      output$AP2 <- renderUI({
        getAbsolutePanel("AP2")
      })
      shinyjs::showElement("AP2", animType = "fade",10) 
      shinyjs::hideElement("AP3", animType = "fade",10) 
      shinyjs::hideElement("IND", animType = "fade",10) 
      shinyjs::hideElement("AP3", animType = "fade",10) 
      shinyjs::showElement("AP4", animType = "fade",10) 
      shinyjs::show("show_hosp", animType = "fade",10) 
    } 
    
    if (input$TSP=="Catchment Area")  {
      selected$groups <- c()
        
      proxy %>% setView(zoom = 13.50,
                        lat=-34.634610, 
                        lng=-58.442854 ) %>% hideGroup(group = "pacientes")
      shinyjs::hide("AP2", animType = "fade",10)
      shinyjs::showElement("AP6", animType = "fade",4)
      Sys.sleep(.8)
      shinyjs::hideElement("AP6")
      shinyjs::showElement("AP6", animType = "fade",4)
      Sys.sleep(.8)
      shinyjs::hideElement("AP6")
      shinyjs::showElement("AP6", animType = "fade",4)
      Sys.sleep(.8)
      shinyjs::hideElement("AP6")
      shinyjs::showElement("AP6", animType = "fade",4)
      Sys.sleep(.8)
      shinyjs::hideElement("AP6")
      
      
      output$AP3 <- renderUI({
        getAbsolutePanel("AP3")
      })
      shinyjs::hideElement("AP2", animType = "fade",10) 
      shinyjs::showElement("AP3", animType = "fade",10)
      shinyjs::showElement("AP4", animType = "fade",10)
      shinyjs::show("show_hosp", animType = "fade",10)
    }
    if (input$TSP=="Primary Care Center Info")  {
      shinyjs::hide("show_hosp", animType = "fade",10) 
      proxy %>% hideGroup(group = "dots2") %>% hideGroup(group = "pacientes") %>% hideGroup(group = radios@data$LINK) %>% 
        setView(zoom = 13.50,
                lat=-34.634610, 
                lng=-58.442854 )
      shinyjs::hide("AP2", animType = "fade",10)
      shinyjs::hide("AP3", animType = "fade",10)
      shinyjs::hideElement("FP", animType = "fade",10)
      shinyjs::showElement("AP4", animType = "fade",10)
      shinyjs::hide("show_hosp", animType = "fade",10)
    }
    if (input$TSP=="Patients")  {
      shinyjs::show("show_hosp", animType = "fade",10) 
      shinyjs::show("show_cesacs", animType = "fade",10)
      proxy %>% showGroup(group = "pacientes") %>% showGroup(group = "dots2") %>% hideGroup(group = radios@data$LINK) %>% 
        setView(zoom = 13.50,
                lat=-34.634610, 
                lng=-58.442854 )
      shinyjs::hide("AP2", animType = "fade",10)
      shinyjs::hide("AP3", animType = "fade",10)
      shinyjs::hideElement("FP", animType = "fade",10)
      
    }
    
  })
  
  observeEvent(input$map_marker_click, {
    if (input$TSP=="Primary Care Center Info") {
      shinyjs::hide("AP", animType = "fade",10)
      shinyjs::hide("AP2", animType = "fade",10)
      shinyjs::hide("AP3", animType = "fade",10)
      shinyjs::hide("AP4", animType = "fade",10)
      shinyjs::hide("AP5", animType = "fade",10)
      shinyjs::hide("map", animType = "fade",10)
      shinyjs::show("FP", animType = "fade",10)
      shinyjs::show("AP9", animType = "fade",10)
      shinyjs::show("back", animType = "fade",10)
      
    }
    
    if (input$TSP=="Patients") {
      record <- pacientes[paste(pacientes$first_name,pacientes$last_name)==input$map_marker_click$id,]
      color_si <- "#de2d26"
      color_no <- "#31a354"
      glu <- if (max(record$`Glucemia elevada`)==0) {"NO"} else {"Sí"}
      obe <- if (max(record$Obesidad)==0) {"NO"} else {"Sí"}
      tab <- if (max(record$Tabaquismo)==0) {"NO"} else {"Sí"}
      dlp <- if (max(record$Dislipidemia)==0) {"NO"} else {"Sí"}
      hta <- if (max(record$`Hipertensión Arterial`)==0) {"NO"} else {"Sí"}
      sed <- if (max(record$Sedentarismo)==0) {"NO"} else {"Sí"}
      alc <- if (max(record$`Abuso de alcohol`)==0) {"NO"} else {"Sí"}
      
      showModal(modalDialog(
        title = HTML(paste0('<p style="text-align:right"><span style="font-family:Lucida Sans Unicode, Lucida Grande, sans-serif">Name:<strong>', 
                            paste(record$first_name,record$last_name)[1],
                            '</strong></span></p> ',
                            '<p style="text-align:right"><span style="font-family:Lucida Sans Unicode, Lucida Grande, sans-serif">Age:<strong>', 
                            paste(round(record$age,digits=0),"years")[1],
                            '</strong></span></p> ')),
        HTML(paste0(
          '<p style="text-align:center"><span style="font-family:Georgia,serif"><strong>Risk conditions</strong></span></p>
           <p style="text-align:center">Elevated blood glucose: ',glu,'</p>
           <p style="text-align:center">Obesity: ',obe,'</p>
           <p style="text-align:center">Smoking: ',tab,'</p>
           <p style="text-align:center">Dislipidemia: ',dlp,'</p>
           <p style="text-align:center">Arterial hypertension: ',hta,'</p>
           <p style="text-align:center">Sedentary lifestyle: ',sed,'</p>
           <p style="text-align:center">Alcohol abuse: ',alc,'</p>
           <p>&nbsp;</p>
           <p>&nbsp;</p>
           <p>&nbsp;</p>
           <p>&nbsp;</p>
           <p>&nbsp;</p>')
        ),
        div(style = "border-style: solid; border-color: #bdbdbd;",
            leafletOutput("mini_map2", width = "100%",height = "125px")),
        size = "m", easyClose = T, footer = NULL
      ))
    }
      
    })
    
  
  
  output$plot1 <- renderPlotly({
    getPlot("Nacidos_vivos",comuna7_hist)
  })
  
  output$plot2 <- renderPlotly({
    getPlot("Totales",comuna7_hist)
  })
  
  output$plot3 <- renderPlotly({
    getPlot("Menores_de_1_año",comuna7_hist)
  })
  
  output$plot4 <- renderPlotly({
    getPlot("Neonatales",comuna7_hist)
  })
  
  output$plot5 <- renderPlotly({
    getPlot("Posneonatales",comuna7_hist)
  })
  
  output$plot6 <- renderPlotly({
    getPlot("Muertes_maternas",comuna7_hist)
  })

  output$entidades <- renderText({
    paste0("<p><strong>Selected entities: </strong>",paste(c(selected$groups), collapse=", "),'</p>')
  })
  
  output$pobtot <- renderText({
    value <- sum(radios@data[radios@data$LINK %in% selected$groups,"TOT_POB"])
    paste0("<p><strong>Total population: </strong>",value,'</p>')
  })
  
  output$area <- renderText({
    value <- sum(radios@data[radios@data$LINK %in% selected$groups,"AREA"])
    paste0("<p><strong>Area: </strong>",value,' m<sup>2</p>')
  })
  
  output$hogares <- renderText({
    value <- sum(radios@data[radios@data$LINK %in% selected$groups,"HOGARES"])
    paste0("<p><strong>Households: </strong>",value,'</p>')
  })
  
  output$AP4 <- renderUI({
    getAbsolutePanel("AP4")
  })
  
  output$AP6 <- renderUI({
    getAbsolutePanel("AP6")
  })
  
  output$AP9 <- renderUI({
    getAbsolutePanel("AP9")
  })
  
  output$AP10 <- renderUI({
    getAbsolutePanel("AP10")
  })
  
  output$AP11 <- renderUI({
    getAbsolutePanel("AP11")
  })
  
  
  observeEvent(input$show_cesacs, {
    if (input$show_cesacs) {
      proxy %>% showGroup(group = "dots1")
    } else {
      proxy %>% hideGroup(group = "dots1")
    }
    
  })
  
  observeEvent(input$show_hosp, {
    if (input$show_hosp) {
      proxy %>% showGroup(group = "dots2")
    } else {
      proxy %>% hideGroup(group = "dots2")
    }
  
  })
  
  output$mini_map <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'arrow-down',
      markerColor = "red",
      iconColor = "black",
      library = 'fa'
    )
    if (is.null(input$map_marker_click)==F) {
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
        addAwesomeMarkers(lat = input$map_marker_click$lat,
                          lng = input$map_marker_click$lng,
                          label = input$map_marker_click$id,
                          icon = icons)
    }
  })
  
  output$mini_map2 <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'arrow-down',
      markerColor = "red",
      iconColor = "black",
      library = 'fa'
    )
    if (is.null(input$map_marker_click)==F) {
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>%
        addAwesomeMarkers(lat = input$map_marker_click$lat,
                          lng = input$map_marker_click$lng,
                          label = input$map_marker_click$id,
                          icon = icons) %>% setView(zoom = 20,
                                                    lat = input$map_marker_click$lat,
                                                    lng = input$map_marker_click$lng)
    }
  })
  
  observeEvent(input$selectVariable, {
    proxy %>% hideGroup(group = "pacientes")
    
  })
  
  output$patients_records <- DT::renderDataTable({
    data <- pacientes[pacientes$cesac==input$map_marker_click$id,c("first_name",
                                                                   "last_name",
                                                                   "gender",
                                                                   "age",
                                                                   "email",
                                                                   "tel")]
    data$age <- round(data$age,0)
    colnames(data) <- c("First name",
                        "Last name",
                        "Gender",
                        "Age",
                        "Email",
                        "Telephone #")
    DT::datatable(data)
  })
  
  
  observeEvent(input$show_indicators, {
    indicatorsList <- c(
      'Pregnant women receiving antenatal care',
      'Children 0-9 years of age with health check-ups',
      'Adolescents 10-19 years of age with health check-ups',
      'Children with overweight or obesity ',
      'Adults with colorectal cancer screening',
      'Women with cervical cancer screening',
      'Women with breast cancer screening',
      'Type 2 diabetes control',
      'Blood pressure control among hypertensives'
      
    )
    
    values <- c(80,
                80,
                30,
                40,
                30,
                80,
                70,
                30,
                40)
    
    
    
    lapply(seq_along(indicatorsList), function (i) {
      value <- paste0(format(runif(1,values[i]*.9,values[i]*1.1),digits=2, nsmall=1, big.mark = ",", decimal.mark = "."),"%")
      output[[indicatorsList[i]]] <- renderUI({customInfoBoxInd(indicatorsList[i],value,"#BDD9E3","user-md",indicatorsList[i])})
        
      insertUI(
        selector = "#add",
        where = "afterEnd",
        multiple = T,
        ui = uiOutput(indicatorsList[i])
      )  
    })
      
    
    managementIndicatorsList <- c(
      'Users from the eligible population of the catchment area',
      'Users registered with a family doctor and a family health team',
      '# of family health teams',
      '# of medical specialists',
      'Connectivity bandwith',
      'Use of an electronic medical record',
      'Health workers receiving training',
      '# of workshops and training sessions',
      'Use of an web-based scheduling system',
      '# of scheduled visits for chronic care management'
    )
    
    values <- c('60%',
                '40%',
                '3',
                '8',
                '50mb',
                'Yes',
                '30%',
                '6',
                'No',
                '26')
    
    
    lapply(seq_along(managementIndicatorsList), function (i) {
      value <- values[i]
      
      output[[managementIndicatorsList[i]]] <- renderUI({customInfoBoxManagementInd(managementIndicatorsList[i],value,"#fec44f","chart-bar",managementIndicatorsList[i])})
      
      insertUI(
        selector = "#add2",
        where = "afterEnd",
        multiple = T,
        ui = uiOutput(managementIndicatorsList[i])
      )  
    })
      
    hideElement("AP9")
    hideElement("FP")
    showElement("IND", animType = "fade",10)
    showElement("AP10")
    
  })
  
  observeEvent(input$back_ind, {
    hideElement("IND")
    hideElement("AP10")
    showElement("AP9")
    showElement("FP")
  })
  
  observeEvent(input$back_pat, {
    showElement("FP")
    hideElement("PAT")
    hideElement("AP11")
    showElement("AP9")
  })
  
  observeEvent(input$show_patients, {
    hideElement("AP9")
    hideElement("FP")
    showElement("PAT")
    showElement("AP11")
    
  })
  
  
}
shinyApp(ui,server)




