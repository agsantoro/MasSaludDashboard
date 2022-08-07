getMapa <- function (poligonos, puntos, variable, puntos_hospitales, pacientes) {
  pal <- brewer.pal(8, "YlOrRd") 
  pal <- colorRampPalette(pal)(250)
  pal <- data.frame(order=c(1:length(pal)),
                    pal=pal)
  
  
  poligonos@data$order <- unique(rank(poligonos@data[,variable],ties.method = "random"))
  poligonos@data <- left_join(poligonos@data,
                              pal, by="order")
  icons <- awesomeIcons(
    icon = 'arrow-down',
    markerColor = "red",
    iconColor = "black",
    library = 'fa'
  )
  
  iconsH <- awesomeIcons(
    icon = 'arrow-down',
    markerColor = "blue",
    iconColor = "black",
    library = 'fa'
  )
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addPolygons(data = poligonos,
                fillColor = ~pal,
                fillOpacity = 0.5,
                color = "black",
                stroke = TRUE,
                weight = 2,
                layerId = ~LINK,
                group = "radios",
                label = paste0(variable,": ",format(poligonos@data[,variable],digits=1))) %>%
    addPolygons(data = poligonos,
                fillColor = "#91cf60",
                fillOpacity = 1,
                weight = 5,
                color = "black",
                stroke = TRUE,
                layerId = ~PAIS0210_,
                group = ~LINK) %>%
    hideGroup(group = poligonos@data$LINK) %>% 
    setView(zoom = 13.50,
            lat=-34.634610, 
            lng=-58.442854 ) %>% addProviderTiles(providers$CartoDB) %>%
    addAwesomeMarkers(lat = puntos$V2,
                      lng = puntos$V1,
                      icon = icons,
                      label = puntos$nombre,
                      layerId = puntos$nombre,
                      group = "dots1"
                      ) %>%
    addAwesomeMarkers(lat = puntos_hospitales$V2,
                      lng = puntos_hospitales$V1,
                      icon = iconsH,
                      label = puntos_hospitales$NOMBRE,
                      group = "dots2") %>%
    addCircleMarkers(lng=as.numeric(pacientes$V2),
                     lat=as.numeric(pacientes$V1),
                     radius = 0.1,
                     label = paste(pacientes$first_name,pacientes$last_name),
                     group = "pacientes",
                     layerId = paste(pacientes$first_name,pacientes$last_name))
}

getPlot <- function (var,data) {
  text <- str_replace_all(var,"_"," ")
  data[[var]] <- as.numeric(data[[var]]) 
  plot_ly(data,x=~ANO) %>%
    add_trace(y=data[[var]]  , name = 'trace 0',type = 'scatter', mode = 'lines+markers') %>%
    layout(plot_bgcolor='transparent',
           paper_bgcolor='transparent',
           xaxis = list(title="", tickfont = list(size = 9)),
           yaxis = list(title ="", tickfont = list(size = 9))) %>% config(displayModeBar = F)
  
  
  
}

getAbsolutePanel <- function(id) {
  if (id=="AP2") {
    return(
      tagList(fixedPanel(id="AP2",
                            top = 20, right = 20, bottom=20, draggable = F, width = "30%", style = "z-index:500; min-width: 300px;",
                            column(12,
                                   h4("Historical data"),
                                   h5("Comuna 7 - Ciudad Autónoma de Buenos Aires"),
                                   br(),
                                   br(),
                                   column(6,
                                          p(strong("Live births")),
                                          plotlyOutput("plot1", width = "90%", height = "180px"),
                                          br(),
                                          p(strong("Infant deaths")),
                                          plotlyOutput("plot3", width = "90%", height = "180px"),
                                          br(),
                                          p(strong("Neonatal deaths")),
                                          plotlyOutput("plot4", width = "90%", height = "180px"),
                                          br()
                                   ),
                                   column(6,
                                          p(strong("Total deaths")),
                                          plotlyOutput("plot2", width = "90%", height = "180px"),
                                          br(),
                                          p(strong("Post-neonatla deaths")),
                                          plotlyOutput("plot5", width = "90%", height = "180px"),
                                          br(),
                                          p(strong("Maternal deaths")),
                                          plotlyOutput("plot6", width = "90%", height = "180px"),
                                          br()
                                   )
                            )
                            ,
                            br()
      ))
      
    )

  }
  
  if (id=="AP3") {
    return(
      tagList(  fixedPanel(id="AP3",
                              bottom = 20, right = 20, draggable = F, width = "22%", style = "z-index:500; min-width: 300px;",
                              column(12,
                                     h3("Selected Area"),
                                     br(),
                                     uiOutput("entidades"),
                                     uiOutput("pobtot"),
                                     uiOutput("area"),
                                     uiOutput("hogares")
                              )
                              ,
                              br()
      ))
    )

  }
  
  if (id=="AP4") {
    return(
      tagList(  absolutePanel(id="AP4",
                              bottom = 20, left = 20, draggable = F, width = "50%", style = "z-index:500; min-width: 300px;",
                              column(6,
                                     checkboxInput("show_cesacs", "Show primary care centers in map", value=T),
                                     checkboxInput("show_hosp", "Show hospitals in map", value=T))
                              )
                              ,
                              br()
              )
    )
    
  }
  
  if (id=="AP6") {
    return(
      tagList(absolutePanel(id="AP6", bottom = 20, right = 20, draggable = F, width = "10", style = "z-index:500; min-width: 300px;",
                              column(12,
                                     br(),
                                     p(id="text1", "Select area by clicking"),
                                     tags$head(tags$style("#text1{color: red;
                                                           font-size: 15px;
                                                           font-style: bold;
                                                           }", align="center")
                                               ),
                            
                                     br()
                                    )
                            )
      )
    )
  }
  
  if (id=="AP9") {
    return(
      tagList(absolutePanel(id="AP9", bottom = 30, right = 45, draggable = F,
                            column(12,
                                   actionButton("back","Volver", icon=icon("arrow-left"))
                                   
                            )
      )
      )
    )
  }
  if (id=="AP10") {
    return(
      tagList(absolutePanel(id="AP9", bottom = 30, right = 45, draggable = F,
                            column(12,
                                   actionButton("back_ind","Volver", icon=icon("arrow-left"))
                                   
                            )
      )
      )
    )
  }
  if (id=="AP11") {
    return(
      tagList(absolutePanel(id="AP9", bottom = 30, right = 45, draggable = F,
                            column(12,
                                   actionButton("back_pat","Volver", icon=icon("arrow-left"))
                                   
                            )
      )
      )
    )
  }
  
}

getCenterReport <- function(center_name,input,output,data) {
  data <- data[data$nombre==input$map_marker_click[[1]],]
  paste(
  '<p><span style="font-family:Lucida Sans Unicode,Lucida Grande,sans-serif"><strong>Name:</strong> ',data$nombre,'</span></p>
    
   <p><span style="font-family:Lucida Sans Unicode,Lucida Grande,sans-serif"><strong>Director:</strong> ',data$jefe,'</span></p>
      
   <p><span style="font-family:Lucida Sans Unicode,Lucida Grande,sans-serif"><strong>Specialties:</strong> ',data$especialid,'</span></p>'
  )
  
  
}

getFakeData <- function () {
  load("shp/radios/radios.RData")
  radios@data$sample <- round(radios@data$TOT_POB* runif(1, min=0.001, max=0.003),digits=0)
  
  sfdf <- st_as_sf(radios)
  
  sample_points <- st_sample(sfdf, size = sfdf$sample,
                             type = 'random', exact = TRUE) %>% as.data.frame()
  sample_points$geometry <- substring(sample_points$geometry,9,nchar(sample_points$geometry)) 
  sample_points<- str_split(sample_points$geometry,", lat = ",2)
  
  pacientes <- data.frame(
    V1 = unlist(sample_points)[subset(1:(length(sample_points)*2),(1:(length(sample_points)*2))%%2==0)],
    V2 = unlist(sample_points)[subset(1:(length(sample_points)*2),(1:(length(sample_points)*2))%%2!=0)]
  )
  
  pacientes$V1 <- substring(pacientes$V1,1,(nchar(pacientes$V1)-1))
  
  NAME = randomNames::randomNames(n=nrow(pacientes),
                                  ethnicity = 4,
                                  return.complete.data = T)[,c(1,3,4)] %>% as.data.frame()
  
  pacientes <- cbind(pacientes,NAME) %>% as.data.frame()
  load('shp/centros-de-salud/cesacs.RData')
  
  for (i in 1:nrow(pacientes)) {
    pacientes$cesac[i] <- rep(sample(cesacs$nombre,1),1) 
  }
  
  pacientes$gender[pacientes$gender==1] <- "Female"
  pacientes$gender[pacientes$gender==0] <- "Male"
  pacientes$birth_date <- generator::r_date_of_births(nrow(pacientes))
  pacientes$age <- time_length(difftime(as.Date(Sys.Date()), pacientes$birth_date), "years")
  pacientes$email <- generator::r_email_addresses(nrow(pacientes))
  pacientes$tel <- generator::r_phone_numbers(nrow(pacientes))
  pacientes$`Glucemia elevada` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Obesidad` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Tabaquismo` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Dislipidemia` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Hipertensión Arterial` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Sedentarismo` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes$`Abuso de alcohol` <- sample(c(0,1), replace=TRUE, size=nrow(pacientes))
  pacientes <- pacientes[pacientes$age<96,]
  return(pacientes)
}

customInfoBoxInd <- function (text,valor,color,icono,tooltip) {
  tagList(
    tags$head(
      tags$style(HTML(paste0("/*
 * Component: Info Box
                    * -------------------
                    */
                    .info-box {
                    display: block;
                    min-height: 90px;
                    background: #fff;
                    width: 100%;
                    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                    border-radius: 2px;
                    margin-bottom: 15px;
                    text-overflow: ellipsis;
                    }
                    .info-box small {
                    font-size: 14px;
                    }
                    .info-box .progress {
                    background: rgba(0, 0, 0, 0.2);
                    margin: 5px -10px 5px -10px;
                    height: 2px;
                    
                    }
                    .info-box .progress,
                    .info-box .progress .progress-bar {
                    border-radius: 2px;
                    
                    }
                    .info-box .progress .progress-bar {
                    background: #fff;
                    
                    }
                    .info-box-icon {
                    border-top-left-radius: 2px;
                    border-top-right-radius: 0;
                    border-bottom-right-radius: 0;
                    border-bottom-left-radius: 2px;
                    display: block;
                    float: left;
                    height: 90px;
                    width: 90px;
                    text-align: center;
                    font-size: 45px;
                    line-height: 90px;
                    background: rgba(0, 0, 0, 0.2);
                    }
                    .info-box-icon > img {
                    max-width: 100%;
                    }
                    .info-box-content {
                    padding: 5px 10px;
                    margin-left: 90px;
                    }
                    .info-box-number {
                    display: block;
                    font-weight: bold;
                    font-size: 18px;
                    }
                    .progress-description,
                    .info-box-text {
                    display: block;
                    font-size: 10px;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    }
                    .info-box-more {
                    display: block;
                    }
                    .progress-description {
                    margin: 0;
                    }

                    .bg-yellow,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: ",color," !important;
                    }

                    ")))
    ),
    tags$div(
      title=tooltip,
    infoBox(text,  valor,  icon = icon(icono, verify_fa = FALSE), color = "yellow")
    )
    
  )
  
  
}

customInfoBoxManagementInd <- function (text,valor,color,icono,tooltip) {
  tagList(
    tags$head(
      tags$style(HTML(paste0("/*
 * Component: Info Box
                    * -------------------
                    */
                    .info-box {
                    display: block;
                    min-height: 90px;
                    background: #fff;
                    width: 100%;
                    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                    border-radius: 2px;
                    margin-bottom: 15px;
                    text-overflow: ellipsis;
                    }
                    .info-box small {
                    font-size: 14px;
                    }
                    .info-box .progress {
                    background: rgba(0, 0, 0, 0.2);
                    margin: 5px -10px 5px -10px;
                    height: 2px;
                    
                    }
                    .info-box .progress,
                    .info-box .progress .progress-bar {
                    border-radius: 2px;
                    
                    }
                    .info-box .progress .progress-bar {
                    background: #fff;
                    
                    }
                    .info-box-icon {
                    border-top-left-radius: 2px;
                    border-top-right-radius: 0;
                    border-bottom-right-radius: 0;
                    border-bottom-left-radius: 2px;
                    display: block;
                    float: left;
                    height: 90px;
                    width: 90px;
                    text-align: center;
                    font-size: 45px;
                    line-height: 90px;
                    background: rgba(0, 0, 0, 0.2);
                    }
                    .info-box-icon > img {
                    max-width: 100%;
                    }
                    .info-box-content {
                    padding: 5px 10px;
                    margin-left: 90px;
                    }
                    .info-box-number {
                    display: block;
                    font-weight: bold;
                    font-size: 18px;
                    }
                    .progress-description,
                    .info-box-text {
                    display: block;
                    font-size: 10px;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    }
                    .info-box-more {
                    display: block;
                    }
                    .progress-description {
                    margin: 0;
                    }

                    .bg-red,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: ",color," !important;
                    }

                    ")))
    ),
    tags$div(
      title=tooltip,
      infoBox(text,  valor,  icon = icon(icono, verify_fa = FALSE), color = "red")
    )
    
  )
  
  
}
