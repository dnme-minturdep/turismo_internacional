### UI

navbarPage(title = div(  #### NavBar #####
                         div(
                           id = "img-id",
                           tags$a(img(src = "https://upload.wikimedia.org/wikipedia/commons/8/8e/Ministerio_de_Turismo_y_Deportes_arg.png",
                                      width = 100),href="https://www.yvera.tur.ar/estadistica/",target = '_blank'
                           )),
                         "TURISMO INTERNACIONAL", id = "title"),
           id="navbar",
           position = "fixed-top",
           windowTitle = "Turismo Internacional - Argentina", 
           collapsible = TRUE,
           tabPanel("RECEPTIVO",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        )),
                    br(),
                    br(),
                    h4(stringr::str_to_upper(paste("Turismo Internacional - RECEPTIVO- Datos hasta", Mes_ult, data_receptivo[nrow(data_receptivo),1]))),
                    br(), 
                    fluidPage(
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(1,
                               selectInput("year",
                                           "Año:",
                                           c("Todos",
                                             unique(as.character(data_receptivo$year))),selected = data_receptivo[nrow(data_receptivo),1], multiple =TRUE)
                        ),
                        column(1,
                               selectInput("mes",
                                           "Mes:",
                                           c("Todos",
                                             unique(as.character(data_receptivo$mes))), selected = "Todos" , multiple =TRUE)
                        ),
                        
                        column(2,
                               selectInput("pais_agrupado",
                                           "País de residencia (agrup.):",
                                           c("Todos",
                                             unique(data_receptivo$pais_agrupado)))
                        ),
                        column(2,
                               selectInput("pais",
                                           "País de residencia:",
                                           choices = NULL)
                               
                        ),
                        
                        column(1,
                               selectInput("via",
                                           "Vía:",
                                           c("Todos",
                                             unique(data_receptivo$via)))
                        ),
                        
                        column(2,
                               selectInput("prov",
                                           "Provincia del paso:",
                                           choices = NULL)
                        ),
                        column(1,
                               selectInput("limita",
                                           "Limítrofe con:",
                                           choices = NULL)
                        ),
                        column(2,
                               selectInput("paso_publ",
                                           "Paso:", 
                                           choices = NULL)
                               
                        ),
                      ),
                      
                      fluidRow(
                        column(4,
                               selectInput("agrup", "Mostrar por:", choices = c( 'Mes' = 'mes', 'Vía' = 'via', 'País de residencia (agrup.)'= 'pais_agrupado', 'País de residencia'= 'pais', 'Paso' = 'paso_publ', 'Provincia del paso' = 'prov', 'País con el que limita' = 'limita'),
                                           selected = "mes", multiple = TRUE)
                        ),
                        
                      ),
                      
                      
                      
                      
                      # Create a new row for the table.
                      DT::dataTableOutput("table_receptivo")
                    )),
           tabPanel("EMISIVO",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        )),
                    br(),
                    br(),
                    h4(stringr::str_to_upper(paste("Turismo Internacional - EMISIVO- Datos hasta", Mes_ult, data_emisivo[nrow(data_emisivo),1]))),
                    br(), 
                    fluidPage(
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(1,
                               selectInput("year_e",
                                           "Año:",
                                           c("Todos",
                                             unique(as.character(data_emisivo$year))),selected = data_emisivo[nrow(data_emisivo),1], multiple =TRUE)
                        ),
                        column(1,
                               selectInput("mes_e",
                                           "Mes:",
                                           c("Todos",
                                             unique(as.character(data_emisivo$mes))),selected = "Todos" , multiple =TRUE)
                        ),
                        
                        column(2,
                               selectInput("destino",
                                           "Destino principal:",
                                           c("Todos",
                                             unique(data_emisivo$destino_agrup)))
                        ),
                        
                        column(2,
                               selectInput("via_e",
                                           "Vía:",
                                           c("Todos",
                                             unique(data_emisivo$via)))
                        ),
                        
                        column(2,
                               selectInput("prov_e",
                                           "Provincia del paso:",
                                           choices = NULL)
                        ),
                        column(1,
                               selectInput("limita_e",
                                           "Limítrofe con:",
                                           choices = NULL)
                        ),
                        column(2,
                               selectInput("paso_publ_e",
                                           "Paso:", 
                                           choices = NULL)
                               
                        ),
                      ),
                      
                      
                      
                      fluidRow(
                        column(4,
                               selectInput("agrup_e", "Mostrar por:", choices = c( 'Mes' = 'mes', 'Vía' = 'via', 'Destino principal'= 'destino_agrup', 'Paso' = 'paso_publ', 'Provincia del paso' = 'prov', 'País con el que limita' = 'limita'),
                                           selected = "mes", multiple = TRUE)
                        ),
                        
                      ),
                      
                      # Create a new row for the table.
                      DT::dataTableOutput("table_emisivo")
                    ))
)
