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
           collapsible = TRUE,
           tabPanel("RECEPTIVO",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        )),
                    br(),
                    br(),
                    h4(stringr::str_to_upper("Turismo Internacional - RECEPTIVO")),
                    br(), 
                    fluidPage(
                      # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(4,
                                   selectInput("year",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(data_receptivo$year))))
                            ),
                            column(4,
                                   selectInput("mes",
                                               "Mes:",
                                               c("Todos",
                                                 unique(as.character(data_receptivo$mes))))
                            ),
                            column(4,
                                   selectInput("pais",
                                               "País:",
                                               c("Todos",
                                                 unique(as.character(data_receptivo$pais))))
                            )
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
                    h4(stringr::str_to_upper("Turismo Internacional - EMISIVO")),
                    br(), 
                    fluidPage(
                      # Create a new Row in the UI for selectInputs
                      fluidRow(
                        column(4,
                               selectInput("year_e",
                                           "Año:",
                                           c("Todos",
                                             unique(as.character(data_emisivo$year))))
                        ),
                        column(4,
                               selectInput("mes_e",
                                           "Mes:",
                                           c("Todos",
                                             unique(as.character(data_emisivo$mes))))
                               
                        ),
                        column(4,
                               selectInput("destino",
                                           "Destino:",
                                           c("Todos",
                                             unique(as.character(data_emisivo$destino))))
                        )
                      ),
                      # Create a new row for the table.
                      DT::dataTableOutput("table_emisivo")
                    ))
           )
