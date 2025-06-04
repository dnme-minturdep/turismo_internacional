### UI

navbarPage(title = div(  #### NavBar #####
                         div(
                           id = "img-id",
                           tags$a(img(src = "https://tableros.yvera.tur.ar/recursos/logo_sinta.png",
                                      width = 100),href="https://www.yvera.tur.ar/sinta/",target = '_blank'
                           )),
                         icon("globe"),"TURISMO INTERNACIONAL", id = "title", class = "navbar1"),
           id="navbar",
           position = "fixed-top",
           windowTitle = "Turismo Internacional - Argentina", 
           collapsible = TRUE,
           header = includeCSS("styles.css"),
           
           #RESUMEN####
           tabPanel("RESUMEN",
                    
                    tags$head(includeHTML("/srv/DataDNMYE/login_shiny/turismo-internacional.html")),
                    
                    div(id= "container-info",
                        useWaiter(),
                        waiter_show_on_load(html = loading_screen, color = "white"),
                        br(),
                        h5(tags$p("El tablero de TURISMO INTERNACIONAL presenta las estimaciones de turismo receptivo y emisivo de  la Argentina,
                        permitiendo cuantificar y caracterizar a los viajes de los visitantes internacionales (turistas y excursionistas). En cada pestaña puede acceder a distinta información.")),
                        h5(tags$ul(tags$p(tags$b("• SERIE HISTÓRICA:"), "Serie trimestral de viajes, gasto en dólares y pernoctaciones de visitantes residentes y no residentes, basada en estimaciones propias y en información de la Dirección Nacional de Estadísticas del Sector Externo y Cuentas Internacionales (INDEC). Gráfico y tabla interactiva."),
                                   tags$p(tags$b("• RECEPTIVO:"),"Viajes de visitantes no residentes en el país.Tabla interactiva."), 
                                   tags$p(tags$b("• EMISIVO:"), "Viajes de visitantes residentes al exterior.Tabla interactiva."),
                                   tags$p(tags$b("• PERFIL RECEPTIVO:"), "Caracterización de los turistas receptivos, basada en los datos de la Encuesta de Turismo Internacional (ETI)."),
                                   tags$p(tags$b("• METODOLOGÍA:"), "Fuentes de datos, detalles técnicos y metodológicos y definiciones y conceptos utilizados."))
                           ),
                        br(),
                        
                        h3(glue::glue("Datos de último mes: {mes_ult_nro} {anio_ult}")),
                        fluidRow(column(6,
                                 wellPanel( 
                                   h6("Turismo receptivo",  style = "color: #37BBED; font-size: 2rem"),
                                   fluidRow(
                                     column(6, shinydashboard::valueBoxOutput("boxreceptivo", width = "100%") %>% withSpinner()),
                                     column(6, shinydashboard::valueBoxOutput("boxreceptivo_var", width = "100%") %>% withSpinner())
                                   )
                                 )
                          ),
                          column(6,
                                 wellPanel( 
                                   h6("Turismo emisivo",  style = "color: #37BBED; font-size: 2rem"),
                                   fluidRow(
                                     column(6, shinydashboard::valueBoxOutput("boxemisivo", width = "100%") %>% withSpinner()),
                                     column(6, shinydashboard::valueBoxOutput("boxemisivo_var", width = "100%") %>% withSpinner())
                                   )
                                 )
                          )
                        ),
                          
                        h3(glue::glue("Datos de último año: acumulado Enero - {mes_ult_nro} {anio_ult}")),
                        fluidRow(column(6,
                                        wellPanel( 
                                          h6("Turismo receptivo",  style = "color: #37BBED; font-size: 2rem"),
                                          fluidRow(
                                            column(6, shinydashboard::valueBoxOutput("boxreceptivo_ac", width = "100%") %>% withSpinner()),
                                            column(6, shinydashboard::valueBoxOutput("boxreceptivo_var_ac", width = "100%") %>% withSpinner())
                                          )
                                        )
                        ),
                        column(6,
                               wellPanel( 
                                 h6("Turismo emisivo",  style = "color: #37BBED; font-size: 2rem"),
                                 fluidRow(
                                   column(6, shinydashboard::valueBoxOutput("boxemisivo_ac", width = "100%") %>% withSpinner()),
                                   column(6, shinydashboard::valueBoxOutput("boxemisivo_var_ac", width = "100%") %>% withSpinner())
                                 )
                               )
                        )
                        ),
                        
                        
                        
                        
                        br(),
                        fluidRow(
                          column(7, plotlyOutput("graf_pais_ti", height = 500)),
                          column(5, plotlyOutput('graf_via_ti', height = 390)),
                          ),
                        br(), 
                        h5("   Aquí puede acceder al último ", 
                           tags$a(href="https://tableros.yvera.tur.ar/internacional.html", 
                                  target = '_blank', 
                                  "reporte, "),
                           tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/turismo-internacional", 
                                  target = '_blank', 
                                  "informe mensual,"), 
                           tags$a(href="https://datos.yvera.gob.ar/dataset/turismo-internacional-total-pais", 
                                  target = '_blank',
                                  "y datos abiertos"),
                           "de turismo internacional.")#,
                        
                        
                        #fluidRow(
                        #  
                        #  column(4, selectInput("pais_agrup_graf",
                        #                        "País de residencia/destino:",
                        #                        choices = c("Todos", sort(unique(data_receptivo$pais_agrupado))),
                        #                        selected = "Todos" )),
                        #  ),
                        #
                        #br(), br(), br(),
                        #
                        #plotlyOutput("grafico_serie"),
                        #helpText("Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes.",  
                        #         style = "text-align: left;"),
                        #
                        )),
           
           #SERIE ####
           tabPanel("SERIE HISTÓRICA",
                    
                    div(id= "container-info",
                        br(),
                        h4("SERIE HISTÓRICA TRIMESTRAL DE VIAJES, GASTO Y PERNOCTACIONES"),
                        h5(tags$p(glue::glue("Datos hasta trimestre {trim_ult_gasto} del año {anio_ult_gasto}."))), 
                        h5(tags$p(" En esta sección se presenta la serie histórica trimestral de ",  
                        tags$b("turismo receptivo y emisivo de viajes,
                               gasto en dólares y pernoctaciones de turistas y excursionistas,"),
                               "incluyendo los indicadores de estadía media, 
                         gasto promedio por viaje y gasto promedio diario. Pueden 
                      visualizarse en forma de gráfico o tabla.")),
                        br(),
                          
                          h3("GRÁFICO INTERACTIVO"),
                          fluidRow(
                            
                            
                            column(3,
                                   radioButtons("metrica",
                                                label = "Métrica:",
                                                choiceNames =  list("Viajes","Gasto (en millones de US$)","Estadía media", "Gasto por viaje (en US$)","Gasto diario (en US$)"),
                                                choiceValues = list("Viajes","Gasto","Estadia_media", "Gasto_viaje","Gasto_diario"),
                                                #choices = c("Viajes","Gasto","Estadia_media", "Gasto_viaje","Gasto_diario"),
                                                selected = "Viajes")
                            ),
                            
                            column(2,
                                   radioButtons("periodo",
                                                label = "Serie:",
                                                choices = c("Anual","Trimestral"),
                                                selected = "Anual")
                            ),
                            
                            column(3,
                                   checkboxGroupInput("tipo_visitante_graf",
                                                      label = "Tipo de visitante:",
                                                      choices = c("Turistas", 
                                                                  "Excursionistas"),
                                                      selected = "Turistas")
                            ),
                            
                            column(3,
                                   selectInput("pais_agrup_graf_serie",
                                               "País de residencia/destino:",
                                               c("Todos",
                                                 sort(data_receptivo %>% distinct(pais_agrupado) %>% pull())))
                            ),
                          ),
                          
                          plotlyOutput("grafico_gasto"),
                          helpText("Nota: Hasta 1994 no se dispone de datos a nivel trimestral. Hasta 2003 no hay datos de excursionistas por país de residencia/destino.",  
                                   style = "text-align: left;"),
                          helpText("Fuente: Dirección de Mercados y Estadisticas (Subsecretaría de Turismo) y Dirección Nacional de Estadísticas del Sector Externo y Cuentas Internacionales (INDEC).",  
                                   style = "text-align: left;"),
                          br(),
                          
                          
                          h3("TABLA INTERACTIVA"),
                          
                        
                        
                        fluidPage(
                          h3("FILTROS"),
                          h5("Los siguientes comandos permiten filtrar los datos"),
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(2,
                                   radioButtons("tipo_turismo_g",
                                                label = "Turismo:",
                                                choices = c("Receptivo", 
                                                            "Emisivo"),
                                                selected = "Receptivo")
                            ),
                            
                            column(2,
                                   checkboxGroupInput("tipo_visitante_g",
                                                      label = "Viajes de:",
                                                      choices = c("Turistas", 
                                                                  "Excursionistas"),
                                                      selected = "Turistas")
                            ),
                            
                            column(2,
                                   selectInput("anio_g",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(gasto$anio))),
                                               selected = gasto[nrow(gasto),1], multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("trim_g",
                                               "Trimestre:",
                                               c("Todos",
                                                 unique(as.character(gasto$trim))), selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("pais_agrupado_g",
                                               "País de residencia/destino:",
                                               c("Todos",
                                                 sort(unique(gasto$pais_agrupado))))
                            ),
                          ),
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable"),
                          fluidRow(
                            column(4,
                                   selectInput("agrup_g", "Mostrar por:", choices = c( 'Trimestre' = 'trim',
                                                                                       'Tipo de visitante' = 'tipo_visitante', 
                                                                                       'País de residencia/destino'= 'pais_agrupado'), 
                                               
                                               selected = "trim", multiple = TRUE)
                            ),
                            column(2, br(),
                                   actionButton("btnSearchSerie", "Buscar", 
                                                icon = icon("magnifying-glass"),
                                                style ="color: #fff; background-color: #b5b5b5;", width = "100%")
                                   )
                            #, 
                            #column(2,
                            #       radioButtons("round_s",
                            #                    label = "Redondeo:",
                            #                    choices = c("Básico","Decimales"),
                            #                    selected = "Básico")
                            #)
                          ),
                         
                    
                          # Create a new row for the table.
                          DT::dataTableOutput("tabla_serie") %>% 
                            withSpinner(), 
                          
                          fluidRow(downloadButton("downloadExcelSerie","Descargar en excel"),
                                   downloadButton("downloadCSVSerie","Descargar en csv")),
                          
                          br(),
                          h6("* Hasta 1994 no se dispone de datos a nivel trimestral. Hasta 2003 no hay datos de excursionistas por país de residencia/destino. Datos provisorios desde 2020."),
                          h5("Fuente: Dirección de Mercados y Estadisticas (Subsecretaría de Turismo) y Dirección Nacional de Estadísticas del Sector Externo y Cuentas Internacionales (INDEC)"),
                          br()#,
                          
                          
                        ))),
           
           
           
           #RECEPTIVO ####
           tabPanel("RECEPTIVO",
                    
                    div(id="container-info",
                        br(),
                        h4(stringr::str_to_upper(paste("VIAJES DE VISITANTES NO RESIDENTES- Datos hasta", mes_ult_nro, anio_ult))),
                        fluidPage(
                          h3("FILTROS"),
                          h5("Los siguientes comandos permiten filtrar los datos"),
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(3,
                                   checkboxGroupInput("tipo_visitante",
                                                      label = "Viajes de:",
                                                      choices = c("Turistas", 
                                                                  "Excursionistas"),
                                                      selected = "Turistas")
                                   ),
                            column(3,
                                   selectInput("anio",
                                               "Año:",
                                               c("Todos",
                                                 1990:anio_ult),
                                               selected = anio_ult, 
                                               multiple =TRUE)
                                   ),
                                   #sliderTextInput("anio",
                                   #            "Año:",
                                   #            grid = TRUE, force_edges = TRUE,
                                   #            choices = seq(1990, 2023, by = 1),
                                   #            selected = 2023)),
                            column(3,
                                   selectInput("mes",
                                               "Mes:",
                                               c("Todos",
                                                 levels(mes_ult_nro)), 
                                               selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("via",
                                               "Medio de transporte*:",
                                               c("Todos",
                                                 "Aéreo",
                                                 "Fluvial/marítimo",
                                                 "Terrestre",
                                                 "Sin dato"))
                                   )
                            ),
                          fluidRow(
                            column(3,
                                   selectInput("pais_agrupado",
                                               "País de residencia (agrup.):",
                                               c("Todos",
                                                 "Bolivia",
                                                 "Brasil",
                                                 "Chile",
                                                 "EE.UU. y Canadá",
                                                 "Europa",
                                                 "Paraguay",
                                                 "Resto de América",
                                                 "Resto del mundo",
                                                 "Uruguay",
                                                 "Sin dato"))
                            ),
                            column(3,
                                   selectInput("pais",
                                               "País de residencia:",
                                               choices = NULL)
                                   ),
                            column(3,
                                   selectInput("ruta",
                                               "Región natural*:",
                                               c("Todos",
                                                 sort(data_receptivo %>% distinct(ruta_natural) %>% pull())))
                                   ),
                            column(3,
                                   selectInput("prov",
                                               "Provincia del paso*:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("limita",
                                               "Limítrofe con*:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("paso_publ",
                                               "Paso*:", 
                                               choices = NULL
                                               )
                                   ),
                          ),
                          h5('*Esta información no refleja la estimación de turismo 
                             receptivo por provincia/región natural. Ver "Notas técnicas", en la pestaña Metodología.'),
                          
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable."),
                          fluidRow(
                            column(6,
                                   selectInput("agrup", "Mostrar por:", 
                                               choices = c( 'Mes' = 'mes', 
                                                            'Trimestre' = 'trim',
                                                            'Vía' = 'via', 
                                                            'Tipo de visitante' = 'tipo_visitante', 
                                                            'País de residencia (agrup.)'= 'pais_agrupado', 
                                                            'País de residencia'= 'pais', 
                                                            'Región natural' = 'ruta_natural',
                                                            'Paso' = 'paso_publ',
                                                            'Provincia del paso' = 'prov', 
                                                            'País con el que limita' = 'limita',
                                                            'Género' = 'sexo',
                                                            'Tramos de edad' = 'grupoetario'),
                                               selected = "mes", multiple = TRUE, width = "100%")
                            ),
                            column(2, br(),
                                   actionButton("btnSearchReceptivo", "Buscar", 
                                                icon = icon("magnifying-glass"),
                                                style ="color: #fff; background-color: #b5b5b5;", width = "100%")
                            )
                            
                            #,
                            # column(6,
                            #        radioButtons("round",
                            #                     label = "Redondeo:",
                            #                     choiceNames =  list("Sin decimales (suma de parciales 
                            #                                         puede diferir del total)",
                            #                                         "Con decimales 
                            #                                         (recomendado para descarga)"),
                            #                     choiceValues = list("Sin decimales","Con decimales"),
                            #                     #choices = c("Sin decimales","Con decimales"),
                            #                     selected = "Sin decimales")
                            # )
                            
                            
                          ),
                          
                          textOutput(outputId = "titulo", container = h3),
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("table_receptivo") %>% 
                            withSpinner(), 
                          
                          fluidRow(downloadButton("downloadExcelRec","Descargar en excel"),
                                                    downloadButton("downloadCSVRec","Descargar en csv")),
                          br(),
                          h5("Fuente: Dirección de Mercados y Estadísticas, Subsecretaría de Turismo."),
                          
                          br(), 
                          h5("   Aquí puede acceder al último ", 
                             tags$a(href="https://tableros.yvera.tur.ar/internacional.html", 
                                    target = '_blank', 
                                    "reporte, "),
                             tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/turismo-internacional", 
                                    target = '_blank', 
                                    "informe mensual,"), 
                             tags$a(href="https://datos.yvera.gob.ar/dataset/turismo-internacional-total-pais", 
                                    target = '_blank',
                                    "y datos abiertos"),
                             "de turismo internacional.")#,
                          
                          
                        ))),
           
           #EMISIVO ####
           tabPanel("EMISIVO",
                    
                    div(id="container-info",
                        br(),
                        h4(stringr::str_to_upper(paste("VIAJES AL EXTERIOR DE TURISTAS RESIDENTES- Datos hasta", mes_ult_nro, anio_ult))),
                        fluidPage(
                          h3("FILTROS"),
                          h5("Los siguientes comandos permiten filtrar los datos"),
                          
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(3,
                                   checkboxGroupInput("tipo_visitante_e",
                                                      label = "Viajes de:",
                                                      choices = c("Turistas", 
                                                                  "Excursionistas"),
                                                      selected = "Turistas")
                            ),
                            column(3,
                                   selectInput("anio_e",
                                               "Año:",
                                               c("Todos",
                                                 1990:anio_ult),
                                               selected = anio_ult,
                                               multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("mes_e",
                                               "Mes:",
                                               c("Todos",
                                                 levels(mes_ult_nro)), 
                                               selected = mes_ult_nro,
                                               multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("via_e",
                                               "Medio de transporte:",
                                               c("Todos",
                                                 "Aéreo",
                                                 "Fluvial/marítimo",
                                                 "Terrestre",
                                                 "Sin dato"))
                            )),
                          fluidRow(
                            column(3,
                                   selectInput("destino",
                                               "Destino principal:",
                                               c("Todos",
                                                 "Bolivia",
                                                 "Brasil",
                                                 "Chile",
                                                 "EE.UU. y Canadá",
                                                 "Europa",
                                                 "Paraguay",
                                                 "Resto de América",
                                                 "Resto del mundo",
                                                 "Sin dato"))
                            ),
                            column(3,
                                   selectInput("prov_e",
                                               "Provincia del paso:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("limita_e",
                                               "Limítrofe con:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("paso_publ_e",
                                               "Paso:", 
                                               #multiple =TRUE,
                                               choices = NULL)
                                               
                                   
                            )
                          ),
                          
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable"),
                          fluidRow(
                            column(6,
                                   selectInput("agrup_e", "Mostrar por:", choices = c( 'Mes' = 'mes', 
                                                                                       'Trimestre' = 'trim',
                                                                                       'Vía' = 'via', 
                                                                                       'Tipo de visitante' = 'tipo_visitante', 
                                                                                       'Destino principal'= 'destino_agrup', 
                                                                                       'Paso' = 'paso_publ', 
                                                                                       'Provincia del paso' = 'prov', 
                                                                                       'País con el que limita' = 'limita',
                                                                                       'Género' = 'sexo',
                                                                                       'Tramos de edad' = 'grupoetario'),
                                               selected = "mes", multiple = TRUE, width = "100%")
                            ),
                            column(2, br(),
                                   actionButton("btnSearchEmisivo", "Buscar", 
                                                icon = icon("magnifying-glass"),
                                                style ="color: #fff; background-color: #b5b5b5;", width = "100%")
                            )
                            # column(6,
                            #        radioButtons("round_e",
                            #                     label = "Redondeo:",
                            #                     choiceNames =  list("Sin decimales (suma de parciales 
                            #                                         puede diferir del total)",
                            #                                         "Con decimales 
                            #                                         (recomendado para descarga)"),
                            #                     choiceValues = list("Sin decimales","Con decimales"),
                            #                     selected = "Sin decimales")
                            # )
                            
                            
                          ),
                          textOutput(outputId = "titulo_e", container = h3),
                          
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("table_emisivo") %>% 
                            withSpinner(), 
                          
                          fluidRow(downloadButton("downloadExcelEmi","Descargar en excel"),
                                   downloadButton("downloadCSVEmi","Descargar en csv")),
                          
                          br(),
                          h5("Fuente: Dirección de Mercados y Estadísticas, Subsecretaría de Turismo"),
                          br(), 
                          h5("   Aquí puede acceder al último ", 
                             tags$a(href="https://tableros.yvera.tur.ar/internacional.html", 
                                    target = '_blank', 
                                    "reporte, "),
                             tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/turismo-internacional", 
                                    target = '_blank', 
                                    "informe mensual,"), 
                             tags$a(href="https://datos.yvera.gob.ar/dataset/turismo-internacional-total-pais", 
                                    target = '_blank',
                                    "y datos abiertos"),
                             "de turismo internacional.")#,
                          
                          
                        ))),
           
           #PERFIL RECEPTIVO ####
           tabPanel("PERFIL RECEPTIVO",
                    
                    div(id="container-info",
                        br(),
                        h4("PERFIL TURISMO RECEPTIVO"),
                        h5(glue::glue("Datos desde Enero de 2019 a {mes_ult_nro} {anio_ult}.")),
                        h5("Esta pestaña permite caracterizar el perfil del turismo receptivo por los pasos relevados en 
                           la Encuesta de Turismo Internacional (ETI). Se pueden analizar algunas características de los turistas (país de residencia,
                           tipo de alojamiento principal en el país, motivo de viaje), así como conocer los destinos (localidades, provincias) que han 
                           visitado en la Argentina."),
                        fluidPage(
                          h3("FILTROS"),
                          h5("Los siguientes comandos permiten filtrar los datos."),
                          fluidRow(
                            column(3,
                                   selectInput("paso",
                                               "Paso:",
                                               choices = unique(as.character(localidad$paso_final)),
                                               selected = "Ezeiza y Aeroparque", 
                                               multiple =FALSE)
                            ),
                            column(3,
                                   selectInput("anio_encuesta",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(localidad$anio))),
                                               selected = localidad[nrow(localidad),1],
                                               multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("trim",
                                               "Trimestre:",
                                               c("Todos",
                                                 unique(as.character(localidad$trim))), selected = "Todos" , multiple =TRUE)
                            ),
                            # column(3,
                            #        selectInput("mes_encuesta",
                            #                    "Mes:",
                            #                    c("Todos",
                            #                      unique(as.character(localidad$mes))), selected = "Todos" , multiple =TRUE)
                            # ),
                            column(3,
                                   selectInput("provincia",
                                               "Provincia visitada:",
                                               c("Todas",
                                                 sort(unique(as.character(localidad$provincia)))), selected = "Todas" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("pais_origen",
                                               "País de residencia:",
                                               choices = NULL, 
                                               #c("Todos",
                                               #  sort(unique(as.character(localidad$pais_origen)))), 
                                               #selected = "Todos" , 
                                               multiple = TRUE)
                            ),
                            column(3,
                                   selectInput("alojamiento",
                                               "Tipo de alojamiento principal en el país:",
                                               c("Todos",
                                                 sort(unique(as.character(localidad$alojamiento)))), selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("motivo_viaje",
                                               "Motivo de viaje:",
                                               c("Todos",
                                                 unique(as.character(localidad$motivo_viaje))), selected = "Todos" , multiple =TRUE)
                            )
                          ),
                          
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable."),
                          fluidRow(
                            column(3,
                                   selectInput("agrup_p", "Mostrar por:", 
                                               choices = c( 'Año' = 'anio', 
                                                            'Trimestre' = 'trim',
                                                            #'Mes' = 'mes', 
                                                            'Pais de residencia' = 'pais_origen', 
                                                            'Tipo de alojamiento principal en el país' = 'alojamiento', 
                                                            'Motivo de viaje' = 'motivo_viaje',
                                                            'Provincia visitada' = 'provincia',
                                                            'Ciudad visitada' = 'ciudad'),
                                               selected = c("anio", "trim"), multiple = TRUE)
                                   ),
                            
                            column(2, br(),
                                   actionButton("btnSearchPerfil", "Buscar", 
                                                icon = icon("magnifying-glass"),
                                                style ="color: #fff; background-color: #b5b5b5;", width = "100%")
                            )
                            ),
                          
                          h3("PERFIL DE TURISTAS NO RESIDENTES (ETI)"),
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("tabla_eti") %>% 
                            withSpinner(), 
                          
                          
                          fluidRow(downloadButton("downloadExcelPerfil","Descargar en excel"),
                                   downloadButton("downloadCSVPerfil","Descargar en csv")),
                          
                          br(),
                          h5("Fuente: Encuesta de Turismo Internacional (ETI)."),
                          h6("*La suma de turistas en las localidades de una provincia es mayor al total provincial, 
                          por los casos que visitan más de una ciudad en la misma provincia."),
                          h6("*Solamente se consideran las visitas a destinos con al menos un pernocte, excepto en 
                          los casos de cruceros, donde puede no haber pernocte."),
                          h6("**Los datos del gasto se calculan trimestralmente", tags$b(" y están en 0 hasta el cierre del trimestre.")),
                          h6("**El gasto por destino visitado (localidad/provincia) está estimado como el gasto promedio 
                             diario en el país por la cantidad de noches en el destino."),
                          h6("***Si la columna casos muestrales arroja menos de 50 casos, se sugiere reducir la 
                          cantidad de variables consideradas,  ampliar el periodo temporal, o agrupar 
                          localidades/provincias."), 
                          br(), 
                          h5("Aquí puede acceder a los", 
                             tags$a(href="https://datos.yvera.gob.ar/dataset/encuesta-turismo-internacional", 
                                         target = '_blank', 
                                         "datos abiertos de la ETI"
                                    ),
                             "y a nuestra publicación en la ",
                             tags$a(href="https://bitacora.yvera.tur.ar/posts/2022-05-31-intro-eti/", 
                                         target = '_blank', 
                                         "bitácora."
                                    )
                             )                          )#cierre fluid page
                          )#cierre div
                        ),#cierre tabpanel
                    
           #METODOLOGIA ####
           tabPanel("METODOLOGÍA",
                    
                    div(id = "container-info",
                        
                        h3("FUENTES DE DATOS"),
                        br(),
                        h5("   • La estimación de ", tags$b("viajes"), "del turismo internacional (receptivo y emisivo) 
                        para el total del país surge de los registros migratorios provistos por la Dirección 
                        Nacional de Migraciones (DNM)"),
                        
                        h5("   • El", tags$b("gasto y las pernoctaciones,"),"tienen como fuente las estimaciones elaboradas por la 
                        Dirección Nacional de Estadísticas del Sector Externo y Cuentas Internacionales (DNESEyCI) del INDEC.  "), 
                        
                        h5("  • La fuente de información del ", tags$b("perfil receptivo"), " es la Encuesta de 
            Turismo Internacional (ETI), realizada por el INDEC y el Ministerio de Turismo y Deportes."),
                        
                        br(),
                        
                        h3("ACLARACIONES TÉCNICAS"),
                        h5("•", tags$b("Serie histórica:"), "Los datos de gasto desde el 2016 en adelante surgen directamente de la DNESEyCI de INDEC, 
                        mientras que los datos de años anteriores surgen de un empalme realizado por la DMyE para poder mantener
                        la comparabilidad de la serie histórica."), 
                        
                        h5("•", tags$b("Receptivo y Emisivo:")),
                        
                        tags$ul(h5("-La estimación del turismo internacional se contabiliza al finalizar el viaje, por tanto, el turismo 
                           receptivo se contabiliza a la salida y el emisivo a la entrada.")),
                        tags$ul(h5("-Diferencias entre", tags$b("turistas no residentes y viajes de turistas no residentes:"), 
                        "Un turista receptivo, luego de visitar la Argentina puede ir a otro país cercano y volver a ingresar antes de regresar a su país de
                        residencia habitual. Por ello, un turista no residente puede realizar más de un viaje en el mismo mes: la ", tags$b("cantidad de viajes receptivos puede ser mayor que la
                        cantidad de turistas receptivos."), "Se establece como supuesto que los visitantes no residentes que utilizan la vía aérea siempre abandonan definitivamente el país, por tal motivo
                        la cantidad de viajes coincide con la cantidad de turistas. Este supuesto no se aplica en el resto de las vías de acceso al país.
                        ")), 
                        
                        tags$ul(h5("-La apertura por provincia/región natural no refleja la estimación del turismo receptivo de cada provincia,
                        sino sólo los viajes de turistas contabilizados en los pasos fronterizos de la misma.")),  
                        
                        tags$ul(h5("-Al descargar los datos, se recomienda hacerlo con decimales, para que la suma de los parciales 
                           coincida con el total.")),
                        tags$ul(h5("-A continuación, se muestran las variables disponibles según año, 
                                   tipo de visitante y tipo de turismo:")),
                        tableOutput("aperturas_variables"),
                        
                        tags$ul(h6("* En emisivo, no hay datos de destino de excursionistas entre 2016 y julio de 2021.")),
                        
                        h5("•", tags$b("Perfil receptivo (ETI):"),"La encuesta presenta información de turistas, no de viajes de turistas. Por tanto, el dato de los pasos 
                        'Cristo Redentor' y 'Puerto de Buenos Aires' es menor al de la solapa receptivo, ya que en la misma se contabilizan los viajes. En la vía aérea, ambos números coinciden por definición."), 
                        
                        
                        br(),
                        
                        h5(" • Para más detalles,", tags$a(href="https://www.yvera.tur.ar/sinta/informe/documentos/descarga/5dc0460bcfa3e053142696.pdf", target = '_blank', "ver el documento metodológico "),
                           "de la estimación del turismo internacional de la Argentina, los apartados correspondientes del",
                           tags$a(href="https://dnme-minturdep.github.io/DT3_registros_adminsitrativos/situaci%C3%B3n-nacional.html", target = '_blank', "Documento Técnico N°1"),
                           "sobre utilización de los registros migratorios y del",
                           tags$a(href="https://dnme-minturdep.github.io/DT1_medicion_turismo/encuestas-nacionales.html#eti", target = '_blank', "Documento Técnico N°3"),
                           "sobre la ETI."),
                        
                        h3 ("DEFINICIONES Y CONCEPTOS"),
                        br(),
                        h5(tags$ul(tags$p(tags$b(" • Viaje"),": Refiere a todo desplazamiento de una persona a un lugar fuera de su entorno habitual, por cualquier 
                    motivo y duración, desde el momento de su salida hasta su regreso."),
                                   tags$p(tags$b(" • Viaje receptor"),": Es el viaje a un país efectuado por una persona no residente desde el
momento en que llega a un país hasta el momento en que sale del mismo."),
                                   tags$p(tags$b(" • Viaje emisor"),": Es el viaje realizado fuera de un país por una persona residente en el
mismo desde el momento en que deja su lugar de residencia habitual hasta su regreso"),
                                   tags$p(tags$b(" • Viaje turístico"),": Es el viaje realizado por los visitantes. El viaje realizado por un visitante no residente (turismo receptivo) se registra desde
                                      el momento que llega a la Argentina hasta que sale. Un visitante no residente
                                      (turista o excursionista) puede salir de Argentina para efectuar visitas a otros países
                                      y volver a ingresar en un corto período de tiempo, antes de regresar a su país de
                                      residencia habitual. Por ello, la cantidad de viajes receptivos puede ser mayor que la
                                      cantidad de turistas/excursionistas receptivos.
                                      En tanto, el viaje realizado por un visitante residente en Argentina (turismo emisivo)
                                      se registra desde el momento en que sale del país hasta que retorna,
                                      independientemente de la cantidad de países visitados. Por ello, la cantidad de
                                      viajes emisivos coincide con la cantidad de visitantes emisivos.
                                      "),
                                   tags$p(tags$b(" • Visitante"),": Es una persona que viaja a un destino principal distinto al de su entorno habitual, por una duración 
                                inferior a un año, con cualquier finalidad principal que no sea ser empleado por una entidad residente en el país 
                                o lugar visitado."),
                                   tags$p(tags$b(" • Turista"),": Un visitante se clasifica como turista si su viaje incluye una pernoctación."),  
                                   tags$p(tags$b(" • Excursionista"),": Un visitante se clasifica como excursionista si su viaje no incluye una pernoctación."),  
                                   tags$p(tags$b(" • Entorno habitual"),": Se define como la zona geográfica (aunque no necesariamente contigua) en la que una 
                    persona realiza sus actividades cotidianas habituales. Incluye el lugar de residencia habitual del hogar al que 
                    pertenece, su lugar de trabajo o estudio, y cualquier otro lugar que visite con regularidad y frecuencia, aún 
                    cuando dicho lugar esté lejos de su lugar de residencia habitual."),
                                   tags$p(tags$b(" • Turismo receptivo"),": Engloba las actividades realizadas por un visitante no residente
en el país de referencia, como parte de un viaje turístico receptor."), 
                                   tags$p(tags$b(" • Turismo emisivo"),": Abarca las actividades realizadas por un visitante residente fuera
del país de referencia, como parte de un viaje turístico emisor."),  
                                   tags$p(tags$b(" • Turismo internacional"),": Comprende el turismo receptivo y el turismo emisivo."),
                                   tags$p(tags$b(" • País de residencia habitual"),": Es aquel en el cual una persona tiene su lugar de 
                                          residencia habitual."),
                                   tags$p(tags$b(" • Pernoctaciones:"), " es el número de noches que cada viajero permanece alojado, fuera de su lugar de
                                   residencia habitual, en el lugar visitado."),
                                   tags$p(tags$b(" • Estadía media:"), "es el cociente entre las pernoctaciones y la cantidad de viajes de turistas."),
                                   tags$p(tags$b(" • Gasto total:"), " comprende todo gasto de consumo de bienes y servicios efectuado por un turista o por cuenta de un turista durante su estadía en el lugar visitado.
                                                    Excluye el valor del transporte internacional, pero incluye el valor del transporte dentro del país visitado. 
                                                    Expresado en dólares a precios corrientes."),
                                   tags$p(tags$b(" • Gasto por viaje:"), " resulta del cociente entre el gasto total y la cantidad de viajes de turistas. Es una medida aproximada de lo que cada turista gasta en promedio en un viaje.
                                   Expresado en dólares a precios corrientes."),
                                   tags$p(tags$b(" • Gasto promedio diario:"), "resulta del cociente entre el gasto total y el número de pernoctaciones. Es una medida aproximada del gasto diario promedio que cada turista realiza
                                   en el lugar visitado. Expresado en dólares a precios corrientes."),
                        )), br()
                    )
           ),
           
           footer = includeHTML("/srv/shiny-server/recursos/shiny_footer.html")
           
)
           
