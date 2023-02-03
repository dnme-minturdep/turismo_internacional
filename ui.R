### UI

navbarPage(title = div(  #### NavBar #####
                         div(
                           id = "img-id",
                           tags$a(img(src = "https://tableros.yvera.tur.ar/recursos/logo_sinta.png",
                                      width = 150),href="https://www.yvera.tur.ar/sinta/",target = '_blank'
                           )),
                         icon("globe"),"TURISMO INTERNACIONAL", id = "title", class = "navbar1"),
           id="navbar",
           position = "fixed-top",
           windowTitle = "Turismo Internacional - Argentina", 
           collapsible = TRUE,
           header = includeCSS("styles.css"),
           
           #RESUMEN ####
           tabPanel("RESUMEN",
                    
                    div(id= "container-info",
                        useWaiter(),
                        waiter_show_on_load(html = loading_screen, color = "white"),
                        h4(tags$p("El tablero de TURISMO INTERNACIONAL presenta las estimaciones de turismo receptivo y emisivo de  la Argentina,
                        permitiendo cuantificar y caracterizar a los viajes de los visitantes internacionales (turistas y excursionistas).
                        En la pestaña ", tags$b("RECEPTIVO"), "puede encontrar información sobre los viajes de visitantes no residentes en el país y en la 
                        solapa", tags$b("EMISIVO"), "sobre los viajes de visitantes residentes al exterior. Para profundizar en la caracterización de los
                        turistas receptivos puede hacerlo en la pestaña", tags$b("PERFIL RECEPTIVO,"), " basada en los datos de la Encuesta de Turismo Inernacional (ETI). 
                        Para más información del tablero 
                        y las fuentes de datos diríjase a la sección de ", tags$b("METODOLOGÍA."))),
                        br(),
                        fluidRow(
                          
                          column(4, selectInput("pais_agrup_graf",
                                                "País de residencia/destino:",
                                                choices = c("Todos", sort(unique(data_receptivo$pais_agrupado))),
                                                selected = "Todos" )),
                          ),
                        
                        br(), br(), br(),
                        
                        plotlyOutput("grafico_serie"),
                        helpText("Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes.",  
                                 style = "text-align: left;"),
                        br(),
                        fluidRow(
                          column(7, plotOutput("graf_pais_ti", height = 500)),
                          column(5, plotOutput('graf_via_ti', height = 390)),
                          ),
                        br(), 
                        h4("   Aquí puede acceder al último ", 
                           tags$a(href="https://tableros.yvera.tur.ar/internacional.html", 
                                  target = '_blank', 
                                  "reporte, "),
                           tags$a(href="https://www.yvera.tur.ar/sinta/informe/info/turismo-internacional", 
                                  target = '_blank', 
                                  "informe mensual,"), 
                           tags$a(href="https://datos.yvera.gob.ar/dataset/turismo-internacional-total-pais", 
                                  target = '_blank',
                                  "y datos abiertos"),
                           "de turismo internacional."),
           )),
           
           #RECEPTIVO ####
           tabPanel("RECEPTIVO",
                    
                    div(id="container-info",
                        br(),
                        h4(stringr::str_to_upper(paste("TURISMO RECEPTIVO- Datos hasta", Mes_ult, data_receptivo[nrow(data_receptivo),1]))),
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
                                   selectInput("year",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(data_receptivo$year))),
                                               selected = data_receptivo[nrow(data_receptivo),1], multiple =TRUE)
                                  ),
                            column(3,
                                   selectInput("mes",
                                               "Mes:",
                                               c("Todos",
                                                 unique(as.character(data_receptivo$mes))), 
                                               selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("via",
                                               "Medio de transporte*:",
                                               c("Todos",
                                                 unique(data_receptivo$via)))
                                   )
                            ),
                          fluidRow(
                            column(3,
                                   selectInput("pais_agrupado",
                                               "País de residencia (agrup.):",
                                               c("Todos",
                                                 sort(unique(data_receptivo$pais_agrupado))))
                            ),
                            column(3,
                                   selectInput("pais",
                                               "País de residencia:",
                                               choices = NULL)
                                   ),
                            column(3,
                                   selectInput("ruta",
                                               "Ruta natural*:",
                                               c("Todos",
                                                 unique(data_receptivo$ruta_natural)))
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
                          h5("*Esta información refiere al paso de salida del país. Por ello, al filtrar una provincia o ruta natural no se 
                             obtiene información de la totalidad de turistas internacionales que la visitaron, sino sólo de 
                             los que salieron del país por un paso de la misma."),
                          
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable."),
                          fluidRow(
                            column(3,
                                   selectInput("agrup", "Mostrar por:", 
                                               choices = c( 'Mes' = 'mes', 
                                                            'Trimestre' = 'trim',
                                                            'Vía' = 'via', 
                                                            'Tipo de visitante' = 'tipo_visitante', 
                                                            'País de residencia (agrup.)'= 'pais_agrupado', 
                                                            'País de residencia'= 'pais', 
                                                            'Ruta natural' = 'ruta_natural',
                                                            'Paso' = 'paso_publ',
                                                            'Provincia del paso' = 'prov', 
                                                            'País con el que limita' = 'limita',
                                                            'Género' = 'sexo',
                                                            'Tramos de edad' = 'grupoetario'),
                                               selected = "mes", multiple = TRUE)
                            ),
                            
                            
                          ),
                          h3("VIAJES DE VISITANTES NO RESIDENTES"),
                          
                          
                          
                          
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("table_receptivo") %>% 
                            withSpinner(), br(),
                          h5("Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes."),
                          
                        ))),
           #PERFIL RECEPTIVO ####
           tabPanel("PERFIL RECEPTIVO",
                    
                    div(id="container-info",
                        br(),
                        h4(glue("Esta pestaña permite caracterizar el perfil del turismo receptivo que egresó del país por los pasos de Ezeiza y Aeroparque, a partir de
                           la Encuesta de Turismo Internacional (ETI), desde Enero de 2019 a {mes_eti} {year_eti}. Se pueden analizar algunas características de los turistas (país de residencia,
                           tipo de alojamiento principal en el país, motivo de viaje), así como conocer los destinos (localidades, provincias) que han 
                           visitado en la Argentina. Aquí puede acceder a los  "), 
                           tags$a(href="https://datos.yvera.gob.ar/dataset/encuesta-turismo-internacional", 
                                  target = '_blank', 
                                  "datos abiertos de la ETI"),
                           " y a nuestra publicación en la ",
                           tags$a(href="https://bitacora.yvera.tur.ar/posts/2022-05-31-intro-eti/", 
                                  target = '_blank', 
                                  "bitácora.")
                           ),
                        fluidPage(
                          h3("FILTROS"),
                          h5("Los siguientes comandos permiten filtrar los datos."),
                          fluidRow(
                            column(3,
                                   selectInput("anio",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(localidad$anio))),selected = localidad[nrow(localidad),1], multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("trim",
                                               "Trimestre:",
                                               c("Todos",
                                                 unique(as.character(localidad$trim))), selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("mes",
                                               "Mes:",
                                               c("Todos",
                                                 unique(as.character(localidad$mes))), selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("provincia",
                                               "Provincia visitada:",
                                               c("Todas",
                                                 sort(unique(as.character(localidad$provincia)))), selected = "Todas" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("pais_origen",
                                               "País de residencia:",
                                               c("Todos",
                                                 sort(unique(as.character(localidad$pais_origen)))), selected = "Todos" , multiple =TRUE)
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
                                                            'Mes' = 'mes', 
                                                            'Pais de residencia' = 'pais_origen', 
                                                            'Tipo de alojamiento principal en el país' = 'alojamiento', 
                                                            'Motivo de viaje' = 'motivo_viaje',
                                                            'Provincia visitada' = 'provincia',
                                                            'Ciudad visitada' = 'ciudad'),
                                               selected = c("anio", "trim"), multiple = TRUE)
                            )),
                          
                          h3("PERFIL DE TURISTAS NO RESIDENTES. EZEIZA-AEROPARQUE (ETI)"),
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("tabla_eti") %>% 
                            withSpinner(), br(),
                          h5("Fuente: Encuesta de Turismo Internacional (ETI)."),
                          h6("*La suma de turistas en las localidades de una provincia es mayor al total provincial, 
                          por los casos que visitan más de una ciudad en la misma provincia."),
                          h6("*Solamente se consideran las visitas a destinos con al menos un pernocte, excepto en 
                          los casos de cruceros, donde puede no haber pernocte."),
                          h6("**Los datos del gasto se calculan trimestralmente, por eso", tags$b("no se muestran a nivel mensual.")),
                          h6("**El gasto por destino visitado (localidad/provincia) está estimado como el gasto promedio 
                             diario en el país por la cantidad de noches en el destino."),
                          h6("***Si la columna casos muestrales arroja menos de 50 casos, se sugiere reducir la 
                          cantidad de variables consideradas,  ampliar el periodo temporal, o agrupar 
                          localidades/provincias.")
                          
                          )
                    )
           ),
           #EMISIVO ####
           tabPanel("EMISIVO",
                    
                    div(id="container-info",
                        
                        br(),
                        h4(stringr::str_to_upper(paste("TURISMO EMISIVO- Datos hasta", Mes_ult, data_emisivo[nrow(data_emisivo),1]))),
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
                                   selectInput("year_e",
                                               "Año:",
                                               c("Todos",
                                                 unique(as.character(data_emisivo$year))),selected = data_emisivo[nrow(data_emisivo),1], multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("mes_e",
                                               "Mes:",
                                               c("Todos",
                                                 unique(as.character(data_emisivo$mes))),selected = "Todos" , multiple =TRUE)
                            ),
                            column(3,
                                   selectInput("via_e",
                                               "Medio de transporte*:",
                                               c("Todos",
                                                 unique(data_emisivo$via)))
                            ),),
                          fluidRow(
                            column(3,
                                   selectInput("destino",
                                               "Destino principal:",
                                               c("Todos",
                                                 sort(unique(data_emisivo$destino_agrup))))
                            ),
                            column(3,
                                   selectInput("prov_e",
                                               "Provincia del paso*:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("limita_e",
                                               "Limítrofe con*:",
                                               choices = NULL)
                            ),
                            column(3,
                                   selectInput("paso_publ_e",
                                               "Paso*:", 
                                               #multiple =TRUE,
                                               choices = NULL)
                                               
                                   
                            ),
                          ),
                          h5("*Esta información refiere al paso de ingreso al país."),
                          
                          h3("VISUALIZACIÓN"),
                          h5("Selecciona el nivel de apertura con que se visualizan los datos. Escriba varios términos en el buscador para mostrar por más de una variable"),
                          fluidRow(
                            column(4,
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
                                               selected = "mes", multiple = TRUE)
                            ),
                            
                          ),
                          h3("VIAJES DE VISITANTES RESIDENTES"),
                          
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("table_emisivo") %>% 
                            withSpinner(), br(),
                          h5("Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes."),
                          
                        ))),
           
           #METODOLOGIA ####
           tabPanel("METODOLOGÍA",
                    
                    div(id = "container-info",
                        br(),
                        h3("NOTAS TÉCNICAS"),
                        br(),
                        h4("   La estimación del turismo internacional (receptivo y emisivo) para el total del país surge de distintas fuentes 
             de datos."), 
                        h4("   La fuente principal de información de las pestañas RECEPTIVO y EMISIVO son los registros migratorios provistos por la Dirección Nacional de 
             Migraciones (DNM), cuyo análisis permite distinguir los viajes de turistas y excursionistas de otros tipos de viajeros internacionales 
             (tripulantes, diplomáticos, etc.). Los datos por paso refieren al de ingreso al país para el turismo emisivo y al de egreso de la Argentina para el turismo receptivo. 
             Por ello, al filtrar una provincia o ruta natural no se obtiene información de la totalidad de turistas internacionales que la visitaron, sino sólo de 
             los que salieron del país por un paso de la misma."),
                        h4("   Los datos por género y edad están disponibles desde noviembre de 2017."),
                        h4("   Los datos de excursionistas residentes por destino están disponibles desde agosto de 2021."),
                        br(),
                        h4("   La fuente de información de la solapa PERFIL RECEPTIVO, que contiene información sobre el turismo receptivo en el Aeropuerto Internacional de 
                        Ezeiza y Aeroparque Jorge Newbery, es la Encuesta de 
            Turismo Internacional (ETI), realizada por el INDEC y el Ministerio de Turismo y Deportes. La misma tiene 
            como objetivo caracterizar el flujo y medir el gasto de los visitantes no residentes durante su permanencia en 
            Argentina (turismo receptivo) y de los visitantes residentes en Argentina durante su permanencia en el exterior 
            (turismo emisivo). Solo se presenta información sobre los turistas que salieron por estos aeropuertos debido a que los datos sobre excursionistas y sobre el resto de 
            los pasos donde se realiza la ETI, al tener menor cantidad muestral, no permite mostrar datos mensuales ni aperturas por país tan desagregadas. 
            De todas formas, los pasos Ezeiza y Aeroparque representan más del 75% de los turistas relevados por la encuesta."), 
                        h4("   Para más detalles,", tags$a(href="https://www.yvera.tur.ar/sinta/informe/documentos/descarga/5dc0460bcfa3e053142696.pdf", target = '_blank', "ver el documento metodológico "),
                           "de la estimación del turismo internacional de la Argentina, los apartados correspondientes del",
                           tags$a(href="https://dnme-minturdep.github.io/DT3_registros_adminsitrativos/situaci%C3%B3n-nacional.html", target = '_blank', "Documento Técnico N°1"),
                           "sobre utilización de los registros migratorios y del",
                           tags$a(href="https://dnme-minturdep.github.io/DT1_medicion_turismo/encuestas-nacionales.html#eti", target = '_blank', "Documento Técnico N°3"),
                           "sobre la ETI."),
                        br(),
                        
                        h3 ("DEFINICIONES Y CONCEPTOS"),
                        br(),
                        h4(tags$ul(tags$p(tags$b(" • Viaje"),": Refiere a todo desplazamiento de una persona a un lugar fuera de su entorno habitual, por cualquier 
                    motivo y duración, desde el momento de su salida hasta su regreso."),
                                   tags$p(tags$b(" • Viaje receptor"),": Es el viaje a un país efectuado por una persona no residente desde el
momento en que llega a un país hasta el momento en que sale del mismo."),
                                   tags$p(tags$b(" • Viaje emisor"),": Es el viaje realizado fuera de un país por una persona residente en el
mismo desde el momento en que deja su lugar de residencia habitual hasta su regreso"),
                                   tags$p(tags$b(" • Viaje turístico"),": Es el viaje realizado por los visitantes."),
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
                                   tags$p(tags$b(" • País de residencia habitual"),": Es aquel en el cual una persona tiene su lugar de residencia habitual.")
                        )), br()
                    )
           ),
           
           footer = includeHTML("/srv/shiny-server/recursos/shiny_footer.html")
           
)
           
