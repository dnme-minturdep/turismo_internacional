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
           tabPanel("SERIE HISTÓRICA",
                    br(),
                    plotlyOutput("fig1"),
                    br()
                    ),
                    br(),
           tabPanel("RECEPTIVO",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        )),
                    br(),
                    br(),
                    h4(stringr::str_to_upper(paste("RECEPTIVO- Datos hasta", Mes_ult, data_receptivo[nrow(data_receptivo),1]))),
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
                    h4(stringr::str_to_upper(paste("EMISIVO- Datos hasta", Mes_ult, data_emisivo[nrow(data_emisivo),1]))),
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
                    )),

         tabPanel("METODOLOGÍA",
         br(),
         h3("NOTAS TÉCNICAS"),
         br(),
         h4("   La estimación del turismo internacional (receptivo y emisivo) para el total del país surge de distintas fuentes 
             de datos."), 
         h4("   La fuente principal de información son los registros migratorios provistos por la Dirección Nacional de 
             Migraciones (DNM), cuyo análisis permite distinguir los turistas de otros tipos de viajeros internacionales 
             (excursionistas, tripulantes, etc.)."),
         h4("   Para los Aeropuertos Internacionales de Ezeiza, Córdoba, Mendoza, Aeroparque Jorge Newbery y el Paso 
            Internacional Cristo Redentor (Los Libertadores y Horcones), la fuente de información es la Encuesta de 
            Turismo Internacional (ETI), realizada por el INDEC y el Ministerio de Turismo y Deportes. La misma tiene 
            como objetivo caracterizar el flujo y medir el gasto de los visitantes no residentes durante su permanencia en 
            Argentina (turismo receptivo) y de los visitantes residentes en Argentina durante su permanencia en el exterior 
            (turismo emisivo)."), 
        h4("   Para más detalles,", tags$a(href="https://www.yvera.tur.ar/estadistica/documentos/descarga/5dc0460bcfa3e053142696.pdf", "ver el documento metodológico "),"de la estimación del turismo Internacional de la Argentina:"),
        br(),
        br(),
        h3 ("DEFINICIONES Y CONCEPTOS"),
        br(),
        h4(tags$ul(
                    tags$p(tags$b(" • Turismo internacional"),": Comprende el turismo receptivo y el turismo emisivo."),
                    tags$p(tags$b(" • Turismo receptivo"),": Refiere a los turistas no residentes en Argentina que han visitado el país."), 
                    tags$p(tags$b(" • Turismo emisivo"),": Refiere a los turistas residentes en Argentina que han viajado al exterior del país."),  
                    tags$p(tags$b(" • Turista"),": Un visitante se clasifica como turista si su viaje incluye una pernoctación."),  
                    tags$p(tags$b(" • Visitante"),": Es una persona que viaja a un destino principal distinto al de su entorno habitual, por una duración 
                                inferior a un año, con cualquier finalidad principal que no sea ser empleado por una entidad residente en el país 
                                o lugar visitado."),
                    tags$p(tags$b(" • Viaje"),": Refiere a todo desplazamiento de una persona a un lugar fuera de su entorno habitual, por cualquier 
                    motivo y duración, desde el momento de su salida hasta su regreso."),
                    tags$p(tags$b(" • Entorno habitual"),": Se define como la zona geográfica (aunque no necesariamente contigua) en la que una 
                    persona realiza sus actividades cotidianas habituales. Incluye el lugar de residencia habitual del hogar al que 
                    pertenece, su lugar de trabajo o estudio, y cualquier otro lugar que visite con regularidad y frecuencia, aún 
                    cuando dicho lugar esté lejos de su lugar de residencia habitual."),
                    tags$p(tags$b(" • País de residencia habitual"),": Es aquel en el cual una persona tiene su lugar de residencia habitual.")
                    )),

       )
           
)