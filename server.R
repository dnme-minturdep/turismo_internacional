#SERVER

function(input, output, session) {
  
  # GRAFICO ####
  Sys.sleep(3)
  waiter_hide()  
  
  output$fig1 <- renderPlotly(fig1)
  
  
  
  
  # RECEPTIVO ####
  
  #  Reactivo de pais agrupado segun input 
  
  pais_ag <- reactive ({
    if (input$pais_agrupado == "Todos") {
      pais_ag <- data_receptivo
    } else {
      pais_ag <- data_receptivo[data_receptivo$pais_agrupado == input$pais_agrupado,  ]
    } 
  })
  
  #al cambiar input de pais_agrup actualizo choices de pais
  
  observeEvent(pais_ag(), {
    if (input$pais_agrupado == "Todos") {
      updateSelectInput(session, inputId = "pais", choices = c("Todos",
                                                               sort(unique(data_receptivo$pais))))
    } else {
      updateSelectInput(session, inputId = "pais", choices = c("Todos", (unique(pais_ag()$pais)))) 
    }
  })
  
  
  # Reactivo de pais segun input
  
  pais <- reactive({
    req(input$pais)
    if (input$pais == "Todos") {
      pais <- pais_ag()
    } else {
      pais <- pais_ag()[pais_ag()$pais == input$pais,  ]
    }
  })
  
  
  # Reactivo via segun input,(toma base de pais) 
  
  via <- reactive ({
    if (input$via == "Todos") {
      via <- pais()
    } else {
      via <- pais()[pais()$via == input$via,  ]
    } 
  })
  
  
  # Actualizacion de choices de ruta al cambiar input. 
  
  
  observeEvent(via(), {
    if (input$limita == "Todos" & input$via == "Todos") {
      updateSelectInput(session, inputId = "ruta", choices = c("Todos",
                                                               sort(unique(data_receptivo$ruta_natural))))
    } else {
      updateSelectInput(session, inputId = "ruta", choices = c("Todos",sort(unique(via()$ruta_natural)))) 
    }
  })
  
  
  # Reactivo de ruta segun input .
  
  ruta <- reactive({
    req(input$ruta)
    if (input$ruta == "Todos") {
      ruta <- via()
    } else {
      ruta <- via()[via()$ruta == input$ruta,  ]
    }
  })
  
  
  
  # Actualizacion de choices de prov al cambiar input de ruta
  
  
  observeEvent(ruta(), {
    if (input$ruta == "Todos" & input$via == "Todos") {
      updateSelectInput(session, inputId = "prov", choices = c("Todos",
                                                               sort(unique(data_receptivo$prov))))
    } else {
      updateSelectInput(session, inputId = "prov", choices = c("Todos",sort(unique(ruta()$prov)))) 
    }
  })
  
  
  # Reactivo de prov segun input .
  
  prov <- reactive({
    req(input$prov)
    if (input$prov == "Todos") {
      prov <- ruta()
    } else {
      prov <- ruta()[ruta()$prov == input$prov,  ]
    }
  })
  
  # Actualizacion de choices de limita al cambiar input. 
  
  observeEvent(prov(), {
    if (input$via == "Todos" & input$ruta == "Todos" & input$prov == "Todos") {
      updateSelectInput(session, inputId = "limita", choices = c("Todos",
                                                                 sort(unique(data_receptivo$limita)))) 
    } else {
      updateSelectInput(session, inputId = "limita", choices = c("Todos",sort(unique(prov()$limita))))
    }
  })
  
  
  # Reactivo de limita segun input.
  
  limita <- reactive({
    req(input$limita)
    if (input$limita == "Todos") {
      limita <- prov()
    } else {
      limita <- prov()[prov()$limita == input$limita,  ]
    }
  })
  
  
  # Actualizacion de choices de paso al cambiar input. 
  
  observeEvent(limita(), {
    if (input$limita == "Todos" & input$via == "Todos" & input$ruta == "Todos" & input$prov == "Todos") {
      updateSelectInput(session, inputId = "paso_publ", choices = c("Todos",
                                                                    sort(unique(data_receptivo$paso_publ))))
    } else {
      updateSelectInput(session, inputId = "paso_publ", choices = c("Todos",sort(unique(limita()$paso_publ))))
    }
  })
  
  
  # Reactivo de paso segun input.
  
  paso <- reactive({
    req(input$paso_publ)
    if (input$paso_publ == "Todos") {
      paso <- limita()
    } else {
      paso <- limita()[limita()$paso_publ == input$paso_publ,  ]
    }
  })

  # TABLA RECEPTIVO
  
#seleccion visitantes para titulo del cuadro:

  seleccion_vis <- reactive ({
    if (length(input$tipo_visitante) == 1) {
      seleccion_vis <- paste0("VIAJES DE ", toupper(input$tipo_visitante), " NO RESIDENTES")
    } else {
      seleccion_vis <- "VIAJES DE VISITANTES NO RESIDENTES"
    } 
  })
  
  output$titulo <- renderText({seleccion_vis()})
  output$table_receptivo <- DT::renderDT(server = FALSE,
                                         
                                         DT::datatable(extensions = 'Buttons',
                                                       options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                                      dom = 'lfrtipB',
                                                                      buttons = list('copy',
                                                                                     list(
                                                                                       extend = 'collection',
                                                                                       buttons = c('csv', 'excel'),
                                                                                       text = 'Download'
                                                                                     ))),
                                                       {
                                                         tabla <- paso()
                                                         req(input$tipo_visitante)
                                                           tabla <- tabla[tabla$tipo_visitante %in% input$tipo_visitante,]		
                                                         
                                                         req(input$year)
                                                         if (all(input$year != "Todos")) {
                                                           tabla <- tabla[tabla$year %in% input$year,]		
                                                         }
                                                         req(input$mes)
                                                         if (all(input$mes != "Todos")) {
                                                           tabla <- tabla[tabla$mes %in% input$mes,]		
                                                         }
                                                         
                                                         if (all(input$tipo_visitante == "Turistas")){
                                                         tabla <- tabla %>%
                                                           group_by_at(.vars = c( "year", input$agrup)) %>%
                                                           summarise ("Viajes de turistas" = round(sum(turistas))) 
                                                         } else if (all(input$tipo_visitante == "Excursionistas")){
                                                           tabla <- tabla %>%
                                                             group_by_at(.vars = c( "year", input$agrup)) %>%
                                                             summarise ("Viajes de excursionistas" = round(sum(turistas))) 
                                                         } else {
                                                           tabla <- tabla %>%
                                                             group_by_at(.vars = c( "year", input$agrup)) %>%
                                                             summarise ("Viajes de visitantes" = round(sum(turistas))) 
                                                         } 
                                                         
                                                         #etiquetas receptivo según selección en ui.
                                                         
                                                         etiquetas <- gsub ("year", "Año", (colnames(tabla)))
                                                         etiquetas <- gsub ("mes", "Mes", etiquetas)
                                                         etiquetas <- gsub ("trim", "Trimestre", etiquetas)
                                                         etiquetas <- gsub ("via", "Vía", etiquetas)
                                                         etiquetas <- gsub ("ruta_natural", "Ruta natural", etiquetas)
                                                         etiquetas <- gsub ("pais_agrupado", "País de residencia (agrup.)", etiquetas)
                                                         etiquetas <- gsub ("pais" , "País de residencia", etiquetas)
                                                         etiquetas <- gsub ("paso_publ" , "Paso", etiquetas)
                                                         etiquetas <- gsub ("prov", "Provincia del paso", etiquetas)
                                                         etiquetas <- gsub ("limita" ,"Limita con", etiquetas)
                                                         etiquetas <- gsub ("sexo", "Género", etiquetas)
                                                         etiquetas <- gsub ("grupoetario", "Tramos de edad", etiquetas)
                                                         etiquetas <- gsub ("tipo_visitante" ,"Tipo de visitante", etiquetas)
  
                                                         tabla
                                                       }, rownames= FALSE, colnames = etiquetas)
  )
  
  
  
  # EMISIVO ####
  
  # Reactivo via_e segun input 
  
  via_e <- reactive ({
    if (input$via_e == "Todos") {
      via_e <- data_emisivo
    } else {
      via_e <- data_emisivo[data_emisivo$via == input$via_e,  ]
    } 
  }) 
  
  
  # Actualizacion de choices de prov_e al cambiar input. 
  
  
  observeEvent(via_e(), {
    if (input$limita_e == "Todos" & input$via_e == "Todos") {
      updateSelectInput(session, inputId = "prov_e", choices = c("Todos",
                                                                 sort(unique(data_emisivo$prov)))) 
    } else {
      updateSelectInput(session, inputId = "prov_e", choices = c("Todos", sort(unique(via_e()$prov)))) 
    }
  })
  
  
  # Reactivo de prov_e segun input .
  
  prov_e <- reactive({
    req(input$prov_e)
    if (input$prov_e == "Todos") {
      prov_e <- via_e()
    } else {
      prov_e <- via_e()[via_e()$prov == input$prov_e,  ]
    }
  })
  
  # Actualizacion de choices de limita_e al cambiar input. 
  
  observeEvent(prov_e(), {
    if (input$via_e == "Todos" & input$prov_e == "Todos") {
      updateSelectInput(session, inputId = "limita_e", choices = c("Todos",
                                                                   sort(unique(data_emisivo$limita)))) 
    } else {
      updateSelectInput(session, inputId = "limita_e", choices = c("Todos",sort(unique(prov_e()$limita)))) 
    }
  })
  
  
  # Reactivo de limita_e segun input.
  
  limita_e <- reactive({
    req(input$limita_e)
    if (input$limita_e == "Todos") {
      limita_e <- prov_e()
    } else {
      limita_e <- prov_e()[prov_e()$limita == input$limita_e,  ]
    }
  })
  
  
  # Actualizacion de choices de paso_e al cambiar input. 
  
  observeEvent(limita_e(), {
    if (input$limita_e == "Todos" & input$via_e == "Todos" & input$prov_e == "Todos") {
      updateSelectInput(session, inputId = "paso_publ_e", choices = c("Todos",
                                                                      sort(unique(data_emisivo$paso_publ)))) 
    } else {
      updateSelectInput(session, inputId = "paso_publ_e", choices = c("Todos", sort(unique(limita_e()$paso_publ)))) 
    }
  })
  
  
  # Reactivo de paso_e segun input.
  
  paso_e <- reactive({
    req(input$paso_publ_e)
    if (input$paso_publ_e == "Todos") {
      paso_e <- limita_e()
    } else {
      paso_e <- limita_e()[limita_e()$paso_publ == input$paso_publ_e,  ]
    }
  })
  
  
  
  
  
  # TABLA EMISIVO
  #seleccion visitantes para titulo del cuadro:
  
  seleccion_vis_e <- reactive ({
    if (length(input$tipo_visitante_e) == 1) {
      seleccion_vis_e <- paste0("VIAJES DE ", toupper(input$tipo_visitante_e), " RESIDENTES")
    } else {
      seleccion_vis_e <- "VIAJES DE VISITANTES RESIDENTES"
    } 
  })
  
  output$titulo_e <- renderText({seleccion_vis_e()})
  
  output$table_emisivo <- DT::renderDataTable(server = FALSE,
                                              
                                              DT::datatable(extensions = 'Buttons', 
                                                            options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                                           dom = 'lfrtipB',
                                                                           buttons = list('copy',
                                                                                          list(
                                                                                            extend = 'collection',
                                                                                            buttons = c('csv', 'excel'),
                                                                                            text = 'Download'
                                                                                          ))),
                                                            {
                                                              tabla_e <- paso_e()
                                                              req(input$tipo_visitante_e)
                                                              tabla_e <- tabla_e[tabla_e$tipo_visitante %in% input$tipo_visitante_e,]		
                                                              
                                                              req(input$year_e)
                                                              if (all(input$year_e != "Todos")) {
                                                                tabla_e <- tabla_e[tabla_e$year %in% input$year_e,]		
                                                              }
                                                              req(input$mes_e)
                                                              if (all(input$mes_e != "Todos")) {
                                                                tabla_e <- tabla_e[tabla_e$mes %in% input$mes_e,]		
                                                              }
                                                              req(input$destino)
                                                              if (all(input$destino != "Todos")) {
                                                                tabla_e <- tabla_e[tabla_e$destino == input$destino,]		
                                                              }
                                                              
                                                              if (all(input$tipo_visitante_e == "Turistas")){
                                                                tabla_e <- tabla_e %>%
                                                                  group_by_at(.vars = c( "year", input$agrup_e)) %>%
                                                                  summarise ("Viajes de turistas" = round(sum(turistas))) 
                                                              } else if (all(input$tipo_visitante_e == "Excursionistas")){
                                                                tabla_e <- tabla_e %>%
                                                                  group_by_at(.vars = c( "year", input$agrup_e)) %>%
                                                                  summarise ("Viajes de excursionistas" = round(sum(turistas))) 
                                                              } else {
                                                                tabla_e <- tabla_e %>%
                                                                  group_by_at(.vars = c( "year", input$agrup_e)) %>%
                                                                  summarise ("Viajes de visitantes" = round(sum(turistas))) 
                                                              } 
                                                              
                                                              
                                                              #etiquetas emisivo según selección en ui.
                                                              
                                                              etiquetas_e <- gsub ("year", "Año", (colnames(tabla_e)))
                                                              etiquetas_e <- gsub ("mes", "Mes", etiquetas_e)
                                                              etiquetas_e <- gsub ("trim", "Trimestre", etiquetas_e)
                                                              etiquetas_e <- gsub ("via", "Vía", etiquetas_e)
                                                              etiquetas_e <- gsub ("destino_agrup" , "Destino principal", etiquetas_e)
                                                              etiquetas_e <- gsub ("paso_publ" , "Paso", etiquetas_e)
                                                              etiquetas_e <- gsub ("prov", "Provincia del paso", etiquetas_e)
                                                              etiquetas_e <- gsub ("limita" ,"Limita con", etiquetas_e)
                                                              etiquetas_e <- gsub ("sexo", "Género", etiquetas_e)
                                                              etiquetas_e <- gsub ("grupoetario", "Tramos de edad", etiquetas_e)
                                                              etiquetas_e <- gsub ("tipo_visitante" ,"Tipo de visitante", etiquetas_e)
                                                              
                                                              tabla_e
                                                            }, rownames= FALSE, colnames = etiquetas_e)
  )
  
# ETI ####  

  output$tabla_eti<- DT::renderDT(server = FALSE,
                                  
                                  DT::datatable(extensions = 'Buttons',
                                                options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                               dom = 'lfrtipB',
                                                               buttons = list('copy',
                                                                              list(
                                                                                extend = 'collection',
                                                                                buttons = c('csv', 'excel'),
                                                                                text = 'Download'
                                                                              ))),   
                                                {
                                                  tabla <- localidad
                                                  #filtros:
                                                  req(input$anio)
                                                  if (all(input$anio != "Todos")) {
                                                    tabla <- tabla[tabla$anio %in% input$anio,]		
                                                  }
                                                  req(input$trim)
                                                  if (all(input$trim != "Todos")) {
                                                    tabla <- tabla[tabla$trim %in% input$trim,]		
                                                  }
                                                  req(input$mes)
                                                  if (all(input$mes != "Todos")) {
                                                    tabla <- tabla[tabla$mes %in% input$mes,]		
                                                  }
                                                  req(input$provincia)
                                                  if (all(input$provincia != "Todas")) {
                                                    tabla <- tabla[tabla$provincia %in% input$provincia,]		
                                                  }
                                                  req(input$motivo_viaje)
                                                  if (all(input$motivo_viaje != "Todos")) {
                                                    tabla <- tabla[tabla$motivo_viaje %in% input$motivo_viaje,]		
                                                  }
                                                  req(input$alojamiento)
                                                  if (all(input$alojamiento != "Todos")) {
                                                    tabla <- tabla[tabla$alojamiento %in% input$alojamiento,]		
                                                  }
                                                  req(input$pais_origen)
                                                  if (all(input$pais_origen != "Todos")) {
                                                    tabla <- tabla[tabla$pais_origen %in% input$pais_origen,]		
                                                  }
                                                  
                                                  
                                                  
                                                  
                                                  # Grupos
                                                  tabla <- tabla %>%
                                                    group_by_at(.vars = c( "id", input$agrup_p)) %>%
                                                    summarise (turistas = first(wpf),
                                                               casos =  first(p18_1), 
                                                               noches = sum(noches * wpf),
                                                               gasto = sum(gasto * wpf),                                                                      ) %>%
                                                    group_by_at(.vars = c( input$agrup_p)) %>%
                                                    summarise (Turistas = sum(turistas),
                                                               Noches = sum(noches),
                                                               Gasto = round(sum(gasto),1),
                                                               Casos_Muestrales = sum(casos))
                                                  #saco gasto al mostrar por mes
                                                  if (any(input$agrup_p == "mes")) {
                                                    tabla <- tabla %>% select (-Gasto)
                                                  }
                                                  
                                                  #etiquetas receptivo según selección en ui.
                                                  
                                                  etiquetas <- gsub ("anio", "Año", (colnames(tabla)))
                                                  etiquetas <- gsub ("mes", "Mes", etiquetas)
                                                  etiquetas <- gsub ("trim", "Trimestre", etiquetas)
                                                  etiquetas <- gsub ("provincia", "Provincia visitada", etiquetas)
                                                  etiquetas <- gsub ("ciudad", "Ciudad visitada", etiquetas)
                                                  etiquetas <- gsub ("pais_origen", "País de residencia", etiquetas)
                                                  etiquetas <- gsub ("alojamiento", "Tipo alojamiento principal en el país", etiquetas)
                                                  etiquetas <- gsub ("motivo_viaje", "Motivo de viaje", etiquetas)
                                                  etiquetas <- gsub ("Turistas", "Turistas no residentes*", etiquetas)
                                                  etiquetas <- gsub ("Gasto", "Gasto en U$**", etiquetas)
                                                  etiquetas <- gsub ("Casos_Muestrales", "Casos Muestrales***", etiquetas)
                                                  
                                                  tabla
                                                }, rownames= FALSE, colnames = etiquetas)
  )
  
  # SERIE ####  
  
  output$tabla_serie<- DT::renderDT(server = FALSE,
                                  
                                  DT::datatable(extensions = 'Buttons',
                                                options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                               dom = 'lfrtipB',
                                                               buttons = list('copy',
                                                                              list(
                                                                                extend = 'collection',
                                                                                buttons = c('csv', 'excel'),
                                                                                text = 'Download'
                                                                              ))),
                                                {
                                                  tabla <- gasto
                                                  #filtros:
                                                  req(input$tipo_turismo_g)
                                                  tabla <- tabla[tabla$residencia == input$tipo_turismo_g,]	
                                                  
                                                  req(input$tipo_visitante_g)
                                                  tabla <- tabla[tabla$tipo_visitante %in% input$tipo_visitante_g,]		
                                                  
                                                  req(input$year_g)
                                                  if (all(input$year_g != "Todos")) {
                                                    tabla <- tabla[tabla$anio %in% input$year_g,]		
                                                  }
                                                  req(input$trim_g)
                                                  if (all(input$trim_g != "Todos")) {
                                                    tabla <- tabla[tabla$trim %in% input$trim_g,]		
                                                  }
                                                  req(input$pais_agrupado_g)
                                                  if (all(input$pais_agrupado_g != "Todos")) {
                                                    tabla <- tabla[tabla$pais_agrupado %in% input$pais_agrupado_g,]		
                                                  }
                                                  
                                                  tabla <- tabla %>%
                                                    group_by_at(.vars = c( "anio", input$agrup_g)) %>%
                                                    summarise ("Viajes" = round(sum(casos_ponderados)), 
                                                               "Gasto" = round(sum(gasto),1),
                                                               "Pernoctaciones" = round(sum(pernoctes)), 
                                                               "Estadía media**" = round(Pernoctaciones/Viajes, 1),
                                                               gasto_viaje = round(Gasto/Viajes*1000000,1),
                                                               gasto_diario = round(Gasto/Pernoctaciones*1000000, 1)) %>%
                                                    ungroup() %>% 
                                                    mutate(gasto_diario= if_else(Pernoctaciones == 0, gasto_viaje, gasto_diario)) %>%  #Excursionistas gasto diario es el gasto por viaje 
                                                    rename ("Gasto*" = Gasto, 
                                                            "Gasto promedio por viaje*" = gasto_viaje,
                                                            "Gasto promedio diario***" = gasto_diario)
                                                
                                                  
                                                  etiquetas_g <- gsub ("anio", "Año", (colnames(tabla)))
                                                  etiquetas_g <- gsub ("trim", "Trimestre", etiquetas_g)
                                                  etiquetas_g <- gsub ("tipo_visitante", "Tipo de visitante", etiquetas_g)
                                                  etiquetas_g <- gsub ("pais_agrupado", "País residencia/destino", etiquetas_g)
                                                  #etiquetas_g <- gsub ("Gasto", "Gasto*", etiquetas_g)
                                                  #etiquetas_g <- gsub ("Gasto*promedio por viaje", "Gasto promedio por viaje*", etiquetas_g)
                                                  #etiquetas_g <- gsub ("Estadía media", "Estadía media**", etiquetas_g)
                                                  #etiquetas_g <- gsub ("Gasto*promedio diario", "Gasto promedio diario***", etiquetas_g)
                                                  
                                                  tabla
                                                  } , rownames= FALSE, colnames = etiquetas_g)
                                  )
                                                
  
# GRAFICOS ####
## 1. Por pais #####
    
datos_grafico1_sel <- eventReactive(input$pais_agrup_graf,{
    req(input$pais_agrup_graf)
  if (input$pais_agrup_graf == "Todos") {
   datos_grafico1_sel <- data_graficos %>% 
     group_by(periodo, turismo) %>%
     summarise(turistas = round(sum(turistas))) 
   } else {
     datos_grafico1_sel <- data_graficos %>% 
     filter(pais_agrupado == input$pais_agrup_graf |
            destino_agrup == input$pais_agrup_graf) %>% 
     group_by(periodo, turismo, pais_agrupado, destino_agrup) %>% 
       summarise(turistas = round(sum(turistas)))
     }
})
  
output$grafico_serie <- renderPlotly({ 
  grafico_1  <- ggplot(datos_grafico1_sel(), aes(periodo, turistas, colour = turismo, group =1, text = paste('Fecha:', format(periodo,"%b%y"),
                                                                                                       '<br>Viajes:',format(turistas,big.mark=".",
                                                                                                                            decimal.mark = ","), 
                                                                                                       '<br>Turismo:',turismo)))+   
    geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5) + 
    geom_line(size = 0.5 , alpha = 0.8) + 
    geom_point(size = 1.0, alpha = 0.8)+ 
    scale_color_manual(values = c(cols_arg2[1], cols_arg2[2])) + 
    scale_x_date(date_breaks = "1 months", date_labels = "%b%y", expand = c(0,10))+ 
    scale_y_continuous(#breaks = seq(min(datos_grafico1_sel()$turistas), max(datos_grafico1_sel()$turistas), by = 200000),
                       n.breaks = 6,
                       labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(legend.position = "bottom", 
          axis.text.x =element_text (size =12, angle=90),
          axis.text.y = element_text(size = 12),
          legend.text = element_text (size =12),
          plot.caption =  element_text (size =12, hjust = 0.0)) +
    labs(title = "Evolución mensual de los viajes de turistas internacionales.",
         subtitle = glue ("Emisivo y receptivo \n Enero 2016-{Mes_ult}-{year_ult}"),
         y = "", 
         x = "", 
         color= "",
         caption =  "Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes,
         en base a información de la Dirección Nacional de Migraciones y la Encuesta de Turismo Internacional." )
  
   ggplotly(grafico_1, tooltip = "text")  %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.6))
 })

output$graf_pais_ti <- renderPlot(graf_pais_ti)

output$graf_via_ti <- renderPlot(graf_via_ti)

## 2. Gasto #####

datos_gasto_grafico2_sel <-eventReactive(list(input$pais_agrup_graf_serie, 
                                              input$tipo_visitante_graf, 
                                              input$periodo),{
  req(input$pais_agrup_graf_serie, input$tipo_visitante_graf,input$periodo)
  datos_gasto_grafico2_sel <- gasto %>% 
    filter(tipo_visitante %in% input$tipo_visitante_graf)
  if (input$periodo == "Anual") {
    datos_gasto_grafico2_sel <- datos_gasto_grafico2_sel %>% 
      mutate(periodo = as.Date(paste0(anio,"-01-01"), "%Y-%m-%d"))
  }
  if (input$pais_agrup_graf_serie != "Todos") {
    datos_gasto_grafico2_sel <- datos_gasto_grafico2_sel %>% 
    filter(pais_agrupado == input$pais_agrup_graf_serie)
    } 
  datos_grafico2_sel <- datos_gasto_grafico2_sel %>% 
    group_by(periodo, residencia) %>% 
    summarise (Viajes = round(sum(casos_ponderados)), 
               Gasto = round(sum(gasto),1),
               Pernoctaciones = round(sum(pernoctes)), 
               Estadia_media= if_else(Viajes == 0,0, round(Pernoctaciones/Viajes, 1)),
               Gasto_viaje= if_else(Viajes == 0,0,round(Gasto/Viajes*1000000,1)),
               Gasto_diario= round(Gasto/Pernoctaciones*1000000, 1)) %>% 
    ungroup() %>% 
    mutate(Gasto_diario= if_else(Pernoctaciones == 0, Gasto_viaje, Gasto_diario)) #Excursionistas gasto diario es el gasto por viaje 
})

output$grafico_gasto <- renderPlotly({ 
  req(input$metrica)
  eje_y <- input$metrica
  grafico_2  <- ggplot(datos_gasto_grafico2_sel(), 
                       aes(x=periodo, y = !!as.name(eje_y), 
                           colour = residencia, 
                           group =1,
                           text = paste('Año:', format(periodo,"%Y"),
                                        '<br>Valor:',format(!!as.name(eje_y),big.mark=".",
                                                            decimal.mark = ","), 
                                        '<br>Turismo:',residencia),
                       ))+
    geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5) + 
    geom_line(linewidth = 1 , alpha = 0.8) + 
    geom_point(size = 1.5, alpha = 0.8)+ 
    scale_color_manual(values = c(cols_arg2[1], cols_arg2[2])) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(.01,.01))+ 
    scale_y_continuous(#breaks = seq(min(datos_grafico1_sel()$turistas), max(datos_grafico1_sel()$turistas), by = 200000),
      n.breaks = 5,
      labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(legend.position = "bottom", 
          axis.text.x =element_text (size =12, angle=90),
          axis.text.y = element_text(size = 12),
          legend.text = element_text (size =12),
          plot.caption =  element_text (size =12, hjust = 0.0)) +
    labs( x = "", 
          color= ""
      )
  
  ggplotly(grafico_2, tooltip = "text")  %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.6))
  
})


  
}




