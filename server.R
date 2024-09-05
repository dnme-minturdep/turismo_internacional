#SERVER

function(input, output, session) {
  
  # GRAFICO ####
  
  output$fig1 <- renderPlotly(fig1)
  
  # RESUMEN ####
  
  output$boxreceptivo <- renderValueBox({
    
    value <- lbl_int(dato_mensual$casos[dato_mensual$turismo_internac == "Receptivo"])
    valueBox(value = value, 
             subtitle = "Viajes de turistas",
             icon = "plane-arrival",
             color = dnmye_colores("azul verde")
             )
  })
  
  output$boxreceptivo_var <- renderValueBox({
    
    value <- lbl_percent(dato_mensual$var[dato_mensual$turismo_internac == "Receptivo"])
    valueBox(value = value, 
             subtitle = "Var.ia %",
             icon = "chart-line",
             color = dnmye_colores("azul verde")
             )
  })
  
  output$boxemisivo <- renderValueBox({
    
    value <- lbl_int(dato_mensual$casos[dato_mensual$turismo_internac == "Emisivo"])
    valueBox(value = value, 
             subtitle = "Viajes de turistas",
             icon = "plane-departure",
             color = dnmye_colores("rosa")
             )
  })
  
  output$boxemisivo_var <- renderValueBox({
    
    value <- lbl_percent(dato_mensual$var[dato_mensual$turismo_internac == "Emisivo"])
    valueBox(value = value, 
             subtitle = "Var.ia %",
             icon = "chart-line",
             color = dnmye_colores("rosa")
    )
  })
  
  
  
  output$boxreceptivo_ac <- renderValueBox({
    
    value <- lbl_int(dato_acumulado$casos[dato_acumulado$turismo_internac == "Receptivo"])
    valueBox(value = value, 
             subtitle = "Viajes de turistas",
             icon = "plane-arrival",
             color = dnmye_colores("azul verde")
    )
  })
  
  output$boxreceptivo_var_ac <- renderValueBox({
    
    value <- lbl_percent(dato_acumulado$var[dato_acumulado$turismo_internac == "Receptivo"])
    valueBox(value = value, 
             subtitle = "Var.ia %",
             icon = "chart-line",
             color = dnmye_colores("azul verde")
    )
  })
  
  output$boxemisivo_ac <- renderValueBox({
    
    value <- lbl_int(dato_acumulado$casos[dato_acumulado$turismo_internac == "Emisivo"])
    valueBox(value = value, 
             subtitle = "Viajes de turistas",
             icon = "plane-departure",
             color = dnmye_colores("rosa")
             #color = "#EE3D8F"
    )
  })
  
  output$boxemisivo_var_ac <- renderValueBox({
    
    value <- lbl_percent(dato_acumulado$var[dato_acumulado$turismo_internac == "Emisivo"])
    valueBox(value = value, 
             subtitle = "Var.ia %",
             icon = "chart-line",
             color = dnmye_colores("rosa")
    )
    
  })
  
  
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
  
  
  tabla_final_rec <- reactive({
    
    tabla <- paso()
    req(input$tipo_visitante)
    tabla <- tabla[tabla$tipo_visitante %in% input$tipo_visitante,]		
    
    req(input$anio)
    if (all(input$anio != "Todos")) {
      tabla <- tabla[tabla$anio %in% input$anio,]		
    }
    req(input$mes)
    if (all(input$mes != "Todos")) {
      tabla <- tabla[tabla$mes %in% input$mes,]		
    }
    
    tabla <- tabla %>%
      group_by_at(.vars = c( "anio", input$agrup)) %>%
      summarise (viajes = sum(turistas)) %>% 
      ungroup()
    
    
    #opcion decimales
    # req(input$round)
    # if (input$round == "Sin decimales"){
    #   tabla <- tabla %>%
    #     mutate(viajes = round(viajes))
    # }
    
    #opcion tipo visitante:
    
    req(input$tipo_visitante)
    if (all(input$tipo_visitante == "Turistas")){
      tabla <- tabla %>%
        rename ("Viajes de turistas" = viajes) 
    } else if (all(input$tipo_visitante == "Excursionistas")){
      tabla <- tabla %>%
        rename ("Viajes de excursionistas" = viajes)  
    } else {
      tabla <- tabla %>%
        rename ("Viajes de visitantes" = viajes)   
    }
    
    tabla <- tabla %>% 
      rename(any_of(c("Año"="anio",
                      "Mes"="mes",
                      "Trimestre"="trim",
                      "Vía"="via",
                      "Ruta Natural"="ruta_natural",
                      "País de residencia (agrup.)"="pais_agrupado",
                      "País de residencia"="pais",
                      "Paso"="paso_publ",
                      "Provincia del paso"="prov",
                      "Limita con"="limita",
                      "Género"="sexo",
                      "Tramos de edad"="grupoetario",
                      "tipo_visitante"="Tipo de visitante"
                      )))
    
    #etiquetas receptivo según selección en ui.
    # etiquetas <- gsub ("anio", "Año", (colnames(tabla)))
    # etiquetas <- gsub ("mes", "Mes", etiquetas)
    # etiquetas <- gsub ("trim", "Trimestre", etiquetas)
    # etiquetas <- gsub ("via", "Vía", etiquetas)
    # etiquetas <- gsub ("ruta_natural", "Ruta natural", etiquetas)
    # etiquetas <- gsub ("pais_agrupado", "País de residencia (agrup.)", etiquetas)
    # etiquetas <- gsub ("pais" , "País de residencia", etiquetas)
    # etiquetas <- gsub ("paso_publ" , "Paso", etiquetas)
    # etiquetas <- gsub ("prov", "Provincia del paso", etiquetas)
    # etiquetas <- gsub ("limita" ,"Limita con", etiquetas)
    # etiquetas <- gsub ("sexo", "Género", etiquetas)
    # etiquetas <- gsub ("grupoetario", "Tramos de edad", etiquetas)
    # etiquetas <- gsub ("tipo_visitante" ,"Tipo de visitante", etiquetas)
  })
  
  waiter_hide()
  
  col_format <- reactive({
    
    tabla_final_rec() %>% select(starts_with("Viajes de")) %>% colnames()
    })
  
  output$titulo <- renderText({seleccion_vis()})
  output$table_receptivo <- renderDT(server = FALSE,
                                         
                                         datatable(#extensions = 'Buttons',
                                                       options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                                      dom = 'lfrtipB'
                                                                      # buttons = list('copy',
                                                                      #                list(
                                                                      #                  extend = 'collection',
                                                                      #                  buttons = c('csv', 'excel'),
                                                                      #                  text = 'Download'
                                                                      #                ))
                                                                      ),
                                                       tabla_final_rec()
                                                       , rownames= FALSE#, colnames = etiquetas
                                                       ) %>% 
                                       formatRound(columns = col_format(), mark = ".", digits = 0)
  )
  
  output$downloadExcelRec <- downloadHandler(
    filename = function() {
      "turismo_receptivo.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(tabla_final_rec(), file)
    }
  )
  
  output$downloadCSVRec <- downloadHandler(
    filename = function() {
      "turismo_receptivo.csv"
    },
    content = function(file) {
      write_csv(tabla_final_rec(), file)
    }
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
  
  
  tabla_final_emi <- reactive({
    
    
    tabla_e <- paso_e()
    req(input$tipo_visitante_e)
    tabla_e <- tabla_e[tabla_e$tipo_visitante %in% input$tipo_visitante_e,]		
    
    req(input$anio_e)
    if (all(input$anio_e != "Todos")) {
      tabla_e <- tabla_e[tabla_e$anio %in% input$anio_e,]		
    }
    req(input$mes_e)
    if (all(input$mes_e != "Todos")) {
      tabla_e <- tabla_e[tabla_e$mes %in% input$mes_e,]		
    }
    req(input$destino)
    if (all(input$destino != "Todos")) {
      tabla_e <- tabla_e[tabla_e$destino == input$destino,]		
    }
    
    tabla_e <- tabla_e %>%
      group_by_at(.vars = c( "anio", input$agrup_e)) %>%
      summarise (viajes = sum(turistas)) %>% 
      ungroup()
    
    
    #opcion decimales
    # req(input$round_e)
    # if (input$round_e == "Sin decimales"){
    #   tabla_e <- tabla_e %>%
    #     mutate(viajes = round(viajes))
    # }
    
    #opcion tipo visitante:
    
    req(input$tipo_visitante_e)
    if (all(input$tipo_visitante_e == "Turistas")){
      tabla_e <- tabla_e %>%
        rename ("Viajes de turistas" = viajes) 
    } else if (all(input$tipo_visitante_e == "Excursionistas")){
      tabla_e <- tabla_e %>%
        rename ("Viajes de excursionistas" = viajes)  
    } else {
      tabla_e <- tabla_e %>%
        rename ("Viajes de visitantes" = viajes)   
    }
    
    
    tabla_e <- tabla_e %>% 
      rename(any_of(c("Año"="anio",
                      "Mes"="mes",
                      "Trimestre"="trim",
                      "Vía"="via",
                      "Destino principal"="destino_agrup",
                      "Paso"="paso_publ",
                      "Provincia del paso"="prov",
                      "Limita con"="limita",
                      "Género"="sexo",
                      "Tramos de edad"="grupoetario",
                      "tipo_visitante"="Tipo de visitante"
      )))
    
    #if (all(input$tipo_visitante_e == "Turistas")){
    #  tabla_e <- tabla_e %>%
    #    group_by_at(.vars = c( "anio", input$agrup_e)) %>%
    #    summarise ("Viajes de turistas" = round(sum(turistas))) 
    #} else if (all(input$tipo_visitante_e == "Excursionistas")){
    #  tabla_e <- tabla_e %>%
    #    group_by_at(.vars = c( "anio", input$agrup_e)) %>%
    #    summarise ("Viajes de excursionistas" = round(sum(turistas))) 
    #} else {
    #  tabla_e <- tabla_e %>%
    #    group_by_at(.vars = c( "anio", input$agrup_e)) %>%
    #    summarise ("Viajes de visitantes" = round(sum(turistas))) 
    #} 
    
    
    ##etiquetas emisivo según selección en ui.
    
    # etiquetas_e <- gsub ("anio", "Año", (colnames(tabla_e)))
    # etiquetas_e <- gsub ("mes", "Mes", etiquetas_e)
    # etiquetas_e <- gsub ("trim", "Trimestre", etiquetas_e)
    # etiquetas_e <- gsub ("via", "Vía", etiquetas_e)
    # etiquetas_e <- gsub ("destino_agrup" , "Destino principal", etiquetas_e)
    # etiquetas_e <- gsub ("paso_publ" , "Paso", etiquetas_e)
    # etiquetas_e <- gsub ("prov", "Provincia del paso", etiquetas_e)
    # etiquetas_e <- gsub ("limita" ,"Limita con", etiquetas_e)
    # etiquetas_e <- gsub ("sexo", "Género", etiquetas_e)
    # etiquetas_e <- gsub ("grupoetario", "Tramos de edad", etiquetas_e)
    # etiquetas_e <- gsub ("tipo_visitante" ,"Tipo de visitante", etiquetas_e)
    
  })
  
  col_format_e <- reactive({
    
    tabla_final_emi() %>% select(starts_with("Viajes de")) %>% colnames()
  })
  
  output$titulo_e <- renderText({seleccion_vis_e()})
  
  output$table_emisivo <- renderDataTable(server = FALSE,
                                              
                                              datatable(#extensions = 'Buttons', 
                                                            options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                                           dom = 'lfrtipB'
                                                                           # buttons = list('copy',
                                                                           #                list(
                                                                           #                  extend = 'collection',
                                                                           #                  buttons = c('csv', 'excel'),
                                                                           #                  text = 'Download'
                                                                           #                ))
                                                                           ),
                                                            
                                                              tabla_final_emi()
                                                            
                                                            , rownames= FALSE) %>% 
                                            formatRound(columns = col_format_e(), mark = ".", digits =  0)
  )
  
  output$downloadExcelEmi <- downloadHandler(
    filename = function() {
      "turismo_emisivo.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(tabla_final_emi(), file)
    }
  )
  
  output$downloadCSVEmi <- downloadHandler(
    filename = function() {
      "turismo_emisivo.csv"
    },
    content = function(file) {
      write_csv(tabla_final_emi(), file)
    }
  )
  
# ETI ####  
  
  #  Reactivo de mes segun input en paso ((####SEGUIR))
  
  paso_select <- reactive ({
      paso_select <- localidad %>% filter(paso_final == input$paso)
})
  
  
  #al cambiar input de paso actualizo choices de pais
  
  observeEvent(paso_select(), {
      updateSelectInput(session, inputId = "pais_origen", choices = c("Todos",
                                                               sort(unique(paso_select()$pais_origen),)),
                        selected = "Todos")
    
  })
  
  #al cambiar input de paso actualizo choices de mes:
  
  observeEvent(paso_select(), {
    updateSelectInput(session, inputId = "mes_encuesta", choices = c("Todos",
                                                                    unique(as.character(paso_select()$mes))),
                      selected = "Todos")
    
  })
  
  # Reactivo de tabla_pais segun input.
  
  tabla_pais <- reactive({
    req(input$pais_origen)
    if (input$pais_origen == "Todos") {
      tabla_pais <- paso_select()
    } else {
      tabla_pais <- paso_select()[paso_select()$pais_origen == input$pais_origen,  ]
    }
  })
  
  
  # Reactivo de tabla_pais segun input.
  
  # tabla_pais_mes <- reactive({
  #   req(input$mes_encuesta)
  #   if (input$mes_encuesta == "Todos") {
  #     tabla_pais_mes <- tabla_pais()
  #   } else {
  #     tabla_pais_mes <- tabla_pais()[tabla_pais()$mes == input$mes_encuesta,  ]
  #   }
  # })
  
  tabla_final_perfil <- reactive({
    
    tabla <- tabla_pais()
    #filtros:
    req(input$paso)
    tabla <- tabla[tabla$paso_final == input$paso,]		
    
    req(input$anio_encuesta)
    if (all(input$anio_encuesta != "Todos")) {
      tabla <- tabla[tabla$anio %in% input$anio_encuesta,]		
    }
    req(input$trim)
    if (all(input$trim != "Todos")) {
      tabla <- tabla[tabla$trim %in% input$trim,]		
    }
    # req(input$mes_encuesta)
    # if (all(input$mes_encuesta != "Todos")) {
    #   tabla <- tabla[tabla$mes %in% input$mes_encuesta,]		
    # }
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
      group_by_at(.vars = c( "id", input$agrup_p)) %>% #agrupo por id para no duplicar casos por localida
      summarise (turistas = first(wpf),
                 casos =  first(p18_1), 
                 noches = sum(noches * wpf),
                 gasto = sum(gasto * wpf)) %>%
      group_by_at(.vars = c( input$agrup_p)) %>%
      summarise (Turistas = sum(turistas),
                 Noches = sum(noches),
                 Gasto = round(sum(gasto),1),
                 Casos_Muestrales = sum(casos)) %>% 
      ungroup()
    
    #saco gasto al mostrar por mes
    # if (any(input$agrup_p == "mes")) {
    #   tabla <- tabla %>% select (-Gasto)
    # }
    
    tabla <- tabla %>% 
      rename(any_of(c(
        "Año"="anio",
        #"Mes"="mes",
        "Trimestre"="trim",
        "Provincia visitada"="provincia",
        "Ciudad visitada"="ciudad",
        "País de residencia"="pais_origen",
        "Tipo alojamiento principal en el país"="alojamiento",
        "Motivo de viaje"="motivo_viaje",
        "Turistas no residentes*"="Turistas",
        "Gasto en US$**"="Gasto",
        "Casos Muestrales***"="Casos_Muestrales"
      )
      ))
    
    #etiquetas receptivo según selección en ui.
    
    # etiquetas <- gsub ("anio", "Año", (colnames(tabla)))
    # etiquetas <- gsub ("mes", "Mes", etiquetas)
    # etiquetas <- gsub ("trim", "Trimestre", etiquetas)
    # etiquetas <- gsub ("provincia", "Provincia visitada", etiquetas)
    # etiquetas <- gsub ("ciudad", "Ciudad visitada", etiquetas)
    # etiquetas <- gsub ("pais_origen", "País de residencia", etiquetas)
    # etiquetas <- gsub ("alojamiento", "Tipo alojamiento principal en el país", etiquetas)
    # etiquetas <- gsub ("motivo_viaje", "Motivo de viaje", etiquetas)
    # etiquetas <- gsub ("Turistas", "Turistas no residentes*", etiquetas)
    # etiquetas <- gsub ("Gasto", "Gasto en US$**", etiquetas)
    # etiquetas <- gsub ("Casos_Muestrales", "Casos Muestrales***", etiquetas)
    
  })
  output$tabla_eti<- renderDT(server = FALSE,
                                  
                                  datatable(#extensions = 'Buttons',
                                                options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                               dom = 'lfrtipB'#,
                                                               # buttons = list('copy',
                                                               #                list(
                                                               #                  extend = 'collection',
                                                               #                  buttons = c('csv', 'excel'),
                                                               #                  text = 'Download'
                                                               #                ))
                                                               ),   
                                                
                                                tabla_final_perfil()
                                                
                                                , rownames= FALSE) %>% 
                                formatRound(columns = c("Turistas no residentes*",
                                                        "Casos Muestrales***"), mark = ".", digits = 0) %>% 
                                formatCurrency(columns = c("Gasto en US$**"), mark = ".", dec.mark = ",", digits = 1, currency = "$ ")
  )
  
  output$downloadExcelPerfil<- downloadHandler(
    filename = function() {
      "perfil_receptivo.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(tabla_final_perfil(), file)
    }
  )
  
  output$downloadCSVPerfil <- downloadHandler(
    filename = function() {
      "perfil_receptivo.csv"
    },
    content = function(file) {
      write_csv(tabla_final_perfil(), file)
    }
  )
  
  
  
  tabla_final_serie <- reactive({
    
    tabla <- gasto
    #filtros:
    req(input$tipo_turismo_g)
    tabla <- tabla[tabla$residencia == input$tipo_turismo_g,]	
    
    req(input$tipo_visitante_g)
    tabla <- tabla[tabla$tipo_visitante %in% input$tipo_visitante_g,]		
    
    req(input$anio_g)
    if (all(input$anio_g != "Todos")) {
      tabla <- tabla[tabla$anio %in% input$anio_g,]		
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
      summarise (Viajes = sum(casos_ponderados), 
                 Gasto = sum(gasto),
                 Pernoctaciones = sum(pernoctes), 
                 estadia = round(Pernoctaciones/Viajes, 1),
                 gasto_viaje = round(Gasto/Viajes*1000000,1),
                 gasto_diario = round(Gasto/Pernoctaciones*1000000, 1)) %>%
      ungroup() 
    
    if (any(input$agrup_g == "trim")) {
      tabla <- tabla %>%
        mutate(estadia= case_when(anio == 2020 & trim %in% c("3", "4") ~ NA_real_,
                                  anio == 2021 & trim %in% c("1","2","3") ~ NA_real_,
                                  TRUE ~ estadia))
    }
    
    # Redondeo Basico (saqué opción de no redondear)
    #req(input$round_s)
    #if (input$round_s == "Básico") {
    tabla <- tabla %>% 
      mutate(Viajes = round(Viajes), 
             Gasto = round(Gasto,1),
             Pernoctaciones = round(Pernoctaciones), 
      ) 
    #}
    
    tabla <- tabla %>% 
      mutate(gasto_diario= if_else(Pernoctaciones == 0, gasto_viaje, gasto_diario)) %>%  #Excursionistas gasto diario es el gasto por viaje 
      rename ("Gasto en millones de US$" = Gasto, 
              "Gasto promedio por viaje en US$" = gasto_viaje,
              "Gasto promedio diario en US$" = gasto_diario, 
              "Estadía media en noches" = estadia, 
              "Año"= anio
      )
    
    
    tabla <- tabla %>% 
      rename(any_of(c(
        "Trimestre"="trim",
        "Tipo de visitantes"="tipo_visitante",
        "País residencia/destino"
      )))
    
    #etiquetas de columnas opcionales segun seleccion: 
    
    # etiquetas <- gsub ("trim", "Trimestre", colnames(tabla))
    # etiquetas <- gsub ("tipo_visitante", "Tipo de visitante", etiquetas)
    # etiquetas <- gsub ("pais_agrupado", "País residencia/destino", etiquetas)
    
    
  })
  
  
  # SERIE ####  
  
  output$tabla_serie<- renderDT(server = FALSE,
                                
                                datatable(#extensions = 'Buttons',
                                  options = list(lengthMenu = c(12, 25, 50), pageLength = 12, 
                                                 dom = 'lfrtipB'#,
                                                 # buttons = list('copy',
                                                 #                list(
                                                 #                  extend = 'collection',
                                                 #                  buttons = c('csv', 'excel'),
                                                 #                  text = 'Download'
                                                 #                ))
                                  ),
                                  
                                  tabla_final_serie()
                                  
                                  , rownames= FALSE) %>% 
                                  formatCurrency(columns = c(
                                    "Gasto en millones de US$",
                                    "Gasto promedio por viaje en US$",
                                    "Gasto promedio diario en US$"
                                  ), mark = ".", dec.mark = ",", digits = 1, currency = "$ "
                                  ) %>% 
                                  formatRound(columns = c("Estadía media en noches"),
                                              mark = ".", dec.mark = ",", digits = 1) %>% 
                                  formatRound(columns = c("Viajes", "Pernoctaciones"),
                                              mark = ".", digits = 0)
  )
  
  output$downloadExcelSerie <- downloadHandler(
    filename = function() {
      "ti_indicadores.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(tabla_final_serie(), file)
    }
  )
  
  output$downloadCSVSerie <- downloadHandler(
    filename = function() {
      "ti_indicadores.csv"
    },
    content = function(file) {
      write_csv(tabla_final_serie(), file)
    }
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
  grafico_1  <- ggplot(datos_grafico1_sel(), aes(periodo, turistas, colour = turismo, group =1, 
                                                 text = paste('Fecha:', format(periodo,"%b%y"),
                                                              '<br>Viajes:',format(turistas,big.mark=".",
                                                                                   decimal.mark = ","),
                                                              '<br>Turismo:',turismo)))+   
    geom_hline(yintercept = 0, color = "grey", alpha =0.7, linewidth = 0.5) + 
    geom_line(linewidth = 0.5 , alpha = 0.8) + 
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
         subtitle = glue ("Emisivo y receptivo \n Enero 2016-{Mes_ult}-{anio_ult}"),
         y = "", 
         x = "", 
         color= "",
         caption =  "Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes,
         en base a información de la Dirección Nacional de Migraciones y la Encuesta de Turismo Internacional." )
  
  
ggplotly(grafico_1, tooltip = "text")  %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.6))
 })

output$graf_pais_ti <- renderPlotly(ggplotly(graf_pais_ti, tooltip = "text"))

output$graf_via_ti <- renderPlotly(ggplotly(graf_via_ti, tooltip = "text"))


## 2. Gasto #####

datos_gasto_grafico2_sel <-eventReactive(
  list(input$pais_agrup_graf_serie, 
       input$tipo_visitante_graf, 
       input$periodo),{
         req(input$pais_agrup_graf_serie, input$tipo_visitante_graf,input$periodo)
         
         datos_gasto_grafico2_sel <- gasto %>% 
           filter(tipo_visitante %in% input$tipo_visitante_graf)
         
         if (input$pais_agrup_graf_serie != "Todos") {
           datos_gasto_grafico2_sel <- datos_gasto_grafico2_sel %>% 
             filter(pais_agrupado == input$pais_agrup_graf_serie)
         }
         
         if (input$periodo == "Anual") {
           datos_gasto_grafico2_sel <- datos_gasto_grafico2_sel %>% 
             mutate(periodo = as.Date(paste0(anio,"-01-01"), "%Y-%m-%d")) %>% 
             group_by(periodo, residencia) %>% 
             summarise (Viajes = round(sum(casos_ponderados)), 
                        Gasto = round(sum(gasto),1),
                        Pernoctaciones = round(sum(pernoctes)), 
                        Estadia_media= if_else(Viajes == 0,0, round(Pernoctaciones/Viajes, 1)),
                        Gasto_viaje= if_else(Viajes == 0,0,round(Gasto/Viajes*1000000,1)),
                        Gasto_diario= round(Gasto/Pernoctaciones*1000000, 1)) %>% 
             ungroup() %>% 
             mutate(trim = "--",
                    Gasto_diario= if_else(Pernoctaciones == 0, Gasto_viaje, Gasto_diario), #Excursionistas gasto diario es el gasto por viaje 
             )
         } else {
           datos_gasto_grafico2_sel <- datos_gasto_grafico2_sel %>% 
             group_by(periodo, residencia, trim) %>% 
             summarise (Viajes = round(sum(casos_ponderados)), 
                        Gasto = round(sum(gasto),1),
                        Pernoctaciones = round(sum(pernoctes)), 
                        Estadia_media= if_else(Viajes == 0,0, round(Pernoctaciones/Viajes, 1)),
                        Gasto_viaje= if_else(Viajes == 0,0,round(Gasto/Viajes*1000000,1)),
                        Gasto_diario= round(Gasto/Pernoctaciones*1000000, 1)) %>% 
             ungroup() %>% 
             mutate(Gasto_diario= if_else(Pernoctaciones == 0, Gasto_viaje, Gasto_diario), #Excursionistas gasto diario es el gasto por viaje 
                    Estadia_media = if_else( periodo >= "2020-07-01" & 
                                               periodo < "2021-10-01",
                                             0,
                                             Estadia_media))
         }
       }
)

output$grafico_gasto <- renderPlotly({ 
  req(input$metrica)
  eje_y <- input$metrica
  grafico_2  <- ggplot(datos_gasto_grafico2_sel(), 
                       aes(x=periodo, y = !!as.name(eje_y), 
                           colour = residencia, 
                           group =1,
                           text = if (input$periodo == "Anual") {
                             paste('Año:', format(periodo,"%Y"),
                                   '<br>Valor:',format(!!as.name(eje_y),big.mark=".",
                                                       decimal.mark = ","), 
                                   '<br>Turismo:',residencia)
                           }else{
                             paste('Año:', format(periodo,"%Y"),
                                   '<br>Trimestre:', trim,
                                   '<br>Valor:',format(!!as.name(eje_y),big.mark=".",
                                                       decimal.mark = ","), 
                                   '<br>Turismo:',residencia)
                             
                           }
                       ))+
    geom_hline(yintercept = 0, color = "grey", alpha =0.7, linewidth = 0.5) + 
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

# METODOLOGIA ####

output$aperturas_variables <- renderTable({
  aperturas
})

}




