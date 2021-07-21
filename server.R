#SERVER

function(input, output, session) {

# GRAFICO
  
<<<<<<< HEAD
output$grafico_1 <- renderPlot(grafico_1)
=======
output$fig1 <- renderPlotly(fig1)
>>>>>>> d2dee2062119137555729255221225151a0255fd
                                         
  
    
#### RECEPTIVO

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
      updateSelectInput(session, inputId = "pais", choices = c("Todos", sort(unique(pais_ag()$pais)))) 
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
  
  
  
  # Actualizacion de choices de prov al cambiar input. 
  
  
  observeEvent(via(), {
    if (input$limita == "Todos" & input$via == "Todos") {
      updateSelectInput(session, inputId = "prov", choices = c("Todos",
                                                               unique(data_receptivo$prov))) 
    } else {
      updateSelectInput(session, inputId = "prov", choices = c("Todos",unique(via()$prov))) 
    }
  })
  
  
  # Reactivo de prov segun input .
  
  prov <- reactive({
    req(input$prov)
    if (input$prov == "Todos") {
      prov <- via()
    } else {
      prov <- via()[via()$prov == input$prov,  ]
    }
  })
  
  # Actualizacion de choices de limita al cambiar input. 
  
  observeEvent(prov(), {
    if (input$via == "Todos" & input$prov == "Todos") {
      updateSelectInput(session, inputId = "limita", choices = c("Todos",
                                                                 unique(data_receptivo$limita))) 
    } else {
      updateSelectInput(session, inputId = "limita", choices = c("Todos",unique(prov()$limita))) 
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
    if (input$limita == "Todos" & input$via == "Todos" & input$prov == "Todos") {
      updateSelectInput(session, inputId = "paso_publ", choices = c("Todos",
                                                                    unique(data_receptivo$paso_publ))) 
    } else {
      updateSelectInput(session, inputId = "paso_publ", choices = c("Todos",unique(limita()$paso_publ))) 
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
  
  output$table_receptivo <- DT::renderDT(server = FALSE,
    
    DT::datatable(extensions = 'Buttons', options = list(
      dom = 'frtipB',
      buttons = 
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel'),
          text = 'Download'
        ))),    
      {
        tabla <- paso()
        req(input$year)
        if (all(input$year != "Todos")) {
          tabla <- tabla[tabla$year %in% input$year,]		
        }
        req(input$mes)
        if (all(input$mes != "Todos")) {
          tabla <- tabla[tabla$mes %in% input$mes,]		
        }
        
        tabla <- tabla %>%
          group_by_at(.vars = c( "year", input$agrup)) %>%
          summarise ("Turistas no residentes" = round(sum(turistas))) 
        
        #etiquetas receptivo según selección en ui.
        
        etiquetas <- gsub ("year", "Año", (colnames(tabla)))
        etiquetas <- gsub ("mes", "Mes", etiquetas)
        etiquetas <- gsub ("via", "Vía", etiquetas)
        etiquetas <- gsub ("pais_agrupado", "País de residencia (agrup.)", etiquetas)
        etiquetas <- gsub ("pais" , "País de residencia", etiquetas)
        etiquetas <- gsub ("paso_publ" , "Paso", etiquetas)
        etiquetas <- gsub ("prov", "Provincia del paso", etiquetas)
        etiquetas <- gsub ("limita" ,"Limita con", etiquetas)
        
        tabla
      }, rownames= FALSE, colnames = etiquetas)
  )
  

  
#### EMISIVO
  
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
                                                               unique(data_emisivo$prov))) 
    } else {
      updateSelectInput(session, inputId = "prov_e", choices = c("Todos",unique(via_e()$prov))) 
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
                                                                 unique(data_emisivo$limita))) 
    } else {
      updateSelectInput(session, inputId = "limita_e", choices = c("Todos",unique(prov_e()$limita))) 
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
                                                                    unique(data_emisivo$paso_publ))) 
    } else {
      updateSelectInput(session, inputId = "paso_publ_e", choices = c("Todos",unique(limita_e()$paso_publ))) 
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
  output$table_emisivo <- DT::renderDataTable(server = FALSE,
    
    DT::datatable(extensions = 'Buttons', options = list(
      dom = 'frtipB',
      buttons = 
        list('copy', list(
          extend = 'collection',
          buttons = c('csv', 'excel'),
          text = 'Download'
        ))),
      {
        tabla_e <- paso_e()
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
        
        
        tabla_e <- tabla_e %>%
          group_by_at(.vars = c( "year", input$agrup_e)) %>%
          summarise ("Turistas residentes" = round(sum(turistas))) 
        
        #etiquetas emisivo según selección en ui.
        
        etiquetas_e <- gsub ("year", "Año", (colnames(tabla_e)))
        etiquetas_e <- gsub ("mes", "Mes", etiquetas_e)
        etiquetas_e <- gsub ("via", "Vía", etiquetas_e)
        etiquetas_e <- gsub ("destino_agrup" , "Destino principal", etiquetas_e)
        etiquetas_e <- gsub ("paso_publ" , "Paso", etiquetas_e)
        etiquetas_e <- gsub ("prov", "Provincia del paso", etiquetas_e)
        etiquetas_e <- gsub ("limita" ,"Limita con", etiquetas_e)
        
        tabla_e
      }, rownames= FALSE, colnames = etiquetas_e)
  )
    
  
}




