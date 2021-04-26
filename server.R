

function(input, output) {
  
  # TABLA RECEPTIVO
  output$table_receptivo <- DT::renderDataTable(

      
  DT::datatable(extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))),    
    {
    
    data_receptivo 
    
    if (input$year != "Todos") {
      data_receptivo <- data_receptivo[data_receptivo$year %in% input$year,]		
    }
    if (input$mes != "Todos") {
       data_receptivo <- data_receptivo[data_receptivo$mes %in% input$mes,]	
    }  
    if (input$paso_publ != "Todos") {
      data_receptivo <- data_receptivo[data_receptivo$paso_publ == input$paso_publ,]
    }
    if (input$via != "Todas") {
        data_receptivo <- data_receptivo[data_receptivo$via == input$via,]
    }  
    if (input$pais != "Todos") {
        data_receptivo <- data_receptivo[data_receptivo$pais == input$pais,]
    }
    
    if (input$paso_publ != "Todos") {
        data_receptivo <- data_receptivo[data_receptivo$paso_publ == input$paso_publ,]
    }
    if (input$prov != "Todos") {
        data_receptivo <- data_receptivo[data_receptivo$prov == input$prov,]
    }
    if (input$limita != "Todos") {
        data_receptivo <- data_receptivo[data_receptivo$limita == input$limita,]
    }
    data_receptivo <- data_receptivo %>%
        group_by_at(.vars = c(input$agrup)) %>%
      summarise(round(sum(turistas))) 
    
    
    data_receptivo
  }))
  
 
  
  
  
  # TABLA EMISIVO
  output$table_emisivo <- DT::renderDataTable(
    
  DT::datatable(
    {
    
    data_emisivo 
    
      if (input$year_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$year %in% input$year_e,]		
      }
      if (input$mes_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$mes %in% input$mes_e,]	
      }  
      if (input$paso_publ_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$paso_publ == input$paso_publ_e,]
      }
      if (input$via_e != "Todas") {
        data_emisivo <- data_emisivo[data_emisivo$via == input$via_e,]
      }  
      if (input$destino != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$destino_agrup == input$destino,]
      }
      
      if (input$paso_publ_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$paso_publ == input$paso_publ_e,]
      }
      if (input$prov_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$prov == input$prov_e,]
      }
      if (input$limita_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$limita == input$limita_e,]
      }
      data_emisivo <- data_emisivo %>%
        group_by_at(.vars = c(input$agrup_e)) %>%
        summarise(round(sum(turistas))) 
      
    data_emisivo
  }))
  
  
}
