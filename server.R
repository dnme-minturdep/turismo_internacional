

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
      data_receptivo <- data_receptivo[data_receptivo$year == input$year,]
    }
    if (input$mes != "Todos") {
        data_receptivo <- data_receptivo[data_receptivo$mes == input$mes,]
    }  
    if (input$pais != "Todos") {
      data_receptivo <- data_receptivo[data_receptivo$pais == input$pais,]
    }
    data_receptivo
  }))
  
  
  # TABLA EMISIVO
  output$table_emisivo <- DT::renderDataTable(
    
  DT::datatable(
    {
    
    data_emisivo 
    
    if (input$year_e != "Todos") {
      data_emisivo <- data_emisivo[data_emisivo$year == input$year_e,]
    }
    if (input$mes_e != "Todos") {
        data_emisivo <- data_emisivo[data_emisivo$mes == input$mes_e,]
    }
    if (input$destino != "Todos") {
      data_emisivo <- data_emisivo[data_emisivo$destino == input$destino,]
    }
    data_emisivo
  }))
  
  
}
