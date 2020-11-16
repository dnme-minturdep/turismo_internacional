

function(input, output) {
  
  # TABLA RECEPTIVO
  output$table_receptivo <- DT::renderDataTable(
    
  DT::datatable(    
    {
    
    data_receptivo 
    
    if (input$year != "Todos") {
      data_receptivo <- data_receptivo[data_receptivo$year == input$year,]
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
    
    if (input$year != "Todos") {
      data_emisivo <- data_emisivo[data_emisivo$year == input$year,]
    }
    if (input$destino != "Todos") {
      data_emisivo <- data_emisivo[data_emisivo$destino == input$destino,]
    }
    data_emisivo
  }))
  
  
}
