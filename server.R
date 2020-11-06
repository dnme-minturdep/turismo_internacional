

function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data.table::fread("data/turistas_internacionales_con destino.csv") 
  #  if (input$man != "All") {
  #    data <- data[data$manufacturer == input$man,]
  #  }
  #  if (input$cyl != "All") {
  #    data <- data[data$cyl == input$cyl,]
  #  }
  #  if (input$trans != "All") {
  #    data <- data[data$trans == input$trans,]
  #  }
    data
  }))
  
}