if (colnames(data_emisivo)[3] == "destino_agrup") {data_emisivo <- rename(data_emisivo,"Destino principal" = destino_agrup)}
if (colnames(data_emisivo)[4] == "paso_publ") {data_emisivo <- rename(data_emisivo, Paso = paso_publ)}
if (colnames(data_emisivo)[5] == "prov") {data_emisivo <- rename(data_emisivo, Provincia = prov)}
if (colnames(data_emisivo)[6] == "limita") {data_emisivo <- rename(data_emisivo, Limita = limita)}

#"year" = "Año", "mes" = "Mes", "via" = "Vía", "destino_agrup" = "Destino",
#"paso_publ" = "paso", "prov" = "Provincia", "limita" = "Limita ", "round(sum(turistas))" = "Turistas")

#colnames = c("Año", "Mes", ", Vía", "Destino",
#          "Paso", "Provincia", "Limita", "Turistas")

#rename (year = Año, mes = Mes, via = Vía, destino_agrup = Destino,
#        paso_publ = paso, prov = Provincia, limita = Limita )

#if (input$agrup_e %in% year) {
#  data_emisivo <- rename(data_emisivo, Año = year)
#}

if (colnames(data_emisivo) %in%  "year") {data_emisivo <- rename(data_emisivo, Año = year)}
if (colnames(data_emisivo) %in%  "via") {data_emisivo <- rename(data_emisivo, Vía = via)}

data_emisivo2 <- data_emisivo
if (colnames(data_emisivo) %in%  "year") {data_emisivo2 <- rename(data_emisivo2, Año = year)}




  
  
  
  saber si una tabla contiene determinadas variables. 

colnames(data_emisivo)
colnames(data_emisivo) %in%  "year"
colnames(data_emisivo) ==  "year"

year = Año, mes = Mes, via = Vía, destino_agrup = Destino,
#paso_publ = paso, prov = Provincia, limita = Limita , round(sum(turistas)) = Turistas)


etiquetas_e <- gsub ("year", "Año", (colnames(data_emisivo)))
etiquetas_e <- gsub ("mes", "Mes", etiquetas_e)
etiquetas_e <- gsub ("via", "Vía", etiquetas_e)
etiquetas_e <- gsub ("destino_agrup" , "Destino principal", etiquetas_e)
etiquetas_e <- gsub ("paso_publ" , "Paso", etiquetas_e)
etiquetas_e <- gsub ("prov", "Provincia del paso", etiquetas_e)
etiquetas_e <- gsub ("limita" ,"Limita con", etiquetas_e)
etiquetas_e <- gsub ("round(sum(turistas))", "Turistas", etiquetas_e)

etiquetas_e <- gsub ("year", "Año", (colnames(data_emisivo)))
etiquetas_e <- gsub ("mes", "Mes", etiquetas_e)
etiquetas_e <- gsub ("via", "Vía", etiquetas_e)
etiquetas_e <- gsub ("destino_agrup" , "Destino principal", etiquetas_e)
etiquetas_e <- gsub ("paso_publ" , "Paso", etiquetas_e)
etiquetas_e <- gsub ("prov", "Provincia del paso", etiquetas_e)
etiquetas_e <- gsub ("limita" ,"Limita con", etiquetas_e)
etiquetas_e <- gsub ("round(sum(turistas))", "Turistas", etiquetas_e)
etiquetas_e <- gsub ("turistas", "Turistas", etiquetas_e)



"destino_agrup" = "Destino",
#"paso_publ" = "paso", "prov" = "Provincia", "limita" = "Limita ", "round(sum(turistas))" = "Turistas")