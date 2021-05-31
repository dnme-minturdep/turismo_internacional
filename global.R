#GLOBAL

# Load packages #
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(glue) # Interpreted String Literals, CRAN v1.4.2
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.9
library(data.table)
library(shiny)

# CONFIGs Globales

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


# LEVANTO DATOS (como Data table)
datos <- readRDS("data/turismo_internacional_pais.rds")

#mes de numero a texto.

datos <- datos[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", mes == 3 ,"Marzo",mes == 4 ,"Abril",						
                                mes == 5 ,"Mayo", mes == 6 ,"Junio", mes == 7 ,"Julio", mes == 8 ,"Agosto",						
                                mes == 9 ,"Septiembre", mes == 10 ,"Octubre", mes == 11 ,"Noviembre",						
                                mes == 12 ,"Diciembre"))] 						

Mes_ult <- as.tibble(datos[nrow(datos),2])

#reordeno niveles

datos$mes<- factor(datos$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	"Mayo",	
                                         "Junio",	"Julio",	"Agosto",	"Septiembre",	
                                         "Octubre",	"Noviembre",	"Diciembre"), 
                   ordered = TRUE)	


datos <- datos %>%
  rename(year = 'año')  %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         casos = as.numeric(casos))


#### RECEPTIVO 

data_receptivo <-  datos[turismo_internac == "Receptivo", ] 
data_receptivo <- data_receptivo[, .(turistas = sum(casos)), by = .(year, mes, via, pais_agrupado, pais, paso_publ, prov, limita)] 


#### EMISIVO

data_emisivo <-  datos[turismo_internac == "Emisivo", ] 
data_emisivo <- data_emisivo[, .(turistas = sum(casos)), by = .(year, mes, via, destino_agrup, pais, paso_publ, prov, limita)] 
