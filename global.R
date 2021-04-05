# Load packages #
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(glue) # Interpreted String Literals, CRAN v1.4.2
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.9
library(data.table)

# CONFIGs Globales

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


# LEVANTO DATOS (como Data table)
datos <- readRDS("data/turismo_internacional_pais.rds")

#class(datos)

#### RECEPTIVO (por mes)

data_receptivo <-  datos %>% 
  filter(turismo_internac == "Receptivo") %>% 
  rename(year = 'año', 
         sentido = turismo_internac)  %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         casos = round(as.numeric(casos)))

data_receptivo <- data_receptivo %>%
  group_by(year, mes, via, pais, paso_publ, prov, limita) %>%
  summarise(turistas = sum(casos)) 

  
#mes de numero a texto.

data_receptivo <- data_receptivo %>%
  mutate(mes = fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", mes == 3 ,"Marzo",mes == 4 ,"Abril",
                     mes == 5 ,"Mayo", mes == 6 ,"Junio", mes == 7 ,"Julio", mes == 8 ,"Agosto",
                     mes == 9 ,"Septiembre", mes == 10 ,"Octubre", mes == 11 ,"Noviembre",
                     mes == 12 ,"Diciembre"))

#reordeno niveles

data_receptivo$mes<- factor(data_receptivo$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	"Mayo",	
                                                           "Junio",	"Julio",	"Agosto",	"Septiembre",	
                                                           "Octubre",	"Noviembre",	"Diciembre"), 
                            ordered = TRUE)	


#### EMISIVO
data_emisivo <- datos %>% 
  filter(turismo_internac == "Emisivo") %>% 
  rename(year = 'año', 
         sentido = turismo_internac)  %>% 
  group_by(year, mes, destino_agrup) %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
          casos = round(as.numeric(casos)))%>% 
  summarise(turistas = sum(casos)) %>% 
  rename(destino = destino_agrup) 

#runApp(display.mode = "showcase")
