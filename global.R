# Load packages #
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(glue) # Interpreted String Literals, CRAN v1.4.2
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.9

# CONFIGs Globales

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


# LEVANTO DATOS (como Data table)
datos <- readRDS("data/turismo_internacional_pais.rds")

class(datos)

#### RECEPTIVO (por mes)
data_receptivo <-  datos %>% 
  filter(turismo_internac == "Receptivo") %>% 
  rename(year = 'año', 
         sentido = turismo_internac)  %>% 
  group_by(year, mes, pais) %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                   pattern = ",", replacement = "." ), 
           casos = round(as.numeric(casos)))%>% 
  summarise(turistas = sum(casos)) 
  
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

