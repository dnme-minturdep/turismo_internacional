# Load packages #
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(glue) # Interpreted String Literals, CRAN v1.4.2
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.9

datos <- data.table::fread("data/turistas_internacionales_con destino.csv") %>% 
  as_tibble() 

#### RECEPTIVO
data_receptivo <-  datos %>% 
  filter(turismo_internac == "Receptivo") %>% 
  rename(year = 'año', 
         sentido = turismo_internac)  %>% 
  group_by(year, pais, sentido) %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                   pattern = ",", replacement = "." ), 
           casos = round(as.numeric(casos)))%>% 
  summarise(n = sum(casos)) 
  
#### EMISIVO
data_emisivo <- datos %>% 
  filter(turismo_internac == "Emisivo") %>% 
  rename(year = 'año', 
         sentido = turismo_internac)  %>% 
  group_by(year, destino_agrup, sentido) %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         casos = round(as.numeric(casos)))%>% 
  summarise(n = sum(casos)) %>% 
  rename(destino = destino_agrup) %>% 
  print()

