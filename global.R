library(tidyverse) 
library(glue) 
library(lubridate) 
library(data.table)
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
library (readxl)									
library(herramientas)
library(comunicacion)

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

#color
cols_arg2 <- c("#EE3D8F", # "(rosa)"
               "#50B8B1", # celeste "
               "#F7941E", # naranja 
               "FFD100", #amarillo
               "#D7DF23", #verde amarillo
               "#9283BE") #violeta

#saco notacion cientifica
options(scipen = 999)

# datos turismo internacional ####

datos <- readRDS("/srv/DataDNMYE/turismo_internacional/turismo_internacional_visitantes.rds")
datos <- datos %>%
  rename(year = 'anio')  %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         sexo = str_replace_all(string = sexo, 
                                 pattern = "SD", replacement = "Sin dato" ), 
         sexo = str_replace_all(string = sexo, 
                                pattern = "X", replacement = "X (identidad no binaria)" ), 
         casos = as.numeric(casos),
         paso_publ = str_replace_all(paso_publ, "Aero ", "Aeropuerto "), 
         grupoetario = case_when(grupoetario == "Menores de 18 años" ~ "Entre 0 y 18 años",
                                 grupoetario == "60 años y más" ~ "Más de 59 años",
                                 grupoetario == "SD" ~ "Sin dato", 
                                 TRUE ~ grupoetario)
         
         )

# matcheo ruta natual

ruta <- readRDS("pasos_rutas.RDS")
ruta <- ruta %>% 
  mutate (name = gsub("Ruta de las ", "", name),
          name = gsub("Ruta de la ", "", name),
          name = gsub("Ruta de los ", "", name),
          name = gsub("Ruta del ", "", name),
          name = gsub("la ", "", name), 
          name = gsub("Los ", "", name)) %>% 
  rename(ruta_natural =name) %>% 
  select(paso_publ,ruta_natural)

datos <- left_join(datos,ruta) 

#datos para graficos####

#data para serie, por pais_agrupado y destino

data_graficos <- datos [tipo_visitante == "Turistas", .(turistas = sum(casos)), 
                      by = .(year, mes, pais_agrupado, destino_agrup, 
                               turismo_internac)] 

#completo meses faltantes.

data_graficos <- data.table(complete (data_graficos, 
                                      expand(data_graficos, year, mes, 
                                             nesting(destino_agrup, 
                                                    pais_agrupado, 
                                                    turismo_internac)),
                                      fill = list(turistas = 0)))

# elimino meses posteriores al ultimo, que se completaron por nesting.

mes_ult_nro <- as_tibble(datos[nrow(datos),2])
year_ult <- as_tibble(datos[nrow(datos),1])

data_graficos <- data_graficos %>%
  filter ((year < as.numeric(year_ult)) | (year == as.numeric(year_ult) 
                                           & mes <= as.numeric(mes_ult_nro))) %>% 
  mutate (periodo = dmy(as.character(glue("01/{mes}/{year}"))))%>% 
  rename (turismo = turismo_internac)

# acumulado por destino:

data_grafico_ac_pais <- data_graficos %>%
  filter (year == as.numeric(year_ult)) %>% 
  mutate(pais_destino = case_when(turismo == "Receptivo" ~ pais_agrupado,
                                  turismo == "Emisivo" ~ destino_agrup)) %>% 
  group_by(turismo,pais_destino) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() 
# acumulado por via. 

data_grafico_ac_via <- datos %>%
  filter (year == as.numeric(year_ult), tipo_visitante == "Turistas") %>% 
  rename(turismo = turismo_internac) %>% 
  group_by(turismo, via) %>% 
  summarise(turistas = round(sum(casos_ponderados))) %>% 
  ungroup() 

data_grafico_ac_total <- data_grafico_ac_via %>%
  group_by(turismo) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  mutate(via = "Total")

#agrego total a via. 
data_grafico_ac_via <- bind_rows(data_grafico_ac_via, data_grafico_ac_total)

# datos para tabla ####

#mes de numero a texto.

datos <- datos[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                                mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                                mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                                mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                                mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                                mes == 11 ,"Noviembre", mes == 12 ,"Diciembre"))] 						

Mes_ult <- as_tibble(datos[nrow(datos),2])

#reordeno niveles

datos$mes<- factor(datos$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre"), 
                   ordered = TRUE)	

  ## RECEPTIVO 
  
  data_receptivo <-  datos[turismo_internac == "Receptivo", ] 
  data_receptivo <- data_receptivo[, .(turistas = sum(casos)), 
                                   by = .(year, trim, mes, tipo_visitante, via,
                                          pais_agrupado, pais, 
                                          paso_publ, prov, limita, ruta_natural, 
                                          sexo, grupoetario)] 
  
  # graficos  TI ####
  
  graf_pais_ti <- ggplot(data_grafico_ac_pais, aes(x= pais_destino, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge")+
    geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ",")),
               position = position_dodge(width = 1),
               vjust = -0.25)+
    scale_fill_dnmye() +
    scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(legend.position = "bottom", 
          legend.justification = c(1,1),
          axis.text.x =element_text (size =11, angle = 45, vjust = 0.6),
          axis.text.y = element_text(size = 11),
          legend.text = element_text (size =11),
          title = element_text (size =13),
          plot.caption  = element_text(size = 11, hjust = 0)) +
    labs(title = "Viajes de turistas según país de residencia/destino",
         subtitle = glue("Total país. Acumulado a {Mes_ult} {year_ult}."),
         y = "", 
         x = "", 
         fill = "",
         caption =  "Fuente: Dirección Nacional de Mercados y Estadstica, Ministerio de Turismo y Deportes")
  
  graf_via_ti <- ggplot(data_grafico_ac_via, aes(x= via, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge") +
    geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ",")),
               position = position_dodge(width = 1),
               vjust = -0.25)+
    scale_fill_dnmye() +
    scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(axis.text.x =element_text (size =11),
          axis.text.y = element_text(size = 11),
          legend.position = "" ,
          title = element_text (size =12) )+
    labs(title = "Viajes de turistas según medio de transporte",
         subtitle = glue("Total país. Acumulado a {Mes_ult} {year_ult}."),
         y = "", 
         x = "", 
         fill = "",
         color = "")
  
  
  
  
  
  #### EMISIVO
  
  data_emisivo <-  datos[turismo_internac == "Emisivo", ] 
  data_emisivo <- data_emisivo[, .(turistas = sum(casos)), 
                               by = .(year, trim, mes,  tipo_visitante, via, 
                                      destino_agrup, pais, 
                                      paso_publ, prov, limita,
                                      sexo, grupoetario)] 



# datos eti ####
  
localidad <- readRDS("/srv/DataDNMYE/eti/bases/eti_localidad.rds")

#defino ultimo mes antes de pasarlo a factor
mes_eti <- last(localidad$mes)
year_eti <- last(localidad$anio)

#reordeno niveles

localidad$mes<- factor(localidad$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre"), 
                   ordered = TRUE)	

loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)
