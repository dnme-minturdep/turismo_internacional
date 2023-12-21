library(tidyverse) 
library(glue) 
library(lubridate) 
library(data.table)
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
library(readxl)									
library(herramientas)
library(shinydashboard)
library(shinyWidgets)
library(comunicacion)


source("valuebox.R")

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


# levanto datos: ####
## serie historica (es hasta 2022, pero se toman datos hasta 2015) ####

serie_visitantes <- read_file_srv(
  "turismo_internacional/bases_proceso/base_visitantes_1990_2022.xlsx") %>% 
  filter (anio < 2016) %>% 
  filter(!(anio >= 2010 & tipo_visitante == "Turistas" & 
             turismo_internac == "Receptivo")) %>% 
  rename(casos = casos_ponderados)

## datos turismo internacional visitantes (desde 2016 + receptivo turistas 2010-2015) ####

datos <- read_file_srv(
  "/srv/DataDNMYE/turismo_internacional/turismo_internacional_visitantes.rds") %>% 
  rename(casos = casos_ponderados)  

# ultimos datos
mes_ult_nro <- as_tibble(datos[nrow(datos),mes])
anio_ult <- as_tibble(datos[nrow(datos),anio])

#tabla para graficos####

#data para g por raficos acumulados: pais_agrupado y destino

data_graficos <- datos [tipo_visitante == "Turistas", .(turistas = sum(casos)), 
                      by = .(anio, mes, pais_agrupado, destino_agrup, 
                               turismo_internac)] 

##completo meses faltantes (no es necesario, ya no tengo acá el gráfico de serie mensual)
#
#data_graficos <- data.table(complete (data_graficos, 
#                                      expand(data_graficos, anio, mes, 
#                                             nesting(destino_agrup, 
#                                                    pais_agrupado, 
#                                                    turismo_internac)),
#                                      fill = list(turistas = 0)))
#
## elimino meses posteriores al ultimo, que se completaron por nesting.
#
#data_graficos <- data_graficos %>%
#  filter ((anio < as.numeric(anio_ult)) | (anio == as.numeric(anio_ult) 
#                                           & mes <= as.numeric(mes_ult_nro))) %>% 
#  mutate (periodo = dmy(as.character(glue("01/{mes}/{anio}"))))%>% 
#  rename (turismo = turismo_internac)

data_graficos <- data_graficos %>%
  mutate (periodo = dmy(as.character(glue("01/{mes}/{anio}"))))%>% 
  rename (turismo = turismo_internac)

# acumulado por destino:

data_grafico_ac_pais <- data_graficos %>%
  filter (anio == as.numeric(anio_ult)) %>% 
  mutate(pais_destino = case_when(turismo == "Receptivo" ~ pais_agrupado,
                                  turismo == "Emisivo" ~ destino_agrup)) %>% 
  group_by(turismo, pais_destino) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() 

# acumulado por via. 

data_grafico_ac_via <- datos %>%
  filter (anio == as.numeric(anio_ult), tipo_visitante == "Turistas") %>% 
  rename(turismo = turismo_internac) %>% 
  group_by(turismo, via) %>% 
  summarise(turistas = round(sum(casos))) %>% 
  ungroup() 

data_grafico_ac_total <- data_grafico_ac_via %>%
  group_by(turismo) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  mutate(via = "Total")

#agrego total a via. 
data_grafico_ac_via <- bind_rows(data_grafico_ac_via, data_grafico_ac_total)


# datos para tabla a publicar ####

#completo meses faltantes (2020 trim 2 principalmente)

datos <- data.table(complete (datos,  expand(datos, anio, mes, 
                                             nesting(destino_agrup, 
                                                     pais_agrupado, 
                                                     turismo_internac,
                                                     tipo_visitante,
                                                     via, 
                                                     sexo, 
                                                     grupoetario)),
                                      fill = list(casos = 0, 
                                                  paso_publ = "Sin dato",
                                                  prov = "Sin dato",
                                                  limita = "Sin dato",
                                                  pais = "Sin dato", 
                                                  ruta_natural = "Sin dato" 
                                                  )))

# elimino meses posteriores al ultimo, que se completaron por nesting.
# creo trim para nuevos meses:

datos <- datos %>%
  filter ((anio < as.numeric(anio_ult)) | (anio == as.numeric(anio_ult) 
                                           & mes <= as.numeric(mes_ult_nro))) %>% 
  mutate(trim = case_when (mes %in% 1:3 ~ 1,
                           mes %in% 4:6 ~ 2, 
                           mes %in% 7:9 ~ 3, 
                           mes %in% 10:12 ~ 4))

#ultimos datos (box) ####

dato_box <-  datos %>% 
  filter(anio == as.numeric(anio_ult) | anio == as.numeric(anio_ult-1)) %>%  #Año actual y anterior 
  filter(tipo_visitante == "Turistas") %>% 
  filter(mes <= as.numeric(mes_ult_nro))

dato_acumulado <- dato_box %>% 
  group_by(anio, turismo_internac) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>%
  mutate(var = casos/lag(casos, n = 2)-1) %>% 
  filter(anio == as.numeric (anio_ult))

dato_mensual <- dato_box %>% 
  group_by(anio, mes, turismo_internac) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>%
  filter(mes == as.numeric(mes_ult_nro)) %>% 
  mutate(var = casos/lag(casos, n = 2)-1) %>% 
  filter(anio == as.numeric (anio_ult))

#sumo serie historica

datos <- datos %>% bind_rows(serie_visitantes) %>% 
  arrange(anio,mes)

#mes de numero a texto.

datos <- datos[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                                mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                                mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                                mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                                mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                                mes == 11 ,"Noviembre", mes == 12 ,"Diciembre", 
                                mes == 0, "Sin dato"))] 						

Mes_ult <- as_tibble(datos[nrow(datos),2])

#reordeno niveles

datos$mes<- factor(datos$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre", "Sin dato"), 
                   ordered = TRUE)	

# acumulado por via. 

data_grafico_ac_via <- datos %>%
  filter (anio == as.numeric(anio_ult), tipo_visitante == "Turistas") %>% 
  rename(turismo = turismo_internac) %>% 
  group_by(turismo, via) %>% 
  summarise(turistas = round(sum(casos))) %>% 
  ungroup() 

data_grafico_ac_total <- data_grafico_ac_via %>%
  group_by(turismo) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  mutate(via = "Total")

  ## RECEPTIVO 
  
  data_receptivo <-  datos[turismo_internac == "Receptivo", ] 
  data_receptivo <- data_receptivo[, .(turistas = sum(casos)), 
                                   by = .(anio, trim, mes, tipo_visitante, via,
                                          pais_agrupado, pais, 
                                          paso_publ, prov, limita, ruta_natural, 
                                          sexo, grupoetario)] 
  
  
  # graficos  TI ####
  
  graf_pais_ti <- ggplot(data_grafico_ac_pais, aes(x= pais_destino, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge")+
    geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ","), group = turismo),
               position = position_dodge(width = 1),
               vjust = -0.25, 
               size = 3.5)+
    scale_fill_manual(values = c(cols_arg2[1], cols_arg2[2])) + 
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
         subtitle = glue("Total país. Acumulado a {Mes_ult} {anio_ult}."),
         y = "", 
         x = "", 
         fill = "",
         caption =  "Fuente: Dirección Nacional de Mercados y Estadstica, Ministerio de Turismo y Deportes")
  
  graf_pais_ti
  graf_via_ti <- ggplot(data_grafico_ac_via, aes(x= via, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge") +
    geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ",")),
               position = position_dodge(width = 1),
               vjust = -0.25)+
    scale_fill_manual(values = c(cols_arg2[1], cols_arg2[2])) + 
    scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(axis.text.x =element_text (size =11),
          axis.text.y = element_text(size = 11),
          legend.position = "" ,
          title = element_text (size =12) )+
    labs(title = "Viajes de turistas según medio de transporte",
         subtitle = glue("Total país. Acumulado a {Mes_ult} {anio_ult}."),
         y = "", 
         x = "", 
         fill = "",
         color = "")
  
  
  #### EMISIVO
  
  data_emisivo <-  datos[turismo_internac == "Emisivo", ] 
  data_emisivo <- data_emisivo[, .(turistas = sum(casos)), 
                               by = .(anio, trim, mes,  tipo_visitante, via, 
                                      destino_agrup, pais, 
                                      paso_publ, prov, limita,
                                      sexo, grupoetario)] 



# datos eti ####
  
localidad <- read_file_srv("/srv/DataDNMYE/eti/bases/eti_localidad.rds")

#defino ultimo mes antes de pasarlo a factor

mes_eti <- last(localidad$mes[localidad$paso_final == "Ezeiza y Aeroparque"])
anio_eti <- last(localidad$anio)

#reordeno niveles

localidad$mes<- factor(localidad$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre", "Sin datos"), 
                   ordered = TRUE)	

loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)

# serie historica gasto ####

gasto <- read_file_srv("/srv/DataDNMYE/turismo_internacional/bases_proceso/base_gasto_visitantes.xlsx")

gasto <- gasto %>% 
  mutate(periodo = if_else(trim == 0,
                           lubridate::yq(paste(anio, 1, "-")),
                           yq(paste(anio, trim, "-"))),#para graficos
         trim = as.character(trim), 
         trim= if_else(trim == "0", "Sin dato", trim),
         )

#Por ahora dejamos viajes 0 y gasto y pernoctes en trim 2 2020
#se puede agregar datos por mes y via, con SD en gasto.

trim_ult_gasto <- as_tibble(gasto[nrow(gasto),2])
anio_ult_gasto <- as_tibble(gasto[nrow(gasto),1])

#metodologia ####

#abro aperturas de variables según años

aperturas <- read_file_srv("/srv/DataDNMYE/turismo_internacional/bases_proceso/aperturas_serie_ti.xlsx")



