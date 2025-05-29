library(tidyverse) 
library(glue) 
#library(lubridate) 
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
#library(readxl)									
#library(herramientas)
library(shinydashboard)
library(shinyWidgets)
#library(comunicacion)
library(DT)
#library(arrow)

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
datos <- arrow::open_dataset("/srv/DataDNMYE/turismo_internacional/tablero/datos_ti_tablero.parquet")


# ultimos datos
mes_ult_nro <- datos %>% tail(1) %>% pull(mes)
anio_ult <- datos %>% tail(1) %>% pull(anio)

#tabla para graficos####

#data para g por raficos acumulados: pais_agrupado y destino

data_graficos <- datos %>% 
  filter(tipo_visitante == "Turistas") %>% 
  group_by(anio, mes, pais_agrupado, destino_agrup, 
           turismo_internac) %>% 
  summarise(turistas = sum(casos)) %>% 
  ungroup() %>% 
  #collect() %>%
  mutate(periodo = make_date(anio, mes)) %>% 
  rename(turismo = turismo_internac)

# acumulado por destino:

data_grafico_ac_pais <- data_graficos %>%
  filter (anio == as.numeric(anio_ult)) %>% 
  mutate(pais_destino = case_when(turismo == "Receptivo" ~ pais_agrupado,
                                  turismo == "Emisivo" ~ destino_agrup)) %>% 
  group_by(turismo, pais_destino) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  collect()


dato_box <-  datos %>% 
  filter(anio == anio_ult | anio == anio_ult-1) %>%  #Año actual y anterior 
  filter(tipo_visitante == "Turistas") %>% 
  collect() %>% 
  filter(mes <= mes_ult_nro) 

dato_acumulado <- dato_box %>% 
  group_by(anio, turismo_internac) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>%
  collect() %>% 
  mutate(var = casos/lag(casos, n = 2)-1) %>% 
  filter(anio ==anio_ult) 

dato_mensual <- dato_box %>% 
  group_by(anio, mes, turismo_internac) %>% 
  summarise(casos = sum(casos)) %>% 
  ungroup() %>%
  filter(mes == mes_ult_nro) %>% 
  collect() %>% 
  arrange(anio) %>%
  group_by(turismo_internac) %>% 
  mutate(var = casos/lag(casos)-1) %>% 
  ungroup() %>% 
  filter(anio == anio_ult)

rm(dato_box)
# acumulado por via. 

data_grafico_ac_via <- datos %>%
  filter (anio == anio_ult, tipo_visitante == "Turistas") %>% 
  rename(turismo = turismo_internac) %>% 
  group_by(turismo, via) %>% 
  summarise(turistas = round(sum(casos))) %>% 
  ungroup() %>% 
  collect()

data_grafico_ac_total <- data_grafico_ac_via %>%
  group_by(turismo) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  mutate(via = "Total") %>% 
  collect()

  ## RECEPTIVO 
  
data_receptivo <- datos %>% 
  filter(turismo_internac == "Receptivo") %>% 
  group_by(anio, trim, mes, tipo_visitante, via,
           pais_agrupado, pais, 
           paso_publ, prov, limita, ruta_natural, 
           sexo, grupoetario) %>% 
  summarise(turistas = sum(casos)) %>% 
  ungroup() 
  

# graficos  TI ####
  
graf_pais_ti <- ggplot(data_grafico_ac_pais, aes(x= pais_destino, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge", 
             aes(text = paste0(pais_destino, "<br>", 
                               turismo, ": ", format(turistas, big.mark = ".", decimal.mark = ",")))) +
    # geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ","), group = turismo),
    #            position = position_dodge(width = 1),
    #            vjust = -0.25, 
    #            size = 3.5)+
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
         subtitle = glue("Total país. Acumulado a {mes_ult_nro} {anio_ult}."),
         y = "", 
         x = "", 
         fill = "",
         caption =  "Fuente: Dirección de Mercados y Estadísticas, Subsecretaría de Turismo")
  

graf_via_ti <- ggplot(data_grafico_ac_via, aes(x= via, y= turistas, fill = turismo)) +   
    geom_col(position = "dodge",
             aes(text = paste0(via, "<br>", 
                               turismo, ": ", format(turistas, big.mark = ".", decimal.mark = ",")))) +
    # geom_text (aes(label= format(turistas, big.mark = ".", decimal.mark = ",")),
    #            position = position_dodge(width = 1),
    #            vjust = -0.25)+
    scale_fill_manual(values = c(cols_arg2[1], cols_arg2[2])) + 
    scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
    theme_minimal()+
    theme(axis.text.x =element_text (size =11),
          axis.text.y = element_text(size = 11),
          legend.position = "" ,
          title = element_text (size =12) )+
    labs(title = "Viajes de turistas según medio de transporte",
         subtitle = glue("Total país. Acumulado a {mes_ult_nro} {anio_ult}."),
         y = "", 
         x = "", 
         fill = "",
         color = "")
  
  
#### EMISIVO
data_emisivo <-  datos %>% filter(turismo_internac == "Emisivo") %>% 
  mutate(destino_agrup = ifelse(destino_agrup == "Sin datos", "Sin dato", destino_agrup)) %>% 
  group_by(anio, trim, mes,  tipo_visitante, via, 
           destino_agrup, pais, 
           paso_publ, prov, limita,
           sexo, grupoetario) %>% 
  summarise(turistas = sum(casos)) %>% 
  ungroup()

# datos eti ####
  
localidad <- read_rds("/srv/DataDNMYE/eti/bases/eti_localidad.rds")

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
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_color.png", height = "250px")
)

# serie historica gasto ####
 
gasto <- readxl::read_excel("/srv/DataDNMYE/turismo_internacional/bases_proceso/base_gasto_visitantes.xlsx")

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

aperturas <- readxl::read_excel("/srv/DataDNMYE/turismo_internacional/bases_proceso/aperturas_serie_ti.xlsx")
