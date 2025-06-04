library(tidyverse) 
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
library(shinyWidgets)
library(DT)

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
datos <- read_rds("/srv/DataDNMYE/turismo_internacional/tablero/datos_tablero.rds")
dato_mensual <- datos$dato_mensual
dato_acumulado <- datos$dato_acumulado
data_receptivo <- datos$data_receptivo
data_emisivo <- datos$data_emisivo
graf_pais_ti <- datos$graf_pais_ti
graf_via_ti <- datos$graf_via_ti
rm(datos)
  #arrow::open_dataset("/srv/DataDNMYE/turismo_internacional/tablero/datos_ti_tablero.parquet")


# ultimos datos
mes_ult_nro <- data_receptivo %>% tail(1) %>% pull(mes)
anio_ult <- data_receptivo %>% tail(1) %>% pull(anio)

#tabla para graficos####

#data para g por raficos acumulados: pais_agrupado y destino



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
