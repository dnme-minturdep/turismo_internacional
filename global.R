library(tidyverse) 
library(glue) 
library(lubridate) 
library(data.table)
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

# datos

datos <- readRDS("/srv/DataDNMYE/turismo_internacional/turismo_internacional_pais.rds")
datos <- datos %>%
  rename(year = 'anio')  %>% 
  mutate(casos = str_replace_all(string = casos_ponderados, 
                                 pattern = ",", replacement = "." ), 
         casos = as.numeric(casos),
         paso_publ = str_replace_all(paso_publ, "Aero ", "Aeropuerto ")
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

##DATA PARA GRAFICO 1

#data por pais_agrupado y destino

data_graficos <- datos [, .(turistas = sum(casos)), 
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

datos_grafico1 <- data_graficos %>%
  filter ((year < as.numeric(year_ult)) | (year == as.numeric(year_ult) 
                                           & mes <= as.numeric(mes_ult_nro))) %>% 
  mutate (periodo = dmy(as.character(glue::glue("01/{mes}/{year}"))))%>% 
  group_by(periodo, turismo_internac) %>%
  summarise(turistas = sum(turistas)) %>%
  mutate (turistas= (round(turistas))) %>%
  rename (turismo = turismo_internac)

                                        
## DATOS PARA TABLA:

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
                                   by = .(year, mes, via, pais_agrupado, pais, 
                                          paso_publ, prov, limita, ruta_natural)] 
  
  
  #### EMISIVO
  
  data_emisivo <-  datos[turismo_internac == "Emisivo", ] 
  data_emisivo <- data_emisivo[, .(turistas = sum(casos)), 
                               by = .(year, mes, via, destino_agrup, pais, 
                                      paso_publ, prov, limita)] 


# GRAFICO. (lo pongo al final porque toma dato de Mes_ult en texto)

#color
cols_arg2 <- c("#EE3D8F", # "(rosa)"
               "#50B8B1", # celeste "
               "#F7941E", # naranja 
               "FFD100", #amarillo
               "#D7DF23", #verde amarillo
               "#9283BE") #violeta

#saco notacion cientifica
options(scipen = 999)

grafico_1  <- ggplot(datos_grafico1, aes(periodo, turistas, colour = turismo, group =1, text = paste('Fecha:', format(periodo,"%b%y"),
                                                                                                   '<br>Viajes:',format(turistas,big.mark="."), 
                                                                                                   '<br>Turismo:',turismo)))+   
  geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5) + 
  geom_line(size = 1.2 , alpha = 0.8) + 
  geom_point(size = 2.0, alpha = 0.8)+ 
  scale_color_manual(values = c(cols_arg2[1], cols_arg2[6])) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%b%y", expand = c(0,10))+ 
  scale_y_continuous(breaks = seq(min(datos_grafico1$turistas), max(datos_grafico1$turistas), by = 200000), 
                     labels = scales::number_format(big.mark = ".", decimal.mark = ",")) + 
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x =element_text (size =12, angle=90),
        axis.text.y = element_text(size = 12),
        legend.text = element_text (size =12),
        plot.caption =  element_text (size =12, hjust = 0.0)) +
  labs(title = "EVOLUCIÓN MENSUAL DEL TURISMO INTERNACIONAL",
       subtitle = glue ("Emisivo y receptivo \n Enero 2016-{Mes_ult}-2021"),
       y = "", 
       x = "", 
       color= "",
       caption =  "Fuente: Dirección Nacional de Mercados y Estadistica, Ministerio de Turismo y Deportes" )

fig1 <- ggplotly(grafico_1, tooltip = "text")  %>% 
        layout(legend = list(orientation = "h", x = 0.4, y = -0.6))

loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)