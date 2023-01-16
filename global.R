library(tidyverse) 
library(glue) 
library(lubridate) 
library(data.table)
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
library (readxl)									


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

##DATA PARA GRAFICO 1

#data por pais_agrupado y destino

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
  mutate (periodo = dmy(as.character(glue::glue("01/{mes}/{year}"))))%>% 
  rename (turismo = turismo_internac)


# GRAFICOS RESUMEN
#
##dato mensual 
#
#dato_mensual <- datos %>% 
#  filter (year == as.numeric(year_ult))
#
## %>% 
  


                                        
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
                                   by = .(year, trim, mes, tipo_visitante, via,
                                          pais_agrupado, pais, 
                                          paso_publ, prov, limita, ruta_natural, 
                                          sexo, grupoetario)] 
  
  
  #### EMISIVO
  
  data_emisivo <-  datos[turismo_internac == "Emisivo", ] 
  data_emisivo <- data_emisivo[, .(turistas = sum(casos)), 
                               by = .(year, trim, mes,  tipo_visitante, via, 
                                      destino_agrup, pais, 
                                      paso_publ, prov, limita,
                                      sexo, grupoetario)] 



# datos eti ####

# codigos	

codigos <- read_xlsx("/srv/DataDNMYE/eti/codigos_pais_prov_ciudad.xlsx") %>% 									
  janitor::clean_names(.) %>% 									
  mutate (cod_loc = paste (tipo, id_provincia_pais, id_ciudad, sep = "_"),
          cod_prov = paste (tipo, id_provincia_pais, sep = "_"))

#codigos para matchear provincia.
codigos_prov <-  codigos %>% 
  select (cod_prov, nombre_provincia_pais) %>% 
  group_by (cod_prov) %>% 
  summarise (nombre_provincia_pais = first (nombre_provincia_pais))

#codigos para matchear localidad.
codigos_loc <-  codigos %>% 
  select (cod_loc, nombre_ciudad) %>% 
  group_by (cod_loc) %>% 
  summarise (nombre_ciudad = first (nombre_ciudad))

#levanto base, filtro turistas

eti_tur <- readRDS("/srv/DataDNMYE/eti/bases/eti_nr_2014_2022.rds") %>% 
  filter (wpf > 0, vis == 2)								

#creo variables para pivotear. Selecciono solo pasos Eze y Aerop.

eti_tur <- eti_tur %>% 	
  filter (p3_3 >=2019) %>% #filtro 2019 porque sino tengo problema en cruceros y 2022 porque es lo que publicamos 
  filter (p5 <=3) %>%  # filtro  EyA
  mutate (cod_ciudad1 = paste (p21_1_1a, p21_1_1b, p21_1_2a, sep = "_"), #CONCaT DE CaTEGORÍa pROVINCIa/paÍS, CÓDIGO pROV								
          cod_ciudad2 = paste (p21_2_1a, p21_2_1b, p21_2_2a, sep = "_"),									
          cod_ciudad3 = paste (p21_3_1a, p21_3_1b, p21_3_2a, sep = "_"),									
          cod_ciudad4 = paste (p21_4_1a, p21_4_1b, p21_4_2a, sep = "_"),									
          cod_ciudad5 = paste (p21_5_1a, p21_5_1b, p21_5_2a, sep = "_"),									
          cod_ciudad6 = paste (p21_6_1a, p21_6_1b, p21_6_2a, sep = "_"),									
          cod_ciudad7 = paste (p21_7_1a, p21_7_1b, p21_7_2a, sep = "_"),									
          cod_ciudad8 = paste (p21_8_1a, p21_8_1b, p21_8_2a, sep = "_"),									
          cod_ciudad9 = paste (p21_9_1a, p21_9_1b, p21_9_2a, sep = "_"),									
          cod_ciudad9 = paste (p21_9_1a, p21_9_1b, p21_9_2a, sep = "_"),									
          cod_ciudad10 = paste (p21_10_1a, p21_10_1b, p21_10_2a, sep = "_"),
          crucero_ciudad1 = case_when (p21_1_4==8 | p21_1_6 ==8 | p21_1_8 == 8  ~ 1),
          crucero_ciudad2 = case_when (p21_2_4==8 | p21_2_6 ==8 | p21_2_8 == 8  ~ 1),
          crucero_ciudad3 = case_when (p21_3_4==8 | p21_3_6 ==8 | p21_3_8 == 8  ~ 1), 
          crucero_ciudad4 = case_when (p21_4_4==8 | p21_4_6 ==8 | p21_4_8 == 8  ~ 1), 
          crucero_ciudad5 = case_when (p21_5_4==8 | p21_5_6 ==8 | p21_5_8 == 8  ~ 1), 
          crucero_ciudad6 = case_when (p21_6_4==8 | p21_6_6 ==8 | p21_6_8 == 8  ~ 1), 
          crucero_ciudad7 = case_when (p21_7_4==8 | p21_7_6 ==8 | p21_7_8 == 8  ~ 1), 
          crucero_ciudad8 = case_when (p21_8_4==8 | p21_8_6 ==8 | p21_8_8 == 8  ~ 1), 
          crucero_ciudad9 = case_when (p21_9_4==8 | p21_9_6 ==8 | p21_9_8 == 8  ~ 1), 
          crucero_ciudad10 = case_when (p21_10_4==8 | p21_10_6 ==8 | p21_10_8 == 8  ~ 1), # indica que en la ciudad se alojó en crucero.
          noches_ciudad1 = p21_1_3 + p21_1_5 + p21_1_7,# SUMa DE NOCHES 1, NOCHES 2, NOCHES paQUETE									
          noches_ciudad2 = p21_2_3 + p21_2_5 + p21_2_7,									
          noches_ciudad3 = p21_3_3 + p21_3_5 + p21_3_7,									
          noches_ciudad4 = p21_4_3 + p21_4_5 + p21_4_7,									
          noches_ciudad5 = p21_5_3 + p21_5_5 + p21_5_7,									
          noches_ciudad6 = p21_6_3 + p21_6_5 + p21_6_7,									
          noches_ciudad7 = p21_7_3 + p21_7_5 + p21_7_7,									
          noches_ciudad8 = p21_8_3 + p21_8_5 + p21_8_7,									
          noches_ciudad9 = p21_9_3 + p21_9_5 + p21_9_7,									
          noches_ciudad10 = p21_10_3 + p21_10_5 + p21_10_7) %>% 
  select(wpf, p18_1,estadia, gastoest2, orig_eya, indice_tiempo,motivo_viaje,alojamiento, 						
         cod_ciudad1,	cod_ciudad2,	cod_ciudad3,	cod_ciudad4,	cod_ciudad5,
         cod_ciudad6,	cod_ciudad7,	cod_ciudad8,	cod_ciudad9,	cod_ciudad10,
         noches_ciudad1, noches_ciudad2,	noches_ciudad3,	noches_ciudad4,	
         noches_ciudad5, noches_ciudad6,	noches_ciudad7,	noches_ciudad8,	
         noches_ciudad9,	noches_ciudad10, p21_12_3, crucero_ciudad1 ,
         crucero_ciudad2 ,
         crucero_ciudad3 ,
         crucero_ciudad4 ,
         crucero_ciudad5 ,
         crucero_ciudad6 ,
         crucero_ciudad7 ,
         crucero_ciudad8 ,
         crucero_ciudad9 ,
         crucero_ciudad10) 

#creo id por fila

eti_tur$id = seq(1:nrow(eti_tur)) 


# PIVOTEO A LO LARGO.

#pivoteo tabla, matcheo codigos. Cada fila es una visita a una localidad. 

pivot <- eti_tur %>% 									
  pivot_longer(!c("id","wpf","p18_1", "estadia", "gastoest2", 
                  "orig_eya","motivo_viaje","alojamiento", "indice_tiempo", "p21_12_3"),									
               names_to = c(".value", "ciudad"), 									
               names_sep = "_",
               values_drop_na = TRUE) %>% 
  mutate (cod_prov = str_sub(cod, 1, 5)) %>% 
  left_join ( codigos_prov, by = "cod_prov" )%>% 
  left_join ( codigos_loc, by = c("cod" = "cod_loc")) 


#Dejo solo destinos de arg 
#saco noches 0 (excepto cruceros). saco noches 0 en Buenos Aires 2022, por no ser destino de crucero
#a diferencia de noches en localidad, noches en viaje es una columna, que repite los valores para un mismo id.

pivot_limpia <- pivot %>% 									
  filter( str_detect(cod, "PR") | nombre_provincia_pais == "Antártida") %>%   #Dejo solo destinos de arg 
  rename (provincia = nombre_provincia_pais,
          pais_origen = orig_eya,
          En.viaje = p21_12_3) %>% 
  mutate (crucero = case_when(crucero == 1 & provincia == "Buenos Aires" & 
                                indice_tiempo >= "2022-01-01" ~ 0,
                              is.na(crucero) ~ 0,
                              TRUE ~ crucero), #saco crucero de Bs. As, chequeado 2022. Ver si lo hacemos para todo.En ese caso cambiar correccion crucero final
          provincia = case_when(provincia == "Antártida" ~ "Tierra del Fuego",
                                TRUE ~ provincia),
          En.viaje = na_if(En.viaje, 0)) %>%  #transformo 0 de en viaje en NA, para luego distinguirlo de noches de cruceros
  filter (noches > 0 | crucero == 1 | En.viaje == estadia ) %>%  # dejo casos de noches 0 solo en cruceros, o donde todas las noches son en viaje.
  mutate (noches = case_when( En.viaje == estadia ~ NA_real_,
                              TRUE ~ noches)) # si todas las noches, fueron en viaje, las noches 0 quedan NA

#de esta forma las noches son solo 0 en cruceros, son NA solo en los casos donde solo hay noches en viaje.
#si sacara estas filas, no tendría el total de noches. 
#quizas conviene pivotearla

#PARA SHINY: cada fila es la visita de un grupo de viaje a una localidad. Mayor nivel de desagregacion.

localidad <- pivot_limpia %>%		
  mutate(anio = lubridate::year(indice_tiempo),
         mes  = lubridate::month(indice_tiempo),
         ciudad = case_when(is.na(nombre_ciudad) ~ "Sin dato", 
                            TRUE ~ nombre_ciudad)) %>%  
  group_by(anio, mes, id, provincia, ciudad, estadia, wpf, p18_1, gastoest2, pais_origen,
           alojamiento,motivo_viaje, En.viaje) %>%  									
  summarise (noches = sum(noches,na.rm = T), 
             crucero = sum(crucero, na.rm = T)) %>% #sumo noches en la loclidad, sumo visitas en crucero en una misma localidad									
  ungroup() 

# Los casos que tuvieron noches en viaje (variable en columna, que se repite en cada localidad), las agrego como nuevas filas, 
#con localidad y provincia "en viaje": 

#  prueba pivot en viajes

#localidad2 <- localidad %>% 
#  pivot_longer(cols = En.viaje,names_to = "en_viaje", values_to = "noches_viaje")
#localidad2 <- localidad %>% 
#  pivot_longer(cols = En.viaje, names_to = "ciudad", values_to = "noches",  )

en_viaje <- localidad %>% 
  filter(En.viaje > 0) %>% 
  group_by(anio, mes, id, estadia, wpf, p18_1, gastoest2, pais_origen,
           alojamiento,motivo_viaje) %>% #agrupo por todo excepto variables de loc (loc, prov, crucero, noches)  
  summarise(noches = first(En.viaje)) %>% 
  ungroup %>% 
  mutate(ciudad = "En viaje", 
         provincia = "En viaje", 
         crucero = 0) #por definicion, tipo aloj crucero siempre refiere a una localidad, aunque las noche en viaje sea en crucero. 

localidad<- localidad %>% 
  select(-En.viaje) %>% 
  rbind(en_viaje) %>% 
  mutate (gasto = (gastoest2/estadia) * noches, #gasto en localidad/en viaje
          crucero = if_else(crucero == 0, "no", "si" )) %>%  #revisar casos raros
  as.data.table()

#mes de numero a texto.
localidad <- localidad[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                                mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                                mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                                mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                                mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                                mes == 11 ,"Noviembre", mes == 12 ,"Diciembre"))] 						

#reordeno niveles

localidad$mes<- factor(localidad$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre"), 
                   ordered = TRUE)	


rm(pivot, eti_tur, pivot_limpia, en_viaje)

#pendiente:

#trimestre? crucero? (revisar) 
#Poner un box para seleccionar opciones de suma
#calcular estadia media. gasto promedio diario 

#se puede agregar: alojamiento y motivo de viaje abierto. Alojamiento en localidad. 
#calificacion, actividades (como filtro?), paquete....

loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)
