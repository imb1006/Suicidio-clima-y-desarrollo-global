# Importación de Datos ----------------------------------------------------
library(readr)
library(tidyverse)

# Para tener los datos del clima utilizamos importacion de csv utilizando las opciones: import dataset from text (readr)
Datos_Clima <- read_csv("INPUT/DATA/GlobalLandTemperaturesByCountry.csv")
Datos_Clima #Vemos que los datos son correctos
# Habría que elegir las fechas correctas porque contiene todos los datos desde 1700 al 2013

# Carga de datos de suicidio desde el 2000 al 2019:
Datos_Suicidio <- read_csv("INPUT/DATA/SuicideGlobalRates.csv")
Datos_Suicidio
# La tasa media de suicidios por 100.000 habitantes se encuentra en la columna "FactValueNumeric"

# Carga de datos de los diferentes sectores de empleo por países desde el 2000 al 2013:
Datos_Sectores <- read_delim("INPUT/DATA/EmploymentSectors.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
Datos_Sectores

#Carga de datos de emisiones anuales de CO2 por países

Datos_Emisiones <- read_csv("INPUT/DATA/annual-co2-emissions-per-country.csv")
Datos_Emisiones

# Seleccionamos datos de los años de 2005 a 2013 -------------------------------------------
#vamos a utilizar la letra 'f' de final, porque serán los datos que utlizaremos--> SE PUEDE CAMBIAR, NO DEFINITIVO ()

#Datos del clima de 2005 a 2013
fDatos_Clima <- Datos_Clima %>% filter(between(dt,as.Date("2005-01-01"),as.Date("2013-12-31"))) # hay que hacer algo con esto porque
                                                                                                # en este fichero es tipo date pero en 
                                                                                                # los otros no (son tipo double)
fDatos_Clima

#Datos de suicidios
fDatos_Suicidio <- Datos_Suicidio %>% filter(between(Period,2005,2013))
fDatos_Suicidio

#Datos de los distintos sectores de empleo
fDatos_Sectores <- Datos_Sectores %>% filter(between(Year,2005,2013))
fDatos_Sectores

#Datos de las emisiones de CO2
fDatos_Emisiones <- Datos_Emisiones %>% filter(between(Year,2005,2020))
fDatos_Emisiones

