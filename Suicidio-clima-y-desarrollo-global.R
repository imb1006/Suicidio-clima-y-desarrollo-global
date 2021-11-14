# Importación de Datos ----------------------------------------------------
library(readr)
library(tidyverse)
library(dplyr)

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

# Modificación de tablas --------------------------------------------


# * Clima -----------------------------------------------------------------
# Modificamos la tabla del clima para quitar la columna de desviación y cambiamos el nombre de las columnas
colnames(fDatos_Clima)<-c("Fecha","Temperatura","DesviacionTemperatura","País")
fDatos_Clima


fDatos_Clima2005<- fDatos_Clima %>% filter(between(Fecha,as.Date("2005-01-01"),as.Date("2005-12-31"))) %>% mutate(Año= "2005")
fDatos_Clima2005

fDatos_Clima2006 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2006-01-01"),as.Date("2006-12-31"))) %>% mutate(Año= "2006")
fDatos_Clima2006

fDatos_Clima2007 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2007-01-01"),as.Date("2007-12-31"))) %>% mutate(Año= "2007")
fDatos_Clima2007

fDatos_Clima2008 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2008-01-01"),as.Date("2008-12-31"))) %>% mutate(Año= "2008")
fDatos_Clima2008

fDatos_Clima2009 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2009-01-01"),as.Date("2009-12-31"))) %>% mutate(Año= "2009")
fDatos_Clima2009

fDatos_Clima2010 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2010-01-01"),as.Date("2010-12-31"))) %>% mutate(Año= "2010")
fDatos_Clima2010

fDatos_Clima2011 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2011-01-01"),as.Date("2011-12-31"))) %>% mutate(Año= "2011")
fDatos_Clima2011

fDatos_Clima2012 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2012-01-01"),as.Date("2012-12-31"))) %>% mutate(Año= "2012")
fDatos_Clima2012

fDatos_Clima2013 <- fDatos_Clima %>% filter(between(Fecha,as.Date("2013-01-01"),as.Date("2013-12-31"))) %>% mutate(Año= "2013")
fDatos_Clima2013

# No hemos conseguido hacerlo todo en la misma tabla por eso hemos hecho una tabla para cada año y luego las hemos unido.

f1Datos_Clima <-full_join(x=fDatos_Clima2005,y=full_join(x=fDatos_Clima2006,y=full_join(x=fDatos_Clima2007,y=full_join(x=fDatos_Clima2008,y=full_join(x=fDatos_Clima2009,y=full_join(x=fDatos_Clima2010,y=full_join(x=fDatos_Clima2011,y=full_join(x=fDatos_Clima2012,y=fDatos_Clima2013))))))))
f1Datos_Clima
#View(f1Datos_Clima)

# Vamos a llamar mDatos_Clima, porque está agrupado por meses
mDatos_Clima<-f1Datos_Clima %>% select(Fecha,Temperatura,País,Año)
mDatos_Clima

# Hasta aquí tenemos los datos por meses, ahora tenemos que hacer la media por años. Utilizamos un group_by por País y año
Clima <- mDatos_Clima %>% group_by(País,Año) %>% summarise(Temperatura_Media=mean(Temperatura,na.rm=TRUE))
Clima
#View(Clima)

# Cambio el tipo de la columna Año en la tabla Clima, de character a double/ integer.
Clima <- mutate(Clima,Año=as.integer(Año))
Clima


# * Suicidio --------------------------------------------------------------
# Creamos un tibble "Suicidio" que almacena las columnas que nos interesan de "fDatos_Suicidio" renombradas.
Suicidio <- fDatos_Suicidio %>% 
  select(ParentLocation, Location, Period, Dim1, FactValueNumeric) %>% 
  rename(Region = ParentLocation, Pais = Location, Año = Period, Sexo = Dim1, Tasa_suicidio = FactValueNumeric) %>% 
  mutate(Año = as.integer(Año))
Suicidio
