# Importación de Datos ----------------------------------------------------

# Importamos las librerías que vamos a utilizar
library(readr)
library(tidyverse)
library(dplyr)

# Carga de datos del clima desde 1700 a 2013:
Datos_Clima <- read_csv("INPUT/DATA/GlobalLandTemperaturesByCountry.csv")
Datos_Clima #Vemos que los datos son correctos

# Carga de datos de suicidio desde el 2000 al 2019:
Datos_Suicidio <- read_csv("INPUT/DATA/SuicideGlobalRates.csv")
Datos_Suicidio
# La tasa media de suicidios por 100.000 habitantes se encuentra en la columna "FactValueNumeric"

# Carga de datos de los diferentes sectores de empleo por países desde el 2000 al 2013:
Datos_Sectores <- read_delim("INPUT/DATA/EmploymentSectors.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Datos_Sectores

#Carga de datos de emisiones anuales de CO2 por países
Datos_Emisiones <- read_delim("INPUT/DATA/AnnualCO2EmissionsPerCountry.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Datos_Emisiones

# Seleccionamos datos de los años de 2005 a 2013 -------------------------------------------
#vamos a utilizar la letra 'f' de final, porque serán los datos que utilizaremos--> SE PUEDE CAMBIAR, NO DEFINITIVO ()

#Datos del clima de 2005 a 2013
fDatos_Clima <- Datos_Clima %>% filter(between(dt,as.Date("2005-01-01"),as.Date("2013-12-31"))) # hay que hacer algo con esto porque en este fichero es tipo date pero en los otros no (son tipo double)
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
# Seleccionamos y renombramos las columnas de interés.

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

# Cambio el tipo de la columna Año en la tabla Clima, de character a double/integer.
Clima <- mutate(Clima,Año=as.integer(Año))
Clima


# * Suicidio --------------------------------------------------------------
# Creamos un tibble "Suicidio" que almacena las columnas que nos interesan de "fDatos_Suicidio" renombradas.
Suicidio <- fDatos_Suicidio %>% 
  select(ParentLocation, Location, Period, Dim1, FactValueNumeric) %>% 
  rename(Region = ParentLocation, Pais = Location, Año = Period, Sexo = Dim1, Tasa_suicidio = FactValueNumeric) %>% 
  mutate(Año = as.integer(Año))
Suicidio

# * Emisiones CO2 --------------------------------------------------------------
# Se crea un tibble "Contaminación" que almacena las columnas con las que se va a trabajar de "fDatos_Emisiones" y se renombran las columnas para que tengan los mismos nombres que el resto de tablas.

Contaminacion <- fDatos_Emisiones %>%
  select(Entity, Year, `Annual CO2 emissions`) %>%
  rename(Pais = Entity, Año = Year, Emisiones_Anuales = `Annual CO2 emissions`) %>%
  mutate(Año = as.integer(Año))
Contaminacion

  
# * Sectores Laborales --------------------------------------------------------------
# Se crea un tibble "Sectores" que almacena las columnas con las que se va a trabajar de "fDatos_Sectores" y se renombran las columnas para que tengan los mismos nombres que el resto de tablas.

Sectores <- fDatos_Sectores %>%
  select(Entity, Year, `Employment in agriculture (% of total employment) (modeled ILO estimate)`, `Employment in industry (% of total employment) (modeled ILO estimate)`, `Employment in services (% of total employment) (modeled ILO estimate)`) %>%
  rename(Pais = Entity, Año = Year, Empleo_Agricultura = `Employment in agriculture (% of total employment) (modeled ILO estimate)`, Empleo_Industria = `Employment in industry (% of total employment) (modeled ILO estimate)`, Empleo_Servicios = `Employment in services (% of total employment) (modeled ILO estimate)`) %>%
  mutate(Año = as.integer(Año))
Sectores




# OBJETIVOS ---------------------------------------------------------------
# *Evaluar el impacto de la temperatura media anual de un país en su tasa global de suicidio.-------- 


# *Estudiar la correlación entre los niveles de industrialización de un país (sector secundario) y sus tasas de emisión de CO2.-----  


# *Evaluar si las tasas de suicidios son más elevadas en países preeminentemente industriales, y su asociación al sexo.-------
# Elegimos países con mayor tasa de industrialización:
Sectores

#***Segunda Forma (BUENA?)....................----
Sectores1 <- Sectores %>% filter(Año==2013) %>% filter(Empleo_Industria>=35)
Sectores1

Suicidio
Sectores_Suicidio <-left_join(x=Sectores1, y=Suicidio)
Sectores_Suicidio

# SS <- Sectores_Suicidio %>% select(Pais, Sexo, Tasa_suicidio)
# SS

# ggplot(data = SS, mapping=aes(x =Pais, y=Tasa_suicidio)) +
#   geom_col(aes(fill = Sexo), position = "dodge") 

ggplot(data = Sectores_Suicidio, mapping=aes(x =Pais, y=Tasa_suicidio)) +
  geom_col(aes(fill = Sexo), position = "dodge") 

#***Segunda Forma (BUENA?)....................----
Sectores2 <- Sectores %>% filter(Año==2013)
Sectores2

Sectores_Suicidio2 <-right_join(x=Sectores2, y=Suicidio)
# Lo hacemos con europa para una mejor visualizacion, pero hay que elegir los que más industria tengan 
# y los que menos y compararlos
Sectores_Suicidio2 <- Sectores_Suicidio2 %>% filter(Region=='Europe')
Sectores_Suicidio2 


# ggplot(data = Sectores_Suicidio2, mapping=aes(x =Pais, y=Porcentaje_Sector)) +
#   geom_col(aes(fill = Sectores), position = "dodge")


#gráfico en el que se muestra la tasa de suicidios por paises:
g2 <- ggplot(data = Sectores_Suicidio2 %>% filter(Año==2013), aes(x = Pais, y = Tasa_suicidio)) +
  geom_point(aes(colour = factor(Sexo))) +
  scale_color_discrete(labels = c("Ambos sexos", "Mujer", "Varón"))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))#+
#facet_wrap(Pais)
g2


# PRUEBA DE SUMAR LAS GRÁFICAS DE PUNTOS CON LAS DE BARRAS
# ---MAL---
# Hay que averiguar como superponer las 2 gráficas que hemos hecho antes (g1 y g2)
#
# ggplot(data = ss2, mapping=aes(x =Pais, y=Porcentaje_Sector)) +
#   geom_col(aes(fill = Sectores), position = "dodge") +
#   #ggplot(data = Sectores_Suicidio2, aes(x = Pais, y = Tasa_suicidio)) +
#   geom_point(aes(colour = factor(Sexo)))


# *Análisis del aumento de emisiones de CO2 y su impacto en la temperatura media de una región/país.-----  


# *Comparación de las tasas de suicidio en países desarrollados, en vías de desarrollo y subdesarrollados. Análisis sectorial.-----
