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
fDatos_Emisiones <- Datos_Emisiones %>% filter(between(Year,2005,2013))
fDatos_Emisiones


# Modificación de tablas --------------------------------------------
# Seleccionamos y renombramos las columnas de interés.

# * Clima -----------------------------------------------------------------
# Modificamos la tabla del clima para quitar la columna de desviación y cambiamos el nombre de las columnas
colnames(fDatos_Clima)<-c("Fecha","Temperatura","DesviacionTemperatura","Pais")
fDatos_Clima

# Vamos a llamar mDatos_Clima, porque está agrupado por meses
mDatos_Clima<- fDatos_Clima %>% 
  mutate(Año= as.integer(format(Fecha,'%Y')))  %>% 
  select(Fecha,Temperatura,Pais,Año)

# Hasta aquí tenemos los datos por meses, ahora tenemos que hacer la media por años. Utilizamos un group_by por País y año
Clima <- mDatos_Clima %>% group_by(Pais,Año) %>% summarise(Temperatura_Media=mean(Temperatura,na.rm=TRUE))
Clima
#View(Clima)


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

# #***Primera Forma ....................----
# Sectores1 <- Sectores %>% filter(Año==2013) %>% filter(Empleo_Industria>=35)
# Sectores1
# 
# Suicidio
# Sectores_Suicidio <-left_join(x=Sectores1, y=Suicidio)
# Sectores_Suicidio
# 
# # SS <- Sectores_Suicidio %>% select(Pais, Sexo, Tasa_suicidio)
# # SS
# 
# # ggplot(data = SS, mapping=aes(x =Pais, y=Tasa_suicidio)) +
# #   geom_col(aes(fill = Sexo), position = "dodge") 
# 
# ggplot(data = Sectores_Suicidio, mapping=aes(x =Pais, y=Tasa_suicidio)) +
#   geom_col(aes(fill = Sexo), position = "dodge") 

#***Segunda Forma (BUENA?)....................----
Sectores2 <- Sectores %>% filter(Año==2013)
Sectores2

Sectores_Suicidio2 <-right_join(x=Sectores2, y=Suicidio)
# Lo hacemos con europa para una mejor visualizacion, pero hay que elegir los que más industria tengan 
# y los que menos y compararlos
Sectores_Suicidio2 <- Sectores_Suicidio2 %>% filter(Region=='Europe')
Sectores_Suicidio2 



# ***Forma Buena (P) ------------------------------------------------------

Objetivo3 <- Sectores_Suicidio2 %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo!='Both sexes') %>% ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  facet_wrap(~Sexo, scales='free_y')

Objetivo3

#Para mujeres gráfico separado
Sectores_Suicidio2 %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Female') %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))

#Para hombres gráfico separado
Sectores_Suicidio2 %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Male') %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))



SS2 <- pivot_wider(data = Sectores_Suicidio2, names_from = "Sexo", values_from = "Tasa_suicidio")
SS2 

SS3 <- SS2 %>% 
  pivot_longer(names_to = "Sectores", values_to = "Porcentaje_Sector", cols = c(Empleo_Agricultura:Empleo_Servicios)) %>% 
  filter(Año==2013) %>% 
  select(Pais,Año,Female,Sectores,Porcentaje_Sector)

# Mujeres (Female)
# Gráfico en el que solo se muestran los porcentajes de los sectores por paises
g1 <- ggplot(data = SS3, mapping=aes(x =Pais, y=Porcentaje_Sector)) +
  geom_col(aes(fill = Sectores), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1


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

#Gráfico comparación nivel de industrialización con el aumento de la tasa de suicidios en países de Europa
#SS2 %>% select(Female)

#Quitar?-----
# g3_mujer <- ggplot(data =SS2 %>% select(Pais,Empleo_Industria,Female), aes(x = Empleo_Industria, y = Female)) +
#   geom_point(aes(colour = factor(Pais)))+
#   geom_smooth(method = "lm", colour = "red")
# g3_mujer
# 
# g3_hombre <- ggplot(data =SS2 %>% select(Pais,Empleo_Industria,Male), aes(x = Empleo_Industria, y = Male)) +
#   geom_point(aes(colour = factor(Pais)))+
#   geom_smooth(method = "lm", colour = "red")
# g3_hombre
# 
# g3_AS <- ggplot(data =SS2 %>% select(Pais,Empleo_Industria,`Both sexes`), aes(x = Empleo_Industria, y = `Both sexes`)) +
#   geom_point(aes(colour = factor(Pais)),show.legend = FALSE)+
#   geom_smooth(method = "lm", colour = "red")
# g3_AS

# Gráfico comparación nivel de industrialización con el aumento de la tasa de suicidios en países del mundo

#Quitar?-----
# Sectores3 <- Sectores %>% filter(Año==2013)
# Sectores3
# 
# Sectores_Suicidio3 <-right_join(x=Sectores3, y=Suicidio)
# 
# SS4 <- Sectores_Suicidio3 %>% 
#   pivot_wider(names_from = "Sexo", values_from = "Tasa_suicidio")
# SS4
# 
# g4_mujer <- ggplot(data =SS4 %>% select(Pais,Empleo_Industria,Female), aes(x = Empleo_Industria, y = Female)) +
#   geom_point(aes(colour = factor(Pais)),show.legend = FALSE)+
#   geom_smooth(method = "lm", colour = "red")
# g4_mujer
# 
# g4_hombre <- ggplot(data =SS4 %>% select(Pais,Empleo_Industria,Male), aes(x = Empleo_Industria, y = Male)) +
#   geom_point(aes(colour = factor(Pais)),show.legend = FALSE)+
#   geom_smooth(method = "lm", colour = "red")
# g4_hombre
# 
# g4_AS <- ggplot(data =SS4 %>% select(Pais,Empleo_Industria,`Both sexes`), aes(x = Empleo_Industria, y = `Both sexes`)) +
#   geom_point(aes(colour = factor(Pais)),show.legend = FALSE)+
#   geom_smooth(method = "lm", colour = "red")
# g4_AS


# *Análisis del aumento de emisiones de CO2 y su impacto en la temperatura media de una región/país.----- 

C_O4<- Datos_Clima %>% 
  filter(between(dt,as.Date("1970-01-01"),as.Date("2013-12-31")))
colnames(C_O4)<-c("Fecha","Temperatura","DesviacionTemperatura","Pais")

Clima_Obj_4 <-  C_O4%>% 
  mutate(Año= as.integer(format(Fecha,'%Y'))) %>% 
  select(Fecha,Temperatura,Pais,Año) %>% 
  group_by(Pais,Año) %>% summarise(Temperatura_Media=mean(Temperatura,na.rm=TRUE))


Contaminacion_Obj4<- Datos_Emisiones %>% 
  filter(between(Year,1970,2013))%>%
  select(Entity, Year, `Annual CO2 emissions`) %>%
  rename(Pais = Entity, Año = Year, Emisiones_Anuales = `Annual CO2 emissions`) %>%
  mutate(Año = as.integer(Año))


Clima_Obj_4
Contaminacion_Obj4


# IMPORTANTE: HAY QUE CAMBIAR EN LA TABLA CLIMA LA COLUMNA País A Pais

#Solo cogemos las tablas que necesitamos: Clima y Contaminacion
levels(factor(Clima_Obj_4$Pais))
levels(factor(Contaminacion_Obj4$Pais))

CC <- left_join(x=Contaminacion_Obj4, y=Clima_Obj_4 )

CC_Paises <- pivot_longer(CC, names_to = 'CO2_T', values_to = 'Valores', cols=c(Emisiones_Anuales, Temperatura_Media))

# Estaría guay que la persona introdujera el país que quisiera en algun momento  (escribiendo o mediante un desplegable que tenga lso niveles de países) 
# y fuese ese el país que filtrasemos. Se hace con taps, poniendo en el rMArkdown: 
# '### select the paramether{.tabset .tabset-fade .tabset-pills}'
# '#### Títulos de las gráficas'


# Por el momento elegiremos un país en desarrollo como India

CC_Paises %>% 
  filter(Pais =='India') %>% 
  ggplot(aes(x=Año, y=Valores))+
  geom_point(aes(colour=CO2_T),show.legend=FALSE)+
  geom_smooth(aes(colour=CO2_T),show.legend=FALSE)+
  #facet_wrap(~Pais)+
  facet_wrap(~CO2_T, scales='free_y')



CC_Paises %>% 
  filter(Pais =='India'| Pais=='Spain') %>% 
  ggplot(aes(x=Año, y=Valores))+
  geom_point(aes(colour=CO2_T),show.legend=FALSE)+
  geom_smooth(aes(colour=CO2_T),show.legend=FALSE)+
  facet_wrap(vars(Pais,CO2_T), scales='free_y')


#Lo mismo pero en vez de para un país concreto para el mundo entero, se espera que aumente
CC_Mundo<- CC %>%
  group_by(Año)%>% 
  summarise(Emisiones_Mundo= mean(Emisiones_Anuales, na.rm=TRUE), Temperatura_Mundo= mean(Temperatura_Media, na.rm= TRUE))

CC_Mundo

CC_Mundo %>%
  pivot_longer(names_to = 'CO2_T', values_to = 'Valores', cols=c(Emisiones_Mundo, Temperatura_Mundo)) %>% 
  ggplot(aes(x=Año, y=Valores))+
  geom_point(aes(colour=CO2_T), show.legend=FALSE)+
  geom_smooth(aes(colour=CO2_T), show.legend = FALSE) +
  facet_wrap(vars(CO2_T), scales='free_y')

