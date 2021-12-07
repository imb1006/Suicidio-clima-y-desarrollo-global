# Importación de Datos ----------------------------------------------------

# Importamos las librerías que vamos a utilizar
#library(readr)
library(tidyverse)
#library(dplyr)

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

# Selección de datos de 2005 a 2013 -------------------------------------------

# El nombre del objeto contiene una "s" para indicar que esos datos ya han sido seleccionados según el año.

#Datos del clima de 2005 a 2013
sDatos_Clima <- Datos_Clima %>% filter(between(dt,as.Date("2005-01-01"),as.Date("2013-12-31")))
sDatos_Clima

#Datos de suicidios
sDatos_Suicidio <- Datos_Suicidio %>% filter(between(Period,2005,2013))
sDatos_Suicidio

#Datos de los distintos sectores de empleo
sDatos_Sectores <- Datos_Sectores %>% filter(between(Year,2005,2013))
sDatos_Sectores

#Datos de las emisiones de CO2
sDatos_Emisiones <- Datos_Emisiones %>% filter(between(Year,2005,2013))
sDatos_Emisiones


# Modificación de tablas --------------------------------------------
# Seleccionamos y renombramos las columnas de interés.


# * Clima -----------------------------------------------------------------

# Modificamos la tabla del clima para quitar la columna de desviación y cambiamos el nombre de las columnas
colnames(sDatos_Clima)<-c("Fecha","Temperatura","DesviacionTemperatura","Pais")
sDatos_Clima

# Vamos a llamar mDatos_Clima, porque está agrupado por meses
mDatos_Clima<- sDatos_Clima %>% 
  mutate(Año= as.integer(format(Fecha,'%Y')))  %>% 
  select(Fecha,Temperatura,Pais,Año)

# Hacer la media por años utilizando un group_by por País y año
Clima <- mDatos_Clima %>% group_by(Pais,Año) %>% summarise(Temperatura_Media=mean(Temperatura,na.rm=TRUE))
Clima


# * Suicidio --------------------------------------------------------------
# Creamos un tibble "Suicidio" que almacena las columnas que nos interesan de "sDatos_Suicidio" renombradas.
Suicidio <- sDatos_Suicidio %>% 
  select(ParentLocation, Location, Period, Dim1, FactValueNumeric) %>% 
  rename(Region = ParentLocation, Pais = Location, Año = Period, Sexo = Dim1, Tasa_suicidio = FactValueNumeric) %>% 
  mutate(Año = as.integer(Año))
Suicidio


# * Emisiones CO2 --------------------------------------------------------------

# Se crea un tibble "Contaminación" que almacena las columnas con las que se va a trabajar de "sDatos_Emisiones"
#y se renombran las columnas para que tengan los mismos nombres que el resto de tablas.

Contaminacion <- sDatos_Emisiones %>%
  select(Entity, Year, `Annual CO2 emissions`) %>%
  rename(Pais = Entity, Año = Year, Emisiones_Anuales = `Annual CO2 emissions`) %>%
  mutate(Año = as.integer(Año))
Contaminacion

  
# * Sectores Laborales --------------------------------------------------------------
# Se crea un tibble "Sectores" que almacena las columnas con las que se va a trabajar de "sDatos_Sectores" 
#y se renombran las columnas para que tengan los mismos nombres que el resto de tablas.

Sectores <- sDatos_Sectores %>%
  select(Entity, Year, `Employment in agriculture (% of total employment) (modeled ILO estimate)`, `Employment in industry (% of total employment) (modeled ILO estimate)`, `Employment in services (% of total employment) (modeled ILO estimate)`) %>%
  rename(Pais = Entity, Año = Year, Empleo_Agricultura = `Employment in agriculture (% of total employment) (modeled ILO estimate)`, Empleo_Industria = `Employment in industry (% of total employment) (modeled ILO estimate)`, Empleo_Servicios = `Employment in services (% of total employment) (modeled ILO estimate)`) %>%
  mutate(Año = as.integer(Año))
Sectores



# Objetivos ---------------------------------------------------------------
# Tema común para las gráficas
Tema_Graficas<- theme_bw()+
  theme(panel.border = element_rect(linetype = "solid", fill = NA, colour="grey70"),
        strip.background = element_rect(colour = "grey75", fill = "grey95"),# Borde del titulo al hacer wrap
        strip.text.x = element_text(colour = "grey25", face = "bold.italic", size = 11),
        axis.text = element_text(colour = "grey40", size = 11))


# * 1.Evaluar el impacto de la temperatura media anual de un país en su tasa global de suicidio.-------- 

# Países disponibles en ambos set de datos:
levels(factor(Clima$Pais))
levels(factor(Suicidio$Pais))

# Unimos los dos set de datos que vamos a utilizar obteniendo un tibble que contiene los países de 
#la tabla "Suicidio", que era la que menos tenía.
Clima_Suicidio <-left_join(x = Suicidio, y = Clima) %>% 
  arrange(Pais, Año, Sexo)

levels(factor(Clima_Suicidio$Pais))

# Nota!! Todas las comparaciones entre países se van a realizar seleccionando países Europeos para poder
#obtener una mejor visualización de la influencia de la temperatura en la tasa de suicidios, evitando que
#factores mucho más influyentes (ej. nivel de industrialización) afecten al resultado.

# Gráfica de dispersión de comparación entre varios países europeos
Clima_Suicidio %>% 
  filter(Tasa_suicidio > 0, Sexo == "Female") %>% 
  filter( Pais == "Finland" | Pais == "Germany" | Pais == "Iceland" |
           Pais == "Ireland" | Pais == "Italy" | Pais == "Netherlands" | Pais == "Norway" | Pais == "Poland" |
           Pais == "Portugal" | Pais == "Spain" | Pais == "Sweden" ) %>% 
  ggplot(aes(x = Temperatura_Media, y = Tasa_suicidio))+
  geom_point(aes(colour = Pais))+
  geom_smooth(aes(colour = Pais))+
  labs(title = "Tasa de suicidio y temperatura en Europa",x = "Temperadura media", y = "Tasa de suicidio")

# Gráficas de dispersión por cada país
Clima_Suicidio %>% 
  filter(Tasa_suicidio > 0, Sexo == "Female") %>% 
  filter( Pais == "Finland" | Pais == "Germany" | Pais == "Iceland" |
            Pais == "Ireland" | Pais == "Italy" | Pais == "Netherlands" | Pais == "Norway" | Pais == "Poland" |
            Pais == "Portugal" | Pais == "Spain" | Pais == "Sweden" ) %>% 
  ggplot(aes(x = Temperatura_Media, y = Tasa_suicidio))+
  geom_point(aes(colour = Pais), show.legend = FALSE)+
  geom_smooth(aes(colour = Pais), show.legend = FALSE)+
  labs(title = "Tasa de suicidio y temperatura en Europa", x = "Temperadura media", y = "Tasa de suicidio")+
  facet_wrap(vars(Pais))



# * 2.Estudiar la correlación entre los niveles de industrialización de un país (sector secundario) y sus tasas de emisión de CO2.-----  



# * 3.Comparación de las tasas de suicidio según los distintos sectores de producción de cada país y su relación con el sexo.-------

# Nota!! Vamos a trabajar solo con países de Europa para una mejor visualización de los datos

# Generamos un tibble que unifica los datos de sectores de empleo y de suicidio por países durante el año 2013 
Sectores_Suicidio <- Sectores %>% 
  filter(Año == 2013) %>% 
  right_join(x=., y = Suicidio %>% 
               filter(Año == 2013)) %>% 
  filter(Region == "Europe")

Sectores_Suicidio 

# Gráfico de dispersión que muestra por separado las tasas de suicidios de hombres y mujeres según el % de
# empleo que hay en cada sector en los países europeos.
Sectores_Suicidio %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo!='Both sexes') %>% drop_na() %>% ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  facet_wrap(~Sexo, scales='free_y')+
  labs(title = "Tasa de suicidios por sector en ambos sexos",x = "Porcentaje", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas

# Para mujeres gráfico separado
Sectores_Suicidio %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Female') %>% drop_na() %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  labs(title = "Tasa de suicidios por sector en mujeres",x = "Porcentaje", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas

# Para hombres gráfico separado
Sectores_Suicidio %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Male') %>% drop_na() %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  labs(title = "Tasa de suicidios por sector en hombres",x = "Porcentaje", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas


# * 4.Análisis del aumento de emisiones de CO2 y su impacto en la temperatura media de una región/país.----- 

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
  facet_wrap(~CO2_T, scales='free_y')+
  labs(title = "Tasa de emisiones y temperatura en India",x = "Años", y = " ")+
  Tema_Graficas


CC_Paises %>% 
  filter(Pais =='India'| Pais=='Spain') %>% 
  ggplot(aes(x=Año, y=Valores))+
  geom_point(aes(colour=CO2_T),show.legend=FALSE)+
  geom_smooth(aes(colour=CO2_T),show.legend=FALSE)+
  facet_wrap(vars(Pais,CO2_T), scales='free_y')+
  labs(title = "Tasa de emisiones y temperatura en India y en España",x = "Años", y = " ")+
  Tema_Graficas


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
  facet_wrap(vars(CO2_T), scales='free_y')+
  labs(title = "Tasa de emisiones y temperatura en el mundo",x = "Años", y = " ")+
  Tema_Graficas

