# Importación de Datos ----------------------------------------------------


# Importamos las librerías que vamos a utilizar
library(tidyverse)
# Librerías ya incluidas en tidyverse:
#library(readr)
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
#factores mucho más influyentes (ej. nivel de industrialización, nivel socioeconómico) afecten al resultado.

# Gráfica de dispersión de comparación entre varios países europeos
Clima_Suicidio %>% 
  filter(Tasa_suicidio > 0, Sexo == "Female") %>% 
  filter( Pais == "Finland" | Pais == "Germany" | Pais == "Iceland" |
           Pais == "Ireland" | Pais == "Italy" | Pais == "Netherlands" | Pais == "Norway" | Pais == "Poland" |
           Pais == "Portugal" | Pais == "Spain" | Pais == "Sweden" ) %>% 
  ggplot(aes(x = Temperatura_Media, y = Tasa_suicidio))+
  geom_point(aes(colour = Pais))+
  geom_smooth(aes(colour = Pais))+
  labs(title = "Tasa de suicidio y temperatura en Europa",x = "Temperatura media en ºC", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas

#Otra opcion de visualización
Clima_Suicidio %>% 
  filter(Region == 'Europe', drop.na=TRUE) %>% filter( Año==2010) %>% 
  filter(Tasa_suicidio > 0, Sexo == "Female", drop.na=TRUE) %>% 
  ggplot(aes(x = Temperatura_Media, y = Tasa_suicidio))+
  geom_point(aes(colour = Pais), show.legend = FALSE)+
  geom_smooth( show.legend = FALSE)+
  labs(title = "Tasa de suicidio y temperatura en Europa",x = "Temperatura media en ºC", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas



# Gráficas de dispersión por cada país
Clima_Suicidio %>% 
  filter(Tasa_suicidio > 0, Sexo == "Female") %>% 
  filter( Pais == "Finland" | Pais == "Germany" | Pais == "Iceland" |
            Pais == "Ireland" | Pais == "Italy" | Pais == "Netherlands" | Pais == "Norway" | Pais == "Poland" |
            Pais == "Portugal" | Pais == "Spain" | Pais == "Sweden" ) %>% 
  ggplot(aes(x = Temperatura_Media, y = Tasa_suicidio))+
  geom_point(aes(colour = Pais), show.legend = FALSE)+
  geom_smooth(aes(colour = Pais), show.legend = FALSE)+
  labs(title = "Tasa de suicidio y temperatura en Europa", x = "Temperatura media en ºC", y = "Tasa de suicidio por 100000 habitantes")+
  facet_wrap(vars(Pais))+
  Tema_Graficas



# * 2.Estudiar la correlación entre los niveles de industrialización de un país (sector secundario) y sus tasas de emisión de CO2.-----  

# Países disponibles en ambos sets de datos:
levels(factor(Contaminacion$Pais))
levels(factor(Sectores$Pais))

# Se unen los sets de datos, obteniendo un tibble que contiene los países de 
#la tabla "Contaminacion", que era la que menos nombres contenía.

# Nota!! Se va a realizar un doble análisis:
# 2.1.- Ver la evolución de un país; es decir, si conforme aumenta la industrialización lo hacen también sus emisiones de CO2

Contaminacion_Industria <- Contaminacion %>%
  left_join(x =., y = Sectores) 

levels(factor(Contaminacion_Industria$Pais))

#Gráfico de correlación entre la industrialización y la emisión de CO2 en un país a lo largo de los años. Se ha seleccionado China.

Contaminacion_Industria %>% 
  filter( Pais == "China") %>% 
  ggplot(aes(x = Empleo_Industria, y = Emisiones_Anuales))+
  geom_point(aes(colour = Pais), show.legend = FALSE)+
  geom_smooth(aes(colour = Pais), show.legend = FALSE)+
  labs(title = "Correlación entre el nivel de industrialización y las emisiones de CO2", x = "Porcentaje de empleo en industria", y = "Emisiones de C02 anuales en millones de toneladas")+
  #Nos permite distinguir el país que es, más útil si se hubiera hecho filter con varios países, aquí solo sirve para mostrar el nombre del país
  facet_wrap(vars(Pais))+
  Tema_Graficas

#Como puede observarse, a medida que aumenta el nivel de industria del país, aumentan las emisiones de CO2 producidas por este. Esto confirma la relación de proporcionalidad directa entre ambos factores.



# 2.2.- Una comparación entre países más industrializados y menos, para ver cómo varían sus tasas a lo largo de dicho periodo de tiempo, así como para observar diferencias significativas en las tasas de emisiones de CO2.

# Nota!! Para asegurar que los datos no se vean influenciados por otras variables, se van a seleccionar solo los países que superen en un 25% el empleo basado en la industria, y se van a comparar con aquellos inferiores al 10%

#Contaminacion_Industria_AltosYBajos <- Contaminacion %>%
  #left_join(x =., y = Sectores) %>% 
  #filter(Empleo_Industria >= 25.0 | Empleo_Industria <= 10.0)

#levels(factor(Contaminacion_Industria_AltosYBajos$Pais))

Contaminacion_Industria_Altos <- Contaminacion %>%
  left_join(x =., y = Sectores) %>% 
  filter(Empleo_Industria >= 25.0)

levels(factor(Contaminacion_Industria_Altos$Pais))

Contaminacion_Industria_Bajos <- Contaminacion %>%
  left_join(x =., y = Sectores) %>% 
  filter(Empleo_Industria <= 10.0)

levels(factor(Contaminacion_Industria_Bajos$Pais))

#Si se observan los resultados obtenidos, los países con mayores tasas de industrialización son en su mayoría países desarrollados de Europa y Asia, mientras que los países poco industrializados son considerados el "Tercer Mundo" y se encuentran en África, Sudamérica y Asia

#Se van a seleccionar 5 países de cada grupo aleatoriamente, ya que trabajar con todos ellos llevaría a una mala visualización de los datos en las gráficas

#Emisiones_Altos: Austria, Egypt, Germany, Iran, Malaysia, Mexico, South Africa, Spain, Turkey, United Arab Emirates

Contaminacion_Industria_A <- Contaminacion_Industria_Altos %>%
  filter( Pais == "Austria" | Pais == "Egypt" | Pais == "Germany" | Pais == "Iran" | Pais == "Malaysia" | Pais == "Mexico" | Pais == "South Africa" | Pais == "Spain" |Pais == "Turkey" | Pais == "United Arab Emirates")
 
Contaminacion_Industria_A

Contaminacion_Industria_A %>% 
  ggplot(aes(x = Empleo_Industria, y = Emisiones_Anuales))+
  geom_point(aes(colour = Pais), show.legend = TRUE)+
  geom_smooth(aes(colour = Pais), show.legend = FALSE)+
  labs(title = "Contaminación en países fuertemente industrializados", x = "Porcentaje de empleo en industria", y = "Emisiones de C02 anuales en millones de toneladas")+
  #facet_wrap(vars(Pais))+ Si se quisieran observar por separado, aunque no permite realizar la comparación visualmente
  Tema_Graficas


#Emisiones_Bajos: Ethiopia, Haiti, Laos, Mali, Mozambique, Niger, Sierra Leone, Somalia, Solomon Islands, Uganda

Contaminacion_Industria_B <- Contaminacion_Industria_Bajos %>% 
  filter( Pais == "Ethiopia" | Pais == "Haiti" | Pais == "Laos" | Pais == "Mali" | Pais == "Mozambique" | Pais == "Niger" | Pais == "Sierra Leone" | Pais == "Somalia" |Pais == "Solomon Islands" | Pais == "Uganda") 
  
Contaminacion_Industria_B

Contaminacion_Industria_B %>%
  ggplot(aes(x = Empleo_Industria, y = Emisiones_Anuales))+
  geom_point(aes(colour = Pais), show.legend = TRUE)+
  geom_smooth(aes(colour = Pais), show.legend = FALSE)+
  labs(title = "Contaminación en países débilmente industrializados", x = "Porcentaje de empleo en industria", y = "Emisiones de C02 anuales en millones de toneladas")+
  #facet_wrap(vars(Pais))+ Si se quisieran observar por separado, aunque no permite realizar la comparación visualmente
  Tema_Graficas

#Si se observan ambas gráficas al mismo tiempo, haciendo especial hincapié en el eje de abscisas, donde aparecen las unidades de emisiones de CO2, se observa que los índices son superiores en los países con mayor porcentaje. 

#Sin embargo, se requiere de mayor análisis para eliminar la influencia de otras posibles variables que alteren estos resultados, aunque a simple vista la relación estimada se cumple.



# * 3.Comparación de las tasas de suicidio según los distintos sectores de producción de cada país y su relación con el sexo.-------

# Nota!! Vamos a trabajar solo con países de Europa para una mejor visualización de los datos

# Generamos un tibble que unifica los datos de sectores de empleo y de suicidio por países durante el año 2013 
Sectores_Suicidio <- Sectores %>% 
  filter(Año == 2013) %>% 
  right_join(x=., y = Suicidio %>% 
               filter(Año == 2013)) %>% 
  filter(Region == "Europe")

Sectores_Suicidio 

# Representamos los datos en gráficos de dispersión que muestran la tasa de suicidio según el % de empleo que hay en cada sector
#en los países europeos.

# Separamos los datos de mujeres y hombres porque los de estos últimos presentan una tasa de suicidio mucho mayor que creemos que
#puede influenciar los resultados.

# Mujeres
Sectores_Suicidio %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Female') %>% drop_na() %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  labs(title = "Tasa de suicidios por sector en mujeres",x = "Porcentaje empleo", y = "Tasa de suicidio por 100000 habitantes")+
  Tema_Graficas

# Hombres
Sectores_Suicidio %>% 
  pivot_longer(cols = starts_with("Empleo_"),names_to = "Sectores",values_to = "Porcentaje") %>% 
  filter(Sexo=='Male') %>% drop_na() %>% 
  ggplot(aes(x=Porcentaje,y=Tasa_suicidio))+
  geom_point(aes(colour=Sectores))+
  geom_smooth(aes(colour=Sectores))+
  labs(title = "Tasa de suicidios por sector en hombres",x = "Porcentaje empleo", y = "Tasa de suicidio por 100000 habitantes")+
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


# Por el momento elegiremos un país en desarrollo como India, 
# en el Markdown se elegirán distintos países para su visualización

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

