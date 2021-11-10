# Importación de Datos ----------------------------------------------------
library(readr)

# Para tener los datos del clima utilizamos importacion de csv utilizando las opciones: import dataset from text (readr)
Datos_Clima <- read_csv("INPUT/DATA/GlobalLandTemperaturesByCountry.csv")
Datos_Clima #Vemos que los datos son correctos
# Habría que elegir las fechas correctas porque contiene todos los datos desde 1700 al 2013

# Carga de datos de suicidio desde el 2000 al 2019:
Datos_Suicidio <- read_csv("INPUT/DATA/SuicideGlobalRates.csv")
Datos_Suicidio
# La tasa media de suicidios por 100.000 habitantes se encuentra en la columna "FactValueNumeric"

# Carga de datos de los diferentes sectores de empleo por países desde el 2000 al 2013:
# NO ESTÁ BIEN CARGADO, CORREGIR!!!!!
Datos_Sectores <- read_csv("INPUT/DATA/EmploymentSectors.csv")
