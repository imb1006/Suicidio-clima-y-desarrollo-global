# Importación de Datos ----------------------------------------------------

# Para tener los datos del clima utilizamos importacion de csv utilizando las opciones: import dataset from text (readr)
library(readr)
Datos_Clima <- read_csv("INPUT/DATA/GlobalLandTemperaturesByCountry.csv")
Datos_Clima #Vemos que los datos son correctas

# Habría que elegir las fechas correctas porque ahora mismo tenemos todos los datos desde 1700 al 2013