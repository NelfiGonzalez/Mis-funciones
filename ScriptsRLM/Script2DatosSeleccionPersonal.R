library(car)
library(rsm)
#Cargando funciones de usuario desde repositorio en github 
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")
#Lectura de los datos desde repositorio en github
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")
attach(datos)
names(datos) #Revisando el nombre de las variables guardadas en el data.frame
#Ajuste del MRLM
modelo=lm(punt.Aptitud~.,datos)

#Anova del modelo
tablaAnova=MiAnova(model=modelo)

Anova(modelo) #Produce sumas de cuadrados SS2
anova(modelo) #Produce sumas de cuadrados SS1

#Pruebas F individuales sobre betaj asociado a Xj
linearHypothesis(modelo,"Prueba1=0") #Prueba F, beta1=0
linearHypothesis(modelo,"Prueba2=0") #Prueba F, beta2=0
linearHypothesis(modelo,"Prueba3=0") #Prueba F, beta3=0
linearHypothesis(modelo,"Prueba4=0") #Prueba F, beta4=0

#Prueba F simultanea, beta1=0 y beta2=0
linearHypothesis(modelo,c("Prueba1=0","Prueba2=0"))  

detach(datos)
