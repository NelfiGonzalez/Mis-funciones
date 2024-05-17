library(car)
library(rsm)

#Cargando funciones de usuario desde repositorio en github 
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

##Lectura archivo datosproblemaseleccionpersonal.csv desde github
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")
attach(datos)
names(datos) #Revisando nombre de variables en el data.frame datos
#-------------------------------------------------------------------
#Ajuste del modelo completo y su ANOVA
MF=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)
MiAnova(model=MF)
#-------------------------------------------------------------------
#Ejemplo 1: Test H0: beta1=beta2, beta3=beta4
#Ajuste del modelo reducido y su ANOVA
MR1=lm(punt.Aptitud~I(Prueba1+Prueba2)+I(Prueba3+Prueba4))
MiAnova(model=MR1)

#Ejecucion del test
linearHypothesis(MF,c("Prueba1=Prueba2","Prueba3=Prueba4")) 
#-------------------------------------------------------------------
#Ejemplo 2: Test H0:beta1=beta2=0, beta3=beta4
#Ajuste del modelo reducido y su ANOVA
MR2=lm(punt.Aptitud~I(Prueba3+Prueba4))
MiAnova(model=MR2)

#Ejecucion del test
linearHypothesis(MF,c("Prueba1=0","Prueba2=0","Prueba3=Prueba4"))
#-------------------------------------------------------------------
#Ejemplo 3: Test beta1=beta2=beta3=beta4=0
#Ajuste del modelo reducido y su ANOVA
MR3=lm(punt.Aptitud~1)
summary(MR3)
anova(MR3) #como no hay predictores no puede usarse funcion de usuario miAnova() para la ANOVA del MR

#Ejecucion del test
linearHypothesis(MF,c("Prueba1=0","Prueba2=0","Prueba3=0","Prueba4=0"))
#-------------------------------------------------------------------
#Ejemplo 4: Test beta2=0
#Ajuste del modelo reducido y su ANOVA
MR4=lm(punt.Aptitud~Prueba1+Prueba3+Prueba4)
MiAnova(model=MR4)

#Ejecucion del test
linearHypothesis(MF,c("Prueba2=0"))
#-------------------------------------------------------------------
#Ejemplo 5: Test beta1=beta2=0
#Ajuste del modelo reducido y su ANOVA
MR5=lm(punt.Aptitud~Prueba3+Prueba4)
MiAnova(model=MR5)

linearHypothesis(MF,c("Prueba1=0","Prueba2=0"))
#-------------------------------------------------------------------
#Ejemplo 6: Test beta1=beta2=beta3=beta4
#Ajuste del modelo reducido y su ANOVA
MR6=lm(punt.Aptitud~I(Prueba1+Prueba2+Prueba3+Prueba4))
MiAnova(model=MR6)

#Ejecucion del test
linearHypothesis(MF,c("Prueba1=Prueba2","Prueba2=Prueba3","Prueba3=Prueba4"))
#-------------------------------------------------------------------
#Ejemplo 7: beta3-beta4=0.5
#Ajuste del modelo reducido y su ANOVA
MR7=lm(I(punt.Aptitud-0.5*Prueba3)~Prueba1+Prueba2+I(Prueba3+Prueba4))
MiAnova(model=MR7)

#Ejecucion del test
linearHypothesis(MF,c("Prueba3-Prueba4=0.5"))

#Obteniendo la tabla de valores para las variables en el modelo reducido ajustado
model.frame(MR7)
#-------------------------------------------------------------------
detach(datos)
