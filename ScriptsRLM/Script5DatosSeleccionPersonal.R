library(car)

#Lectura datos desde github
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")
attach(datos)
names(datos) #Revisando el nombre de las variables guardadas en el data.frame
#----------------------------------------------------------------------------------------------------------------------------------
#Ajuste del MRLM: Formula extendida 
modelo=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)

#O bien de la siguiente forma,
modelo=lm(punt.Aptitud~.,datos)

#Test outlier con correccion Bonferroni para la obs. con residuo externamente 
#estudentizado de mayor valor absoluto
outlierTest(modelo)

#Podemos correr el modelo del test outlier:
#indice de las observaciones
i=1:nrow(datos)
#Creando indicadora para el modelo del test outlier
I_out=ifelse(i==which.max(abs(rstudent(modelo))),1,0)
#Ajuste modelo del test
modout=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4+I_out)
summary(modout)

detach(datos)