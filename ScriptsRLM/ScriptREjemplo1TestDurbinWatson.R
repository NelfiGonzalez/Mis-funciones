#Ejemplo 1 aplicacion Test Durbin-Watson
rm(list=ls(all=TRUE))
library(car)
library(rsm)
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Lectura datos serie de tiempo simulada
datos=scan("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/seriesimul.txt",sep=" ",dec=".",skip=1)

#Convirtiendo en serie de tiempo al conjunto de datos
#En la funcion ts() freq=12 indica que por anualidad se registrasn 12 valores (cada mes)
#En la Funcion ts() start=c(2001,1) indica que el primer dato tiene fecha enero de 2001 
datos=ts(datos,freq=12,start=c(2001,1))
datos

#Grafica de la serie de tiempo: Datos vs. fechas; los puntos son interpolados por lineas
win.graph()
plot(datos)

#Definiendo el predictor lineal, el indice de tiempo t, para el modelo de tendencia lineal
t=1:length(datos)

#Ajuste del modelo de tendencia lineal
modelo=lm(datos~t)
summary(modelo)

#Respuesta estimada definida como serie de tiempo con misma frecuencia y fechas que los datos
yhat=ts(fitted(modelo),freq=frequency(datos),start=start(datos))

#Grafica de la serie de datos y tendencia lineal ajustada
win.graph()
plot(datos)
lines(yhat,col=2,lwd=2)
legend("topleft",legend=c("Datos observados","Ajuste del MRL"),col=1:2,lty=1,lwd=2)

#Grafica de residuos vs. t como una serie de tiempo
win.graph()
plot.ts(residuals(modelo))
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma))

#grafica de residuos vs. respuesta estimada
win.graph()
plot(fitted(modelo),residuals(modelo))
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma))

#Realizacion test Durbin Watson con la funcion de usuario pruebaDW1()
pruebaDW1(modelo)

#Panel de graficos de dispersion de residuos vs. residuos rezagados k=1, 2, ...,6 periodos
win.graph()
lag.plot(residuals(modelo),lags=6)

