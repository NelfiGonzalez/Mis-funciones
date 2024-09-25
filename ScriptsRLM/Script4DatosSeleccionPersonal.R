library(car)
library(lmtest)
library(rsm)
library(olsrr)
#----------------------------------------------------------------------------------------------------------------------------------
#Cargando funciones de usuario desde repositorio en github 

source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")
#----------------------------------------------------------------------------------------------------------------------------------
#Lectura datos desde github
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")
attach(datos)
names(datos) #Revisando el nombre de las variables guardadas en el data.frame
#----------------------------------------------------------------------------------------------------------------------------------
#Ajuste del MRLM: Formula extendida 
modelo=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)

#O bien de la siguiente forma,
modelo=lm(punt.Aptitud~.,datos)
#----------------------------------------------------------------------------------------------------------------------------------
#Graficos de residuos ordinarios
#Definiendo rango para eje vertical de graficos residuos ordinarios
miny=min(-2*summary(modelo)$sigma,residuals(modelo))
maxy=max(2*summary(modelo)$sigma,residuals(modelo))
rangoy=c(miny,maxy)

#En la regresion lineal ordinaria los residuos "pearson", "response", coinciden y corresponde a los residuos ordinarios
win.graph(width=9.5,height=6)
residualPlots(modelo,tests=FALSE,type="response",quadratic=FALSE,layout=c(2,3),ylim=rangoy,col=2,cex=1.5)

#o bien
win.graph(width=9.5,height=6)
residualPlots(modelo,tests=FALSE,type="pearson",quadratic=FALSE,layout=c(2,3),ylim=rangoy,col=2,cex=1.5)

#o bien cada una de las graficas con la funcion plot
win.graph(width=9.5,height=6)
layout(rbind(c(1,2,3),c(4,5,6)))
plot(Prueba1,residuals(modelo),ylim=rangoy,cex=1.5)
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),col=2,lty=2)

plot(Prueba2,residuals(modelo),ylim=rangoy,cex=1.5)
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),col=2,lty=2)

plot(Prueba3,residuals(modelo),ylim=rangoy,cex=1.5)
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),col=2,lty=2)

plot(Prueba4,residuals(modelo),ylim=rangoy,cex=1.5)
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),col=2,lty=2)

plot(fitted(modelo),residuals(modelo),ylim=rangoy,cex=1.5)
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),col=2,lty=2)
#----------------------------------------------------------------------------------------------------------------------------------
#Graficos de residuos externamente estudentizados
win.graph(width=9.5,height=6)
residualPlots(modelo,tests=FALSE,type="rstudent",quadratic=FALSE,layout=c(2,3),ylim=c(-3,3),col=2,cex=1.5)

#o bien cada una de las graficas con la funcion plot
win.graph(width=9.5,height=6)
layout(rbind(c(1,2,3),c(4,5,6)))
plot(fitted(modelo),rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba1,rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba2,rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba3,rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba4,rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(fitted(modelo),rstudent(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
#----------------------------------------------------------------------------------------------------------------------------------
#Graficos de residuos internamente estudentizados
win.graph(width=9.5,height=6)
residualPlots(modelo,tests=FALSE,type="rstandard",quadratic=FALSE,layout=c(2,3),ylim=c(-3,3),col=2,cex=1.5)

#o bien cada una de las graficas con la funcion plot
win.graph(width=9.5,height=6)
layout(rbind(c(1,2,3),c(4,5,6)))
plot(Prueba1,rstandard(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba2,rstandard(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba3,rstandard(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(Prueba4,rstandard(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
plot(fitted(modelo),rstandard(modelo),ylim=c(-3,3),cex=1.5);abline(h=c(-2,0,2),col=2,lty=2)
#----------------------------------------------------------------------------------------------------------------------------------
#Test y grafico de normalidad con residuos ordinarios y residuos estudentizados
win.graph(width=8,height=3)
layout(cbind(c(1),c(2),c(3)))
test=shapiro.test(residuals(modelo)) #Test de normalidad sobre residuales ordinarios
qqnorm(residuals(modelo),main="Test normalidad\nresiduos ordinarios",cex.main=0.8)
qqline(residuals(modelo),col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=3)),cex=0.8)
test2=shapiro.test(rstudent(modelo)) #Test de normalidad sobre residuales estudentizados externamente
qqnorm(rstudent(modelo),main="Test normalidad\nresiduos estudentizados externamente",cex.main=0.8)
qqline(rstudent(modelo),col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test2$statistic,test2$p.value),digits=3)),cex=0.8)
test3=shapiro.test(rstandard(modelo)) #Test de normalidad sobre residuales estudentizados internamente
qqnorm(rstandard(modelo),main="Test normalidad\nresiduos estudentizados internamente",cex.main=0.8)
qqline(rstandard(modelo),col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test3$statistic,test3$p.value),digits=3)),cex=0.8)
#----------------------------------------------------------------------------------------------------------------------------------
#Test de homocedasticidad 
#El Test Breusch-Pagan con varianza como funcion lineal de los k predictores 
ncvTest(modelo,var.formula=~Prueba1+Prueba2+Prueba3+Prueba4)

#o bien
bptest(modelo,studentize=FALSE)

#Version Breusch-Pagan estudentizado con varianza como funcion lineal de los k predictores
bptest(modelo,studentize=TRUE)
#----------------------------------------------------------------------------------------------------------------------------------
#Resultados para diagnosticar observaciones de balanceo, influenciales y atipicas
#genera varias graficas de diagnosticos y salida R con resultados de diagnosticos (si hay observaciones de balanceo, influenciales y/o atipicas)
diag_obs(model=modelo,plot.add=TRUE)

#Esta ejecucion solo genera dos graficos: "Diagnostic Plots" y grafico de burbujas de residuos estudentizados vs. los levarage
#Ademas crea un data.frame si se encuentra obs. influyentes, de balanceo y/o atipicas segun cotas en notas de clase
#Informando para tales obs. los valores de dfbetas, dffits, covratios,h_ii, residuos estudentizados externamente
aux=diag_obs(model=modelo,plot.add=FALSE)
aux

#Exportando al directorio de trabajo tabla con medidas diagnosticadas sobre obs. identificadas
write.csv2(aux,"diagnosticos_modelo_puntAptitud.csv",row.names=TRUE)
#----------------------------------------------------------------------------------------------------------------------------------

detach(datos)

