library(car)
library(rsm)
library(rgl)
library(scatterplot3d)
library(GGally)
ggplot2::theme_set(ggplot2::theme_bw()) 

#Cargando funciones de usuario desde repositorio en github
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Datos de Hamilton (1987) pág. 103, tabla 4.1 de Chatterjee and Hadi, 2012 
#El nombre de las variables leidas son Y, X1, X2
HamiltonData=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/tablaDatosHamilton.csv",header=T,sep=";",dec=",")
names(HamiltonData)
attach(HamiltonData)

#matriz de dispersion con boxplots en la diagonal
ggpairs(HamiltonData,diag=list(continuous=wrap("box_no_facet",color="red",fill="lightgoldenrod1",alpha=0.3)),upper=list(continuous = wrap("points",alpha = 0.3, size=2)),lower=list(continuous =wrap("cor",col="darkblue")))

#Ajuste de modelos, tablas de parámetros estimados y tablas ANOVA
#Y vs. X1
modX1=lm(Y~X1)
summary(modX1)
MiAnova(modX1)

#Y vs. X2
modX2=lm(Y~X2)
summary(modX2)
MiAnova(modX2)

#Y vs. X1, X2
modX1X2=lm(Y~.,data=HamiltonData)
summary(modX1X2)
MiAnova(modX1X2)

#Graficos de dispersion tridimensionales con plano de regresion ajustado, Y vs. X1, X2
scatter3d(Y~X1+X2,fov=90,revolutions=1)

s3d.12=scatterplot3d(X1,X2,Y,cex.symbol=2,color=2,box=F,font.lab=4,font.axis=4,zlim=c(5,20))
s3d.12$plane3d(modX1X2, lty.box = "solid")

#Grafico de dispersion de la respuesta observada vs. la ajustada, con ajuste lineal y loess
win.graph()
scatterplot(fitted(modX1X2),Y,smooth=list(style="none",col.smooth=2),regLine=list(col=3),pch=19,cex=1.5)
legend("topleft",legend=c(expression(paste("Ajuste lineal entre",sep=" ",Y,sep=", ",widehat(Y))),expression(paste("Ajuste loess entre",sep=" ",Y,sep=", ",widehat(Y)))),col=c(3,2),lty=c(1,2),lwd=2)

#graficos de variable agregada (added-variable plot) o graficos de regresion parcial (partial regression plot)
win.graph(height=6,width=12)
layout(rbind(c(1),c(2)))
avPlots(modX1X2)

#graficos de residuos mas componente (residuals plus component plot)
win.graph(height=6,width=12)
layout(rbind(c(1),c(2)))
crPlots(modX1X2)

detach(HamiltonData)
