library(car)
library(rsm)
library(rgl)
library(scatterplot3d)
library(GGally)
ggplot2::theme_set(ggplot2::theme_bw()) 

#Cargando funciones de usuario desde repositorio en github 
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Leer archivo Datossimulados.csv desde repositorio en github
Data=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/Datossimulados.csv",sep=";",dec=",",header=T)
attach(Data)
names(Data) #Revisando nombre de variables en el data.frame Data

#Matriz de dispersion
ggpairs(Data,diag=list(continuous=wrap("box_no_facet",color="red",fill="lightgoldenrod1",alpha=0.3)),upper=list(continuous = wrap("points",alpha = 0.3, size=2)),lower=list(continuous =wrap("cor",col="darkblue")))

#Ajuste del MRLM
mod=lm(y~.,data=Data)
summary(mod)
#Exportando al directorio de trabajo la tabla de parametros estimados, a un archivo de nombre tablaModdatossimul.csv
write.csv2(summary(mod)$coefficients,"tablaModdatossimul.csv",row.names=TRUE)

#Anova del modelo
tablaAnova=MiAnova(model=mod)
#Exportando al directorio de trabajo la tabla ANOVA del modelo, al archivo de nombre tablaANOVAModdatossimul.csv
write.csv2(tablaAnova,"tablaANOVAModdatossimul.csv",row.names=TRUE)

#Grafico de dispersion respuesta observada vs. estimada
win.graph()
scatterplot(fitted(mod),y,smooth=list(style="none",col.smooth=2),regLine=list(col=3),pch=19,cex=1.5)
legend("topleft",legend=c(expression(paste("Ajuste lineal entre",sep=" ",Y,sep=", ",widehat(Y))),expression(paste("Ajuste loess entre",sep=" ",Y,sep=", ",widehat(Y)))),col=c(3,2),lty=c(1,2),lwd=2)

#Obtencion de los elementos de la diagonal ppal en la matriz H
hii=hatvalues(mod)
#Estadisticos de resumen sobre los hii
summary(hii)

#Obteniendo matriz de diseno del MRLM
#matrizX=as.matrix(data.frame(model.matrix(mod)))

matrizX=model.matrix(mod)
head(matrizX)

#Estadisticos de resumen sobre los predictores del MRLM
apply(matrizX[,-1],2,summary)

#Calculando la inversa de X^t * X
XtXinversa=solve(t(matrizX)%*%matrizX)

#Definiendo vector x_01 y calculando su estadistico h_00 
newx=matrix(c(1,12,21,30),ncol=1)
t(newx)%*%(XtXinversa%*%newx)

#Definiendo vector x_02 y calculando su estadistico h_00
new2x=matrix(c(1,11.24,19.87,28.59),ncol=1)
t(new2x)%*%(XtXinversa%*%new2x)

#Creando data.frame con coordenadas de valores de los predictores en los puntos de prediccion
Newdata=data.frame(rbind(c(12,21,30),c(11.24,19.87,28.59)))
names(Newdata)=names(mod$model)[-1] #asigna a columnas los nombres de los predictores en el MRLM
Newdata

#Intervalos de confianza del 95% para la respuesta media en x_01 y x_02
predict(mod,newdata=Newdata,interval="confidence",level=0.95)

#Intervalos de prediccion del 95% para la respuesta media en x_01 y x_02
predict(mod,newdata=Newdata,interval="prediction",level=0.95)

detach(Data)
