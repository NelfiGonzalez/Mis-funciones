#--------------------------------------------------------------------------------------------------------------------
#Ejemplo 1 tests carencia de ajuste
#--------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
library(rsm)
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Leyendo datos simulados, ajuste modelo y test carencia de ajuste
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/DatossimulLOF1.csv",dec=",",header=T,sep=";")
datos

attach(datos)
mediay.i=sapply(split(y,as.factor(x)),mean) #cáculo de medias de Y en cada nivel de X

plot(x,y,cex=2)
lines(seq(20,100,by=20),mediay.i,type="b",lty=2,col=4,pch=3,cex=2)
legend("topleft",legend=c("Observaciones","media muestral por tratamiento"),col=c(1,4),pch=c(1,3),cex=1.2)

#Ajuste del MRL
mrls=lm(y~.,data=datos)
summary(mrls)

MiAnova(model=mrls) #Anova ordinaria del MRL
anovLOF(mod=mrls) #Anova con test LOF

#Otra forma para realizar test LOF mediante test lineal general
modLinGeneral=aov(y~factor(x),data=datos) #ajuste del modelo lineal general
#Test Anova comparando modelo RLM y modelo lineal general
anova(mrls,modLinGeneral)

#Grafico de dispersion con respuesta estimada y observada
yhat=fitted(mrls)
win.graph()
plot(x,y,cex=2)
lines(seq(20,100,by=20),mediay.i,type="b",lty=2,col=4,pch=3,cex=2)
lines(x,yhat,lwd=2,col=2, type="b",pch=2,cex=2)
legend("topleft",legend=c("Observaciones","media muestral por tratamiento","Respuesta estimada por el MRLS"),col=c(1,4,2),pch=c(1,3,2),cex=1.2)

#Grafico de dispersion con ajuste lineal y loess de la respuesta observada vs. la estimada
win.graph()
scatterplot(fitted(mrls),y,smooth=list(style="none",col.smooth=2),regLine=list(col=3),pch=19,cex=1.5,id=list(method="mahal", n=5, cex=1, col=carPalette()[-1]))
legend("topleft",legend=c(expression(paste("Ajuste lineal entre",sep=" ",Y,sep=", ",widehat(Y))),expression(paste("Ajuste loess entre",sep=" ",Y,sep=", ",widehat(Y)))),col=c(3,2),lty=c(1,2),lwd=2)

#Media residuos
media.res=sapply(split(residuals(mrls),as.factor(x)),mean)

#Residuos vs. x
win.graph()
plot(x,residuals(mrls),ylim=c(-60,60),cex=2)
abline(h=c(-2*summary(mrls)$sigma,0,2*summary(mrls)$sigma))
lines(seq(20,100,by=20),media.res,type="b",lty=2,col=4,pch=2,cex=2)
legend("topleft",legend=c("residuos","promedio por nivel de x"),pch=c(1,2),col=c(1,4),cex=1.1)

detach(datos)

#--------------------------------------------------------------------------------------------------------------------
#Ejemplo 2 test carencia de ajuste
#--------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Leyendo datos simulados, ajuste modelo y test carencia de ajuste
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/DatossimulLOF2.csv",dec=",",header=T,sep=";")
datos

attach(datos)
mediay.i=sapply(split(y,as.factor(x)),mean) #cáculo de medias de Y en cada nivel de X

plot(x,y,cex=2)
lines(seq(20,100,by=20),mediay.i,type="b",lty=2,col=4,pch=3,cex=2)
legend("topleft",legend=c("Observaciones","media muestral por tratamiento"),col=c(1,4),pch=c(1,3),cex=1.2)

#Ajuste del MRL
mrls=lm(y~.,data=datos)
summary(mrls)

MiAnova(model=mrls) #Anova ordinaria del MRL
anovLOF(mod=mrls) #Anova con test LOF

#Otra forma para realizar test LOF mediante test lineal general
modLinGeneral=aov(y~factor(x),data=datos) #ajuste del modelo lineal general
#Test Anova comparando modelo RLM y modelo lineal general
anova(mrls,modLinGeneral)

#Grafico de dispersion con respuesta estimada y observada
yhat=fitted(mrls)
win.graph()
plot(x,y,cex=2,ylim=c(min(y,yhat),max(y,yhat)))
lines(seq(20,100,by=20),mediay.i,type="b",lty=2,col=4,pch=3,cex=2)
lines(x,yhat,lwd=2,col=2, type="b",pch=2,cex=2)
legend("topleft",legend=c("Observaciones","media muestral por tratamiento","Respuesta estimada por el MRLS"),col=c(1,4,2),pch=c(1,3,2),cex=1.2)

#Grafico de dispersion con ajuste lineal y loess de la respuesta observada vs. la estimada
win.graph()
scatterplot(fitted(mrls),y,smooth=list(style="none",col.smooth=2),regLine=list(col=3),pch=19,cex=1.5,id=list(method="mahal", n=5, cex=1, col=carPalette()[-1]))
legend("topleft",legend=c(expression(paste("Ajuste lineal entre",sep=" ",Y,sep=", ",widehat(Y))),expression(paste("Ajuste loess entre",sep=" ",Y,sep=", ",widehat(Y)))),col=c(3,2),lty=c(1,2),lwd=2)

#Media residuos
media.res=sapply(split(residuals(mrls),as.factor(x)),mean)

#Residuos vs. x
win.graph()
plot(x,residuals(mrls),ylim=c(-500,500),cex=2)
abline(h=c(-2*summary(mrls)$sigma,0,2*summary(mrls)$sigma))
lines(seq(20,100,by=20),media.res,type="b",lty=2,col=4,pch=2,cex=2)
legend("topleft",legend=c("residuos","promedio por nivel de x"),pch=c(1,2),col=c(1,4),cex=1.1)

detach(datos)
