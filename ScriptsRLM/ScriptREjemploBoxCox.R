#Leer archivo datosejemBoxCox.csv desde repositorio en Github
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosejemBoxCox.csv",header=TRUE,sep=";",dec=",")
attach(datos)

names(datos)
plot(x,Y)

mod=lm(Y~x)

#Definiendo rango para eje vertical de graficos residuos ordinarios
miny=min(-2*summary(mod)$sigma,residuals(mod))
maxy=max(2*summary(mod)$sigma,residuals(mod))
rangoy=c(miny,maxy)

#residuos ordinarios vs. x
win.graph()
plot(x,residuals(mod),ylim=rangoy)
abline(h=c(-2*summary(mod)$sigma,0,2*summary(mod)$sigma),col=2,lty=2)

#Grafico de probabilidad normal con residuos ordinarios
win.graph()
qqnorm(residuals(mod));qqline(residuals(mod))

#Aplicacion de Box-Cox por maxinma verosimilitud
library(car)
#Obtencion de la grafica del perfil del log de verosimilitud para lambda
#Debe especificarse el rango de valores lambda para evaluar el perfil, en el argumento lambda=
win.graph()
bcx=boxCox(mod,family="bcPower",lambda=seq(-0.25,0.25,by=0.01),param="lambda",plotit =TRUE)

lambda=bcx$x[which.max(bcx$y)] #extrayendo lambda optimo
lambda 
#lambda hallado es muy proximo a cero, y cero se encuentra dentro del IC para lambda 
#podemos entonces tomar lambda=0 y por tanto transformacion box cox es log(Y)
text(lambda-0.015,-395,labels=expression(lambda[optimo]==-0.007575758),pos=4)

win.graph()
plot(x,log(Y))

#Regresion con respuesta transformada con log natural
logY=log(Y)
mod2=lm(logY~x)
summary(mod2)

#Definiendo rango para eje vertical de graficos residuos ordinarios
miny2=min(-2*summary(mod2)$sigma,residuals(mod2))
maxy2=max(2*summary(mod2)$sigma,residuals(mod2))
rangoy2=c(miny2,maxy2)

#residuos ordinarios vs. x
win.graph()
plot(x,residuals(mod2),ylim=rangoy2)
abline(h=c(-2*summary(mod2)$sigma,0,2*summary(mod2)$sigma),lty=2,col=2)

#Grafico de probabilidad normal con residuos ordinarios
win.graph()
qqnorm(residuals(mod2));qqline(residuals(mod2))

#respuesta estimada en escala log
logyhat=fitted(mod2)

#Obteniendo estimaciones de la respuesta en escala original
yhat=exp(logyhat)*exp(summary(mod2)$sigma^2/2)

#Grafico de dispersion y ajuste en escala de ajuste
win.graph()
plot(x,log(Y))
lines(x,logyhat,lty=1,col=2,lwd=2)
legend("topleft",legend="Respuesta ajustada en escala logaritmo natural",lwd=2,col=2)

#Grafico de dispersion y ajuste en escala original
win.graph()
plot(x,Y)
lines(x,yhat,lty=1,col=2,lwd=2)
legend("topleft",legend="Respuesta ajustada en escala original",lwd=2,col=2)

detach(datos)
