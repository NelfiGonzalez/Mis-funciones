library(car)
library(olsrr)
#Funcion colldiag de la libreria perturb actualmente no disponible en cran R
colldiag <- function(mod,scale=TRUE,center=FALSE,add.intercept=TRUE) {
  result <- NULL
  if (center) add.intercept<-FALSE
  if (is.matrix(mod)||is.data.frame(mod)) {
    X<-as.matrix(mod)
    nms<-colnames(mod)
  }
  else if (!is.null(mod$call$formula)) {
    X<-mod$model[,-1] # delete the dependent variable
  }
  X<-na.omit(X) # delete missing cases
  if (add.intercept) {
    X<-cbind(1,X) # add the intercept
    colnames(X)[1]<-"intercept"
  }
  X<-scale(X,scale=scale,center=center)
  
  svdX<-svd(X)
  svdX$d
  condindx<-svdX$d[1]/svdX$d
  
  Phi=svdX$v%*%diag(1/svdX$d)
  Phi<-t(Phi^2)
  pi<-prop.table(Phi,2)
  
  dim(condindx)<-c(length(condindx),1)
  colnames(condindx)<-"cond.index"
  rownames(condindx)<-1:nrow(condindx)
  colnames(pi)<-colnames(X)
result=cbind(condindx,pi)
  class(result)<-"colldiag"
  result
}

#FUNCION PARA EXTRAER COEFICIENTES ESTIMADOS SUS IC DEL 95%, VIFs Y COEFICIENTES ESTANDARIZADOS
miscoeficientes=function(modeloreg,datosreg){
coefi=coef(modeloreg)
datos2=as.data.frame(scale(datosreg))
coef.std=c(0,coef(lm(update(formula(modeloreg),~.+0),datos2)))
limites=confint(modeloreg,level=0.95)
vifs=c(0,vif(modeloreg))
resul=data.frame(Estimacion=coefi,Limites=limites,Vif=vifs,Coef.Std=coef.std)
cat("Coeficientes estimados, sus I.C, Vifs y Coeficientes estimados estandarizados","\n")
resul
}


#funcion de usuario para multicolinealidad
multicolin=function(modeloRLM,data,center=FALSE){
if(center==FALSE){
res=data.frame(rbind(c(NA,NA),ols_coll_diag(modeloRLM)$vif_t[,-1]),ols_coll_diag(modeloRLM)$eig_cindex,row.names=names(coef(modeloRLM)))
#res=names(coef(modeloRLM))
}
if(center==TRUE){
Ind=colldiag(modeloRLM,center=TRUE)[,1:ncol(data)]
X=model.matrix(modeloRLM)[,-1]
eigenvalue=prcomp(X,center=TRUE,scale=TRUE)$sdev^2
res=data.frame(ols_coll_diag(modeloRLM)$vif_t[,2:3],Eigenvalue=eigenvalue,Ind,row.names=colnames(X))
}
res
}

#Funcion para análisis de puntos outliers, de balanceo y de influencia
diaganalysis=function(modelo){
cat("Resultados de la Funcion influence.measures")
cat("\n")
summary(influence.measures(modelo))
win.graph()
infIndexPlot(modelo,cex.lab=1.5,cex=1.5,cex.axis=1.5)
win.graph()
cat("\n")
cat("Resultados adicionales para la figura de burbujas")
cat("\n")
influencePlot(modelo,xlim=c(0,1),cex.lab=1.5,ylim=c(min(rstudent(modelo))-0.2,max(rstudent(modelo))+0.2),cex.axis=2)
}


#Funcion para tabla AMOVA del MRLM requiere libreria rms
MiAnova=function(model){
library(rsm)
name_response=names(model$model)[1]
nombres=names(model$model)[-1]
miformula=as.formula(paste(name_response,"~",paste(paste("FO(",paste(nombres,sep="",collapse=","),sep=""),")",sep="")))
tablaAnova=anova(rsm(miformula))
rownames(tablaAnova)[1]="Model"
print(tablaAnova)
}

