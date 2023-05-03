library(car)
library(olsrr)
#Función colldiag de la librería perturb que actualmente no está en cran R
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

#FUNCIÓN PARA EXTRAER COEFICIENTES ESTIMADOS SUS IC DEL 95%, VIF'S Y COEFICIENTES ESTANDARIZADOS
miscoeficientes=function(modeloreg,datosreg){
coefi=coef(modeloreg)
datos2=as.data.frame(scale(datosreg))
coef.std=c(0,coef(lm(update(formula(modeloreg),~.+0),datos2)))
limites=confint(modeloreg,level=0.95)
vifs=c(0,vif(modeloreg))
resul=data.frame(Estimación=coefi,Limites=limites,Vif=vifs,Coef.Std=coef.std)
cat("Coeficientes estimados, sus I.C, Vifs y Coeficientes estimados estandarizados","\n")
resul
}


#función de usuario para multicolinealidad
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


