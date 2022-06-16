Comparmediasslices2=function(modelo,nreplicas){
respuesta=modelo$model[,1]
factor1=modelo$model[,2]
factor2=modelo$model[,3]
df1=length(levels(factor1))-1
df2=length(levels(factor2))-1
dfe=anova(modelo)[1][4,]
mediastrat=summary(lsmeans(aov(respuesta~factor1*factor2),~factor1*factor2))
MS2.1=nreplicas*sapply(split(mediastrat$lsmean,mediastrat$factor1),var)
MS1.2=nreplicas*sapply(split(mediastrat$lsmean,mediastrat$factor2),var)
SS2.1=df2*MS2.1
SS1.2=df1*MS1.2
MSE=anova(modelo)[3][4,]
F02.1=MS2.1/MSE
F01.2=MS1.2/MSE
pvalue2.1=pf(F02.1,df1=df2,df2=dfe,lower.tail=F)
pvalue1.2=pf(F01.2,df1=df1,df2=dfe,lower.tail=F)

slicebyfactor2.1=data.frame(niveles=modelo$xlevels[1],Df=df2,"Sum\ Sq"=SS2.1,"Mean Sq"=MS2.1,F0=F02.1,"P value"=pvalue2.1)
slicebyfactor1.2=data.frame(niveles=modelo$xlevels[2],Df=df1,"Sum\ Sq"=SS1.2,"Mean Sq"=MS1.2,F0=F01.2,"P value"=pvalue1.2)
cat("Significancia Efectos",names(modelo$model)[2],"*",names(modelo$model)[3],"sobre respuesta",names(modelo$model)[1],"en cada nivel de",names(modelo$xlevels)[1],"\n")
print(slicebyfactor2.1)
cat("\n")
cat("Significancia Efectos",names(modelo$model)[2],"*",names(modelo$model)[3],"sobre respuesta",names(modelo$model)[1],"en cada nivel de",names(modelo$xlevels)[2],"\n")
print(slicebyfactor1.2)
}


