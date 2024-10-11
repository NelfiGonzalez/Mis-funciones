library(car)
library(olsrr)

rm(list=ls(all=TRUE))

#Cargando funciones de usuario desde repositorio en github 
source("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/FuncionesdeUsuarioRLM.R")

#Lectura de los datos desde archivo externo .csv 
datos=read.table("https://raw.githubusercontent.com/NelfiGonzalez/Regresion-DOE/main/datosproblemaseleccionpersonal.csv",header=TRUE,sep=";",dec=",")

attach(datos)

#Ajuste del MRLM
modelo=lm(punt.Aptitud~.,datos)
miscoeficientes(modelo) #tabla con parametros estimados, sus I.C del 95%, los VIFs y coeficientes estandarizados estimados

#--------------------------------------------------------------------------------------------------------------------------------
#Resultados para analisis de multicolinealidad
#--------------------------------------------------------------------------------------------------------------------------------
#Matriz de correlaciones
cor(datos)

#Tabla de correlaciones sin repetir pares
correlaciones(datos)

#Analisis de multicolinealidad con datos no centrados 
#En la matriz X las variables no se centran pero son escaladadas con sqrt(sum x^2) incluyendo columna intercepto
multicolin(modelo) #mediante la funcion de usuario multicolin()
ols_coll_diag(modelo) #Funcion de la libreria olsrr

#Analisis de multicolinealidad con datos centrados: 
multicolin(modelo,center=TRUE)

#--------------------------------------------------------------------------------------------------------------------------------
#Procedimientos de seleccion de variables: Funciones usadas son de la libreria olsrr
#--------------------------------------------------------------------------------------------------------------------------------
#Seleccion backward con nivel de significancia de 0.05 para eliminar variables
ols_step_backward_p(modelo,p_val=0.05) #tabla resumen
ols_step_backward_p(modelo,p_val=0.05)$model #Estimaciones en modelo final
ols_step_backward_p(modelo,p_val=0.05,details = TRUE) #Impresion detallada de cada paso
ols_step_backward_p(modelo,p_val=0.05,progress = TRUE) #Impresion detallando resultados con mejor modelo

#Seleccion forward con nivel de significancia de 0.05 para ingresar variables
ols_step_forward_p(modelo,p_val=0.05) #tabla resumen
ols_step_forward_p(modelo,p_val=0.05)$model #Estimaciones en modelo final
ols_step_forward_p(modelo,p_val=0.05,details = TRUE) #Impresion detallada de cada paso
ols_step_forward_p(modelo,p_val=0.05,progress = TRUE) #Impresion detallando resultados con mejor modelo

#Seleccion stepwise con nivel de significancia de 0.05 para ingresar y eliminar variables
ols_step_both_p(modelo,p_enter= 0.05, p_remove = 0.05) #tabla resumen
ols_step_both_p(modelo,p_enter= 0.05, p_remove = 0.05)$model #Estimaciones en modelo final
ols_step_both_p(modelo,p_enter= 0.05, p_remove = 0.05,details=TRUE) #Impresion detallada de cada paso
ols_step_both_p(modelo,p_enter= 0.05, p_remove = 0.05,progress=TRUE) #Impresion detallando resultados con mejor modelo

#Todas las regresiones posibles; da informacion del Cp, R2, R2adj
k=ols_step_all_possible(modelo) #guardando tabla de toda las regresiones posibles
                                #con medidas Cp, R2, R2adj entre otras

names(k$result) #observe el nombre de los valores guardados en el data.frame "k$result"

k$result[,c(1:5,8)]  

#Graficas para identificar meejor modelo segun criterios de seleccion: R2, R2adj, Cp, AIC
plot(k)

ols_press(modelo) #Calcula PRESSp solo para modelo completo


#Parametros estimados en modelos de todas las regresiones posibles
ols_step_all_possible_betas(modelo) 


detach(datos)