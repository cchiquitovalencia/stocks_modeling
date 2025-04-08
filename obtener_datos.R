library(quantmod)
library(caret)

# Leer los datos
dji<- readRDS("./dji.RDS")

# Crear variables predictoras
avg10<- rollapply(dji,10,mean)
avg20<- rollapply(dji,20,mean)
std10<- rollapply(dji,10,sd)
std20<- rollapply(dji,20,sd)
rsi5<- RSI(dji,5,"SMA")
rsi14<- RSI(dji,14,"SMA")
macd12269<- MACD(dji,12,26,9,"SMA")
macd7205<- MACD(dji,7,20,5,"SMA")
bbands<- BBands(dji,20,"SMA",2)

# Generar dirección de acciones
Direction<- NULL
Direction[dji> Lag(dji,20)] <- 1
Direction[dji< Lag(dji,20)] <- 0

# Consolidar tabla para regresión
dji<-cbind(dji,
           avg10,
           avg20,
           std10,
           std20,
           rsi5,
           rsi14,
           macd12269,
           macd7205,
           bbands,
           Direction)

# Tratamiento de NA's
dji$Direction <- ifelse(is.na(dji$Direction), 0, dji$Direction)

# Delimitar fracciones de datos
fechas <- as.data.frame(dji) |> 
  row.names()

# Crear conjunto de train y test
issd<- first(fechas[1:(length(fechas) * 0.8)])
ised<- last(fechas[1:(length(fechas) * 0.8)])
ossd<- as.Date(ised) + 1
osed<- last(fechas)

# Seleccionar en tabla con criterio anterior
isrow<- which(index(dji) >= issd & index(dji) <= ised)
osrow<- which(index(dji) >= ossd & index(dji) <= osed)

isdji<- dji[isrow,]
osdji<- dji[osrow,]

# Aplicar media y desv
isme <- apply(isdji, 2, function(x) mean(x, na.rm = TRUE))
isstd <- apply(isdji, 2, function(x) sd(x, na.rm = TRUE))

# Crear matriz de identidad
isidn<- matrix(1,dim(isdji)[1],dim(isdji)[2])
norm_isdji<-  (isdji - t(isme*t(isidn))) / t(isstd*t(isidn))

dm<- dim(isdji)
norm_isdji[,dm[2]] <- Direction[isrow]

# Formular para modelo
formula<- paste("Direction ~ .",sep="")

model<- glm(formula,family="binomial",norm_isdji)

# Predecir
pred<- predict(model,norm_isdji)

# Calcular probabilidad
prob<- 1 / (1+exp(-(pred)))

# Determinar dirección de acuerdo con probabilidad
pred_direction<- NULL
pred_direction[prob> 0.5] <- 1
pred_direction[prob<= 0.5] <- 0

# Crear matriz de confusión del train data
matrix<- confusionMatrix(factor(pred_direction),
                         factor(norm_isdji$Direction),
                         mode = "everything")

osidn<- matrix(1,dim(osdji)[1],dim(osdji)[2])
norm_osdji<-  (osdji - t(isme*t(osidn))) / t(isstd*t(osidn))
norm_osdji[,dm[2]] <- Direction[osrow]

# Crear para el test data
ospred<- predict(model,norm_osdji)
osprob<- 1 / (1+exp(-(ospred)))
ospred_direction<- NULL
ospred_direction[osprob> 0.5] <- 1
ospred_direction[osprob<= 0.5] <- 0
osmatrix<- confusionMatrix(factor(ospred_direction),
                           factor(norm_osdji$Direction))
