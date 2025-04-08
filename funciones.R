# Cargar las bibliotecas necesarias
library(quantmod)
library(caret)
library(TTR)  # Se necesita para las funciones RSI, MACD, BBands
library(zoo)   # Se necesita para el funcionamiento de rollapply

# Función para cargar los datos
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("El archivo", file_path, "no existe"))
  }
  data <- readRDS(file_path)
  if (nrow(data) == 0) {
    stop("Los datos están vacíos")
  }
  return(data)
}

# Función para crear variables predictoras
create_features <- function(data) {
  # Definimos una función personalizada para rollapply que elimine NA
  custom_mean <- function(x) {
    mean(x, na.rm = TRUE)
  }
  
  custom_sd <- function(x) {
    sd(x, na.rm = TRUE)
  }
  
  # Medias móviles
  avg10 <- rollapply(data, 10, custom_mean)
  avg20 <- rollapply(data, 20, custom_mean)
  
  # Desviaciones móviles
  std10 <- rollapply(data, 10, custom_sd)
  std20 <- rollapply(data, 20, custom_sd)
  
  # RSI
  rsi5 <- RSI(data, 5, "SMA")
  rsi14 <- RSI(data, 14, "SMA")
  
  # MACD
  macd12269 <- MACD(data, 12, 26, 9, "SMA")
  macd7205 <- MACD(data, 7, 20, 5, "SMA")
  
  # Bollinger Bands
  bbands <- BBands(data, 20, "SMA", 2)
  
  return(list(
    avg10 = avg10,
    avg20 = avg20,
    std10 = std10,
    std20 = std20,
    rsi5 = rsi5,
    rsi14 = rsi14,
    macd12269 = macd12269,
    macd7205 = macd7205,
    bbands = bbands
  ))
}

# Función para crear la tabla de datos
create_data_table <- function(data, features) {
  # Convertimos el data.frame principal a una matriz para hacer cbind
  data_matrix <- as.matrix(data)
  
  obj1_xts <- xts(
    coredata(data_matrix),
    order.by = as.Date(dimnames(data_matrix)[[1]]),
    dimnames = list(NULL, "DJI.Close")
  )
  
  # Creación de la tabla con las nuevas variables
  data <- cbind(
    obj1_xts,
    features$avg10,
    features$avg20,
    features$std10,
    features$std20,
    features$rsi5,
    features$rsi14,
    features$macd12269,
    features$macd7205,
    features$bbands
  )
  
  # Convertimos de matriz a data.frame
  data <- as.data.frame(data)
  
  # Crear variable Direction
  data$Direction <- NULL
  
  Direction[data$DJI.Close> Lag(data$DJI.Close,20)] <- 1
  Direction[data$DJI.Close< Lag(data$DJI.Close,20)] <- 0
  
  data$Direction <- Direction
  
  data$Direction <- ifelse(is.na(data$Direction), 0, data$Direction)
  
  return(data)
}

# Función para dividir datos train y test
split_time_series <- function(data, proportion = 0.8) {
  fechas <- as.data.frame(data) |> 
    row.names()
  
  # Crear conjunto de train y test
  issd<- first(fechas[1:(length(fechas) * proportion)])
  ised<- last(fechas[1:(length(fechas) * proportion)])
  ossd<- as.Date(ised) + 1
  osed<- last(fechas)
  
  # Seleccionar en tabla con criterio anterior
  isrow<- which(index(data) >= issd & index(data) <= ised)
  osrow<- which(index(data) >= ossd & index(data) <= osed)
  
  isdji<- data[isrow,]
  osdji<- data[osrow,]
  
  return(list(train = isdji, test = osdji))
}

# Función para normalizar los datos
normalize_data <- function(train_data, test_data) {
  # Normalizar train data
  normalized_train <- scale(train_data)
  
  # Normalizar test data con los mismos parámetros
  normalized_test <- scale(test_data)
  
  return(list(train = normalized_train, test = normalized_test))
}

normalize_data <- function(train_data, test_data) {
  # Calcular media y desviación estándar para cada variable en los datos de entrenamiento
  train_mean <- apply(train_data, 2, function(x) mean(x, na.rm = TRUE))
  train_sd <- apply(train_data, 2, function(x) sd(x, na.rm = TRUE))
  
  # Matriz de identidad y normalización de datos
  isidn<- matrix(1,dim(train_data)[1],dim(train_data)[2])
  norm_isdji<-  (isdji - t(train_mean*t(isidn))) / t(train_sd*t(isidn))
  
  dm<- dim(isdji)
  norm_isdji[,dm[2]] <- Direction[isrow]
  
  osidn<- matrix(1,dim(test_data)[1],dim(test_data)[2])
  norm_osdji<-  (osdji - t(train_mean*t(osidn))) / t(train_sd*t(osidn))
  norm_osdji[,dm[2]] <- Direction[osrow]
  
  # Retornar los datos normalizados y las estadísticas
  return(list(
    train = norm_isdji,
    test = norm_osdji,
    mean = train_mean,
    sd = train_sd
  ))
}

# Función para crear y entrenar el modelo
build_model <- function(train_data) {
  formula <- as.formula(Direction ~ .)
  model <- glm(formula, family = "binomial", data = train_data)
  return(model)
}

# Función para hacer predicciones
make_predictions <- function(model, data) {
  predictions <- predict(model, data)
  probabilities <- 1 / (1 + exp(-predictions))
  return(list(predictions = predictions, probabilities = probabilities))
}

# Función para evaluar el modelo
evaluate_model <- function(predicted_direction, actual_direction) {
  confusion_matrix <- confusionMatrix(
    factor(predicted_direction),
    factor(actual_direction),
    mode = "everything"
  )
  return(confusion_matrix)
}