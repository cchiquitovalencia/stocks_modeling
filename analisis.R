# Función principal para ejecutar el análisis
main_analysis <- function() {
  # Inicializar el análisis
  print("Iniciando el análisis principal...")
  
  tryCatch(
    expr = {
      # Cargar datos
      print("Cargando datos...")
      data <- load_data("./dji.RDS")
      if (is.null(data) || nrow(data) == 0) {
        stop("No se pudieron cargar los datos. Verificar la ruta del archivo.")
      }
      print(paste("Datos cargados exitosamente. Número de filas:", nrow(data)))
      
      # Crear variables predictoras
      print("Creando variables predictoras...")
      features <- create_features(data)
      if (is.null(features)) {
        stop("Error al crear las variables predictoras.")
      }
      print("Variables predictoras creadas exitosamente.")
      
      # Crear tabla de datos
      print("Creando tabla de datos...")
      data <- as.xts(create_data_table(data, features))
      if (is.null(data)) {
        stop("Error al crear la tabla de datos.")
      }
      print("Tabla de datos creada exitosamente.")
      
      # Dividir datos en train y test
      print("Dividiendo datos en train y test...")
      splited_data <- split_time_series(data)
      train_data <- splited_data$train
      test_data <- splited_data$test
      
      if (nrow(train_data) == 0 || nrow(test_data) == 0) {
        stop("Error al dividir los datos. Verificar la división.")
      }
      print(paste("Datos divididos exitosamente. Train:", nrow(train_data), "Test:", nrow(test_data)))
      
      # Normalizar datos
      print("Normalizando datos...")
      normalized <- normalize_data(train_data, test_data)
      normalized_train <- normalized$train
      normalized_test <- normalized$test
      
      normalized_train$Direction <- train_data$Direction
      normalized_test$Direction <- test_data$Direction
      
      if (is.null(normalized_train) || is.null(normalized_test)) {
        stop("Error al normalizar los datos.")
      }
      print("Datos normalizados exitosamente.")
      
      # Construir y entrenar el modelo
      print("Construyendo y entrenando el modelo...")
      model <- build_model((normalized_train))
      if (is.null(model)) {
        stop("Error al construir el modelo.")
      }
      print("Modelo construido y entrenado exitosamente.")
      
      # Hacer predicciones para train
      print("Haciendo predicciones para train...")
      train_predictions <- make_predictions(model, normalized_train)
      if (is.null(train_predictions)) {
        stop("Error al hacer predicciones para train.")
      }
      train_direction <- ifelse(train_predictions$probabilities > 0.5, 1, 0)
      print("Predicciones para train completadas.")
      
      # Evaluar modelo en train
      print("Evaluando modelo en train...")
      train_evaluation <- evaluate_model(train_direction, normalized_train$Direction)
      if (is.null(train_evaluation)) {
        stop("Error al evaluar el modelo en train.")
      }
      print("Evaluación en train:")
      print(train_evaluation)
      
      # Hacer predicciones para test
      print("Haciendo predicciones para test...")
      test_predictions <- make_predictions(model, normalized_test)
      if (is.null(test_predictions)) {
        stop("Error al hacer predicciones para test.")
      }
      test_direction <- ifelse(test_predictions$probabilities > 0.5, 1, 0)
      print("Predicciones para test completadas.")
      
      # Evaluar modelo en test
      print("Evaluando modelo en test...")
      test_evaluation <- evaluate_model(test_direction, normalized_test$Direction)
      if (is.null(test_evaluation)) {
        stop("Error al evaluar el modelo en test.")
      }
      print("Evaluación en test:")
      print(test_evaluation)
      
      # Finalizar el análisis
      print("Análisis completado exitosamente.")
    },
    error = function(e) {
      print(paste("Error en el análisis: ", e$message))
      stop(e)
    },
    finally = {
      print("Proceso finalizado.")
    }
  )
}
