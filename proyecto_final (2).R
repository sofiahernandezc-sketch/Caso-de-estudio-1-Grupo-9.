# =============================================================
# Predicción de la resistencia del hormigón (Caso 1: Regresión)
# Estadística Aplicada · USACH · Grupo 09
# Isidora Barrera, Natalia Rojas, Sofía Hernández, Emilia Vásquez
# =============================================================
# Este script reproduce todo nuestro proyecto de una sola vez:
#    Paso 1: leemos y limpiamos los datos, separamos el test por
#            mezcla, armamos los folds y calculamos el baseline.
#    Paso 2: regresión lineal (OLS), intervalos y diagnóstico.
#    Paso 3: ridge, lasso, elastic-net y GAM (splines).
#    Paso 4: árbol, Random Forest, boosting, su promedio y el
#            gráfico de sobreajuste/subajuste.
#    Paso 5: evaluamos UNA sola vez en el test el sistema elegido.
#
# Cómo correrlo:
#   1. Dejar el script en una carpeta con esta estructura:
#         caso1_hormigon/
#         ├── proyecto_final.R          <- este archivo
#         ├── Datos/Concrete_Data.xls
#         └── resultados/              (si ya existe, no borrarla)
#   2. En RStudio: Session -> Restart R, abrir el script y ejecutar
#      todo (Ctrl + A y luego Ctrl + Enter).
#   3. Demora entre 4 y 10 minutos. Al final debería aparecer
#      RMSE_test 5.31 y "REPRODUCIBILIDAD OK".
#
# Cómo lo organizamos:
#  - Cada paso va dentro de local({ }), para que lo que crea un
#    paso no se mezcle con el siguiente. Los pasos se pasan
#    información solo a través de los archivos de la carpeta
#    "resultados".
#  - Si ya había resultados guardados, al final comparamos que
#    los nuevos salgan iguales: así comprobamos que el proyecto
#    es reproducible.
#
# Ojo: al volver a correrlo, el paso 5 avisa que el test "ya se
# había abierto". Es normal: usamos el mismo sistema y la misma
# semilla, así que no es una elección nueva.
# =============================================================

rm(list = ls())
inicio_total <- Sys.time()

# Fijamos el generador de números aleatorios (el de R >= 3.6)
# para que set.seed() dé los mismos resultados en cualquier
# computador.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")


# ---- A. Carpeta de trabajo ----------------------------------
# Si se corre desde RStudio, usamos como carpeta de trabajo la
# del script. Si no se detecta, hay que hacer Session -> Set
# Working Directory -> To Source File Location antes de
# correrlo.
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  carpeta <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (nzchar(carpeta) && dir.exists(file.path(carpeta, "Datos"))) {
    setwd(carpeta)
  }
}


# ---- B. Revisamos que estén los paquetes y los datos --------
paquetes <- c("readxl", "car", "sandwich", "lmtest", "glmnet", "mgcv",
              "rpart", "rpart.plot", "ranger", "gbm")
faltan <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(faltan) > 0) {
  stop("Faltan paquetes. Instálenlos con:\n  install.packages(c(\"",
       paste(faltan, collapse = "\", \""), "\"))")
}
if (!file.exists("Datos/Concrete_Data.xls")) {
  stop("No se encuentra Datos/Concrete_Data.xls en ", getwd(), ". ",
       "Revisen que este archivo esté junto a la carpeta Datos y usen ",
       "Session -> Set Working Directory -> To Source File Location.")
}
cat("Verificaciones OK. Carpeta de trabajo:", getwd(), "\n")


# ---- C. Guardamos los resultados anteriores (si hay) --------
# Los usamos al final para comprobar que todo salga igual.
archivos_control <- c("resultados/cv_baseline_por_fold.csv",
                      "resultados/cv_ols_por_fold.csv",
                      "resultados/cv_paso3_por_fold.csv",
                      "resultados/cv_paso4_por_fold.csv",
                      "resultados/test_resultado_final.csv",
                      "resultados/test_predicciones.csv",
                      "resultados/test_error_por_edad.csv")
previos <- lapply(archivos_control, function(f)
  if (file.exists(f)) read.csv2(f) else NULL)
names(previos) <- archivos_control

# Función para confirmar que cada paso generó su archivo de
# resultados
paso_ok <- function(nombre, archivo, t0) {
  if (!file.exists(archivo) || file.mtime(archivo) < t0 - 1) {
    stop(nombre, " terminó sin generar ", archivo)
  }
  cat(sprintf("\n>>> %s terminado en %.1f minutos\n", nombre,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
anunciar <- function(nombre) {
  cat("\n\n#############################################################\n")
  cat("###", nombre, "\n")
  cat("#############################################################\n")
}


# #############################################################
# PASO 1: datos, limpieza, partición por mezcla y baseline
# #############################################################
anunciar("PASO 1: datos, limpieza, partición por mezcla y baseline")
t0 <- Sys.time()
local({
  # En este paso preparamos todo lo que usan los demás: limpiamos
  # los datos, identificamos cada receta, separamos el test por
  # mezcla y lo guardamos sin tocar, armamos los 5 folds y
  # calculamos los baselines contra los que comparamos todo.


  # ---- 0. Configuración ---------------------------------------
  SEMILLA    <- 2026                        # semilla global
  PROP_TEST  <- 0.20                        # 20% de las mezclas al test
  K_FOLDS    <- 5                           # folds de validación cruzada
  RUTA_DATOS <- "Datos/Concrete_Data.xls"

  set.seed(SEMILLA)

  # install.packages("readxl")   # solo la primera vez
  library(readxl)

  dir.create("resultados", showWarnings = FALSE)


  # ---- 1. Leemos los datos y sacamos los duplicados -----------
  datos <- as.data.frame(read_excel(RUTA_DATOS))
  names(datos) <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                    "arido_grueso", "arido_fino", "edad", "resistencia")

  n_original <- nrow(datos)
  datos <- datos[!duplicated(datos), ]
  cat("Filas originales:", n_original,
      "| duplicados eliminados:", n_original - nrow(datos),
      "| filas utilizables:", nrow(datos), "\n")


  # ---- 2. Variables derivadas ----------------------------------
  # Las calculamos fila a fila (no usan información de otras
  # filas), así que no generan fuga. Las razones agua/cemento y
  # agua/aglomerante tienen sentido en ingeniería (ley de Abrams).
  datos$aglomerante      <- datos$cemento + datos$escoria + datos$ceniza
  datos$agua_cemento     <- datos$agua / datos$cemento
  datos$agua_aglomerante <- datos$agua / datos$aglomerante

  # Grupo de edad: lo usamos para el baseline "media por edad" y
  # para revisar el subgrupo crítico (7 días o menos).
  datos$grupo_edad <- cut(datos$edad, breaks = c(0, 7, 28, 90, 365),
                          labels = c("1-7", "8-28", "29-90", "91-365"))


  # ---- 3. Identificamos cada mezcla (receta) -----------------
  # Una mezcla es una combinación de los 7 componentes.
  # Redondeamos a 1 decimal para que diferencias mínimas de Excel
  # no separen recetas que en realidad son iguales.
  componentes <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                   "arido_grueso", "arido_fino")
  clave <- do.call(paste, c(round(datos[componentes], 1), sep = "_"))
  datos$mezcla <- as.integer(factor(clave))
  cat("Mezclas distintas:", length(unique(datos$mezcla)), "\n")

  # Describimos los datos (solo imprime; no ajusta nada ni usa
  # azar). De aquí salen las cifras de las láminas 2 y 3.
  cat("Valores faltantes:", sum(is.na(datos)), "\n")
  cat(sprintf("Probetas por mezcla: %.2f en promedio\n",
              nrow(datos) / length(unique(datos$mezcla))))
  cat("\nResumen de las variables originales\n")
  print(summary(datos[, c(componentes, "edad", "resistencia")]))
  cat("\nPorcentaje de probetas con 0 kg/m3 del componente\n")
  print(round(100 * colMeans(datos[, c("escoria", "ceniza", "superplast")] == 0), 1))
  cat(sprintf("Probetas con 7 días o menos: %.1f%%\n", 100 * mean(datos$edad <= 7)))
  cat("Probetas por edad de ensayo\n")
  print(table(datos$edad))


  # ---- 4. Separamos entrenamiento y test POR MEZCLA ----------
  # Como cada receta se ensayó a varias edades, todas sus filas
  # van al mismo lado. Así el test tiene recetas que el modelo
  # nunca vio, igual que en el uso real de la planta.
  mezclas      <- unique(datos$mezcla)
  mezclas_test <- sample(mezclas, size = round(PROP_TEST * length(mezclas)))

  test  <- datos[datos$mezcla %in% mezclas_test, ]
  train <- datos[!(datos$mezcla %in% mezclas_test), ]

  # Verificación: si una mezcla quedara en los dos conjuntos, el
  # código se detiene.
  stopifnot(length(intersect(train$mezcla, test$mezcla)) == 0)
  cat("Entrenamiento:", nrow(train), "filas |",
      "Test:", nrow(test), "filas\n")
  cat("Mezclas en entrenamiento:", length(unique(train$mezcla)),
      "| mezclas en test:", length(unique(test$mezcla)), "\n")

  # Guardamos el test en un archivo y lo sacamos de la sesión. No
  # lo volvemos a abrir hasta el paso 5, con el sistema ya
  # elegido.
  saveRDS(test, "resultados/test_congelado.rds")
  rm(test)


  # ---- 5. Folds de validación cruzada POR MEZCLA --------------
  # Usamos estos mismos 5 folds para TODOS los modelos, para
  # compararlos en igualdad de condiciones.
  mezclas_train  <- unique(train$mezcla)
  fold_de_mezcla <- sample(rep(1:K_FOLDS, length.out = length(mezclas_train)))
  names(fold_de_mezcla) <- mezclas_train
  train$fold <- unname(fold_de_mezcla[as.character(train$mezcla)])

  # Verificación: cada mezcla queda en un solo fold.
  stopifnot(all(tapply(train$fold, train$mezcla,
                       function(f) length(unique(f))) == 1))
  print(table(fold = train$fold))

  saveRDS(train, "resultados/train_con_folds.rds")


  # ---- 6. Métricas: RMSE, MAE y R2 ---------------------------
  metricas <- function(real, pred) {
    c(RMSE = sqrt(mean((real - pred)^2)),
      MAE  = mean(abs(real - pred)),
      R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2))
  }


  # ---- 7. Baselines con validación cruzada --------------------
  # Probamos dos: la media global y la media por grupo de edad.
  # Usamos la segunda como referencia porque es más exigente: es
  # lo que haría un técnico sin modelo. Las medias se calculan
  # SOLO con los folds de entrenamiento.
  resultados <- data.frame()

  for (k in 1:K_FOLDS) {
    ent <- train[train$fold != k, ]
    val <- train[train$fold == k, ]

    media_global <- mean(ent$resistencia)
    pred_global  <- rep(media_global, nrow(val))

    medias_edad <- tapply(ent$resistencia, ent$grupo_edad, mean)
    pred_edad   <- unname(medias_edad[as.character(val$grupo_edad)])
    pred_edad[is.na(pred_edad)] <- media_global   # grupo sin datos

    temprana <- val$edad <= 7   # subgrupo crítico

    for (modelo in c("Media global", "Media por edad")) {
      pred  <- if (modelo == "Media global") pred_global else pred_edad
      m     <- metricas(val$resistencia, pred)
      m_sub <- metricas(val$resistencia[temprana], pred[temprana])
      resultados <- rbind(resultados, data.frame(
        modelo = modelo, fold = k,
        RMSE = m[["RMSE"]], MAE = m[["MAE"]], R2 = m[["R2"]],
        RMSE_edad_temprana = m_sub[["RMSE"]]))
    }
  }

  write.csv2(resultados, "resultados/cv_baseline_por_fold.csv", row.names = FALSE)


  # ---- 8. Resumen: media +/- desviación estándar entre folds --
  fmt <- function(x) sprintf("%.2f +/- %.2f", mean(x), sd(x))
  resumen <- do.call(rbind, lapply(split(resultados, resultados$modelo),
    function(r) data.frame(
      modelo             = r$modelo[1],
      RMSE               = fmt(r$RMSE),
      MAE                = fmt(r$MAE),
      R2                 = fmt(r$R2),
      RMSE_edad_temprana = fmt(r$RMSE_edad_temprana))))

  cat("\nResultados de validación cruzada (unidades: MPa)\n")
  print(resumen, row.names = FALSE)
  write.csv2(resumen, "resultados/cv_baseline_resumen.csv", row.names = FALSE)


  # ---- 9. Guardamos las versiones de R y paquetes -------------
  writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")
  cat("\nListo. Archivos guardados en la carpeta 'resultados'.\n")
})
paso_ok("Paso 1", "resultados/cv_baseline_por_fold.csv", t0)

# #############################################################
# PASO 2: regresión lineal (OLS), inferencia y diagnóstico
# #############################################################
anunciar("PASO 2: regresión lineal (OLS), inferencia y diagnóstico")
t0 <- Sys.time()
local({
  # Aquí probamos la regresión lineal: medimos su error con los
  # mismos folds, estimamos una relación con intervalo de
  # confianza y revisamos los supuestos (residuos y
  # multicolinealidad). El test no se usa.


  # ---- 0. Configuración ---------------------------------------
  SEMILLA <- 2026
  set.seed(SEMILLA)

  # Paquetes (instalar solo la primera vez):
  # install.packages(c("car", "sandwich", "lmtest"))
  library(car)        # vif(): multicolinealidad
  library(sandwich)   # vcovCL(): errores estándar por mezcla
  library(lmtest)     # coeftest(), coefci()

  train   <- readRDS("resultados/train_con_folds.rds")
  K_FOLDS <- max(train$fold)


  # ---- 1. Modelos lineales que comparamos ---------------------
  # OLS 1: las 8 variables tal cual.
  # OLS 2: igual, pero con log(edad). El hormigón gana resistencia
  # rápido al principio y después se estabiliza, y el logaritmo
  # capta esa forma. Es una transformación fija, así que no genera
  # fuga.
  # No usamos "aglomerante" porque es la suma exacta de cemento +
  # escoria + ceniza (colinealidad perfecta).
  f_ols <- resistencia ~ cemento + escoria + ceniza + agua + superplast +
                         arido_grueso + arido_fino + edad
  f_ols_log <- resistencia ~ cemento + escoria + ceniza + agua + superplast +
                             arido_grueso + arido_fino + log(edad)


  # ---- 2. Funciones auxiliares --------------------------------
  metricas <- function(real, pred) {
    c(RMSE = sqrt(mean((real - pred)^2)),
      MAE  = mean(abs(real - pred)),
      R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2))
  }

  # Baseline de referencia: media por grupo de edad (igual que en
  # el paso 1)
  pred_media_edad <- function(ent, val) {
    medias <- tapply(ent$resistencia, ent$grupo_edad, mean)
    pred <- unname(medias[as.character(val$grupo_edad)])
    pred[is.na(pred)] <- mean(ent$resistencia)
    pred
  }


  # ---- 3. Validación cruzada con los MISMOS folds del paso 1 --
  resultados <- data.frame()

  for (k in 1:K_FOLDS) {
    ent <- train[train$fold != k, ]
    val <- train[train$fold == k, ]
    temprana <- val$edad <= 7

    predicciones <- list(
      "Baseline (media por edad)" = pred_media_edad(ent, val),
      "OLS"                       = predict(lm(f_ols, data = ent), newdata = val),
      "OLS con log(edad)"         = predict(lm(f_ols_log, data = ent), newdata = val)
    )

    for (modelo in names(predicciones)) {
      pred  <- predicciones[[modelo]]
      m     <- metricas(val$resistencia, pred)
      m_sub <- metricas(val$resistencia[temprana], pred[temprana])
      resultados <- rbind(resultados, data.frame(
        modelo = modelo, fold = k,
        RMSE = m[["RMSE"]], MAE = m[["MAE"]], R2 = m[["R2"]],
        RMSE_edad_temprana = m_sub[["RMSE"]]))
    }
  }

  # Diferencia de RMSE con el baseline en cada fold (negativo = el
  # modelo se equivoca menos)
  rmse_base <- resultados$RMSE[resultados$modelo == "Baseline (media por edad)"]
  resultados$dif_RMSE_vs_baseline <- resultados$RMSE -
    rmse_base[resultados$fold]

  write.csv2(resultados, "resultados/cv_ols_por_fold.csv", row.names = FALSE)


  # ---- 4. Resumen: media +/- desviación estándar entre folds --
  fmt <- function(x) sprintf("%.2f +/- %.2f", mean(x), sd(x))
  orden <- c("Baseline (media por edad)", "OLS", "OLS con log(edad)")
  resumen <- do.call(rbind, lapply(orden, function(mod) {
    r <- resultados[resultados$modelo == mod, ]
    data.frame(modelo             = mod,
               RMSE               = fmt(r$RMSE),
               MAE                = fmt(r$MAE),
               R2                 = fmt(r$R2),
               RMSE_edad_temprana = fmt(r$RMSE_edad_temprana),
               vs_baseline        = fmt(r$dif_RMSE_vs_baseline))
  }))

  cat("\nValidación cruzada por mezcla (unidades: MPa)\n")
  print(resumen, row.names = FALSE)
  write.csv2(resumen, "resultados/cv_ols_resumen.csv", row.names = FALSE)


  # ---- 5. Relación estimada con intervalo de confianza -------
  # Ajustamos el OLS con log(edad) en todo el entrenamiento. Como
  # las filas de una misma mezcla no son independientes, usamos
  # errores estándar agrupados por mezcla (los de summary() serían
  # demasiado optimistas).
  modelo_ols <- lm(f_ols_log, data = train)
  vc_mezcla  <- vcovCL(modelo_ols, cluster = ~ mezcla)

  cat("\nCoeficientes con errores estándar agrupados por mezcla\n")
  print(coeftest(modelo_ols, vcov. = vc_mezcla))

  ic <- coefci(modelo_ols, vcov. = vc_mezcla, level = 0.95)

  # Relevancia práctica: traducimos cada coeficiente al efecto de
  # un cambio realista (por ejemplo, +50 kg/m3 de cemento).
  cambio <- c(cemento = 50, escoria = 50, ceniza = 50, agua = 10,
              superplast = 5, arido_grueso = 50, arido_fino = 50,
              "log(edad)" = log(2))
  descripcion <- c("+50 kg/m3", "+50 kg/m3", "+50 kg/m3", "+10 kg/m3",
                   "+5 kg/m3", "+50 kg/m3", "+50 kg/m3", "duplicar la edad")

  relevancia <- data.frame(
    variable   = names(cambio),
    cambio     = descripcion,
    efecto_MPa = round(coef(modelo_ols)[names(cambio)] * cambio, 2),
    IC95_inf   = round(ic[names(cambio), 1] * cambio, 2),
    IC95_sup   = round(ic[names(cambio), 2] * cambio, 2))

  cat("\nEfecto estimado de un cambio realista (MPa, IC 95%)\n")
  print(relevancia, row.names = FALSE)
  write.csv2(relevancia, "resultados/ols_relevancia.csv", row.names = FALSE)


  # ---- 6. Diagnóstico de residuos -----------------------------
  png("resultados/ols_diagnostico_residuos.png",
      width = 1600, height = 1200, res = 150)
  par(mfrow = c(2, 2))
  plot(modelo_ols)
  dev.off()

  png("resultados/ols_residuos_vs_edad.png",
      width = 1200, height = 800, res = 150)
  plot(train$edad, residuals(modelo_ols), log = "x",
       xlab = "Edad (días, escala log)", ylab = "Residuo (MPa)",
       main = "Residuos de OLS según la edad", pch = 16,
       col = rgb(0, 0, 0, 0.3))
  abline(h = 0, col = "red", lwd = 2)
  dev.off()

  # Prueba de Breusch-Pagan: si p < 0,05, la varianza de los
  # residuos no es constante (heterocedasticidad). Por eso los
  # intervalos de la sección 5 usan errores robustos.
  cat("\nPrueba de Breusch-Pagan (H0: varianza constante)\n")
  print(bptest(modelo_ols))


  # ---- 7. Multicolinealidad (VIF) -----------------------------
  # Regla práctica: VIF > 5 es moderado y > 10 es alto.
  # Esperábamos VIF moderados: los 7 componentes suman casi la
  # masa total de 1 m3, así que cada uno se puede predecir con los
  # demás. Esto vuelve inestables los coeficientes y es lo que nos
  # llevó a probar regularización en el paso 3.
  vif_ols <- vif(modelo_ols)
  cat("\nVIF (factor de inflación de varianza)\n")
  print(round(vif_ols, 2))
  write.csv2(data.frame(variable = names(vif_ols), VIF = round(vif_ols, 2)),
             "resultados/ols_vif.csv", row.names = FALSE)

  cat("\nListo. Resultados del paso 2 guardados en 'resultados'.\n")
})
paso_ok("Paso 2", "resultados/cv_ols_por_fold.csv", t0)

# #############################################################
# PASO 3: ridge, lasso, elastic-net y GAM
# #############################################################
anunciar("PASO 3: ridge, lasso, elastic-net y GAM")
t0 <- Sys.time()
local({
  # En este paso probamos modelos con "freno" (ridge, lasso y
  # elastic-net) y un modelo con curvas (GAM).
  # Para elegir los hiperparámetros sin fuga usamos validación
  # cruzada anidada:
  #  - Afuera, los mismos 5 folds del paso 1 miden el error y lo
  #    comparan con el baseline y el OLS.
  #  - Adentro de cada fold, otra validación cruzada (también por
  #    mezcla) elige lambda y alfa usando solo datos de
  #    entrenamiento.
  #  - glmnet estandariza las variables dentro de cada ajuste, o
  #    sea, también solo con datos de entrenamiento.


  # ---- 0. Configuración ---------------------------------------
  SEMILLA   <- 2026
  K_INTERNO <- 5                      # folds de la CV interna
  ALFAS_EN  <- c(0.25, 0.50, 0.75)    # alfas candidatos para elastic-net
  REGLA_LAMBDA <- "lambda.1se"        # lambda más simple dentro de 1 error estándar del mínimo
  set.seed(SEMILLA)

  # Paquetes (instalar solo la primera vez):
  # install.packages(c("glmnet", "mgcv"))
  library(glmnet)   # ridge, lasso, elastic-net
  library(mgcv)     # GAM con splines

  train <- readRDS("resultados/train_con_folds.rds")
  train$log_edad <- log(train$edad)
  K_FOLDS <- max(train$fold)


  # ---- 1. Variables y fórmulas --------------------------------
  # Para la regularización agregamos las razones agua/cemento y
  # agua/aglomerante, y dejamos que el lasso decida si sirven.
  predictores <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                   "arido_grueso", "arido_fino", "log_edad",
                   "agua_cemento", "agua_aglomerante")

  f_ols_log <- resistencia ~ cemento + escoria + ceniza + agua + superplast +
                             arido_grueso + arido_fino + log_edad

  # GAM: cada variable entra como una curva suave (spline) en vez
  # de una recta. La suavidad de cada curva la elige REML con los
  # datos de entrenamiento del fold.
  f_gam <- resistencia ~ s(cemento) + s(escoria) + s(ceniza) + s(agua) +
                         s(superplast) + s(arido_grueso) + s(arido_fino) +
                         s(log_edad)


  # ---- 2. Funciones auxiliares --------------------------------
  metricas <- function(real, pred) {
    c(RMSE = sqrt(mean((real - pred)^2)),
      MAE  = mean(abs(real - pred)),
      R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2))
  }

  pred_media_edad <- function(ent, val) {
    medias <- tapply(ent$resistencia, ent$grupo_edad, mean)
    pred <- unname(medias[as.character(val$grupo_edad)])
    pred[is.na(pred)] <- mean(ent$resistencia)
    pred
  }

  matriz_x <- function(d) as.matrix(d[, predictores])

  # Folds internos por mezcla (una mezcla nunca queda partida)
  folds_internos <- function(mezcla, K) {
    m <- unique(mezcla)
    f <- sample(rep(1:K, length.out = length(m)))
    names(f) <- m
    unname(f[as.character(mezcla)])
  }

  ajustar_glmnet <- function(ent, alfa, foldid) {
    cv.glmnet(matriz_x(ent), ent$resistencia, alpha = alfa,
              foldid = foldid, standardize = TRUE)
  }

  # Elastic-net: probamos varios alfas con los MISMOS folds
  # internos y nos quedamos con el de menor error.
  ajustar_en <- function(ent, foldid) {
    ajustes <- lapply(ALFAS_EN, function(a) ajustar_glmnet(ent, a, foldid))
    errores <- sapply(ajustes, function(a) min(a$cvm))
    mejor   <- which.min(errores)
    list(modelo = ajustes[[mejor]], alfa = ALFAS_EN[mejor])
  }

  predecir_glmnet <- function(ajuste, val) {
    as.numeric(predict(ajuste, newx = matriz_x(val), s = REGLA_LAMBDA))
  }


  # ---- 3. Validación cruzada (folds del paso 1) ---------------
  resultados     <- data.frame()
  hiperparametros <- data.frame()

  for (k in 1:K_FOLDS) {
    cat("Fold", k, "de", K_FOLDS, "...\n")
    ent <- train[train$fold != k, ]
    val <- train[train$fold == k, ]
    temprana <- val$edad <= 7

    foldid <- folds_internos(ent$mezcla, K_INTERNO)

    ridge <- ajustar_glmnet(ent, 0, foldid)
    lasso <- ajustar_glmnet(ent, 1, foldid)
    en    <- ajustar_en(ent, foldid)
    gam_k <- gam(f_gam, data = ent, method = "REML")

    hiperparametros <- rbind(hiperparametros, data.frame(
      fold = k,
      lambda_ridge = ridge[[REGLA_LAMBDA]],
      lambda_lasso = lasso[[REGLA_LAMBDA]],
      alfa_EN      = en$alfa,
      lambda_EN    = en$modelo[[REGLA_LAMBDA]],
      edf_total_GAM = sum(gam_k$edf)))

    predicciones <- list(
      "Baseline (media por edad)" = pred_media_edad(ent, val),
      "OLS con log(edad)" = predict(lm(f_ols_log, data = ent), newdata = val),
      "Ridge"             = predecir_glmnet(ridge, val),
      "Lasso"             = predecir_glmnet(lasso, val),
      "Elastic-net"       = predecir_glmnet(en$modelo, val),
      "GAM (splines)"     = as.numeric(predict(gam_k, newdata = val))
    )

    for (modelo in names(predicciones)) {
      pred  <- predicciones[[modelo]]
      m     <- metricas(val$resistencia, pred)
      m_sub <- metricas(val$resistencia[temprana], pred[temprana])
      resultados <- rbind(resultados, data.frame(
        modelo = modelo, fold = k,
        RMSE = m[["RMSE"]], MAE = m[["MAE"]], R2 = m[["R2"]],
        RMSE_edad_temprana = m_sub[["RMSE"]]))
    }
  }

  rmse_base <- resultados$RMSE[resultados$modelo == "Baseline (media por edad)"]
  resultados$dif_RMSE_vs_baseline <- resultados$RMSE - rmse_base[resultados$fold]

  write.csv2(resultados, "resultados/cv_paso3_por_fold.csv", row.names = FALSE)
  write.csv2(hiperparametros, "resultados/paso3_hiperparametros_por_fold.csv",
             row.names = FALSE)


  # ---- 4. Resumen ---------------------------------------------
  fmt <- function(x) sprintf("%.2f +/- %.2f", mean(x), sd(x))
  orden <- c("Baseline (media por edad)", "OLS con log(edad)", "Ridge",
             "Lasso", "Elastic-net", "GAM (splines)")
  resumen <- do.call(rbind, lapply(orden, function(mod) {
    r <- resultados[resultados$modelo == mod, ]
    data.frame(modelo             = mod,
               RMSE               = fmt(r$RMSE),
               MAE                = fmt(r$MAE),
               R2                 = fmt(r$R2),
               RMSE_edad_temprana = fmt(r$RMSE_edad_temprana),
               vs_baseline        = fmt(r$dif_RMSE_vs_baseline))
  }))

  cat("\nValidación cruzada por mezcla (unidades: MPa)\n")
  print(resumen, row.names = FALSE)
  write.csv2(resumen, "resultados/cv_paso3_resumen.csv", row.names = FALSE)

  cat("\nHiperparámetros elegidos en cada fold (CV interna)\n")
  print(hiperparametros, row.names = FALSE, digits = 3)


  # ---- 5. Modelos finales en todo el entrenamiento ------------
  # Solo los usamos para interpretar (coeficientes y curvas);
  # lambda se elige con los folds del paso 1.
  ridge_final <- ajustar_glmnet(train, 0, train$fold)
  lasso_final <- ajustar_glmnet(train, 1, train$fold)
  en_final    <- ajustar_en(train, train$fold)

  coeficientes <- data.frame(
    variable    = rownames(coef(lasso_final, s = REGLA_LAMBDA)),
    Ridge       = as.numeric(coef(ridge_final, s = REGLA_LAMBDA)),
    Lasso       = as.numeric(coef(lasso_final, s = REGLA_LAMBDA)),
    Elastic_net = as.numeric(coef(en_final$modelo, s = REGLA_LAMBDA)))
  coeficientes[, -1] <- round(coeficientes[, -1], 4)

  cat("\nCoeficientes con", REGLA_LAMBDA,
      "(un 0 en lasso = variable eliminada)\n")
  cat("Alfa elegido para elastic-net:", en_final$alfa, "\n")
  print(coeficientes, row.names = FALSE)
  write.csv2(coeficientes, "resultados/paso3_coeficientes.csv", row.names = FALSE)

  # Gráficos de la regularización: cómo se eligió lambda y qué
  # variables elimina el lasso
  png("resultados/lasso_cv_lambda.png", width = 1400, height = 900, res = 150)
  plot(lasso_final)
  title("Lasso: error de validación según lambda", line = 2.5)
  dev.off()

  png("resultados/lasso_trayectorias.png", width = 1400, height = 900, res = 150)
  plot(lasso_final$glmnet.fit, xvar = "lambda", label = TRUE)
  abline(v = log(lasso_final[[REGLA_LAMBDA]]), lty = 2, col = "red")
  title("Lasso: coeficientes según lambda (línea roja = lambda elegido)",
        line = 2.5)
  dev.off()


  # ---- 6. GAM final: curvas estimadas y diagnóstico -----------
  gam_final <- gam(f_gam, data = train, method = "REML")

  cat("\nResumen del GAM (edf = flexibilidad de cada curva; edf = 1 es una recta)\n")
  print(summary(gam_final)$s.table)

  cat("\nChequeo de nudos (k-index < 1 con p bajo sugiere aumentar k)\n")
  print(k.check(gam_final))

  png("resultados/gam_efectos_parciales.png", width = 1800, height = 1400, res = 150)
  plot(gam_final, pages = 1, shade = TRUE, scale = 0)
  dev.off()

  png("resultados/gam_residuos_vs_edad.png", width = 1200, height = 800, res = 150)
  plot(train$edad, residuals(gam_final), log = "x",
       xlab = "Edad (días, escala log)", ylab = "Residuo (MPa)",
       main = "Residuos del GAM según la edad", pch = 16,
       col = rgb(0, 0, 0, 0.3))
  abline(h = 0, col = "red", lwd = 2)
  dev.off()

  cat("\nListo. Resultados del paso 3 guardados en 'resultados'.\n")
})
paso_ok("Paso 3", "resultados/cv_paso3_por_fold.csv", t0)

# #############################################################
# PASO 4: árbol, Random Forest, boosting y combinación
# #############################################################
anunciar("PASO 4: árbol, Random Forest, boosting y combinación")
t0 <- Sys.time()
local({
  # En este paso probamos modelos basados en árboles: un árbol
  # solo, Random Forest (muchos árboles promediados), boosting
  # (árboles en secuencia) y el promedio de GAM + RF + GBM.
  # Igual que en el paso 3, los hiperparámetros se eligen con
  # validación cruzada anidada por mezcla, así que no hay fuga. Es
  # el paso más lento: va mostrando su avance en la consola.


  # ---- 0. Configuración ---------------------------------------
  SEMILLA   <- 2026
  K_INTERNO <- 5
  set.seed(SEMILLA)

  # Grillas de hiperparámetros que probamos
  GRID_CP  <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05)          # árbol: poda
  GRID_RF  <- expand.grid(mtry = c(2, 4, 6, 8),                  # RF: variables por corte
                          min.node.size = c(1, 5))               # RF: tamaño mínimo de hoja
  GRID_GBM <- expand.grid(depth = c(2, 4, 6),                    # boosting: profundidad
                          shrinkage = c(0.05, 0.10))             # boosting: tasa de aprendizaje
  N_TREES_RF      <- 500
  N_TREES_GBM_MAX <- 1500
  SEQ_N_TREES     <- seq(100, N_TREES_GBM_MAX, by = 100)         # boosting: n.º de árboles

  # Paquetes (instalar solo la primera vez):
  # install.packages(c("rpart", "rpart.plot", "ranger", "gbm", "mgcv"))
  library(rpart)
  library(rpart.plot)
  library(ranger)
  library(gbm)
  library(mgcv)

  train <- readRDS("resultados/train_con_folds.rds")
  train$log_edad <- log(train$edad)
  K_FOLDS <- max(train$fold)


  # ---- 1. Variables y fórmulas --------------------------------
  # Los árboles no necesitan log(edad): cortar en edad o en
  # log(edad) da lo mismo.
  vars_arbol <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                  "arido_grueso", "arido_fino", "edad",
                  "agua_cemento", "agua_aglomerante")
  f_arbol <- as.formula(paste("resistencia ~", paste(vars_arbol, collapse = " + ")))

  f_ols_log <- resistencia ~ cemento + escoria + ceniza + agua + superplast +
                             arido_grueso + arido_fino + log_edad
  f_gam <- resistencia ~ s(cemento) + s(escoria) + s(ceniza) + s(agua) +
                         s(superplast) + s(arido_grueso) + s(arido_fino) +
                         s(log_edad)


  # ---- 2. Funciones auxiliares --------------------------------
  rmse <- function(real, pred) sqrt(mean((real - pred)^2))

  metricas <- function(real, pred) {
    c(RMSE = rmse(real, pred),
      MAE  = mean(abs(real - pred)),
      R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2))
  }

  pred_media_edad <- function(ent, val) {
    medias <- tapply(ent$resistencia, ent$grupo_edad, mean)
    pred <- unname(medias[as.character(val$grupo_edad)])
    pred[is.na(pred)] <- mean(ent$resistencia)
    pred
  }

  folds_internos <- function(mezcla, K) {
    m <- unique(mezcla)
    f <- sample(rep(1:K, length.out = length(m)))
    names(f) <- m
    unname(f[as.character(mezcla)])
  }

  # Cada función tune_* guarda en "grillas" el error de todas las
  # combinaciones probadas. Lo usamos en la sección 5 para mostrar
  # que las opciones más simples predicen peor.
  grillas <- new.env()

  # --- Árbol de regresión ---
  ajustar_arbol <- function(d, cp) {
    rpart(f_arbol, data = d, method = "anova",
          control = rpart.control(cp = cp, xval = 0))
  }
  tune_arbol <- function(ent, foldid) {
    err <- sapply(GRID_CP, function(cp) {
      mean(sapply(unique(foldid), function(j) {
        a <- ajustar_arbol(ent[foldid != j, ], cp)
        rmse(ent$resistencia[foldid == j], predict(a, ent[foldid == j, ]))
      }))
    })
    grillas$arbol <- data.frame(cp = GRID_CP, RMSE_CV = err)
    GRID_CP[which.min(err)]
  }

  # --- Random Forest ---
  ajustar_rf <- function(d, mtry, mns, importance = "none") {
    ranger(f_arbol, data = d, num.trees = N_TREES_RF, mtry = mtry,
           min.node.size = mns, importance = importance,
           seed = SEMILLA, num.threads = 1)   # 1 hilo = resultados reproducibles
  }
  tune_rf <- function(ent, foldid) {
    err <- sapply(seq_len(nrow(GRID_RF)), function(i) {
      mean(sapply(unique(foldid), function(j) {
        m <- ajustar_rf(ent[foldid != j, ], GRID_RF$mtry[i], GRID_RF$min.node.size[i])
        rmse(ent$resistencia[foldid == j],
             predict(m, ent[foldid == j, ])$predictions)
      }))
    })
    grillas$rf <- data.frame(GRID_RF, RMSE_CV = err)
    GRID_RF[which.min(err), ]
  }

  # --- Gradient Boosting ---
  ajustar_gbm <- function(d, depth, shrinkage, n_trees) {
    set.seed(SEMILLA)   # bag.fraction usa azar
    gbm(f_arbol, data = d[, c("resistencia", vars_arbol)],
        distribution = "gaussian", n.trees = n_trees,
        interaction.depth = depth, shrinkage = shrinkage,
        n.minobsinnode = 10, bag.fraction = 0.8, verbose = FALSE)
  }
  tune_gbm <- function(ent, foldid) {
    err <- matrix(0, nrow(GRID_GBM), length(SEQ_N_TREES))
    folds <- unique(foldid)
    for (i in seq_len(nrow(GRID_GBM))) {
      for (j in folds) {
        m <- ajustar_gbm(ent[foldid != j, ], GRID_GBM$depth[i],
                         GRID_GBM$shrinkage[i], N_TREES_GBM_MAX)
        p <- predict(m, ent[foldid == j, ], n.trees = SEQ_N_TREES)
        err[i, ] <- err[i, ] + apply(p, 2, function(col)
          rmse(ent$resistencia[foldid == j], col)) / length(folds)
      }
    }
    grillas$gbm <- data.frame(GRID_GBM,
                              n_trees_mejor = SEQ_N_TREES[apply(err, 1, which.min)],
                              RMSE_CV = apply(err, 1, min))
    pos <- which(err == min(err), arr.ind = TRUE)[1, ]
    list(depth = GRID_GBM$depth[pos[1]],
         shrinkage = GRID_GBM$shrinkage[pos[1]],
         n_trees = SEQ_N_TREES[pos[2]])
  }


  # ---- 3. Validación cruzada (folds del paso 1) ---------------
  resultados      <- data.frame()
  hiperparametros <- data.frame()

  for (k in 1:K_FOLDS) {
    cat("Fold", k, "de", K_FOLDS, ": ajustando modelos...\n")
    ent <- train[train$fold != k, ]
    val <- train[train$fold == k, ]
    temprana <- val$edad <= 7
    foldid <- folds_internos(ent$mezcla, K_INTERNO)

    cp_k  <- tune_arbol(ent, foldid)
    rf_k  <- tune_rf(ent, foldid)
    gbm_k <- tune_gbm(ent, foldid)

    arbol  <- ajustar_arbol(ent, cp_k)
    rf     <- ajustar_rf(ent, rf_k$mtry, rf_k$min.node.size)
    boost  <- ajustar_gbm(ent, gbm_k$depth, gbm_k$shrinkage, gbm_k$n_trees)
    gam_k  <- gam(f_gam, data = ent, method = "REML")

    hiperparametros <- rbind(hiperparametros, data.frame(
      fold = k, cp_arbol = cp_k, hojas_arbol = sum(arbol$frame$var == "<leaf>"),
      mtry_RF = rf_k$mtry, min_node_RF = rf_k$min.node.size,
      depth_GBM = gbm_k$depth, shrinkage_GBM = gbm_k$shrinkage,
      n_trees_GBM = gbm_k$n_trees))

    p_gam   <- as.numeric(predict(gam_k, newdata = val))
    p_rf    <- predict(rf, val)$predictions
    p_boost <- predict(boost, val, n.trees = gbm_k$n_trees)

    predicciones <- list(
      "Baseline (media por edad)" = pred_media_edad(ent, val),
      "OLS con log(edad)" = predict(lm(f_ols_log, data = ent), newdata = val),
      "GAM (splines)"     = p_gam,
      "Árbol de regresión" = predict(arbol, val),
      "Random Forest"     = p_rf,
      "Boosting (GBM)"    = p_boost,
      # Promedio simple con pesos fijos (1/3 cada uno): no se ajusta
      # nada, así que no hay fuga
      "Promedio GAM + RF + GBM" = (p_gam + p_rf + p_boost) / 3
    )

    for (modelo in names(predicciones)) {
      pred  <- predicciones[[modelo]]
      m     <- metricas(val$resistencia, pred)
      m_sub <- metricas(val$resistencia[temprana], pred[temprana])
      resultados <- rbind(resultados, data.frame(
        modelo = modelo, fold = k,
        RMSE = m[["RMSE"]], MAE = m[["MAE"]], R2 = m[["R2"]],
        RMSE_edad_temprana = m_sub[["RMSE"]]))
    }
  }

  # Diferencias de RMSE fold a fold (negativo = mejor)
  rmse_ref <- function(nombre) resultados$RMSE[resultados$modelo == nombre][resultados$fold]
  resultados$dif_vs_baseline <- resultados$RMSE - rmse_ref("Baseline (media por edad)")
  resultados$dif_vs_GAM      <- resultados$RMSE - rmse_ref("GAM (splines)")
  resultados$dif_vs_GBM      <- resultados$RMSE - rmse_ref("Boosting (GBM)")

  write.csv2(resultados, "resultados/cv_paso4_por_fold.csv", row.names = FALSE)
  write.csv2(hiperparametros, "resultados/paso4_hiperparametros_por_fold.csv",
             row.names = FALSE)


  # ---- 4. Resumen ---------------------------------------------
  fmt <- function(x) sprintf("%.2f +/- %.2f", mean(x), sd(x))
  orden <- c("Baseline (media por edad)", "OLS con log(edad)", "GAM (splines)",
             "Árbol de regresión", "Random Forest", "Boosting (GBM)",
             "Promedio GAM + RF + GBM")
  resumen <- do.call(rbind, lapply(orden, function(mod) {
    r <- resultados[resultados$modelo == mod, ]
    data.frame(modelo             = mod,
               RMSE               = fmt(r$RMSE),
               MAE                = fmt(r$MAE),
               R2                 = fmt(r$R2),
               RMSE_edad_temprana = fmt(r$RMSE_edad_temprana),
               vs_baseline        = fmt(r$dif_vs_baseline),
               vs_GAM             = fmt(r$dif_vs_GAM),
               vs_GBM             = fmt(r$dif_vs_GBM))
  }))

  cat("\nValidación cruzada por mezcla (unidades: MPa)\n")
  print(resumen[, 1:5], row.names = FALSE)
  cat("\nDiferencia de RMSE fold a fold (negativo = mejor que la referencia)\n")
  print(resumen[, c("modelo", "vs_baseline", "vs_GAM", "vs_GBM")], row.names = FALSE)
  write.csv2(resumen, "resultados/cv_paso4_resumen.csv", row.names = FALSE)

  # ¿El promedio le gana al GBM solo? Lo revisamos con una prueba
  # t pareada por fold
  dif_prom_gbm <- resultados$dif_vs_GBM[resultados$modelo == "Promedio GAM + RF + GBM"]
  cat(sprintf(paste0("\nPromedio vs GBM: diferencia de RMSE %.2f +/- %.2f MPa ",
                     "(t pareada, p = %.2f). Si p es alto, empatan en exactitud.\n"),
              mean(dif_prom_gbm), sd(dif_prom_gbm), t.test(dif_prom_gbm)$p.value))


  # ---- 4b. Tabla obligatoria del enunciado --------------------
  # Juntamos los modelos del paso 3 (regularizados) y de este
  # paso. Solo lee resultados ya calculados; no ajusta nada.
  p3 <- read.csv2("resultados/cv_paso3_resumen.csv")
  familia <- c("Baseline (media por edad)" = "—",
               "OLS con log(edad)"         = "Lineal",
               "Ridge"                     = "Regularizado",
               "Lasso"                     = "Regularizado",
               "Elastic-net"               = "Regularizado",
               "GAM (splines)"             = "Flexible",
               "Árbol de regresión"        = "Árbol",
               "Random Forest"             = "Ensemble",
               "Boosting (GBM)"            = "Ensemble",
               "Promedio GAM + RF + GBM"   = "Ensemble")
  columnas <- c("modelo", "RMSE", "MAE", "R2", "RMSE_edad_temprana", "vs_baseline")
  juntos <- rbind(resumen[, columnas], p3[p3$modelo %in% c("Ridge", "Lasso", "Elastic-net"), columnas])
  juntos <- juntos[match(names(familia), juntos$modelo), ]

  tabla_obligatoria <- data.frame(
    Modelo        = juntos$modelo,
    Familia       = unname(familia),
    RMSE          = juntos$RMSE,
    MAE           = juntos$MAE,
    R2            = juntos$R2,
    RMSE_subgrupo_edad_temprana = juntos$RMSE_edad_temprana,
    vs_baseline   = ifelse(juntos$modelo == "Baseline (media por edad)",
                           "n/a", juntos$vs_baseline))

  cat("\nTabla obligatoria (validación cruzada por mezcla, MPa; media +/- DE entre folds)\n")
  print(tabla_obligatoria, row.names = FALSE)
  write.csv2(tabla_obligatoria, "resultados/tabla_obligatoria_validacion.csv",
             row.names = FALSE)

  cat("\nHiperparámetros elegidos en cada fold (CV interna)\n")
  print(hiperparametros, row.names = FALSE)


  # ---- 5. Modelos finales en todo el entrenamiento ------------
  # Elegimos los hiperparámetros con los folds del paso 1 y los
  # guardamos para el paso 5.
  cat("\nAjustando modelos finales en todo el entrenamiento...\n")
  cp_final  <- tune_arbol(train, train$fold)
  rf_final_hp  <- tune_rf(train, train$fold)
  gbm_final_hp <- tune_gbm(train, train$fold)

  # Error de CV de cada combinación de la grilla, de menos a más
  # compleja: muestra cuánto empeora el error al regularizar más
  # (evidencia contra el sobreajuste).
  grilla_cv <- rbind(
    data.frame(modelo = "Árbol de regresión",
               configuracion = sprintf("cp = %g", grillas$arbol$cp),
               RMSE_CV = grillas$arbol$RMSE_CV),
    data.frame(modelo = "Random Forest",
               configuracion = sprintf("mtry = %d, min.node.size = %d",
                                       grillas$rf$mtry, grillas$rf$min.node.size),
               RMSE_CV = grillas$rf$RMSE_CV),
    data.frame(modelo = "Boosting (GBM)",
               configuracion = sprintf("profundidad = %d, shrinkage = %.2f, árboles = %d",
                                       grillas$gbm$depth, grillas$gbm$shrinkage,
                                       grillas$gbm$n_trees_mejor),
               RMSE_CV = grillas$gbm$RMSE_CV))
  grilla_cv$elegida <- ave(grilla_cv$RMSE_CV, grilla_cv$modelo,
                           FUN = function(x) x == min(x)) == 1
  grilla_cv$RMSE_CV <- round(grilla_cv$RMSE_CV, 3)
  cat("\nError de CV de cada combinación de hiperparámetros (MPa)\n")
  print(grilla_cv, row.names = FALSE)
  write.csv2(grilla_cv, "resultados/paso4_grilla_cv.csv", row.names = FALSE)

  saveRDS(list(cp = cp_final, rf = rf_final_hp, gbm = gbm_final_hp),
          "resultados/paso4_hiperparametros_finales.rds")

  cat("Árbol: cp =", cp_final, "\n")
  cat("Random Forest: mtry =", rf_final_hp$mtry,
      "| min.node.size =", rf_final_hp$min.node.size, "\n")
  cat("Boosting: profundidad =", gbm_final_hp$depth,
      "| shrinkage =", gbm_final_hp$shrinkage,
      "| n.º árboles =", gbm_final_hp$n_trees, "\n")

  arbol_final <- ajustar_arbol(train, cp_final)
  rf_final    <- ajustar_rf(train, rf_final_hp$mtry, rf_final_hp$min.node.size,
                            importance = "permutation")
  gbm_final   <- ajustar_gbm(train, gbm_final_hp$depth, gbm_final_hp$shrinkage,
                             gbm_final_hp$n_trees)

  # Brecha entre el error de entrenamiento y el de validación. En
  # los modelos de árboles es normal que el de entrenamiento sea
  # mucho menor; lo que decide es el error fuera de muestra, que
  # es el más bajo de todos.
  gam_final <- gam(f_gam, data = train, method = "REML")
  p_train <- list(
    "OLS con log(edad)"       = fitted(lm(f_ols_log, data = train)),
    "GAM (splines)"           = as.numeric(fitted(gam_final)),
    "Árbol de regresión"      = predict(arbol_final, train),
    "Random Forest"           = predict(rf_final, train)$predictions,
    "Boosting (GBM)"          = predict(gbm_final, train, n.trees = gbm_final_hp$n_trees))
  p_train[["Promedio GAM + RF + GBM"]] <- (p_train[["GAM (splines)"]] +
    p_train[["Random Forest"]] + p_train[["Boosting (GBM)"]]) / 3

  brecha <- data.frame(
    modelo     = names(p_train),
    RMSE_train = round(sapply(p_train, function(p) rmse(train$resistencia, p)), 2),
    RMSE_CV    = round(sapply(names(p_train), function(mod)
                   mean(resultados$RMSE[resultados$modelo == mod])), 2))
  brecha$brecha <- brecha$RMSE_CV - brecha$RMSE_train
  cat("\nError en entrenamiento vs validación cruzada (MPa)\n")
  print(brecha, row.names = FALSE)
  write.csv2(brecha, "resultados/paso4_brecha_train_cv.csv", row.names = FALSE)


  # ---- 5b. Gráfico de sobreajuste y subajuste ----------------
  # Sumamos a la tabla anterior el baseline, el OLS sin
  # transformar y los regularizados, para ver todos los modelos
  # juntos.
  # Error de entrenamiento: el modelo ajustado y evaluado con todo
  # el entrenamiento. Error de validación: promedio de los 5 folds
  # por mezcla. El test no se usa.
  library(glmnet)
  f_ols_simple <- resistencia ~ cemento + escoria + ceniza + agua + superplast +
                                arido_grueso + arido_fino + edad
  pred_reg <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                "arido_grueso", "arido_fino", "log_edad",
                "agua_cemento", "agua_aglomerante")
  Xtr <- as.matrix(train[, pred_reg])
  cv2 <- read.csv2("resultados/cv_ols_por_fold.csv")
  cv3 <- read.csv2("resultados/cv_paso3_por_fold.csv")

  # Regularizados: misma regla que en el paso 3 (lambda.1se y,
  # para elastic-net, el alfa con menor error entre 0,25, 0,50 y
  # 0,75).
  set.seed(SEMILLA)
  ajuste_reg <- function(a) cv.glmnet(Xtr, train$resistencia, alpha = a,
                                      foldid = train$fold, standardize = TRUE)
  m_ridge <- ajuste_reg(0)
  m_lasso <- ajuste_reg(1)
  cand_en <- lapply(c(0.25, 0.50, 0.75), ajuste_reg)
  m_en    <- cand_en[[which.min(sapply(cand_en, function(m) min(m$cvm)))]]
  rmse_reg <- function(m) rmse(train$resistencia,
                               as.numeric(predict(m, Xtr, s = "lambda.1se")))

  extra <- data.frame(
    modelo = c("Baseline (media por edad)", "OLS", "Ridge", "Lasso", "Elastic-net"),
    RMSE_train = round(c(
      rmse(train$resistencia, pred_media_edad(train, train)),
      rmse(train$resistencia, fitted(lm(f_ols_simple, data = train))),
      rmse_reg(m_ridge), rmse_reg(m_lasso), rmse_reg(m_en)), 2),
    RMSE_CV = round(c(
      mean(resultados$RMSE[resultados$modelo == "Baseline (media por edad)"]),
      mean(cv2$RMSE[cv2$modelo == "OLS"]),
      mean(cv3$RMSE[cv3$modelo == "Ridge"]),
      mean(cv3$RMSE[cv3$modelo == "Lasso"]),
      mean(cv3$RMSE[cv3$modelo == "Elastic-net"])), 2))
  extra$brecha <- extra$RMSE_CV - extra$RMSE_train

  ajuste <- rbind(extra, brecha)
  orden_aj <- c("Baseline (media por edad)", "OLS", "OLS con log(edad)", "Ridge",
                "Lasso", "Elastic-net", "GAM (splines)", "Árbol de regresión",
                "Random Forest", "Boosting (GBM)", "Promedio GAM + RF + GBM")
  ajuste <- ajuste[match(orden_aj, ajuste$modelo), ]

  # Criterio práctico que usamos para el diagnóstico (en MPa):
  #  - Subajuste: validación alta (> 7) y brecha chica (< 2). El
  #    modelo es tan simple que se equivoca igual con datos vistos
  #    y nuevos.
  #  - Sobreajuste: validación alta (> 7) y brecha grande (> 2).
  #    Aprende el entrenamiento pero no generaliza.
  #  - Buen ajuste: validación baja (<= 7). En RF y GBM la brecha
  #    es grande porque casi memorizan el entrenamiento, pero lo
  #    que decide es el error de validación, que es el menor.
  ajuste$diagnostico <- ifelse(ajuste$RMSE_CV > 7 & ajuste$brecha < 2, "Subajuste",
                        ifelse(ajuste$RMSE_CV > 7 & ajuste$brecha > 2, "Sobreajuste",
                        ifelse(ajuste$RMSE_CV <= 7, "Buen ajuste", "Intermedio")))
  cat("\nSobreajuste y subajuste: entrenamiento vs validación cruzada (MPa)\n")
  print(ajuste, row.names = FALSE)
  write.csv2(ajuste, "resultados/sobreajuste_subajuste.csv", row.names = FALSE)

  mat <- t(as.matrix(ajuste[, c("RMSE_train", "RMSE_CV")]))
  colnames(mat) <- paste0(ajuste$modelo, "\n(", tolower(ajuste$diagnostico), ")")
  mat <- mat[, ncol(mat):1]                    # primer modelo arriba
  png("resultados/sobreajuste_subajuste.png", width = 1800, height = 1500, res = 150)
  par(mar = c(5, 15, 5, 2))
  bp <- barplot(mat, beside = TRUE, horiz = TRUE, las = 1, cex.names = 0.8,
                col = c("grey75", "#00A6A6"), border = NA,
                xlim = c(0, max(mat) * 1.18), xlab = "RMSE (MPa)",
                main = "Error en entrenamiento vs. validación cruzada por mezcla")
  text(mat + 0.15, bp, labels = sprintf("%.2f", mat), adj = 0, cex = 0.7)
  legend("bottomright", fill = c("grey75", "#00A6A6"), border = NA, bty = "n",
         legend = c("Entrenamiento (datos que vio)", "Validación (datos nuevos)"))
  mtext(paste("Subajuste: ambos altos y parecidos  |",
              "Sobreajuste: entrenamiento bajo y validación alta"),
        side = 3, line = 0.4, cex = 0.8)
  dev.off()


  # ---- 6. Gráficos para interpretar ---------------------------
  # Árbol: con tantas hojas se ve cargado, pero sirve para
  # explicar cómo decide un árbol.
  png("resultados/arbol_regresion.png", width = 2400, height = 1400, res = 150)
  rpart.plot(arbol_final, type = 2, extra = 101, roundint = FALSE,
             main = "Árbol de regresión (resistencia en MPa)")
  dev.off()

  # Importancia por permutación (Random Forest): cuánto empeora el
  # error si desordenamos esa variable.
  imp_rf <- sort(rf_final$variable.importance)
  png("resultados/importancia_rf.png", width = 1400, height = 900, res = 150)
  par(mar = c(5, 10, 4, 2))
  barplot(imp_rf, horiz = TRUE, las = 1,
          xlab = "Aumento del error cuadrático al permutar (MPa²)",
          main = "Random Forest: importancia por permutación")
  dev.off()

  # Influencia relativa (boosting)
  inf_gbm <- summary(gbm_final, n.trees = gbm_final_hp$n_trees, plotit = FALSE)
  png("resultados/importancia_gbm.png", width = 1400, height = 900, res = 150)
  par(mar = c(5, 10, 4, 2))
  barplot(rev(setNames(inf_gbm$rel.inf, inf_gbm$var)), horiz = TRUE, las = 1,
          xlab = "Influencia relativa (%)",
          main = "Boosting: influencia relativa de cada variable")
  dev.off()

  cat("\nImportancia por permutación (Random Forest)\n")
  print(round(rev(imp_rf), 1))
  cat("\nInfluencia relativa (Boosting, %)\n")
  print(inf_gbm, row.names = FALSE, digits = 3)

  cat("\nListo. Resultados del paso 4 guardados en 'resultados'.\n")
})
paso_ok("Paso 4", "resultados/cv_paso4_por_fold.csv", t0)

# #############################################################
# PASO 5: sistema final y evaluación ÚNICA en test
# #############################################################
anunciar("PASO 5: sistema final y evaluación ÚNICA en test")
t0 <- Sys.time()
local({
  # Aquí congelamos el sistema elegido y lo evaluamos en el test,
  # que estuvo guardado desde el paso 1.
  # Regla de oro: el test se abre UNA sola vez, con el sistema ya
  # elegido. Si cambiáramos algo después de verlo, la evaluación
  # dejaría de ser honesta.
  # También calculamos intervalos de predicción, el error por
  # subgrupo de edad, los gráficos de residuos y los PDP/ICE.


  # ---- 0. Configuración ---------------------------------------
  # Sistema elegido con la validación cruzada del paso 4.
  # Opciones: "promedio" (GAM + RF + GBM), "gbm", "rf", "gam".
  # Por qué elegimos el promedio (ver
  # resultados/cv_paso4_resumen.csv):
  #  - Empata con el GBM en exactitud: 5,46 vs 5,48 MPa, y la
  #    diferencia fold a fold es de -0,03 +/- 0,26 MPa, o sea,
  #    ninguno es mejor.
  #  - Es más estable: varía menos entre folds (DE 0,97 vs 1,07;
  #    frente al GAM, +/- 0,41 vs +/- 0,67) y tiene el menor error
  #    en edad temprana (4,67 vs 4,78 MPa).
  #  - Preferimos la estabilidad porque el sistema se usará con
  #    recetas nuevas. El costo es interpretar tres modelos en vez
  #    de uno; para eso usamos PDP/ICE (sección 10).
  SISTEMA <- "promedio"

  SEMILLA     <- 2026
  NIVEL_PI    <- 0.90     # nivel de los intervalos de predicción
  B_BOOTSTRAP <- 2000     # remuestreos para el IC del error en test
  set.seed(SEMILLA)

  # Paquetes (ya instalados en los pasos anteriores)
  library(mgcv)
  library(ranger)
  library(gbm)

  train <- readRDS("resultados/train_con_folds.rds")
  hp    <- readRDS("resultados/paso4_hiperparametros_finales.rds")
  K_FOLDS <- max(train$fold)

  nombres_cv <- c(promedio = "Promedio GAM + RF + GBM", gbm = "Boosting (GBM)",
                  rf = "Random Forest", gam = "GAM (splines)")
  stopifnot(SISTEMA %in% names(nombres_cv))
  cat("Sistema final elegido:", nombres_cv[[SISTEMA]], "\n")


  # ---- 1. Preprocesamiento congelado --------------------------
  # Las mismas transformaciones fijas de los pasos anteriores.
  # También las usamos en el PDP, para recalcular las razones al
  # cambiar una variable.
  recalcular <- function(d) {
    d$aglomerante      <- d$cemento + d$escoria + d$ceniza
    d$agua_cemento     <- d$agua / d$cemento
    d$agua_aglomerante <- d$agua / d$aglomerante
    d$log_edad         <- log(d$edad)
    d
  }
  train <- recalcular(train)

  vars_arbol <- c("cemento", "escoria", "ceniza", "agua", "superplast",
                  "arido_grueso", "arido_fino", "edad",
                  "agua_cemento", "agua_aglomerante")
  f_arbol <- as.formula(paste("resistencia ~", paste(vars_arbol, collapse = " + ")))
  f_gam <- resistencia ~ s(cemento) + s(escoria) + s(ceniza) + s(agua) +
                         s(superplast) + s(arido_grueso) + s(arido_fino) +
                         s(log_edad)


  # ---- 2. Funciones del sistema -------------------------------
  ajustar_sistema <- function(d) {
    set.seed(SEMILLA)
    list(
      gam = gam(f_gam, data = d, method = "REML"),
      rf  = ranger(f_arbol, data = d, num.trees = 500, mtry = hp$rf$mtry,
                   min.node.size = hp$rf$min.node.size,
                   seed = SEMILLA, num.threads = 1),
      gbm = gbm(f_arbol, data = d[, c("resistencia", vars_arbol)],
                distribution = "gaussian", n.trees = hp$gbm$n_trees,
                interaction.depth = hp$gbm$depth, shrinkage = hp$gbm$shrinkage,
                n.minobsinnode = 10, bag.fraction = 0.8, verbose = FALSE)
    )
  }

  predecir_sistema <- function(m, nd) {
    p_gam <- as.numeric(predict(m$gam, newdata = nd))
    p_rf  <- predict(m$rf, nd)$predictions
    p_gbm <- predict(m$gbm, nd, n.trees = hp$gbm$n_trees)
    switch(SISTEMA,
           promedio = (p_gam + p_rf + p_gbm) / 3,
           gbm = p_gbm, rf = p_rf, gam = p_gam)
  }

  pred_media_edad <- function(ent, nd) {
    medias <- tapply(ent$resistencia, ent$grupo_edad, mean)
    p <- unname(medias[as.character(nd$grupo_edad)])
    p[is.na(p)] <- mean(ent$resistencia)
    p
  }

  metricas <- function(real, pred) {
    c(RMSE = sqrt(mean((real - pred)^2)),
      MAE  = mean(abs(real - pred)),
      R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2))
  }


  # ---- 3. Residuos fuera de muestra (para los intervalos) -----
  # Con los folds del paso 1, predecimos cada fila de
  # entrenamiento con un sistema que no la vio. Esos errores nos
  # dan el ancho de los intervalos de predicción (método conformal
  # simple).
  cat("Calculando residuos fuera de muestra en entrenamiento...\n")
  pred_oof <- numeric(nrow(train))
  for (k in 1:K_FOLDS) {
    m_k <- ajustar_sistema(train[train$fold != k, ])
    pred_oof[train$fold == k] <- predecir_sistema(m_k, train[train$fold == k, ])
  }
  res_oof <- train$resistencia - pred_oof
  q_pi <- unname(quantile(abs(res_oof), NIVEL_PI))
  cat(sprintf("Intervalo de predicción al %d%%: predicción +/- %.2f MPa\n",
              round(100 * NIVEL_PI), q_pi))


  # ---- 4. Congelar el sistema final ---------------------------
  sistema_final <- ajustar_sistema(train)
  saveRDS(list(sistema = SISTEMA, hp = hp, q_pi = q_pi, modelos = sistema_final),
          "resultados/sistema_final_congelado.rds")


  # ---- 5. APERTURA ÚNICA DEL TEST -----------------------------
  # Dejamos un registro de cada apertura al final de un archivo,
  # así la primera fecha nunca se pierde.
  marca <- "resultados/test_abierto.txt"
  if (file.exists(marca)) {
    warning("El test ya se había abierto antes (primera apertura: ",
            readLines(marca)[1], "). Reporten el PRIMER resultado, ",
            "no uno nuevo.")
  }
  cat(format(Sys.time()), "| Sistema:", SISTEMA, "\n", file = marca,
      append = TRUE)

  test <- recalcular(readRDS("resultados/test_congelado.rds"))
  stopifnot(length(intersect(train$mezcla, test$mezcla)) == 0)   # sin fuga

  pred_test <- predecir_sistema(sistema_final, test)
  pred_base <- pred_media_edad(train, test)

  m_sis  <- metricas(test$resistencia, pred_test)
  m_base <- metricas(test$resistencia, pred_base)


  # ---- 6. Incertidumbre del error en test (bootstrap por mezcla)
  # Remuestreamos MEZCLAS y no filas, por la misma razón que la
  # partición: las filas de una misma mezcla no son
  # independientes.
  mezclas_test <- unique(test$mezcla)
  boot <- t(replicate(B_BOOTSTRAP, {
    ms  <- sample(mezclas_test, replace = TRUE)
    idx <- unlist(lapply(ms, function(x) which(test$mezcla == x)))
    r_s <- sqrt(mean((test$resistencia[idx] - pred_test[idx])^2))
    r_b <- sqrt(mean((test$resistencia[idx] - pred_base[idx])^2))
    c(RMSE_sistema = r_s, RMSE_baseline = r_b, mejora = r_s - r_b)
  }))
  ic <- apply(boot, 2, quantile, probs = c(0.025, 0.975))

  # Error de validación cruzada del paso 4, para compararlo con el
  # del test
  cv <- read.csv2("resultados/cv_paso4_resumen.csv")
  cv_sis  <- cv$RMSE[cv$modelo == nombres_cv[[SISTEMA]]]
  cv_base <- cv$RMSE[cv$modelo == "Baseline (media por edad)"]

  tabla_test <- data.frame(
    modelo   = c(nombres_cv[[SISTEMA]], "Baseline (media por edad)"),
    RMSE_CV  = c(cv_sis, cv_base),
    RMSE_test = round(c(m_sis[["RMSE"]], m_base[["RMSE"]]), 2),
    IC95_RMSE_test = c(sprintf("%.2f a %.2f", ic[1, "RMSE_sistema"], ic[2, "RMSE_sistema"]),
                       sprintf("%.2f a %.2f", ic[1, "RMSE_baseline"], ic[2, "RMSE_baseline"])),
    MAE_test = round(c(m_sis[["MAE"]], m_base[["MAE"]]), 2),
    R2_test  = round(c(m_sis[["R2"]], m_base[["R2"]]), 3),
    # Mejora en RMSE frente al baseline (negativo = mejor), con IC 95%
    mejora_vs_baseline = c(sprintf("%.2f (IC 95%%: %.2f a %.2f)",
                                   m_sis[["RMSE"]] - m_base[["RMSE"]],
                                   ic[1, "mejora"], ic[2, "mejora"]), "n/a"))

  cat("\n===== RESULTADO FINAL EN TEST (", nrow(test), "filas,",
      length(mezclas_test), "mezclas nunca vistas ) =====\n")
  print(tabla_test, row.names = FALSE)
  cat(sprintf("\nMejora frente al baseline: %.2f MPa (IC 95%%: %.2f a %.2f)\n",
              m_sis[["RMSE"]] - m_base[["RMSE"]], ic[1, "mejora"], ic[2, "mejora"]))
  write.csv2(tabla_test, "resultados/test_resultado_final.csv", row.names = FALSE)


  # ---- 7. Intervalos de predicción: ¿cumplen su cobertura? ----
  test$pred      <- pred_test
  test$pi_inf    <- pred_test - q_pi
  test$pi_sup    <- pred_test + q_pi
  test$cubierto  <- test$resistencia >= test$pi_inf & test$resistencia <= test$pi_sup
  test$residuo   <- test$resistencia - pred_test
  cat(sprintf("\nCobertura del intervalo al %d%% en test: %.1f%% de las probetas\n",
              round(100 * NIVEL_PI), 100 * mean(test$cubierto)))


  # ---- 8. Error por subgrupo de edad (con denominadores) ------
  por_grupo <- do.call(rbind, lapply(levels(test$grupo_edad), function(g) {
    tg <- test[test$grupo_edad == g, ]
    if (nrow(tg) == 0) return(NULL)
    data.frame(grupo_edad = g, n_probetas = nrow(tg),
               n_mezclas = length(unique(tg$mezcla)),
               RMSE_sistema  = round(sqrt(mean(tg$residuo^2)), 2),
               RMSE_baseline = round(sqrt(mean((tg$resistencia -
                                 pred_base[test$grupo_edad == g])^2)), 2),
               sesgo_medio = round(mean(tg$residuo), 2),   # negativo = sobreestima
               cobertura_PI = round(100 * mean(tg$cubierto), 1))
  }))
  cat("\nError por grupo de edad en test (sesgo negativo = el modelo sobreestima)\n")
  print(por_grupo, row.names = FALSE)
  write.csv2(por_grupo, "resultados/test_error_por_edad.csv", row.names = FALSE)

  cat(sprintf("\nSobreestimaciones de más de 5 MPa: %d de %d probetas (%.1f%%)\n",
              sum(test$residuo < -5), nrow(test), 100 * mean(test$residuo < -5)))

  write.csv2(test[, c("mezcla", "edad", "resistencia", "pred", "pi_inf",
                      "pi_sup", "cubierto", "residuo")],
             "resultados/test_predicciones.csv", row.names = FALSE)


  # ---- 9. Gráficos del test -----------------------------------
  png("resultados/test_real_vs_predicho.png", width = 1200, height = 1000, res = 150)
  plot(test$pred, test$resistencia, pch = 16, col = rgb(0, 0, 0, 0.4),
       xlab = "Resistencia predicha (MPa)", ylab = "Resistencia real (MPa)",
       main = "Test: real vs. predicho (banda = intervalo de predicción)")
  abline(0, 1, col = "red", lwd = 2)
  abline(q_pi, 1, col = "red", lty = 2)
  abline(-q_pi, 1, col = "red", lty = 2)
  dev.off()

  png("resultados/test_residuos.png", width = 1600, height = 700, res = 150)
  par(mfrow = c(1, 2))
  plot(test$pred, test$residuo, pch = 16, col = rgb(0, 0, 0, 0.4),
       xlab = "Resistencia predicha (MPa)", ylab = "Residuo (MPa)",
       main = "Residuos vs. predicción")
  abline(h = 0, col = "red", lwd = 2)
  plot(test$edad, test$residuo, log = "x", pch = 16, col = rgb(0, 0, 0, 0.4),
       xlab = "Edad (días, escala log)", ylab = "Residuo (MPa)",
       main = "Residuos según la edad")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()


  # ---- 10. PDP e ICE (con datos de entrenamiento) -------------
  # PDP: efecto promedio de cambiar una variable dejando el resto
  # igual. ICE: la misma curva, pero para cada probeta.
  # Al cambiar una variable base recalculamos las razones
  # derivadas, para no inventar mezclas imposibles.
  pdp_ice <- function(var, n_grid = 30, n_ice = 50) {
    grid <- seq(quantile(train[[var]], 0.02), quantile(train[[var]], 0.98),
                length.out = n_grid)
    muestra <- train[sample(nrow(train), n_ice), ]
    ice <- sapply(grid, function(g) {
      d <- muestra
      d[[var]] <- g
      predecir_sistema(sistema_final, recalcular(d))
    })
    list(grid = grid, ice = ice, pdp = colMeans(ice))
  }

  etiquetas <- c(edad = "Edad (días)", cemento = "Cemento (kg/m3)",
                 agua = "Agua (kg/m3)")
  png("resultados/pdp_ice.png", width = 2100, height = 750, res = 150)
  par(mfrow = c(1, 3))
  for (v in names(etiquetas)) {
    r <- pdp_ice(v)
    matplot(r$grid, t(r$ice), type = "l", lty = 1, col = rgb(0, 0, 0, 0.15),
            xlab = etiquetas[[v]], ylab = "Resistencia predicha (MPa)",
            main = paste("PDP (rojo) e ICE (gris):", v),
            log = if (v == "edad") "x" else "")
    lines(r$grid, r$pdp, col = "red", lwd = 3)
  }
  dev.off()


  # ---- 11. Registro ------------------------------------------
  writeLines(capture.output(sessionInfo()), "resultados/sessionInfo_paso5.txt")
  cat("\nListo. Resultados finales guardados en 'resultados'.\n")
})
paso_ok("Paso 5", "resultados/test_resultado_final.csv", t0)


# #############################################################
# D. Verificación de reproducibilidad
# #############################################################
test_nuevo <- read.csv2("resultados/test_resultado_final.csv")
cat("\n\n=============== RESUMEN FINAL ===============\n")
print(test_nuevo, row.names = FALSE)

# Comparamos las columnas numéricas que tienen ambas versiones
# (menos el número de mezcla, que puede cambiar según el idioma
# del sistema).
iguales <- function(a, b) {
  cols <- intersect(names(a)[sapply(a, is.numeric)], names(b))
  cols <- setdiff(cols, "mezcla")
  nrow(a) == nrow(b) && length(cols) > 0 &&
    all(sapply(cols, function(cl) isTRUE(all.equal(a[[cl]], b[[cl]]))))
}

con_previo <- !sapply(previos, is.null)
if (any(con_previo)) {
  control <- data.frame(
    archivo = basename(archivos_control[con_previo]),
    resultado = ifelse(sapply(archivos_control[con_previo], function(f)
      iguales(previos[[f]], read.csv2(f))), "IGUAL", "DISTINTO"))
  cat("\nComparación con la ejecución anterior:\n")
  print(control, row.names = FALSE)

  if (all(control$resultado == "IGUAL")) {
    cat("\nREPRODUCIBILIDAD OK: todos los resultados coinciden",
        "con la ejecución anterior.\n")
  } else {
    warning("Hay resultados que NO coinciden con la ejecución anterior. ",
            "Revisen versiones de paquetes (resultados/sessionInfo*.txt).")
  }
}

cat(sprintf("\nTiempo total: %.1f minutos\n",
            as.numeric(difftime(Sys.time(), inicio_total, units = "mins"))))
cat("Todos los resultados y gráficos están en la carpeta 'resultados'.\n")
