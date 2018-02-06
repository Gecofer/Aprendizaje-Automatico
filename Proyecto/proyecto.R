# Gema Correa Fernández y Ana Puertas Olea

## ------------------------------------------------------------------------
library("caret", quietly=TRUE) # Para usar preProcess
library("ggplot2", quietly=TRUE) # Para usar plot
library("e1071", quietly=TRUE) # Para usar SVM (Máquina de Soporte de Vectores)
library("ada", quietly=TRUE) # Para usar AdaBoost
library("nnet", quietly=TRUE) # Para usar nnet (Red Neuronal)
library("chemometrics", quietly=TRUE) # Parar usar nnetEval (NN by cross-validation)
library("ROCR", quietly=TRUE) # Para hacer la Curva ROC
library("randomForest", quietly=TRUE) # Para usar Random Forest

## ------------------------------------------------------------------------
# Guardamos el data frame en una varible
BWcancer <- read.csv("datos/wdbc.data", header = F, sep = ",")

# Visualizamos la cabecera del data frame
head(BWcancer)

# Mostramos la dimensión (569 instancias y 32 características)
dim(BWcancer)

## ------------------------------------------------------------------------
# Eliminamos la primera columna
BWcancer <- BWcancer[,-1]

# Modificamos el nombre de los atributo
names(BWcancer) = c("class", 1:30)

# Visualizamos la cabecera del data frame
head(BWcancer)

## ------------------------------------------------------------------------
# Cambiamos a numérico la variable diágnostico
BWcancer[,1] <- as.numeric(BWcancer[,1]) # Obtenemos un 2 en Menigno y un 1 para Benigno
BWcancer[,1][BWcancer[,1] == 1] = 0 # Cambiamos el 1 por el 0 (Benigno) --> No padece cáncer
BWcancer[,1][BWcancer[,1] == 2] = 1 # Cambiamos el 2 por el 1 (Maligno) --> Padece cáncer

# Por último, transformamos en factores nuestra variable class
BWcancer$class = as.factor(BWcancer$class)

## ------------------------------------------------------------------------
# Contamos desde la segunda columna hasta el final, ya que la primera es nuestra 'class'
for(i in 2:ncol(BWcancer)) {
  # Obtenemos el mínimo
  if (min(BWcancer[i]) == 0)
    cat(i-1, "\t")
}

## ------------------------------------------------------------------------
# Sumamos uno en las siguientes atributos
BWcancer$"7" = (BWcancer$"7" + 1)
BWcancer$"8" = (BWcancer$"8" + 1)
BWcancer$"17" = (BWcancer$"17" + 1)
BWcancer$"18" = (BWcancer$"18" + 1)
BWcancer$"27" = (BWcancer$"27" + 1)
BWcancer$"28" = (BWcancer$"28" + 1)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto para hacer el mismo tipo de partición
set.seed(127)

# Nos quedamos con los indices para el training
train = sample (nrow(BWcancer), round(nrow(BWcancer)*0.8))

# Reservamos por separado training y test
BWcancer.train = BWcancer[train,]  
BWcancer.test = BWcancer[-train,]

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto
set.seed(127)

# Transformamos los datos
resultado_preprocesamiento = preProcess(BWcancer.train, 
                                        method = c("BoxCox", "center", "scale", "pca"), 
                                        thres = 0.9)

# Aplicar el preprocesamiento a los datos del training y del test 
BW_cancer_train_preprocesado = predict(resultado_preprocesamiento, BWcancer.train)
BW_cancer_test_preprocesado = predict(resultado_preprocesamiento, BWcancer.test)

## ------------------------------------------------------------------------
attach(BWcancer.train) # Para prescendir y simplificar el prefijo BWcancer.train

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Función que calcula los errores y E_test para regresión logística
errorres_regresion_logistica <- function(m){
  
  probTr = predict(m, type="response")
  probTst = predict(m, data.frame(BW_cancer_test_preprocesado), type="response")
  
  predTst = rep(0, length(probTst)) # predicciones por defecto 0 
  predTst[probTst >= 0.5] = 1 # >= 0.5 clase 1
  
  predTr = rep(0, length(probTr)) # predicciones por defecto 0 
  predTr[probTr >= 0.5] = 1 # >= 0.5 clase 1
  
  # Para el calculo del Etest
  print(table(pred=predTst, real=BW_cancer_test_preprocesado$class)) 
  
  # Calculamos Etest
  Etest = mean(predTst != BW_cancer_test_preprocesado$class)
  
  list(Etest=Etest)
}

## ------------------------------------------------------------------------
# Creamos el modelo
ml = glm(class ~ ., family = binomial(logit), data = BW_cancer_train_preprocesado) 

# Realizamos un análisis del modelo
summary(ml) 

# Obtenemos el error
errorres_regresion_logistica(ml) 

## ------------------------------------------------------------------------
# Obtenemos las probabilidades
prob_GLM = predict(ml, data.frame(BW_cancer_test_preprocesado), type=c("response"))  

## ------------------------------------------------------------------------
# Función que dibuja uan curva ROC
plotROC <- function(modelo, etiq_real, adicionar=FALSE,color="red") {
  pred <- prediction(modelo, etiq_real)    
  perf <- performance(pred,"tpr","fpr")
  plot(perf, col=color, add=adicionar, main="Curva ROC", lwd = 2)
  segments(0, 0, 1, 1, col='black')
  grid()
}

# Cruva ROC para el modelo lineal
plotROC(prob_GLM, BW_cancer_test_preprocesado$class)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla
set.seed(127)

# Usamos tune.svm para encontrar el mejor coste y gamma para svm
x = subset(BW_cancer_train_preprocesado, select=-class)
svm_tune = tune.svm(x = x, y = class, kernel = "radial")
# print(svm_tune)

# Obtenemos el mejor modelo
svm_tune$best.model

## ------------------------------------------------------------------------
# Establecemos una semilla
set.seed(127)

# Guardamos el mejor gamma y cost
mejor_gamma = svm_tune$best.model$gamma
mejor_coste = svm_tune$best.model$cost

# Obtenemos el modelo
modelo_svm = svm(class ~., data = BW_cancer_train_preprocesado, kernel = "radial", 
                 cost = mejor_coste, gamma = mejor_gamma, decision.values = TRUE, 
                 probability = TRUE)

# Obtenemos la predición tanto para train como para test (Nos devuelve las etiquetas)
prediccion_train = predict(modelo_svm, data.frame(BW_cancer_train_preprocesado), 
                           decision.values = TRUE, probability = TRUE)
prediccion_test = predict(modelo_svm, data.frame(BW_cancer_test_preprocesado), 
                          decision.values = TRUE, probability = TRUE)

# Obtenemos nuestra matriz de confusión
table(pred=prediccion_test, real=BW_cancer_test_preprocesado$class)

# Obtenemos la probabilidad entre las clases positivas y negativas
test_prob = attributes(predict(modelo_svm, data.frame(BW_cancer_test_preprocesado), 
                               decision.values = T))$decision.values

# Como la probabilidad no se encuentra entre [0,1], vamos a normalizarla
# donde el mínimo seaa 0 y el máximo seaa 1 
test_prob_normalizado = (test_prob - min(test_prob)) / ( max(test_prob) - min(test_prob) )
# summary(test_prob_normalizado)

# Ponemos que las clases que tengan el mismo peso 
pred_test_SVM = rep(0, length(test_prob_normalizado)) # Predicciones por defecto 0
pred_test_SVM[test_prob_normalizado >= 0.5] = 1 # Clase 1 -> cáncer maligno

# Pintamos la curva ROC para SVM
plotROC(pred_test_SVM, BW_cancer_test_preprocesado$class)

## ------------------------------------------------------------------------
# Calculamos el error Etest
Etest_SVM = 0
Etest_SVM = mean(pred_test_SVM != BW_cancer_test_preprocesado$class)
cat("E_test para SVM", Etest_SVM)

## ------------------------------------------------------------------------
print(table(pred=pred_test_SVM, real=BW_cancer_test_preprocesado$class)) 

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto
set.seed(127)

# Obtenemos el modelo
modelo_rf = randomForest(class ~., data = BW_cancer_train_preprocesado, ntree=15)

# Realizamos las predicciones
prediccion_train = predict(modelo_rf, BW_cancer_train_preprocesado,type="prob")
prediccion_test = predict(modelo_rf, BW_cancer_test_preprocesado, type="prob")
RF <- prediccion_test[,2]

# Poner la probabilidad entre 0 y 1, ponemos que las clases que tengan el mismo peso 
pred_test_RF = rep(0, length(RF)) # Predicciones por defecto 0
pred_test_RF[RF >= 0.5] = 1 # Clase 1 -> cáncer maligno

# Matriz de confusión
print(table(pred=pred_test_RF, real=BW_cancer_test_preprocesado$class)) 

# Calculamos el error 
Etest_RF = 0
Etest_RF = mean(pred_test_RF != BW_cancer_test_preprocesado$class)
cat("Etest para Random Forest", Etest_RF)

# Pintamos la curva ROC
plotROC(pred_test_RF,BW_cancer_test_preprocesado$class)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto
set.seed(127)

library(rpart) # Librería necesaria para usar la función stump

# Según la referencia anterior, es stump (2-split)
stump = rpart.control(cp = -1 , maxdepth = 1 , minsplit = 0) 

# Obtenemos el modelo
# type = "discrete" realiza Boosting.
adaboost_stump <- ada(class~., data = BW_cancer_train_preprocesado, iter = 50, 
                      loss = "e", type = "discrete", control = stump)

# Obtenemos las predicciones 
prediccion_train = predict(adaboost_stump, BW_cancer_train_preprocesado)
prediccion_test = predict(adaboost_stump, data.frame(BW_cancer_test_preprocesado) )

# Matriz de confusión
print(table(pred=prediccion_test, real=BW_cancer_test_preprocesado$class)) 

# Calculamos el error Etest
Etest_ada = 0
Etest_ada = mean(prediccion_test != BW_cancer_test_preprocesado$class)
cat("Etest para AdaBoost", Etest_ada)

## ------------------------------------------------------------------------
# Obtenemos la probabilidad entre las clases positivas y negativas
test_prob_adab = predict(adaboost_stump, data.frame(BW_cancer_test_preprocesado), 
                         type = "probs")
# Nos quedamos con una columna de test_prob_adab, da igual si es la 1 o la 2,
# ya que son la inversa respectivamente

# Dibujamos la curva ROC
plotROC(test_prob_adab[,2], BW_cancer_test_preprocesado$class)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto
set.seed(127)

# Transformamos los datos
resultado_preprocesamiento = preProcess(BWcancer, method = c("BoxCox", "center", 
                                                             "scale", "pca"), thres = 0.9)

# Aplicar el preprocesamiento a los datos  
BWcancer_preprocesado = predict(resultado_preprocesamiento, BWcancer)

# Nos quedamos con los indices para el training
train = sample (nrow(BWcancer), round(nrow(BWcancer)*0.8))

# Obtenemos nuestro modelo
# size -> number of hidden units
modelo_nn <- nnetEval(BWcancer_preprocesado, BWcancer_preprocesado$class, train, 
                      kfold = 10, size = 16, maxit=100)

# Obtenemos el Eout
mean(modelo_nn$testerr)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto
set.seed(127)

# Obtenemos el modelo
nn <- nnet(class ~., BW_cancer_train_preprocesado,size = 16, maxit=100)

# test_prob_nn --> ya nos devuelve la probabilidad (nos quedamos con la primera columna)
test_prob_nn <- predict(nn,BW_cancer_test_preprocesado)

# Mostramos la Curva ROC
plotROC(test_prob_nn[,1], BW_cancer_test_preprocesado$class)

## ------------------------------------------------------------------------
# Para que nos muestre las gráficas juntas, abrimos plot antes
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))
axis(1)
axis(2)
title(main = "Curvas ROC")
title(xlab= "False Positive rate")
title(ylab= "True Positive rate")

# Boosting
plotROC(test_prob_adab[,2], BW_cancer_test_preprocesado$class, color="yellow", 
        adicionar = TRUE)
# Redes Neuroanles
plotROC(test_prob_nn[,1],BW_cancer_test_preprocesado$class, color = "magenta", 
        adicionar = TRUE)
# RandomForest
plotROC(pred_test_RF,BW_cancer_test_preprocesado$class, color = "green", 
        adicionar = TRUE)
# SVM
plotROC(pred_test_SVM,BW_cancer_test_preprocesado$class, color = "blue", 
        adicionar = TRUE)
# GLM
plotROC(prob_GLM,BW_cancer_test_preprocesado$class, color = "red", 
        adicionar = TRUE)


# Agregamos una leyenda
legend(0.68, 0.5, c("AdaBoost", "Red Neuronal",  "Random Forest", "SVM", "GLM"), 
       cex = 0.8, col = c("yellow", "magenta", "green", "blue",  "red"), 
       lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Función que calcula el área bajo la curva
areaROC <- function(prediccion, etiq_real) {
  pred <- prediction(prediccion, etiq_real)
  auc <- performance(pred,"auc")
  attributes(auc)$y.values[[1]]
}

# Guardamos los valores obtenidos
Area <- data.frame(
  modelos = c("AdaBoost","Red Neuronal","Random Forest", "SVM ","GLM"), 
  valor =  c(areaROC(test_prob_adab[,2], BW_cancer_test_preprocesado$class),                                                          areaROC(test_prob_nn[,1],BW_cancer_test_preprocesado$class),
             areaROC(pred_test_RF,BW_cancer_test_preprocesado$class), 
             areaROC(pred_test_SVM,BW_cancer_test_preprocesado$class),
             areaROC(prob_GLM,BW_cancer_test_preprocesado$class) ) )
Area

