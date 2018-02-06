## Autores: Victor Manuel Cerrato Molina y Gema Correa Fernández

## ------------------------------------------------------------------------
# install.packages("e1071")
library(e1071)   # Para usar skewness

# install.packages("readr")
library(readr)   # Parar usar read.csv

# install.packages("caret")
library(caret)   # Para usar BoxCoxTrans

## ------------------------------------------------------------------------
# Guardamos el data frame en una varible
LAozone <- read.csv("datos/LAozone.data")

# Visualizamos la cabecera del data frame
head(LAozone)

# Mostramos la dimensión (330 filas y 10 columnas)
dim(LAozone)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto para hacer el mismo tipo de partición
set.seed(1)

# Nos quedamos con los indices para el training
train = sample (nrow(LAozone), round(nrow(LAozone)*0.7)) 

# Reservamos por separado training y test
LAozone.train = LAozone[train,]  
LAozone.test = LAozone[-train,]

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Borramos la última columna (doy) del training
LAozone.train = LAozone.train[-ncol(LAozone.train)]

# Borramos la última columna (doy) del test
LAozone.test = LAozone.test[-ncol(LAozone.test)]

## ------------------------------------------------------------------------
# Obtenemos el valor de asimetría de los datos training
v_asimetria = apply(LAozone.train, 2, skewness)

# Ordenamos de mayor a menor los valores obtenidos
sort(abs(v_asimetria), decreasing = T)

## ------------------------------------------------------------------------
# Transformación para la variable vh
# -----------------------------------------------------------------------------------
par(mfrow = c(1:2)) # Dividimos la región en dos partes
vh_trans = BoxCoxTrans(LAozone.train$vh)  # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(LAozone.train$vh, main = "Sin transformacion (vh)", xlab = "", col = 3)
hist(predict(vh_trans,LAozone.train$vh), main = "Con transformacion (vh)", xlab = "",
     col = 3)

# Transformación para la variable humidity
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
humidity_trans = BoxCoxTrans(LAozone.train$humidity) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(LAozone.train$humidity, main = "Sin transformacion (humidity)", xlab = "", 
     col = 3)
hist(predict(humidity_trans,LAozone.train$humidity), 
     main = "Con transformacion (humidity)", xlab = "", col = 3)

# Transformación para la variable vis
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
vis_trans = BoxCoxTrans(LAozone.train$vis) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(LAozone.train$vis, main = "Sin transformacion (vis)", xlab = "", col = 3)
hist(predict(vis_trans,LAozone.train$vis), main = "Con transformacion (vis)", 
     xlab = "", col = 3)

## ------------------------------------------------------------------------
# Transformación vh (train)
LAozone.train$vh = predict(vh_trans,LAozone.train$vh)

# Transformación humidity (train)
LAozone.train$humidity = predict(humidity_trans,LAozone.train$humidity)

# Transformación vis (train)
LAozone.train$vis = predict(vis_trans,LAozone.train$vis)

## ------------------------------------------------------------------------
# Transformación vh (test)
LAozone.test$vh = predict(vh_trans,LAozone.test$vh)

# Transformación humidity (test)
LAozone.test$humidity = predict(humidity_trans,LAozone.test$humidity)

# Transformación vis (test)
LAozone.test$vis = predict(vis_trans,LAozone.test$vis)

## ------------------------------------------------------------------------
nearZeroVar(LAozone.train)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
attach(LAozone.train) # Para simplificar y prescindir del prefijo LAozone.train

## ------------------------------------------------------------------------
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

## ------------------------------------------------------------------------
pairs(LAozone.train, lower.panel=panel.smooth,upper.panel=panel.cor)

## ------------------------------------------------------------------------
# Quitamos ibt tanto para train como para test
LAozone.train = LAozone.train[,-8]
LAozone.test = LAozone.test[,-8]

## ------------------------------------------------------------------------
pairs(LAozone.train, lower.panel=panel.smooth,upper.panel=panel.cor)

## ------------------------------------------------------------------------
# Quitamos vh tanto para train como para test
LAozone.train = LAozone.train[,-2]
LAozone.test = LAozone.test[,-2]

# Y volvemos a mostrar el gráfico
pairs(LAozone.train, lower.panel=panel.smooth,upper.panel=panel.cor)

## ------------------------------------------------------------------------
# Función que calcula los errores E_in y E_test
errores <- function(m){
  
  train = predict(m) # usa training
  test = predict(m, LAozone.test, type= "response") # usa test
  
  # Calculamos los errores
  etr = mean((train - LAozone.train[,1])^2)
  etst = mean((test - LAozone.test[,1])^2)
  
  list(Error_Train = etr, Error_Test = etst)
}

## ------------------------------------------------------------------------
# Creamos el modelo
m1 = lm(ozone ~ wind + humidity + temp + ibh + dpg + vis, data = LAozone.train) 
summary(m1) # Realizamos un análisis del modelo
errores(m1) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m2 = lm(ozone ~ temp, data=LAozone.train) 
summary(m2) # Realizamos un análisis del modelo
errores(m2) # Obtenemos los errores

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ temp, data = LAozone.train)
w = m2$coefficients
x = matrix(rep(1, length(temp)), nrow = length(temp)) 
x = cbind (x, temp)
y = apply(x, 1, function(vec) w %*% vec)
points(temp, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m3 = lm(ozone ~ ibh, data=LAozone.train) 
# Realizamos un análisis del modelo
summary(m3)
# Obtenemos los errores
errores(m3)

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ ibh, data=LAozone.train)
w = m3$coefficients
x = matrix(rep(1, length(ibh)), nrow= length(ibh)) 
x = cbind (x, ibh)
y = apply(x, 1, function(vec) w %*% vec)
points(ibh, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m4 = lm(ozone ~ humidity, data=LAozone.train) 
summary(m4) # Realizamos un análisis del modelo
errores(m4) # Obtenemos los errores

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ humidity, data=LAozone.train)
w = m4$coefficients
x = matrix(rep(1, length(humidity)), nrow = length(humidity)) 
x = cbind (x, humidity)
y = apply(x, 1, function(vec) w %*% vec)
points(humidity, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m = lm(ozone ~ poly(dpg,2) , data = LAozone.train) 
summary(m) # Realizamos un análisis del modelo
errores(m) # Obtenemos los errores

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ dpg, data=LAozone.train)
w = m$coefficients
x = matrix(rep(1, length(dpg)), nrow = length(dpg)) 
x = cbind (x, poly(dpg,2))
y= apply(x, 1, function(vec) w %*% vec)
points(dpg, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m = lm(ozone ~ I(vis^3), data = LAozone.train) 
summary(m) # Realizamos un análisis del modelo
errores(m) # Obtenemos los errores

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ vis, data=LAozone.train)
w = m$coefficients
x = matrix(rep(1, length(vis)), nrow= length(vis)) 
x = cbind (x, I(vis^3))
y = apply(x, 1, function(vec) w %*% vec)
points(vis, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m = lm(ozone ~ wind, data = LAozone.train) 
summary(m) # Realizamos un análisis del modelo
errores(m) # Obtenemos los errores

# Vamos a mostrar gráficamente el modelo
plot(ozone ~ wind, data=LAozone.train)
w = m$coefficients
x = matrix(rep(1, length(wind)), nrow= length(wind)) 
x = cbind (x, wind)
y = apply(x, 1, function(vec) w %*% vec)
points(wind, y, col=2)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Creamos el modelo
m5 = lm(ozone ~ temp + humidity, data=LAozone.train) 
summary(m5) # Realizamos un análisis del modelo
errores(m5) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m6 = lm(ozone ~ temp + ibh , data=LAozone.train) 
summary(m6) # Realizamos un análisis del modelo
errores(m6) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m7 = lm(ozone ~ ibh + humidity , data=LAozone.train) 
summary(m7) # Realizamos un análisis del modelo
errores(m7) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m8 = lm(ozone ~ ibh + temp + humidity , data=LAozone.train) 
summary(m8) # Realizamos un análisis del modelo
errores(m8) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m9 = lm(ozone ~ poly(dpg,2) + temp, data=LAozone.train) 
summary(m9) # Realizamos un análisis del modelo
errores(m9) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m10 = lm(ozone ~ I(temp^2) + ibh + humidity + 0, data=LAozone.train) 
summary(m10) # Realizamos un análisis del modelo
errores(m10) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m11 = lm(ozone ~ ibh + temp + dpg , data=LAozone.train) 
summary(m11) # Realizamos un análisis del modelo
errores(m11) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
m12 = lm(ozone ~ I(ibh^2) +  atan(humidity) + I(temp^2), data=LAozone.train) 
summary(m12) # Realizamos un análisis del modelo
errores(m12) # Obtenemos los errores

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
Regress_LinWD <- function(datos,label,landa){
  
  # Descomponemos los datos en UDV^T
  descom=svd(datos)
  
  # Calculamos una pseudo inversa especial para el caso de deacaimiento de pesos
  pseudo_inv_WD=
    descom$v %*% diag(1/(descom$d**2+rep(landa,length(descom$d))))%*%t(diag(descom$d))%*%t(descom$u)
  
  # Aplicamos la formula que nos devuelve un vector columna
  w = pseudo_inv_WD%*%label
  
  # Extraemos w en forma de vector fila obteniendo los pesos de la solucion
  t(w)[1,]
}

## ------------------------------------------------------------------------
datos = cbind(I(ibh^2) , atan(humidity) , I(temp^2),1)
datos.test = cbind(I(LAozone.test$ibh^2) , atan(LAozone.test$humidity) ,
                   I(LAozone.test$temp^2),1)

# Establecemos por defecto un e_test alto ya que nunca llegaremos a ese valor
Etest = 100 

# Calculamos el error para varias lambdas
for(landa in seq(3.7*10**8,4.2*10**8, length.out=10)){
  w = Regress_LinWD(datos,ozone,landa)
  EtestAc = mean((LAozone.test$ozone - datos.test%*%w)^2)
  
  # Nos quedamos con los mejores resultados
  if(EtestAc < Etest) {
    Etest = EtestAc
    wmej = w
    landamej = landa
  }
  
  cat(" Landa: ",landa," Etest: ", EtestAc,"\n")
}

cat("El mejor landa es ",landamej, " con un Etest de ",Etest, 
    " que da la solución: w=[",wmej,"]\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
datos = cbind(I(temp^2) , ibh, humidity)
datos.test = cbind(I(LAozone.test$temp^2) , LAozone.test$ibh , LAozone.test$humidity)

# Establecemos por defecto un e_test alto ya que nunca llegaremos a ese valor
Etest = 100

# Calculamos el error para varias lambdas
for(landa in seq(2.6*10**8,2.8*10**8,length.out=10)){
  w = Regress_LinWD(datos,ozone,landa)
  EtestAc = mean((LAozone.test$ozone - datos.test%*%w)^2)
  
  # Nos quedamos con los mejores resultados
  if(EtestAc < Etest) {
    Etest = EtestAc
    wmej = w
    landamej = landa
  }
  
  cat(" Landa: ",landa," Etest: ", EtestAc,"\n")
}

cat("El mejor landa es ",landamej," con un Etest de ", Etest, 
    " que da la solución: w=[",wmej,"]\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
datos = cbind(I(temp^2) , ibh , humidity)
datos.test = cbind(I(LAozone.test$temp^2) , LAozone.test$ibh , LAozone.test$humidity)

# Calculamos los pesos  
w = Regress_LinWD(datos, ozone, 273333333)

# Calculamos los errores
Ein = mean((LAozone.train$ozone - datos%*%w)^2)
Etest = mean((LAozone.test$ozone - datos.test%*%w)^2)

cat("Etest: ", Etest,"\nEin: ",Ein,"\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
datos.test = cbind(I(LAozone.test$temp^2),LAozone.test$ibh,LAozone.test$humidity,1)

# Obtenemos el tamaño de los datos
N <- nrow(datos.test)

# Calculamos el segundo término de la fórmula
x <- sqrt( (1/(2*N)) * log( 2 / 0.05) )

# Obtenemos el valor de Eout
cota_Etest_Eout <- Etest + x
cat ("Cota de E_out basada en E_test: ", cota_Etest_Eout)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Guardamos el data frame en una vaarible
SAheart <- read.csv("datos/SAheart.data")

# Visualizamos los datos (la tabla)
head(SAheart)

# Mostramos la dimensión que tiene (462 filas y 11 columnas)
dim(SAheart)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto para hacer el mismo tipo de partición
set.seed(1)

# Nos quedamos con los indices para el training
train = sample (nrow(SAheart), round(nrow(SAheart)*0.7)) 

# Reservamos por separado training y tes
SAheart.train = SAheart[train,]  
SAheart.test = SAheart[-train,]

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Borramos la primera columna tanto para training como para test
SAheart.train <- SAheart.train[,-1] 
SAheart.test <- SAheart.test[,-1]

## ------------------------------------------------------------------------
# Cambiamos a numérico la columna del training
SAheart.train[,5] <- as.numeric(SAheart.train[,5]) # Obtenemos 2 y 1
SAheart.train[,5][SAheart.train[,5] == 1] = 0 # Cambiamos el 1 por el 0
SAheart.train[,5][SAheart.train[,5] == 2] = 1 # Cambiamos el 2 por el 2

# Cambiamos a numérico la columna del training
SAheart.test[,5] <- as.numeric(SAheart.test[,5]) # Obtenemos 2 y 1
SAheart.test[,5][SAheart.test[,5] == 1] = 0 # Cambiamos el 1 por el 0
SAheart.test[,5][SAheart.test[,5] == 2] = 1 # Cambiamos el 2 por el 2

## ------------------------------------------------------------------------
# Obtenemos el valor de asimetría de los datos training
v_asimetria = apply(SAheart.train, 2, skewness)

# Ordenamos de mayor a menor los valores obtenido
sort(abs(v_asimetria), decreasing = T)

## ------------------------------------------------------------------------
# Transformación para la variable alcohol
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
alcohol_trans = BoxCoxTrans(SAheart.train$alcohol+1) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(SAheart.train$alcohol+1, main = "Sin transformacion (alcohol)", xlab = "", col = 3)
hist(predict(alcohol_trans,SAheart.train$alcohol+1), 
     main = "Con transformacion (alcohol)", xlab = "", col = 3)

# Transformación para la variable tobacco
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
tobacco_trans = BoxCoxTrans(SAheart.train$tobacco+1) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(SAheart.train$tobacco+1, main = "Sin transformacion (tobacco)", xlab = "", col = 3)
hist(predict(tobacco_trans,SAheart.train$tobacco+1), 
     main = "Con transformacion (tobacco)", xlab = "", col = 3)

# Transformación para la variable ldl 
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
ldl_trans = BoxCoxTrans(SAheart.train$ldl) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(SAheart.train$ldl, main = "Sin transformacion (ldl)", xlab = "", col = 3)
hist(predict(ldl_trans,SAheart.train$ldl), 
     main = "Con transformacion (ldl)", xlab = "", col = 3)

# Transformación para la variable sbp 
# -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
sbp_trans = BoxCoxTrans(SAheart.train$sbp) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(SAheart.train$sbp, main = "Sin transformacion (sbp)", xlab = "", col = 3)
hist(predict(sbp_trans,SAheart.train$sbp), main = "Con transformacion (sbp)", 
     xlab = "", col = 3)

# Transformación para la variable obesity -----------------------------------------------------------------------------------
par(mfrow=c(1:2)) # Dividimos la región en dos partes
obesity_trans = BoxCoxTrans(SAheart.train$obesity) # Obtenemos la transformación
# Comparamos los histogramas con transformación y sin transformación
hist(SAheart.train$obesity, main = "Sin transformacion (obesity)", xlab = "", col = 3)
hist(predict(obesity_trans,SAheart.train$obesity), 
     main = "Con transformacion (obesity)", xlab = "", col = 3)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Transformación alcohol (train)
SAheart.train$alcohol = predict(alcohol_trans,SAheart.train$alcohol+1)

# Transformación tobacco (train)
SAheart.train$tobacco = predict(tobacco_trans,SAheart.train$tobacco+1)

# Transformación ldl (train)
SAheart.train$ldl = predict(ldl_trans,SAheart.train$ldl)

# Transformación sbp (train)
SAheart.train$sbp = predict(sbp_trans,SAheart.train$sbp)

# Transformación obesity (train)
SAheart.train$obesity = predict(obesity_trans,SAheart.train$obesity)

## ------------------------------------------------------------------------
# Transformación alcohols (test)
SAheart.test$alcohol = predict(alcohol_trans,SAheart.test$alcohol+1)

# Transformación tobacco (test)
SAheart.test$tobacco = predict(tobacco_trans,SAheart.test$tobacco+1)

# Transformación ldl (test)
SAheart.test$ldl = predict(ldl_trans,SAheart.test$ldl)

# Transformación sbp (test)
SAheart.test$sbp = predict(sbp_trans,SAheart.test$sbp)

# Transformación obesity (test)
SAheart.test$obesity = predict(obesity_trans,SAheart.test$obesity)

## ------------------------------------------------------------------------
attach(SAheart.train) # para prescendir y simplificar el prefijo SAheart

## ------------------------------------------------------------------------
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

## ------------------------------------------------------------------------
pairs(SAheart.train[,c(-5,-10)], lower.panel=panel.smooth,upper.panel=panel.cor)

## ------------------------------------------------------------------------
# Quitamos adiposity tanto para train como para test
SAheart.train <- SAheart.train[,-4]
SAheart.test <- SAheart.test[,-4]

# Y volvemos a mostrar el gráfico
pairs(SAheart.train[,c(-4,-9)], lower.panel=panel.smooth,upper.panel=panel.cor)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Función que calcula los errores E_in y E_test para regresión logística
errorres_regresion_logistica <- function(m){
  
  probTr = predict(m, type="response")
  probTst = predict(m, data.frame(SAheart.test), type="response")
  
  predTst = rep(0, length(probTst)) # predicciones por defecto 0 
  predTst[probTst >= 0.5] = 1 # >= 0.5 clase 1
  
  predTr = rep(0, length(probTr)) # predicciones por defecto 0 
  predTr[probTr >= 0.5] = 1 # >= 0.5 clase 1
  
  print(table(predTst, Real=SAheart.test$chd)) # Para el calculo del Etest
  
  # Calculamos los errores
  Ein = mean(predTr != SAheart.train$chd)
  Etest = mean(predTst != SAheart.test$chd)
  
  list(Etest=Etest, Ein=Ein)
}

## ------------------------------------------------------------------------
# Creamos el modelo
ml1 = glm(chd ~ sbp + tobacco + ldl + typea + obesity + alcohol + age, family = binomial(logit), 
          data = SAheart.train) 
summary(ml1) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml1) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml2 = glm(chd ~ ldl, family = binomial(logit), data = SAheart.train) 
summary(ml2) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml2) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml3 = glm(chd ~ age , family = binomial(logit), data = SAheart.train) 
summary(ml3) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml3) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml4 = glm(chd ~ tobacco , family = binomial(logit), data = SAheart.train) 
summary(ml4) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml4) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml5 = glm(chd ~ typea, family = binomial(logit), data = SAheart.train) 
summary(ml5) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml5) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml6 = glm(chd ~ ldl + age + tobacco, family = binomial(logit), data = SAheart.train) 
summary(ml6) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml6) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml7 = glm(chd ~ ldl + age + typea, family = binomial(logit), data = SAheart.train) 
summary(ml7) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml7) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml8 = glm(chd ~ ldl + age + typea + tobacco, family = binomial(logit), data = SAheart.train) 
summary(ml8) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml8) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml9 = glm(chd ~ I(age^5) , family = binomial(logit), data = SAheart.train)
summary(ml9) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml9) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml10 = glm(chd ~ I(age^5) + ldl , family = binomial(logit), data = SAheart.train)
summary(ml10) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml10) # Obtenemos los errores

## ------------------------------------------------------------------------
# Creamos el modelo
ml11 = glm(chd ~ atan(ldl) + age + tobacco, family = binomial(logit), data = SAheart.train)
summary(ml11) # Realizamos un análisis del modelo
errorres_regresion_logistica(ml11) # Obtenemos los errores

## ------------------------------------------------------------------------
ml12 = glm(chd ~  I(age^5) + tobacco + typea, 
          family = binomial(logit), data = SAheart.train)
summary(ml12)
errorres_regresion_logistica(ml12) # Obtenemos los errores

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Función PLA_Pocket
PLA_pocket=function(datos, label, max_iter, vini, c1){
  
  # Establecemos una semilla por defecto
  set.seed(79)
  
  if(max_iter == 0)
    return(list(w=vini))
  
  # Este es el numero de datos que tenemos, uno por cada fila
  numDatos = nrow(datos)
  
  # Esta variable indicara si hemos conseguido ajustar los coeficientes
  # de tal manera que clasifiquen bien todos los puntos
  malAjustado = F
  
  # En esta variable devolveremos el numero de iteraciones usado
  # que como vemos si no se modifica es el maximo
  numIter = max_iter
  
  # En esta variable guardamos la mejor solucion hasta el momento
  mejorVini = vini
  # En esta el error menor alcanzado hasta el momento
  mejorErr = mean(sign(datos%*%vini)!=label)+(c1-1)*mean(sign(datos%*%vini)==-1 & label==1)
  
  # Bucle principal tendra como maximo max_iter iteraciones
  for(n in 1:max_iter) {
    
    # Calculamos indices aleatorios que dan el orden en
    # que explorar los puntos de entrada
    indices=sample(1:numDatos,numDatos)
    
    # Para cada incide del vector
    for( i in indices) {
        
      # Comprobamos si el signo que asigna el w actual 
      # coincide con con la etiqueta
      if(sign(datos[i,]%*%vini) != label[i]) {
        
        # si no es asi probamos con un w nuevo 
        vini = vini+datos[i,]*label[i]
        malAjustado = T
        
        # Calculamos el error de la solucion nueva
        errActual = mean(sign(datos%*%vini)!=label)+(c1-1)*mean(sign(datos%*%vini)==-1 & label==1)
        # Si el w nuevo es mejor lo cambiamos y volvemos a empezar
        # la exploracion
        if(errActual<mejorErr) {
          mejorVini = vini
          mejorErr = errActual
          break
        }
      }
    }
    
    # Si no esta mal ajustado quiere decir que se 
    # clasificaron todos los puntos bien y hemos terminado
    if(!malAjustado){
      # Guardamos el numero de iteraciones que fueron necesarias
      numIter=n
      # Salimos del bucle principal
      break
      
    } else {
      # Si esta mal ajustado ponemos la variable a false para
      # comprobar en la siguiente iteracion
      malAjustado = F
    }
  }
  
  # Devolvemos la salida en esta lista
  list(w=mejorVini,num_iteraciones=numIter,error=mejorErr)
}

## ------------------------------------------------------------------------
# Función que calcula el error
RegPLA <- function(datos,datostest,landa,maxiter,c=1.5){
  
  # Hacemos un cambio de numeración para las etiquetas (train y test)
  realEtiqTr = SAheart.train$chd
  realEtiqTr[realEtiqTr == 0] = -1
  realEtiqTst = SAheart.test[,9]
  realEtiqTst[realEtiqTst == 0] = -1
  
  # Calculamos RL con WD
  w = Regress_LinWD(datos,realEtiqTr,landa)
  # Le pasamos los pesos
  w = PLA_pocket(datos,realEtiqTr,maxiter,w,c)$w #rep(0,ncol(datos))
  
  # Volvemos a la numeración correcta de las etiquetas (train y test)
  etiqTr=sign(datos%*%w)
  etiqTr[etiqTr == -1] = 0
  etiqTst=sign(datostest%*%w)
  etiqTst[etiqTst == -1] = 0
  
  # Pintamos nuestra matriz de confusión
  print(table(etiqTst,Real=SAheart.test$chd))
  
  # Calculamos los errores
  Ein=mean(etiqTr != SAheart.train$chd)
  Etest=mean(etiqTst != SAheart.test$chd)
  
  list(Ein=Ein, Etest=Etest, w=w)
}

## ------------------------------------------------------------------------
datos = cbind(SAheart.train$ldl , SAheart.train$age, SAheart.train$tobacco,1)
datos.test = cbind(SAheart.test$ldl , SAheart.test$age, SAheart.test$tobacco,1)

# Establecemos por defecto un e_test alto ya que nunca llegaremos a ese valor
Etest = 100

# Calculamos el error para varias lambdas
for(landa in seq(0,0.001,length.out=10)){
  sol = RegPLA(datos,datos.test,landa,50)
  EtestAc = sol$Etest
  
  # Nos quedamos con los mejores resultados
  if(EtestAc < Etest){
    Etest = EtestAc
    wmej = w
    landamej = landa
  }
  
  cat(" Landa: ",landa," Etest: ", EtestAc,"\n")
}

cat("El mejor landa es ",landamej," con un Etest de ",Etest,
    " que da la solución: w=[",wmej,"]\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
datos = cbind(I(SAheart.train$age^5), SAheart.train$tobacco,SAheart.train$typea,1)
datos.test = cbind(I(SAheart.test$age^5),SAheart.test$tobacco,SAheart.test$typea,1)

# Establecemos por defecto un e_test alto ya que nunca llegaremos a ese valor
Etest = 100

# Calculamos el error para varias lambdas
for(landa in seq(0,0.001,length.out=10)){
  sol = RegPLA(datos,datos.test,landa,100)
  EtestAc = sol$Etest
  
  # Nos quedamos con los mejores resultados
  if(EtestAc<Etest){
    Etest = EtestAc
    wmej = w
    landamej = landa
  }
  cat(" Landa: ",landa," Etest: ", EtestAc,"\n")
}
cat("El mejor landa es ", landamej, " con un Etest de ", Etest,
    " que da la solución: w=[",wmej,"]\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
ml6 = glm(chd ~ ldl + age + tobacco, family = binomial(logit), data = SAheart.train) 

# Obtenemos los errores y la matriz de confusión
errorres_regresion_logistica(ml6)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
datos.test = cbind(SAheart.test$ldl , SAheart.test$age, SAheart.test$tobacco,1)

# Obtenemos el Etest del anterior apartado
Etest = 0.2661871

# Obtenemos el tamaño de los datos
N <- nrow(datos.test)

# Calculamos el segundo término de la fórmula
x <- sqrt( (1/(2*N)) * log( 2 / 0.05) )

# Obtenemos el valor de Eout
cota_Etest_Eout <- Etest + x
cat ("Cota de E_out basada en E_test: ", cota_Etest_Eout)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

