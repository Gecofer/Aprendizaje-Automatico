## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
set.seed(3) # Inicializamos la semilla a un número por defecto

## ------------------------------------------------------------------------
# Creamos una función para la funcion no lineal E(u,v)
E <- function(u, v){
  ((u^2)*(exp(v)) - 2*(v^2)*exp(-u))^2
}

# Creamos una función para su gradiente (derivadas parciales)
E_dev <- function(u, v){
    Eu <- 2 * ( (u^2)*exp(v) - 2*(v^2)*exp(-u) ) * ( 2*exp(v)*u + 2*exp(-u)*(v^2) )
    Ev <- 2 * ( (u^2)*exp(v) - 2*(v^2)*exp(-u) ) * ( (u^2)*exp(v) - 4*exp(-u)*v )
    c(Eu, Ev)
}

## ------------------------------------------------------------------------
# Algoritmo Gradiente Descendente
GD <- function(funcion, funcion_gradiente, eta=0.1, vini=c(0,0), umbral=10^(-4), 
               iteraciones_max=500){
  
  # Inicializar los pesos
  w_old <- c(0,0)
  w_new <- vini
  
  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Array creado donde guardaremos el valor de la funcion, para luego crear la gráfica
  f <- rep(iteraciones_max)
  f[iter] <- funcion(w_new[1],w_new[2])
  
  # Mostramos el punto de partida
  cat("Punto de partida: ", w_new[1], w_new[2])

  # Mostramos los valores del gradiente al principio
  cat("\nGradiente (inicial): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos el valor de la función al principio
  cat("\nValor función (inicial): ", funcion(w_new[1], w_new[2]))

  # La condición de parada será cuando el valor de la funcion sea menor o igual a 
  # un umbral, cuando se hayan completado un número máximo de iteraciones o cuando 
  # la diferencia entre dos puntos en valor absoluto sea muy cercana.
  # La condición de la diferencia entre dos puntos en valor absoluto sea muy cercana, 
  # descarta muchos valores, ya que avanza muy lentamente
  while ( (iter <= iteraciones_max) & (funcion(w_new[1], w_new[2]) > umbral) 
         & (abs(funcion(w_new[1], w_new[2]) - funcion(w_old[1], w_old[2])) > umbral) ) {
    
    # Asignamos el valor nuevo, al antiguo
    w_old <- w_new
  
    # Calculamos el gradiente
    gradiente <- funcion_gradiente(w_new[1], w_new[2]) 
    
    # Establecemos la dirección a moverse
    direccion <- -gradiente
    
    # Actualizamos los pesos
    w_new <- w_old + eta * direccion 

    # Incrementamos el valor de la iteración (época)
    iter <- iter + 1
    
    # Guardamos el nuevo valor de f
    f[iter] = funcion(w_new[1],w_new[2])
  }
  
  # Mostramos los valores del gradiente al terminar
  cat("\nGradiente (final): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos la solución final
  cat("\nSolución: ", w_new[1], w_new[2])
  
  # Mostramos el valor de la función al final
  cat("\nValor función (final): ", funcion(w_new[1], w_new[2]))
  
  # Devolvemos el número de iteraciones necesarias para encontrar la solución
  cat("\nIteraciones: ",iter-1)
  cat("\n")
  
  # Mostramos el valor de la funcion
  list(valor_funcion = f[1:iter])
}

## ------------------------------------------------------------------------
GD (E, E_dev, eta=0.1, vini=c(1,1), umbral=10^(-4), iteraciones_ma=500)

## ------------------------------------------------------------------------
# Guardamos la expresion
expresion = expression((u^2*exp(v) - 2*v^2*exp(-u))^2)

# Vamos hacer la derivada a esa función, dependiendo de los valores 'u' y 'v'
duv = deriv(expresion,c("u","v"))

# Establecemos el punto donde realizar la derivada
u = 1
v = 1

# Evaluamos la derivada
eval(duv)
# Evaluamos la expresion 
eval(expresion)

## ------------------------------------------------------------------------
# Guardamos el valor de la función
resultado01 = GD(E, E_dev, eta=0.1, vini=c(1,1), umbral=10^(-4), iteraciones_ma=500)
# La mostramos gráficamente
plot(resultado01$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "E(u,v)", main = "Gradiente Descendiente E(u,v)")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
GD(E, E_dev, eta=0.1, vini=c(1,1), umbral=10^(-4), iteraciones_ma=500)

## ------------------------------------------------------------------------
# Creamos una función para la funcion f(x,y)
f <- function(x, y){
  (x-2)^2 + 2*(y-2)^2 + 2*sin(2*pi*x)*sin(2*pi*y)
}

# Creamos una función para su gradiente (derivadas parciales)
f_dev <- function(x, y){
    fx <- 4*pi*cos(2*pi*x)*sin(2*pi*y) + 2*(x-2)
    fy <- 4*pi*sin(2*pi*x)*cos(2*pi*y) + 4*(y-2)
    c(fx, fy)
}

## ------------------------------------------------------------------------
# Guardamos el valor de la función con la tasa de aprendizaje de 0.1 
resultado02 = GD(f, f_dev, vini = c(1,1), eta = 0.1, umbral = 10^(-4),  
                 iteraciones_max = 50) 
resultado02

## ------------------------------------------------------------------------
# Guardamos el valor de la función con la tasa de aprendizaje de 0.01 
resultado03 = GD(f, f_dev, vini = c(1,1), eta = 0.01, umbral = 10^(-4),  
                 iteraciones_max = 50) 
resultado03

## ------------------------------------------------------------------------
# Guardamos la expresion
expresion = expression((x-2)^2 + 2*(y-2)^2 + 2*sin(2*pi*x)*sin(2*pi*y))

# Vamos hacer la derivada a esa función, dependiendo de los valores 'u' y 'v'
dxy = deriv(expresion,c("x","y"))

# Establecemos el punto donde realizar la derivada
x = 1
y = 1

# Evaluamos la derivada
eval(dxy)
# Evaluamos la expresion 
eval(expresion)

## ------------------------------------------------------------------------
# Abrimos plot
plot(c(0,10), c(0,5.3), type = "n", xlab = "Iteracion", ylab = "f(x,y)", 
     main = "GD con tasa de 0.1 y 0.01")

# Mostramos gráficamente la recta para la tasa  0.1
lines(resultado02$valor_funcion, type = "o", pch = 16, col = 2)

# Mostramos gráficamente la recta para la tasa  0.01
lines(resultado03$valor_funcion, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(8, 5, c("eta = 0,01", "eta = 0.1"), cex = 0.8, col = c("blue", "red"), 
       pch=16, lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))

# Punto de inicio (2.1, 2.1) con tasa de aprendizaje de 0.1:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.1 
resultado04 = GD(f, f_dev, vini = c(2.1,2.1), eta = 0.1, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado04$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (2.1, 2.1) con tasa 0.1", xlim = c(1,3.25), 
     ylim = c(-1.5, 1.5))

# Punto de inicio (2.1, 2.1) con tasa de aprendizaje de 0.01:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.01 
resultado05 = GD(f, f_dev, vini = c(2.1,2.1), eta = 0.01, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado05$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (2.1, 2.1) con tasa 0.01", xlim = c(1,3.25), 
     ylim = c(-1.5, 1.5))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))

# Punto de inicio (3, 3) con tasa de aprendizaje de 0.1:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.1 
resultado06 = GD(f, f_dev, vini = c(3,3), eta = 0.1, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado06$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (3, 3) con tasa 0.1", xlim = c(0,10), 
     ylim = c(0,5))

# Punto de inicio (3, 3) con tasa de aprendizaje de 0.01:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.01 
resultado07 = GD(f, f_dev, vini = c(3,3), eta = 0.01, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado07$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (3, 3) con tasa 0.01", xlim = c(0,10), 
     ylim = c(0,5))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))

# Punto de inicio (1.5, 1.5) con tasa de aprendizaje de 0.1:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.1 
resultado08 = GD(f, f_dev, vini = c(1.5,1.5), eta = 0.1, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado08$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1.5, 1.5) con tasa 0.1", xlim = c(0,5), 
     ylim = c(-2,6.5))

# Punto de inicio (1.5, 1.5) con tasa de aprendizaje de 0.01:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.01 
resultado09 = GD(f, f_dev, vini = c(1.5,1.5), eta = 0.01, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado09$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1.5, 1.5) con tasa 0.01", xlim = c(0,5), 
     ylim = c(-2,6.5))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))

# Punto de inicio (1, 1) con tasa de aprendizaje de 0.1:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.1 
resultado09 = GD(f, f_dev, vini = c(1,1), eta = 0.1, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado09$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1, 1) con tasa 0.1", xlim = c(0,10), 
     ylim = c(0,6))

# Punto de inicio (1, 1) con tasa de aprendizaje de 0.01:
# -------------------------------------------------------------------------------
# Nos creamos una función con la tasa de aprendizaje de 0.01 
resultado10 = GD(f, f_dev, vini = c(1,1), eta = 0.01, umbral = 10^(-4), 
                 iteraciones_max = 50) 
# Lo mostramos gráficamente
plot(resultado10$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1, 1) con tasa 0.01", xlim = c(0,10), 
     ylim = c(0,6))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Generación de nube de puntos de manera uniforme
simula_unif = function (N = 2, dim = 2, rango = c(0,1)){
 m = matrix(runif(N*dim, min=rango[1], max=rango[2]),nrow = N, ncol=dim, byrow=T)
 m
}

# Generaración de una recta
simula_recta = function (intervalo = c(-1,1), visible=F){
  
  # Se generan 2 puntos
  ptos = simula_unif(2,2,intervalo) 
  
  # calculamos la pendiente
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1])
  
  # Calculamos el punto de corte
  b = ptos[1,2]-a*ptos[1,1] 

  # Se pinta la recta y los 2 puntos
  if (visible) { 
    
    # Si no esta abierto el dispositivo lo abre con plot
    if (dev.cur()==1) 
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    
    # Pinta en verde los puntos
    points(ptos,col=3)  
    
    # y la recta
    abline(b,a,col=3)
  }
  
  # Devuelve el par pendiente y punto de corte
  c(a,b) 
}

## ------------------------------------------------------------------------
# Función que calcula la norma
norma <- function(w_old, w_new){
  sqrt(sum((w_old - w_new)^2))
}

# Algoritmo de Regresión Logística
RL <- function(datos, etiquetas, vini = c(0,0,0), eta = 0.01, iteraciones_max = 500){ 
  
  # Inicializar los pesos
  w_new <- vini
  w_old <- c(0,0,0)

  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Añadimos una columna para poder hacer el producto vectorial
  datos <- cbind(1, datos)

  # Variable booleana que nos servirá para salir o continuar en el bucle 
  continuar <- FALSE
 
  # La condición de parada será cuando se hayan completado un número máximo de 
  # iteraciones o cuando la norma de la diferencia entre dos puntos sea menor 
  # que un determinado valor
  while (iter <= iteraciones_max | !continuar){
    
    # Asignamos el valor nuevo, al antiguo
    w_old <- w_new
    
    # Hacemos una permutación aleatoria de los datos
    permutacion <- sample(1:length(etiquetas)) 
    
    # Recorremos la permutación
    for(i in permutacion){
      
      # Calculamos el gradiente (basándonos en la fórmula explicada anteriormente)
      gradiente <- (-etiquetas[i]*datos[i,]) / 
        (1 + exp(etiquetas[i] * w_new %*%datos[i,]))
      
      # Actualizamos los pesos
      w_new <- w_new - eta * gradiente
    }
    
    # Incrementamos el valor de la iteración, cuando hacemos una pasada a las muestras
    iter <- iter + 1
    
    # Si la norma de la diferencia entre dos puntos es menor que un determinado valor
    # salirnos del bucle (condición de parada)
    # Esta condición, no la meto dentro del while, ya que al principio ambos valores 
    # son 0 por lo que nunca entraría en el bucle
    if(norma(w_old, w_new) < eta ){ 
      continuar <- TRUE
    }
  }
      
  cat("Pesos: ", w_new, "\nValores de la Recta: ", -w_new[1]/w_new[3], -w_new[2]/w_new[3], 
       "\nIteraciones: ", iter-1)
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos aleatoriamente una lista de números aleatorios
datos01 = simula_unif(N=2, dim=2, rango=c(0,2))

# Generamos una recta 
recta01 = simula_recta(intervalo=c(0,2))

# Aplicamos la función a cada punto y almacenamos el signo del resultado en una lista
etiquetas01 = sign(datos01[,2] - recta01[1]*datos01[,1] - recta01[2])

# Obtenemos los valores de la Regresión Logística
RL(datos01, etiquetas01, c(0,0,0), 0.01, 500)

## ------------------------------------------------------------------------
# Mostramos gráficamente los puntos
plot (datos01, col = etiquetas01+3, xlab = "X", ylab = "Y",  xlim = c(0,2), 
      ylim = c(0,2), main = "Regresión Logística para 2 puntos")
# Dibujamos la recta
abline(0.4329298, 0.9136762, lwd = 1.5)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos aleatoriamente una lista de números aleatorios
datos02 = simula_unif(N=100, dim=2, rango=c(0,2))

# Generamos una recta 
recta02 = simula_recta(intervalo=c(0,2))

# Aplicamos la función a cada punto y almacenamos el signo del resultado en una lista
etiquetas02 = sign(datos02[,2] - recta02[1]*datos02[,1] - recta02[2])

# Obtenemos los valores de la Regresión Logística
RL(datos02, etiquetas02, c(0,0,0), 0.01, 500)

## ------------------------------------------------------------------------
# Lo mostramos gráficamente
plot (datos02, col = etiquetas02+3, xlab = "X", ylab = "Y",  xlim = c(0,2), 
      ylim = c(0,2), main = "Regresión Logística para 100 puntos")
# Dibujamos la recta
abline(0.2971057, 1.047737, lwd = 1.5)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Algoritmo de Regresión Logística (1)
RL1 <- function(datos, etiquetas, vini = c(0,0,0), eta = 0.01, iteraciones_max = 500){ 
  
  # Inicializar los pesos
  w_new <- vini
  w_old <- c(0,0,0)

  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Añadimos a una columna para poder hacer el producto vectorial
  datos <- cbind(1, datos)

  # Variable booleana que nos servirá para salir o continuar en el bucle 
  continuar <- FALSE
 
  # La condición de parada cuando se hayan completado un número máximo de iteraciones 
  # o cuando la norma de la diferencia entre dos puntos sea menor que un determinado
  # valor
  while (iter <= iteraciones_max | !continuar){
    
    # Asignamos el valor nuevo, al antiguo
    w_old <- w_new
    
    # Hacemos una permutación aleatoria de los datos
    permutacion <- sample(1:length(etiquetas)) 
    
    # Recorremos la permutación
    for(i in permutacion){
      
      # Calculamos el gradiente (basándonos en la fórmula explicada anteriormente)
      gradiente <- (-etiquetas[i]*datos[i,]) / (1 + exp(etiquetas[i] * w_new %*%datos[i,]))
      
      # Actualizamos los pesos
      w_new <- w_new - eta * gradiente
    }
    
    # Incrementamos el valor de la iteración, cuando hacemos una pasada a las muestras
    iter <- iter + 1
    
    # Si la norma de la diferencia entre dos puntos es menor que un determinado valor
    # salirnos del bucle (condición de parada)
    # Esta condición, no la meto dentro del while, ya que al principio ambos valores son 0
    # por lo que nunca entraría en el bucle
    if(norma(w_old, w_new) < eta ){ 
      continuar <- TRUE
    }
  }
      
  c(w_new, iter-1)
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos las etiquetas aleatoriamente
datos03 <- simula_unif(N = 1000, dim = 2, rango = c(0,2))

# Generamos una recta
recta03 <- simula_recta(intervalo = c(0,2))

# Etiquetamos los puntos a partir de la función de la recta
etiquetas03 <- sign(datos03[,2] - recta03[1]*datos03[,1] - recta03[2])

# Calculamos la recta de regresión que separa ambas clases
recta_regresion01 <- RL1(datos03, etiquetas03)
recta_regresion02 = c(-recta_regresion01[1]/recta_regresion01[3], 
                      - recta_regresion01[2]/recta_regresion01[3])

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos03[,2] - recta_regresion02[1]*datos03[,1] 
                              - recta_regresion02[2])

# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
Ein01 <- 0
Ein01 = sum(etiquetas03 != etiquetas_regresion01) / length(etiquetas03)

Ein01

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos los datos aleatoriamente
datos03.test <- simula_unif(N = 1000, dim = 2, rango = c(0,2))

# Generamos una recta 
recta03.test <- simula_recta(intervalo = c(0,2))

# Etiquetamos las etiquetas a partir de la función de la recta
etiquetas03.test <- sign(datos03.test[,2] - recta03.test[1]*datos03.test[,1] 
                         - recta03.test[2])

# Usamos la recta de regresión creada en los datos de entrenamiento
etiquetas_regresion01.test <- sign(datos03.test[,2] 
                                   - recta_regresion02[2]*datos03.test[,1] 
                                   - recta_regresion02[2])

# Calculamos Eout como las etiquetas que son distintas, partido el total de las etiquetas
Eout01 <- 0
Eout01 = sum(etiquetas03.test != etiquetas_regresion01.test) / length(etiquetas03.test)

Eout01

## ------------------------------------------------------------------------
# Leemos el fichero con los datos de entrenamiento 
digit.train <- read.table("datos/zip.train", quote="\"", comment.char="", 
                          stringsAsFactors=FALSE)

# Guardamos los 4 y los 8
digitos48.train = digit.train[digit.train$V1==4 | digit.train$V1==8,]

# Etiquetas
digitos.train = digitos48.train[,1]  
ndigitos.train = nrow(digitos48.train)
  
# Se retira la clase y se monta una matriz de tamanio 16x16
grises = array(unlist(subset(digitos48.train,select=-V1)),c(ndigitos.train,16,16))
grises.train = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

# Visualizamos las imágenes 
par(mfrow=c(2,2)) 

for(i in 1:4){
  imagen = grises[i,,16:1] # Se rotan para verlas bien
  image(z=imagen)
}
  
# Etiquetas correspondientes a las 4 imágenes
digitos.train[1:4] 

# Guardamos las etiquetas
etiquetas48.train <- digitos48.train$V1
# Convertimos las etiquetas de 4 a -1 y de 8 a 1
# [(4-6) / 2 = -1] y [(8-6) / 2 = 1]
etiquetas48.train <- (etiquetas48.train-6)/2

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# INTENSIDAD ---------------------------------------------------------------
# Creamos un array con la media de la imagen
# Para calcular la intensidad, se devuelve el número medio de blancos y 
# negros (grises). 
intensidad.train = unlist(lapply(grises.train, FUN=mean))

# SIMETRÍA -----------------------------------------------------------------
# Creamos una función para calcular la simetría
simetria <- function(matriz){
  
  # Matriz_original 
  matriz_original = matriz[1:256]
  
  # Calculamos una nueva imagen invirtiendo el orden de las columnas
  matriz_invertida = matriz[,ncol(matriz):1]
  
  # Calculamos la diferencia entre la matriz original y la matriz invertida
  diferencia_matriz = matriz_original - matriz_invertida
  
  # Calculamos la media global de los valores absolutos de la matriz
  simetria = mean(abs(diferencia_matriz))
  
  simetria
}

# Guardamos la simetria de la imagen
simetria.train = unlist(lapply(grises.train, simetria))

## ------------------------------------------------------------------------
# Dibujamos la nube de puntos
plot(x=intensidad.train, y=simetria.train, col=etiquetas48.train+3, xlab = "Intensidad", 
     ylab = "Simetria", main="Intensidad y Simetría (train)")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Leemos el fichero con los datos de test 
digit.test <- read.table("datos/zip.test", quote="\"", comment.char="", 
                         stringsAsFactors=FALSE)
 
# Guardamos los 1 y los 5  
digitos48.test = digit.test[digit.test$V1==4 | digit.test$V1==8,]

# Etiquetas
digitos.test = digitos48.test[,1]  
ndigitos.test = nrow(digitos48.test)
  
# Se quita la clase y se monta una matriz 16x16
grises = array(unlist(subset(digitos48.test,select=-V1)),c(ndigitos.test,16,16))
grises.test = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

# Recordamos que las etiquetas que tenemos son 4 y 8
# por tanto las pasamos a -1 y 1
etiquetas48.test <- digitos48.test$V1
etiquetas48.test <- (etiquetas48.test-6)/2

## ------------------------------------------------------------------------
# INTENSIDAD ---------------------------------------------------------------
# Creamos un array con la media de la imagen
intensidad.test = unlist(lapply(grises.test, FUN=mean))

# SIMETRÍA -----------------------------------------------------------------
# Creamos uan función para calcular la simetría
simetria <- function(matriz){
  # Matriz_original 
  matriz_original = matriz[1:256]
  
  # Calculamos una nueva imagen invirtiendo el orden de las columnas
  matriz_invertida = matriz[,ncol(matriz):1]
  
  # Calculamos la diferencia entre la matriz original y la matriz invertida
  diferencia_matriz = matriz_original - matriz_invertida
  
  # Calculamos la media global de los valores absolutos de la matriz
  simetria = mean(abs(diferencia_matriz))
  
  simetria
}

# Creamos un array con la simetría de la imagen
simetria.test = unlist(lapply(grises.test, simetria))

## ------------------------------------------------------------------------
# Dibujamos los puntos
plot(x=intensidad.test, y=simetria.test, col=etiquetas48.test+3, xlab="Intensidad", 
     ylab="Grado Simetria", main="Intensidad y Simetría (test)")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
Regress_Lin <- function(datos, label) {
  # Añadimos una columna a los datos, para hacer el producto
  x <- cbind(1, data.matrix(datos))
   
  # Obtenemos la transformación SVD
  x.svd <- svd(x)
  
  # Descomposición de X en valores singulares
  v <- x.svd$v
  d <- x.svd$d
  
  # Comprobamos todos los términos de la diagonal
  diagonal <- diag(ifelse(d>0.0001, 1/d, d))
  
  # Calculamos (X^T) * X = V * (D^2) * (V^T)
  xx <- v %*% (diagonal^2) %*% t(v)
  
  # Calculamos la pseudoinversa: (X^T) * etiqueta
  pseudoinversa <- xx %*% t(x)
  
  # Calculamos wlin
  w <- pseudoinversa %*% as.numeric(label)
  
  # Devolvemos los coeficientes del hiperplano: w1 + w2*x + w3*y = 0
  c(w)
}

## ------------------------------------------------------------------------
# Función que calcula Ein
Ein <- function(datos, label, pesos){
  
    # Se obtiene el signo de los datos con respecto a los pesos
    signo =  sign(datos%*% pesos)
  
    # Comparamos en cuantas te equivocas con respecto a la verdadera etiqueta
    # nrow(datos) -> para que sea tanto por uno
    error <- sum (signo != sign(label)) / nrow(datos)
    
    # Devolvemos el error
    error
}

## ------------------------------------------------------------------------
PLA_Pocket <- function(datos, label, vini, iteraciones_max) {
  
  # Asigamos a w el valor inicial del vector
  w <- vini
  
  # Variable que contará el número de iteraciones para parar el PLA
  # en caso de que no sea linealmente separable
  iteraciones = 1 
  
  # Variable booleana que nos permitirá seguir o no,
  # viendo cuando se ha cambiado el valor o no
  continuar = TRUE
  
  # Añadimos a la matriz de datos, una tercera columna para poder hacer 
  # el producto  
  datos = cbind(datos,1)
  
  # Calculamos el error al principio
  error = Ein(datos, label, w)
  
  # Iterar en las muestra, mejorando la solución
  while ((iteraciones <= iteraciones_max) & continuar) {
    continuar = FALSE
    
    # Hacemos un bucle interno iterando sobre cada dato
    # cogiendo para ello los índices aleatoriamente
    for(i in (sample(nrow(datos)))) {
      
      # Obtenemos el signo del producto vectorial de los datos por los pesos
      signo = sign(datos[i,] %*% w)
      
      # Comparamos si el signo obtenido antes, coincide con el de la etiqueta 
      if (signo != label[i]) { # Sino coincide
        
        # Actualizamos el peso: w_new = w_old + x*etiqueta
        w_current = w + datos[i,]*label[i]
      
        # Cambiamos el valor de la variable booleana
        continuar <- TRUE
        
        # Después de actualizar los pesos, obtenemos el nuevo error
        nuevo_error = Ein(datos, label, w_current)
        
        # Comparamos si el error del principio es menor o igual 
        # que el nuevo
        if(error > nuevo_error){
          
          # Los mostramos por pantalla
          cat("Antiguo error = ", error, "\n")
          cat("Nuevo error = ", nuevo_error, "\n")
          
          # Actualizamos los valores
          w = w_current
          error = nuevo_error
        }
      }
    }
        
    # Incrementamos el número de iteraciones
    iteraciones = iteraciones + 1
  }
    
  # Devolvemos los parámetros del hiperplano del perceptrón
  # y el numero de iteraciones
  c(w)
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Juntamos los valores obtenidos de simetria e intensidad del train
datos.train <- cbind(intensidad.train, simetria.train)

# Dibujamos los puntos del train
plot(datos.train, xlab = "Intensidad Promedio", ylab = "Simetria",
     col = etiquetas48.train+3, main = "Regresión Lineal con PLA Pocket (train)")

# Hacemos Regresión Lineal 
pesos01.train <- Regress_Lin(datos.train, etiquetas48.train)

# -pesos01.train[1]/pesos01.train[3], -pesos01.train[2]/pesos01.train[3], es mi 'g',
# es decir, la función que intenta aproximar a la verdadera funcion 'f'

# Dibujamos la línea que separa los puntos (mediante regresión lineal)
abline(-pesos01.train[1]/pesos01.train[3], -pesos01.train[2]/pesos01.train[3],
       col = 5, lwd = 2) # Línea color azul es de regresión lineal

# Hacemos el PLA Pocket
pesos02.train <- PLA_Pocket(datos.train, etiquetas48.train, pesos01.train, 100)

# Dibujamos la línea que separa los puntos (mediante PLA Pocket)
abline(-pesos02.train[1]/pesos02.train[3], -pesos02.train[2]/pesos02.train[3], 
       col = 3, lwd = 2) # Línea color verde es de PLA Pocket

# Agregamos una leyenda
legend(-0.83, 0.9, c("Regresión Lineal", "PLA Pocket"), cex = 0.8, col = c(5, 3), 
      lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Juntamos los valores obtenidos de simetria e intensidad del test
datos.test <- cbind(intensidad.test, simetria.test)

# Dibujamos los puntos del test
plot(datos.test, xlab = "Intensidad Promedio", ylab = "Simetria", 
     col = etiquetas48.test+3, main = "Regresión Lineal con PLA Pocket (test)")

# Dibujamos la línea que separa los puntos (mediante regresión lineal)
abline(-pesos01.train[1]/pesos01.train[3], -pesos01.train[2]/pesos01.train[3],
       col = 5, lwd = 2) # Línea color amarillo es de regresión lineal

# Dibujamos la línea que separa los puntos (mediante PLA Pocket)
abline(-pesos02.train[1]/pesos02.train[3], -pesos02.train[2]/pesos02.train[3], 
       col = 3, lwd = 2) # Línea color verde es de PLA Pocket

# Agregamos una leyenda
legend(-0.75, 0.95, c("Regresión Lineal", "PLA Pocket"), cex = 0.8, col = c(5, 3), 
      lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Calculamos la recta de regresión que separa ambas clases (PLA Pocket)
recta_regresion03 <- c(-pesos02.train[1]/pesos02.train[3], 
                      - pesos02.train[2]/pesos02.train[3])

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos.train[,2] 
                              - recta_regresion03[1]*datos.train[,1] 
                              - recta_regresion03[2])

# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
Ein <- 0
Ein <- sum(etiquetas48.train != etiquetas_regresion01) / length(etiquetas48.train)

Ein

## ------------------------------------------------------------------------
## ya tienes arriba la recta creada
# Calculamos la recta de regresión que separa ambas clases (PLA Pocket)
recta_regresion03 = c(-pesos02.train[1]/pesos02.train[3], 
                      - pesos02.train[2]/pesos02.train[3])

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos.test[,2] - 
                                recta_regresion03[1]*datos.test[,1] -
                                recta_regresion03[2])

# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
Etest <- 0
Etest = sum(etiquetas48.test != etiquetas_regresion01) / length(etiquetas48.test)

Etest

## ------------------------------------------------------------------------
# Obtenemos el tamaño de los datos
N <- nrow(datos.train)

# Calculamos el segundo término de la fórmula
x <- sqrt( (8/N) * log((4*((2*N)^(3) + 1))/0.05) )

# Obtenemos el valor de Eout
cota_Ein_Eout <- Ein + x
cat ("Cota de generalizacion basada en E_in: ", cota_Ein_Eout)

## ------------------------------------------------------------------------
# Obtenemos el tamaño de los datos
N <- nrow(datos.test)

# Calculamos el segundo término de la fórmula
x <- sqrt( (1/(2*N)) * log( 2 / 0.05) )

# Obtenemos el valor de Eout
cota_Ein_Eout <- Etest + x
cat ("Cota de generalizacion basada en E_test: ", cota_Ein_Eout)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
simula_gaus = function(N = 2, mean = 0, dim = 2, sigma) {
  if (missing(sigma)) 
    stop("Debe dar un vector de varianzas")
  
  # Para la generación se usa sd, y no la varianza
  sigma = sqrt(sigma)  
  
  #if(dim != length(sigma)) 
   # stop ("El numero de varianzas es distinto de la dimensión")
  
  # Genera 1 muestra, con las desviaciones especificadas
  simula_gauss1 = function() rnorm(dim, mean = mean, sd = sigma) 
  
  # Repite N veces, simula_gauss1 y se hace la traspuesta
  m = t(replicate(N,simula_gauss1())) 
  m
}

## ------------------------------------------------------------------------
# Regresión Lineal con “weight decay” 
Regress_Lin_weight_decay = function(datos,label,lambda) {
  
  # Calculamos la inversa
  inversa <- solve(t(datos) %*% datos + lambda * diag(nrow = ncol(datos), ncol = ncol(datos)))
  
  # Calculamos pseudoinversa
  pseudoinversa <- inversa %*% t(datos)
  
  # Calculamos los pesos
  w <- pseudoinversa %*% label
  
  # Devolvemos los pesos
  w
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Para realizar el ejemplo, introducimos todo en una función
# Como entrada le pasaremos a la función el número de datos,
# la dimensión, un sigma y un lambda 
# (Todos definidos en el enunciado del ejercicio)
Validación_Cruzada <- function(N = 11, d = 3, sigma = 0.5, lambda = 0.05) {
  
  # Vector que guardar todos los errores 
  ecv = vector()
  # Vector que guardar errores del primer conjunto [13]
  e1 = vector() 
  # Vector que guardar errores del segundo conjunto [23]
  e2 = vector() 
  
  # Definimos el vector de pesos genernado d+1 valores de una Gaussiana
  #de media 0 y desviación típica 1
  wf = as.vector(simula_gaus(N=1, dim=d+1, mean = 0, sigma=c(1)))
  
  # La lista (d+10, d+20, ..., d+115)
  tams = seq(from=10, to=110, by=10)
  
  # Recorremos los valores e iniciamos la validación cruzada
  for(i in tams) {
    
    # Las coordenadas 'x', se generará como valores extraídos de una
    # gaussiana de media 1 y de desviación típica 1
    datos = simula_gaus(N, mean = 1, dim = d, sigma = c(1))
    
    # Añadimos una nueva columna, para poder hacer el producto escalar
    datos = cbind(datos,1)
    
    # Generamos las etiquetas a partir de wf*xn + sigma*ruido 
    # Donde ruido es una gaussiana de media 0 y desviación típica
    # 1, fijar sigma a 0.5
    etiquetas = apply (datos, 1, 
                      
                      FUN = function(x){ 
                        
                        # Calculamos la fórmula
                        wf %*% x + sigma * rnorm(1, 0, 0.5)
                        }
                      )
  
    # Calculamos el error
    error = sapply(1:N, 
                   
                   FUN = function(x){
                          
                     # Calculamos wreg como la fórmula explicada al
                     # principio del ejercicio
                     # Cuando pongamos [-x,] haremos referencia a los
                     # datos del train y cuando pogamos [x,], haremos
                     # referencia a los datos del test
                     
                     wreg = Regress_Lin_weight_decay(datos[-x,], etiquetas[-x], 
                                                     lambda/N)
                     
                     (1/N) * ( ( etiquetas[x] - (datos[x,] %*% wreg) )^2 )
                     }
                   )
  
    # Guardamos la media de todos lo errores
    ecv = c(mean(error), ecv)
    # Guardamos la media de los primeros errores (e1)
    e1 = c(error[1],e1)
    # Guardamos la media de los segundos errores (e2)
    e2 = c(error[2],e2)
    
  }

  list(errores = error, Ecv = mean(ecv), E1 = mean(e1), E2 = mean(e2))
}

# Probamos el algoritmo
Validación_Cruzada()

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Guardamos el resultado
resultado <- Validación_Cruzada()

# Lo mostramos gráficamente
plot(resultado$errores, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "Error", main = "Regularización")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Para realizar el ejemplo, introducimos todo en una función
# Como entrada le pasaremos a la función el número de datos,
# la dimensión, un sigma y un lambda 
# (Todos definidos en el enunciado del ejercicio)
Validación_Cruzada1 <- function(N = 11, d = 3, sigma = 0.5, lambda = 0.05) {
  
  # Vector que guardar todos los errores 
  ecv = vector()
  # Vector que guardar errores del primer conjunto [13]
  e1 = vector() 
  # Vector que guardar errores del segundo conjunto [23]
  e2 = vector() 
  
  # Definimos el vector de pesos genernado d+1 valores de una Gaussiana
  #de media 0 y desviación típica 1
  wf = as.vector(simula_gaus(N=1, dim=d+1, mean = 0, sigma=c(1)))
  
  # La lista (d+10, d+20, ..., d+115)
  tams = seq(from=10, to=110, by=10)
  
  # Recorremos los valores e iniciamos la validación cruzada
  for(i in tams) {
    
    # Las coordenadas 'x', se generará como valores extraídos de una
    # gaussiana de media 1 y de desviación típica 1
    datos = simula_gaus(N, mean = 1, dim = d, sigma = c(1))
    
    # Añadimos una nueva columna, para poder hacer el producto escalar
    datos = cbind(datos,1)
    
    # Generamos las etiquetas a partir de wf*xn + sigma*ruido 
    # Donde ruido es una gaussiana de media 0 y desviación típica
    # 1, fijar sigma a 0.5
    etiquetas = apply (datos, 1, 
                      
                      FUN = function(x){ 
                        
                        # Calculamos la fórmula
                        wf %*% x + sigma * rnorm(1, 0, 0.5)
                        }
                      )
  
    # Calculamos el error
    error = sapply(1:N, 
                   
                   FUN = function(x){
                          
                     # Calculamos wreg como la fórmula explicada al
                     # principio del ejercicio
                     # Cuando pongamos [-x,] haremos referencia a los
                     # datos del train y cuando pogamos [x,], haremos
                     # referencia a los datos del test
                     
                     wreg = Regress_Lin_weight_decay(datos[-x,], etiquetas[-x], 
                                                     lambda/N)
                     
                     (1/N) * ( ( etiquetas[x] - (datos[x,] %*% wreg) )^2 )
                     }
                   )
  
    # Guardamos la media de todos lo errores
    ecv = c(mean(error), ecv)
    # Guardamos la media de los primeros errores (e1)
    e1 = c(error[1],e1)
    # Guardamos la media de los segundos errores (e2)
    e2 = c(error[2],e2)
  }
  
   c(mean(ecv), mean(e1), mean(e2))
}


## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Lanzamos 1000 el algoritmo
res <- replicate(1000, Validación_Cruzada1())

# Guardamos los valores de Ecv (para hacer media y varianza)
Ecv = res[1,]
cat ("La media de Ecv es", mean(Ecv))
cat ("\nLa varianza de Ecv es", var(Ecv))

# Guardamos los valores de E1 (para hacer media y varianza)
E1 = res[2,]
cat ("\n\nLa media de E1 es", mean(E1))
cat ("\nLa varianza de E1 es", var(E1))

# Guardamos los valores de E2 (para hacer media y varianza)
E2 = res[3,]
cat ("\n\nLa media de E2 es", mean(E2))
cat ("\nLa varianza de E2 es", var(E2))

## ------------------------------------------------------------------------
# Abrimos plot
plot(c(0.2,1000), c(0,0.06), type = "n", xlab = "Iteracion", ylab = "E(u,v)", 
     main = "Regularización (1000 veces)")

# Mostramos gráficamente la recta para Ecv
lines(Ecv, type = "o", pch = 16, col = 2)
# Mostramos gráficamente la recta para E1
lines(E1, type = "o", pch = 16, col = 3)
# Mostramos gráficamente la recta para E2
lines(E2, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(9, 0.06, c("Ecv", "E1", "E2"), 
       cex = 0.8, col = c("blue", "green", "red"), pch=16, lty=c(1,1))

## ------------------------------------------------------------------------
# Abrimos plot
plot(c(0,15), c(0,0.025), type = "n", xlab = "Iteracion", ylab = "E(u,v)", 
     main = "Regularización (1000 veces)")

# Mostramos gráficamente la recta para Ecv
lines(Ecv, type = "o", pch = 16, col = 2)
# Mostramos gráficamente la recta para E1
lines(E1, type = "o", pch = 16, col = 3)
# Mostramos gráficamente la recta para E2
lines(E2, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(1, 0.025, c("Ecv", "E1", "E2"), 
       cex = 0.8, col = c("blue", "green", "red"), pch=16, lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
CD <- function(funcion, funcion_gradiente, eta=0.1, vini=c(0,0), umbral=10^(-4), 
               iteraciones_max=500){
  
  # Inicializar los pesos
  w_new <- vini
  
  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Array creado donde guardaremos el valor de la funcion, para luego crear la gráfica
  f <- rep(iteraciones_max)
  f[iter] <- funcion(w_new[1],w_new[2])
  
  # Mostramos el punto de partida
  cat("Punto de partida: ", w_new[1], w_new[2])

  # Mostramos los valores del gradiente al principio
  cat("\nGradiente (inicial): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos el valor de la función al principio
  cat("\nValor función (inicial): ", funcion(w_new[1], w_new[2]))

  # La condición de parada será cuando el valor de la funcion sea menor o igual a 
  # un umbral, cuando se hayan completado un número máximo de iteraciones 
  while ((iter <= iteraciones_max) & (funcion(w_new[1], w_new[2]) > 10^(-4))) {
    
    # Paso 1: nos movemos a lo largo de la coordenada u
    gradiente <- funcion_gradiente(w_new[1], w_new[2]) 
    direccion <- -gradiente
    w_new[1] <- w_new[1] + eta * direccion[1]
    
    # Paso 2: reevaluamos y nos movemos a lo largo de la coordenada v
    gradiente <- funcion_gradiente(w_new[1], w_new[2]) 
    direccion <- -gradiente
    w_new[2] <- w_new[2] + eta * direccion[2]
    
    # Incrementamos el valor de la iteración
    iter <- iter + 1
    
    # Guardamos el nuevo valor de f
    f[iter] = funcion(w_new[1],w_new[2])
  }
  
  # Mostramos los valores del gradiente al terminar
  cat("\nGradiente (final): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos la solución final
  cat("\nSolución: ", w_new[1], w_new[2])
  
  # Mostramos el valor de la función al final
  cat("\nValor función (final): ", funcion(w_new[1], w_new[2]))
  
  # Devolvemos el número de iteraciones necesarias para encontrar la solución
  cat("\nIteraciones: ",iter-1)
  cat("\n")
  
  # Mostramos el valor de la funcion
  list(valor_funcion = f[1:iter])
}

## ------------------------------------------------------------------------
# Guardamos el valor con las condiciones específicadas en el enunciado
resultado11 = CD(E, E_dev, eta=0.1, vini=c(1,1), umbral=10^(-4), iteraciones_ma=15)
# Lo mostramos gráficamente
plot(resultado11$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "E(u,v)", main = "Coordenada Descendente E(u,v)")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Gradiente Descendente Modificado
GD1 <- function(funcion, funcion_gradiente, eta=0.1, vini=c(0,0), umbral=10^(-4), 
               iteraciones_max=500){
  
  # Inicializar los pesos
  w_new <- vini
  
  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Array creado donde guardaremos el valor de la funcion, para luego crear la gráfica
  f <- rep(iteraciones_max)
  f[iter] <- funcion(w_new[1],w_new[2])
  
  # Mostramos el punto de partida
  cat("Punto de partida: ", w_new[1], w_new[2])

  # Mostramos los valores del gradiente al principio
  cat("\nGradiente (inicial): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos el valor de la función al principio
  cat("\nValor función (inicial): ", funcion(w_new[1], w_new[2]))

  # La condición de parada será cuando el valor de la funcion sea menor o igual a 
  # un umbral, cuando se hayan completado un número máximo de iteraciones 
  while ((iter <= iteraciones_max) & (funcion(w_new[1], w_new[2]) > 10^(-4))) {
    
    # Calculamos el gradiente
    gradiente <- funcion_gradiente(w_new[1], w_new[2]) 
    
    # Establecemos la dirección a moverse
    direccion <- -gradiente
    
    # Actualizamos los pesos
    w_new <- w_new + eta * direccion
    
    # Incrementamos el valor de la iteración
    iter <- iter + 1
    
    # Guardamos el nuevo valor de f
    f[iter] = funcion(w_new[1],w_new[2])
  }
  
  # Mostramos los valores del gradiente al terminar
  cat("\nGradiente (final): ", funcion_gradiente(w_new[1],w_new[2]))
  
  # Mostramos la solución final
  cat("\nSolución: ", w_new[1], w_new[2])
  
  # Mostramos el valor de la función al final
  cat("\nValor función (final): ", funcion(w_new[1], w_new[2]))
  
  # Devolvemos el número de iteraciones necesarias para encontrar la solución
  cat("\nIteraciones: ",iter-1)
  cat("\n")
  
  # Mostramos el valor de la funcion
  list(valor_funcion = f[1:iter])
}

## ------------------------------------------------------------------------
resultado12 = GD1(E, E_dev, eta=0.1, vini=c(1,1), umbral=10^(-4), iteraciones_ma=15)

## ------------------------------------------------------------------------
# Abrimos plot
plot(c(0,16), c(0,60000), type = "n", xlab = "Iteracion", ylab = "E(u,v)", 
     main = "Gradiente y Coordenada Descendente")

# Mostramos gráficamente la recta para CD
lines(resultado11$valor_funcion, type = "o", pch = 16, col = 2)

# Mostramos gráficamente la recta para GD
lines(resultado12$valor_funcion, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(8, 50000, c("Gradiente Descedente", "Coordenada Descendente"), 
       cex = 0.8, col = c("blue", "red"), pch=16, lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Coordenada Descedente con tasa de 0.001
resultado13 = CD(E, E_dev, eta=0.001, vini=c(1,1), umbral=10^(-4), iteraciones_ma=15)

# Gradiente Descedente con tasa de 0.001
resultado14 = GD1(E, E_dev, eta=0.001, vini=c(1,1), umbral=10^(-4), iteraciones_ma=15)

# Abrimos plot
plot(c(0,16), c(0,3.5), type = "n", xlab = "Iteracion", ylab = "E(u,v)", 
     main = "Gradiente y Coordenada Descendente")

# Mostramos gráficamente la recta para la tasa  0.1
lines(resultado13$valor_funcion, type = "o", pch = 16, col = 2)

# Mostramos gráficamente la recta para la tasa  0.01
lines(resultado14$valor_funcion, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(0, 1, c("Gradiente Descedente", "Coordenada Descendente"), 
       cex = 0.8, col = c("blue", "red"), pch=16, lty=c(1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Derivada Hessiana
f_hess = function(x,y){
  
  dxx = 2-8*pi^2*sin(2*pi*x)*sin(2*pi*y)
  dyy = 4-8*pi^2*sin(2*pi*x)*sin(2*pi*y)
  dyx = 8*pi^2*cos(2*pi*x)*cos(2*pi*y)
  dxy = 8*pi^2*cos(2*pi*x)*cos(2*pi*y)
  
  matrix(c(dxx,dxy,dxy,dyy),2)
}

## ------------------------------------------------------------------------
# Método de Newton
Metodo_Newton = function (funcion, funcion_gradiente, funcion_hessiana,
                          vini = c(0,0), umbral = 10^(-5), iteraciones_max = 50) {
  
  # Inicializar los pesos
  w_old <- c(0,0)
  w_new <- vini
  
  
  # Variable que lleva el número de iteraciones realizadas
  iter <- 0
  
  # Array creado donde guardaremos el valor de la funcion, para luego crear la gráfica
  f <- rep(iteraciones_max)
  f[iter] <- funcion(w_new[1],w_new[2])
  
  # Mostramos el punto de partida
  cat("Punto de partida: ", w_new[1], w_new[2])
  
  # Mostramos el valor de la función al principio
  cat("\nValor función (inicial): ", funcion(w_new[1], w_new[2]))
  
  # La condición de parada será cuando el valor de la funcion sea menor o igual a 
  # un umbral, cuando se hayan completado un número máximo de iteraciones o cuando 
  # la diferencia entre dos puntos en valor absoluto sea muy cercana.
  # La condición de la diferencia entre dos puntos en valor absoluto sea muy cercana, 
  # descarta muchos valores, ya que avanza muy lentamente
  
  while(funcion(w_new[1],w_new[2]) > umbral & (iter < iteraciones_max ) 
        & (abs(funcion(w_new[1], w_new[2]) 
               - funcion(w_old[1], w_old[2])) > umbral) ){
    
    # Asignamos el valor nuevo, al antiguo
    w_old <- w_new
    
    # Calculamos el gradiente
    gradiente = funcion_gradiente(w_new[1],w_new[2])
    
    # Calculamos la hessiana
    hessiana = funcion_hessiana(w_new[1],w_new[2])
    
    # Calculamos la dirección
    direccion = - solve(hessiana) %*% gradiente
    
    # Actualizamos los pesos
    w_new = w_old + direccion

    # Incrementamos el valor de la iteración (época)
    iter = iter + 1
    
    # Guardamos el nuevo valor de f
    f[iter] = funcion(w_new[1],w_new[2])
  }
  
  # Mostramos el valor de la función al final
  cat("\nValor función (final): ", funcion(w_new[1], w_new[2]))
  
  # Mostramos la solución final
  cat("\nSolución: ", w_new[1], w_new[2])
  cat("\n")
  
  # Mostramos el valor de la funcion
  list(valor_funcion = f[1:iter])
}

# Ejecutamos el algoritmo
Metodo_Newton(f, f_dev, f_hess, vini=c(1,1))

## ------------------------------------------------------------------------
# Guardamos el resultado
data = Metodo_Newton(f, f_dev, f_hess, vini=c(1,1))

# Los mostramos por pantalla
plot(data$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Método de Newton con inicio (1,1)",)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dividimos la región de dibujo en 4 partes
par(mfrow=c(2,2)) 

## Punto (2.1, 2.1) -------------------------------------------------------------------
resultado04 = Metodo_Newton(f, f_dev, f_hess, vini = c(2.1,2.1), iteraciones_max = 30)
# Lo mostramos gráficamente
plot(resultado04$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (2.1, 2.1)")

## Punto (3, 3) -----------------------------------------------------------------------
resultado04 = Metodo_Newton(f, f_dev, f_hess, vini = c(3,3), iteraciones_max = 30)
# Lo mostramos gráficamente
plot(resultado04$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (3, 3)")

## Punto (1.5, 1.5) -------------------------------------------------------------------
resultado04 = Metodo_Newton(f, f_dev, f_hess, vini = c(1.5,1.5), iteraciones_max = 30)
# Lo mostramos gráficamente
plot(resultado04$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1.5, 1.5)")
## Punto (1, 1) ----------------------------------------------------------------------
resultado04 = Metodo_Newton(f, f_dev, f_hess, vini = c(1,1), iteraciones_max = 30)
# Lo mostramos gráficamente
plot(resultado04$valor_funcion, type = "o", pch = 16, col = 4, xlab = "Iteracion", 
     ylab = "f(x,y)", main = "Inicio (1, 1)")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Newton 
resultado05 = Metodo_Newton(f, f_dev, f_hess, vini = c(1,1), iteraciones_max = 30)

# Gradiente Descendente con 0.1
resultado06 = GD(f, f_dev, vini =c(1,1), eta = 0.1, umbral = 10^(-4), 
                 iteraciones_max = 30)
# Gradiente Descendente con 0.01
resultado07 = GD(f, f_dev, vini =c(1,1), eta = 0.01, umbral = 10^(-4), 
                 iteraciones_max = 30)

# Abrimos plot
plot(c(0,10), c(0,6), type = "n", xlab = "Iteracion", ylab = "f(x,y)", 
     main = "Comparación MN con GD")

# Mostramos gráficamente las tres líneas
# Newton
lines(resultado05$valor_funcion, type = "o", pch = 16, col = 2)
# Gradiente Descendente con 0.1
lines(resultado06$valor_funcion, type = "o", pch = 16, col = 3)
# Gradiente Descendente con 0.01
lines(resultado07$valor_funcion, type = "o", pch = 16, col = 4)

# Agregamos una leyenda
legend(8, 6,c("Newton", "GD 0.1", "GD 0.01"), cex = 0.8, 
       col =c("red", "green", "blue"),pch=16, lty=c(1,1,1))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto, para la generación de números aleatorios
set.seed(3)

Eout02 <- 0 # Establecemos eout a cero
Ein02 <- 0 # Establecemos Ein a cero
 
# Ejecutar el experimento 100 veces
for (i in seq(1,100)){
  
  # Ein
  # -------------------------------------------------------------------------
  # Generamos aleatoriamente una lista de números aleatorios
  datos04 = simula_unif(N = 100, dim = 2, rango = c(0,2))
  # Generamos una recta
  recta04 <- simula_recta(intervalo = c(0,2))
  # Etiquetamos los puntos a partir de la función de la recta
  etiquetas04 <- sign(datos04[,2] - recta04[1]*datos04[,1] - recta04[2])
  # Calculamos la recta de regresión que separa ambas clases
  recta_regresion03 <- RL1(datos04, etiquetas04)
  recta_regresion04 = c(-recta_regresion03[1]/recta_regresion03[3], 
                        - recta_regresion03[2]/recta_regresion03[3])
  # Etiquetamos los puntos a partir de la recta de regresión
  etiquetas_regresion05 <- sign(datos04[,2] - recta_regresion04[1]*datos04[,1] 
                                - recta_regresion04[2])
  # Calculamos el valor de Ein como las etiquetas que son distintas, 
  # partido el total de las etiquetas
  Ein02 = Ein02 + sum(etiquetas04 != etiquetas_regresion05) / length(etiquetas04)
  
  # Eout
  # -------------------------------------------------------------------------
  # Generamos aleatoriamente una lista de números aleatorios
  datos04.test = simula_unif(N = 100, dim = 2, rango = c(0,2))
  # Generamos una recta 
  recta04.test <- simula_recta(intervalo = c(0,2))
  # Etiquetamos las etiquetas a partir de la función de la recta
  etiquetas04.test <- sign(datos04.test[,2] - recta04.test[1]*datos04.test[,1] 
                           - recta04.test[2])

  # Etiquetamos los puntos a partir de la recta de regresión ya creada
  etiquetas_regresion02.test <- sign(datos04.test[,2] 
                                     - recta_regresion04[1]*datos04.test[,1] 
                                     - recta_regresion04[2])
  # Calculamos el valor de Eout
  Eout02 = Eout02 + sum(etiquetas04.test != etiquetas_regresion02.test) /
    length(etiquetas04.test)
}
  
cat ("Ein:", Ein02/100)
cat ("\nEout: ", Eout02/100)

## ------------------------------------------------------------------------
sol <- replicate(100, RL1(datos04, etiquetas04)[4])
mean(sol)

