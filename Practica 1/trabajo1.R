# Gema Correa Fernández
# Trabajo 1: Programación

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
set.seed(3) # Inicializamos la semilla a un número por defecto

## ------------------------------------------------------------------------
simula_unif = function (N = 2, dim = 2, rango = c(0,1)) {
  m = matrix(runif(N*dim, min=rango[1], max=rango[2]), nrow=N, ncol=dim, byrow=T)
  m
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos nuestros números aleatorios
unif <- simula_unif(N = 50, dim = 2, rango = c(-50, 50))

## ------------------------------------------------------------------------
# Dibujamos los puntos
plot (unif, main = "Distribución Uniforme", col = 2, xlab = "X", ylab = "Y", 
      xlim = c(-50, 50), ylim = c(-50, 50))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
simula_gaus = function(N = 2, dim = 2, sigma) {
  if (missing(sigma)) 
    stop("Debe dar un vector de varianzas")
  
  # Para la generación se usa sd, y no la varianza
  sigma = sqrt(sigma)  
  
  if(dim != length(sigma)) 
    stop ("El numero de varianzas es distinto de la dimensión")
  
  # Genera 1 muestra, con las desviaciones especificadas
  simula_gauss1 = function() rnorm(dim, sd = sigma) 
  
  # Repite N veces, simula_gauss1 y se hace la traspuesta
  m = t(replicate(N,simula_gauss1())) 
  m
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos nuestros números aleatorios
gaus = simula_gaus(N = 50, dim = 2, sigma = c(5, 7))

## ------------------------------------------------------------------------
# Dibujamos los puntos
plot (gaus, main = "Distribución Normal o Gaussiana", col = 4, xlab = "X", ylab = "Y", 
      xlim = c(-50, 50), ylim = c(-50, 50))

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
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
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos aleatoriamente una lista de números aleatorios
datos01 = simula_unif(N=50, dim=2, rango=c(-50,50))

# Generamos una recta 
recta01 = simula_recta(intervalo=c(-50,50))

# Aplicamos la función a cada punto y almacenamos el signo del resultado en una lista
# con esto acabamos de etiquetar cada punto 
etiquetas01 = sign(datos01[,2] - recta01[1]*datos01[,1] - recta01[2])

# Representamos los puntos
plot (datos01, main = "Clasificación por recta (etiquetas sin ruido)", 
      col = etiquetas01+3, xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta01[2], recta01[1], lwd = 1.5)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
generar_ruido = function (etiqueta, porcentaje=10) {
  # Obtenemos el tamaño de las etiquetas  
  et = length(etiqueta)
  
  # Obtenemos el tamaño de las etiqueas con valor 1
  et1 = sum(etiqueta == 1) 
    
  # Obtenemos el tamaño de las etiqueas con valor -1
  et2 = et - et1 

  # Obtenemos el indice de cada etiqueta
  ind1 = which(etiqueta == 1) 
  ind2 = which(etiqueta == -1)
  
  # Introducimos el ruido en las etiquetas de manera aleatoria
  # dependiendo del porcentaje de ruido que queramos meter
  etiqueta[sample(ind1, et1*porcentaje/100)] = -1
  etiqueta[sample(ind2, et2*porcentaje/100)] = 1
  
  etiqueta
}

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos aleatoriamente una lista de números aleatorios
datos02 = simula_unif(N=50, dim=2, rango=c(-50,50))

# Generamos una recta 
recta02 = simula_recta(intervalo=c(-50,50))

# Aplicamos la función a cada punto y almacenamos el signo del resultado en una lista
etiquetas02 = sign(datos02[,2] - recta02[1]*datos02[,1] - recta02[2])

# Introducimos el ruido en las etiquetas
etiquetas_ruido01 = generar_ruido(etiquetas02, 10)

# Representamos los puntos
plot (datos02, main = "Clasificación por recta (etiquetas con ruido)",col = etiquetas_ruido01+3, 
      xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta02[2], recta02[1], lwd = 1.5)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Calcular etiquetas positivas
sum(etiquetas02 == 1)

# Calcular etiquetas negativas
sum(etiquetas02 == -1)

## ------------------------------------------------------------------------
# Porcentaje a introducir ruido en las etiquetas positivas
(sum(etiquetas02 == 1)) / 10

# Porcentaje a introducir ruido en las etiquetas negativas
(sum(etiquetas02 == -1)) / 10

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
pintar_frontera = function(f,rango=c(-50,50)) {
  x = y = seq(rango[1],rango[2],length.out = 100)
  z = outer(x,y,FUN=f)
  
  # Si no está abierto el dispositivo lo abre con plot
  if (dev.cur()==1)
   plot(1, type="n", xlim=rango, ylim=rango)
 
  contour(x,y,z, levels = 0, drawlabels = FALSE, add = T, 
          xlim = rango, ylim = rango, xlab = "x", ylab = "y", lwd = 1.5)
}

## ------------------------------------------------------------------------
# Nos creamos una función que definirá la frontera de clasificación 
# de los puntos de la muestra
f1_xy <- function(x,y) {
  return ((x-10)^2 + (y-20)^2 - 400)
}

# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))
  
# Visualizar el etiquetado generado en el ejercicio 2b (etiquetas con ruido)
# --------------------------------------------------------------------------------
# Representamos los puntos
plot (datos02, main = "Clasificación por Recta (ruido)",col = etiquetas_ruido01+3, 
      xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta02[2], recta02[1], lwd = 1.5)
# --------------------------------------------------------------------------------

# Abrimos otro dispositivo con plot, para que no nos cree ambas gráficas juntas
plot(1, main = "Clasificación por Círculo", type="n", xlab = "X", ylab = "Y", 
     xlim=c(-50,50), ylim=c(-50,50))

# Pintamos la frontera que define la primera función
pintar_frontera(f1_xy)
  
# Obtenemos los puntos que van a estar fuera de la frontera
etiquetado01 <- subset(datos02, f1_xy(datos02[,1],datos02[,2]) > 0) 

# Obtenemos los puntos que van a estar dentro de la frontera
etiquetado02 <- subset(datos02, f1_xy(datos02[,1],datos02[,2]) <= 0)

# Pintar los puntos en la segunda gráfica
points(etiquetado01, col = "blue")
points(etiquetado02, col = "red")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Nos creamos una función que definirá la frontera de clasificación 
# de los puntos de la muestra
f2_xy <- function(x,y) {
  (0.5*(x+10)^2 + (y-20)^2 - 400)
}

# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))
  
# Visualizar el etiquetado generado en el ejercicio 2b (etiquetas con ruido)
# --------------------------------------------------------------------------------
# Representamos los puntos
plot (datos02, main = "Clasificación por Recta (ruido)",col = etiquetas_ruido01+3, 
      xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta02[2], recta02[1], lwd = 1.5)
# --------------------------------------------------------------------------------

# Abrimos otro dispositivo con plot, para que no nos cree ambas gráficas juntas
plot(1, main = "Clasificación por Óvalo", type="n", xlab = "X", ylab = "Y", 
     xlim=c(-50,50), ylim=c(-50,50))

# Pintamos la frontera que define la primera función
pintar_frontera(f2_xy)

# Obtenemos los puntos que van a estar fuera de la frontera
etiquetado03 <- subset(datos02, f2_xy(datos02[,1],datos02[,2]) > 0) 

# Obtenemos los puntos que van a estar dentro de la frontera
etiquetado04 <- subset(datos02, f2_xy(datos02[,1],datos02[,2]) <= 0)

# Pintar los puntos en la segunda gráfica
points(etiquetado03, col = "blue")
points(etiquetado04, col = "red")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Nos creamos una función que definirá la frontera de clasificación 
# de los puntos de la muestra
f3_xy <- function(x,y) {
   (0.5*(x-10)^2 - (y+20)^2 - 400)
}

# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))
  
# Visualizar el etiquetado generado en el ejercicio 2b (etiquetas con ruido)
# --------------------------------------------------------------------------------
# Representamos los puntos
plot (datos02, main = "Clasificación por Recta (ruido)",col = etiquetas_ruido01+3, 
      xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta02[2], recta02[1], lwd = 1.5)
# --------------------------------------------------------------------------------

# Abrimos otro dispositivo con plot, para que no nos cree ambas gráficas juntas
plot(1, main = "Clasificación por Hipérbola", type="n", xlab = "X", ylab = "Y", 
     xlim=c(-50,50), ylim=c(-50,50))

# Pintamos la frontera que define la primera función
pintar_frontera(f3_xy)

# Obtenemos los puntos que van a estar fuera de la frontera
etiquetado05 <- subset(datos02, f3_xy(datos02[,1],datos02[,2]) > 0) 

# Obtenemos los puntos que van a estar dentro de la frontera
etiquetado06 <- subset(datos02, f3_xy(datos02[,1],datos02[,2]) <= 0)

# Pintar los puntos en la segunda gráfica
points(etiquetado05, col = "blue")
points(etiquetado06, col = "red")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Nos creamos una función que definirá la frontera de clasificación 
# de los puntos de la muestra
f4_xy <- function(x,y) {
  (y - 20*x^2 - 5*x + 3)
}

# Dividimos la región de dibujo en dos partes
par(mfrow=c(1:2))
  
# Visualizar el etiquetado generado en el ejercicio 2b (etiquetas con ruido)
# --------------------------------------------------------------------------------
# Representamos los puntos
plot (datos02, main = "Clasificación por Recta (ruido)",col = etiquetas_ruido01+3, 
      xlab = "X", ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Representamos la recta (parámetros al revés en abline)
abline(recta02[2], recta02[1], lwd = 1.5)
# --------------------------------------------------------------------------------

# Abrimos otro dispositivo con plot, para que no nos cree ambas gráficas juntas
plot(1, main = "Clasificación por Parábola", type="n", xlab = "X", ylab = "Y", 
     xlim=c(-50,50), ylim=c(-50,50))

# Pintamos la frontera que define la primera función
pintar_frontera(f4_xy)

# Obtenemos los puntos que van a estar fuera de la frontera
etiquetado07 <- subset(datos02, f4_xy(datos02[,1],datos02[,2]) > 0) 

# Obtenemos los puntos que van a estar dentro de la frontera
etiquetado08 <- subset(datos02, f4_xy(datos02[,1],datos02[,2]) <= 0)

# Pintar los puntos en la segunda gráfica
points(etiquetado07, col = "blue")
points(etiquetado08, col = "red")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
ajusta_PLA <- function(datos, label, max_iter, vini) {
  
  # Asigamos a w el valor inicial del vector
  w <- vini
  
  # Variable que contará el número de iteraciones para parar el PLA
  # en caso de que no sea linealmente separable
  iteraciones = 1 
  
  # Variable booleana que nos permitirá seguir o no,
  # viendo cuando se ha cambiado el valor o no
  continuar = T
  
  # Añadimos a la matriz de datos, una tercera columna para poder hacer 
  # el producto  
  datos = cbind(rep(1, nrow(datos)), datos)
  
  # Iterar en las muestra, mejorando la solución
  while (iteraciones <= max_iter && continuar) {
    continuar = FALSE
    
    # Hacemos un bucle interno iterando sobre cada dato
    # cogiendo para ello los índices aleatoriamente
    for(i in (sample(nrow(datos)))) {
      # Obtenemos el signo del producto vectorial de los datos por los pesos
      signo = sign(datos[i,] %*% w)
      
      # Comparamos si el signo obtenido antes, coincide con el de la etiqueta 
      if (signo != label[i]) { # Sino coincide
        # Actualizamos el peso: wnew = wold + x*etiqueta
        w = w + datos[i,]*label[i]
        
        # Establecemos que se ha cambiado el valor
        continuar = TRUE
        
        # Incrementamos el número de iteraciones
        iteraciones = iteraciones + 1
      }
    }
  }
  
  # Devolvemos los parámetros del hiperplano del perceptrón
  # y el numero de iteraciones
  c(-w[1]/w[3], -w[2]/w[3], iteraciones) 
}

## ------------------------------------------------------------------------
# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones y el vector inicial a 0
perceptron01 = (ajusta_PLA(datos01, etiquetas01, 2000, c(0,0,0)))

# Dibujamos los puntos en la gráfica  
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", 
     ylab = "Y", main="PLA con vector a cero") 

# Representamos la recta hecha por el perceptron
abline(a=perceptron01[1], b=perceptron01[2], lwd=1.5) 

# Mostramos el número de iteraciones que hacen falta para converger
cat(perceptron01[3],"iteraciones para converger.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Para obtener las gráficas juntas
par(mfrow=c(2,2)) 

# Para un máximo de 20 iteraciones
# --------------------------------------------------------------------------------
perceptron03 = (ajusta_PLA(datos01, etiquetas01, 20, c(0,0,0)))
plot(datos01, col = etiquetas01+3, , xlab = "X", ylab = "Y", main="20 iteraciones", 
     xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron03[1], b=perceptron03[2], lwd=1.5)
# --------------------------------------------------------------------------------
  
# Para un máximo de 200 iteraciones
# --------------------------------------------------------------------------------
perceptron04 = (ajusta_PLA(datos01, etiquetas01, 200, c(0,0,0)))
plot(datos01, col = etiquetas01+3, , xlab = "X", ylab = "Y", main="200 iteraciones", 
     xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron04[1], b=perceptron04[2], lwd=1.5)
# Para un máximo de 20000 iteraciones
# --------------------------------------------------------------------------------
perceptron05 = (ajusta_PLA(datos01, etiquetas01, 20000, c(0,0,0)))
plot(datos01, col = etiquetas01+3, , xlab = "X", ylab = "Y", main="20000 iteraciones", 
     xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron05[1], b=perceptron05[2], lwd=1.5)
# --------------------------------------------------------------------------------
  
# Para un máximo de 200000 iteraciones
# --------------------------------------------------------------------------------
perceptron06 = (ajusta_PLA(datos01, etiquetas01, 200000, c(0,0,0)))
plot(datos01, col = etiquetas01+3, , xlab = "X", ylab = "Y", main="200000 iteraciones", 
     xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron06[1], b=perceptron06[2], lwd=1.5)

cat("Con un máximo de 20 iteraciones, converge a ", perceptron03[3],"iteraciones.")
cat("Con un máximo de 200 iteraciones, converge a ", perceptron04[3],"iteraciones.")
cat("Con un máximo de 20000 iteraciones, converge a ", perceptron05[3],"iteraciones.")
cat("Con un máximo de 20000 iteraciones, converge a ", perceptron06[3],"iteraciones.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones 
# y el vector con valores aleatorios entre [0,1]
perceptron07 = (ajusta_PLA(datos01, etiquetas01, 2000, runif(3)))

# Dibujamos los puntos en la gráfica  
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", 
     ylab = "Y", main="PLA con vector aleatario") 

# Representamos la recta hecha por el perceptron
abline(a=perceptron07[1], b=perceptron07[2], lwd=1.5) 

# Mostramos el número de iteraciones que hacen falta para converger
cat(perceptron07[3],"iteraciones para converger.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones 
# y el vector con valores aleatorios entre [0,1]
perceptron08 = replicate(10, ajusta_PLA(datos01, etiquetas01, 2000, runif(3))[3])

# Mostramos el número de iteraciones que hacen falta para converger
# Realizamos la media de las 10 iteraciones
cat(mean(perceptron08),"iteraciones para converger.")

## ------------------------------------------------------------------------
# Para obtener las gráficas juntas
par(mfrow=c(2,2)) 

# Para un máximo de 20 iteraciones
# --------------------------------------------------------------------------------
perceptron09 = (ajusta_PLA(datos01, etiquetas01, 20, runif(3)))
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", ylab = "Y",
     main="20 iteraciones") 
abline(a=perceptron09[1], b=perceptron09[2], lwd=1.5) 
cat("Con un máximo de 20 iteraciones, converge a ", perceptron09[3],"iteraciones.")
  
# Para un máximo de 200 iteraciones
# --------------------------------------------------------------------------------
perceptron10 = (ajusta_PLA(datos01, etiquetas01, 200, runif(3)))
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", 
     ylab = "Y", main="200 iteraciones") 
abline(a=perceptron10[1], b=perceptron10[2], lwd=1.5) 
cat("Con un máximo de 200 iteraciones, converge a ", perceptron10[3],"iteraciones.")

# Para un máximo de 20000 iteraciones
# --------------------------------------------------------------------------------
perceptron11 = (ajusta_PLA(datos01, etiquetas01, 20000, runif(3)))
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", 
     ylab = "Y", main="20000 iteraciones") 
abline(a=perceptron11[1], b=perceptron11[2], lwd=1.5) 
cat("Con un máximo de 20000 iteraciones, converge a ", perceptron11[3],"iteraciones.")

# Para un máximo de 200000 iteraciones
# -------------------------------------------------------------------------------- 
perceptron12 = (ajusta_PLA(datos01, etiquetas01, 200000, runif(3)))
plot(datos01, col = etiquetas01+3, xlim=c(-50,50), ylim = c(-50,50), xlab = "X", 
     ylab = "Y", main="200000 iteraciones") 
abline(a=perceptron12[1], b=perceptron12[2], lwd=1.5) 
cat("Con un máximo de 200000 iteraciones, converge a ", perceptron12[3],"iteraciones.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Para un máximo de 20 iteraciones
# --------------------------------------------------------------------------------
perceptron13 = replicate(10, ajusta_PLA(datos01, etiquetas01, 20, runif(3)))
cat("Con un máximo de 20 iteraciones, converge a ", 
    mean(perceptron13[3]),"iteraciones.")
  
# Para un máximo de 200 iteraciones
# --------------------------------------------------------------------------------
perceptron14 = replicate(10, ajusta_PLA(datos01, etiquetas01, 200, runif(3)))
cat("Con un máximo de 200 iteraciones, converge a ", 
    mean(perceptron14[3]),"iteraciones.")

# Para un máximo de 20000 iteraciones
# --------------------------------------------------------------------------------
perceptron15 = replicate(10, ajusta_PLA(datos01, etiquetas01, 20000, runif(3)))
cat("Con un máximo de 20000 iteraciones, converge a ", 
    mean(perceptron15[3]),"iteraciones.")

# Para un máximo de 200000 iteraciones
# -------------------------------------------------------------------------------- 
perceptron16 = replicate(10, ajusta_PLA(datos01, etiquetas01, 200000, runif(3)))
cat("Con un máximo de 200000 iteraciones, converge a ", 
    mean(perceptron16[3]),"iteraciones.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Dibujamos los puntos en la gráfica 
plot (datos02, main = "PLA con vector cero (ruido)", col = etiquetas_ruido01+3, xlab = "X", 
      ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones y el vector inicial a 0
# Ahora usamos las etiquetas con ruido
perceptron_ruido01 = (ajusta_PLA(datos02, etiquetas_ruido01, 2000, c(0,0,0)))

# Representamos la recta hecha por el perceptron
abline(a=perceptron_ruido01[1], b=perceptron_ruido01[2], lwd=1.5) 

# Mostramos el número de iteraciones que hacen falta para converger
cat(perceptron_ruido01[3],"iteraciones para converger.")

## ------------------------------------------------------------------------
# Para obtener las gráficas juntas
par(mfrow=c(2,2)) 
  
# Para un máximo de 200 iteraciones
# --------------------------------------------------------------------------------
perceptron_ruido02 = (ajusta_PLA(datos02, etiquetas_ruido01, 200, c(0,0,0)))
plot(datos01, col = etiquetas_ruido01+3, , xlab = "X", ylab = "Y", 
     main="200 iteraciones", xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron_ruido02[1], b=perceptron_ruido02[2], lwd=1.5)
cat("Con un máximo de 200 iteraciones, converge a ", 
    perceptron_ruido02[3],"iteraciones.")

# Para un máximo de 20000 iteraciones
# --------------------------------------------------------------------------------
perceptron_ruido03 = (ajusta_PLA(datos02, etiquetas_ruido01, 20000, c(0,0,0)))
plot(datos01, col = etiquetas_ruido01+3, , xlab = "X", ylab = "Y", 
     main="20000 iteraciones", xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron_ruido03[1], b=perceptron_ruido03[2], lwd=1.5)
cat("Con un máximo de 20000 iteraciones, converge a ",  
    perceptron_ruido03[3],"iteraciones.")
  
# Para un máximo de 200000 iteraciones
# --------------------------------------------------------------------------------
perceptron_ruido04 = (ajusta_PLA(datos02, etiquetas_ruido01, 200000, c(0,0,0)))
plot(datos01, col = etiquetas_ruido01+3, , xlab = "X", ylab = "Y", 
     main="200000 iteraciones", xlim = c(-50,50), ylim = c(-50,50)) 
abline(a=perceptron_ruido04[1], b=perceptron_ruido04[2], lwd=1.5)
cat("Con un máximo de 20000 iteraciones, converge a ", 
    perceptron_ruido04[3],"iteraciones.")

## ------------------------------------------------------------------------
# Dibujamos los puntos en la gráfica 
plot (datos02, main = "PLA con vector aleatorio (ruido)", col = etiquetas_ruido01+3, xlab = "X", 
      
      ylab = "Y", xlim = c(-50,50), ylim = c(-50,50))

# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones y el vector inicial a 0
# Ahora usamos las etiquetas con ruido
perceptron_ruido05 = (ajusta_PLA(datos02, etiquetas_ruido01, 2000, runif(3)))

# Representamos la recta hecha por el perceptron
abline(a=perceptron_ruido05[1], b=perceptron_ruido05[2], lwd=1.5) 

# Mostramos el número de iteraciones que hacen falta para converger
cat(perceptron_ruido05[3],"iteraciones para converger.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Guardamos los valores del PLA en una variable
# Le pasamos al perceptrón un máximo de 2000 iteraciones 
# y el vector con valores aleatorios entre [0,1]
perceptron_ruido06 = replicate(10, ajusta_PLA(datos02, etiquetas_ruido01, 2000, runif(3))[3])

# Mostramos el número de iteraciones que hacen falta para converger
# Realizamos la media de las 10 iteraciones
cat(mean(perceptron_ruido06),"iteraciones para converger.")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Leemos el fichero con los datos de entrenamiento 
digit.train <- read.table("datos/zip.train", quote="\"", comment.char="", 
                          stringsAsFactors=FALSE)

# Guardamos los 1 y los 5  
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]

# Etiquetas
digitos.train = digitos15.train[,1]  
ndigitos.train = nrow(digitos15.train)
  
# Se retira la clase y se monta una matriz de tamanio 16x16
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos.train,16,16))
grises.train = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

# Visualizamos las imágenes 
par(mfrow=c(2,2)) 

for(i in 1:4){
  imagen = grises[i,,16:1] # Se rotan para verlas bien
  image(z=imagen)
}
  
# Etiquetas correspondientes a las 4 imágenes
digitos.train[1:4] 

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
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
simetria = unlist(lapply(grises.train, simetria))

## ------------------------------------------------------------------------
# Calculamos la intensidad de la imagen
intensidad = unlist(lapply(grises.train, FUN=mean))

## ------------------------------------------------------------------------
# Primero convertimos las etiquetas 1 y 5 a 1 y -1, respectivamente
etiquetas03 <- digitos15.train$V1
etiquetas03 <- (etiquetas03-3)/2

# Dibujamos la nube de puntos
plot(x=intensidad, y=simetria, col=etiquetas03+3, xlab = "Intensidad", ylab = "Simetria", 
     main="Intensidad y Simetría")

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
  c(-w[2]/w[3], -w[1]/w[3])
}

# Juntamos los valores obtenidos de simetria e intensidad
datos.train <- cbind(intensidad, simetria)

# Calculamos la recta de regresión que separa ambas clases
recta_regresion01 = Regress_Lin(datos.train, etiquetas03)

# Mostramos los coeficientes del hiperplano
recta_regresion01

## ------------------------------------------------------------------------
# Dibujamos los puntos
plot(x=intensidad, y=simetria, col=etiquetas03+3, xlab="Intensidad", 
     ylab="Grado Simetria", main="Recta Regresión")

# Dibujamos la recta
curve(0.2038923*x+0.4775669, add=T)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos las etiquetas aleatoriamente
datos04 <- simula_unif(N = 100, dim = 2, rango = c(-10:10))

# Generamos una recta
recta03 <- simula_recta(intervalo = c(-10:10))

# Etiquetamos los puntos a partir de la función de la recta
etiquetas04 <- sign(datos04[,2] - recta03[1]*datos04[,1] - recta03[2])

# Calculamos la recta de regresión que separa ambas clases
recta_regresion02 <- Regress_Lin(datos04, etiquetas04)

# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion01 <- sign(datos04[,2] - 
                                recta_regresion02[2]*datos04[,1] -
                                recta_regresion02[2])

# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
Ein = sum(etiquetas04 != etiquetas_regresion01) / length(etiquetas04)

Ein

## ------------------------------------------------------------------------
# Leemos el fichero con los datos de test 
digit.test <- read.table("datos/zip.test", quote="\"", comment.char="", 
                         stringsAsFactors=FALSE)
 
# Guardamos los 1 y los 5  
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]

# Etiquetas
digitos.test = digitos15.test[,1]  
ndigitos.test = nrow(digitos15.test)
  
# Se quita la clase y se monta una matriz 16x16
grises = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos.test,16,16))
grises.test = lapply(seq(dim(grises)[1]), function(m) {matrix(grises[m,,],16)})

# Recordamos que las etiquetas que tenemos son 5 y 1
# por tanto las pasamos a 1 y -1
etiquetas.test <- digitos15.test$V1
etiquetas.test <- (etiquetas.test-3)/2

## ------------------------------------------------------------------------
# Creamos un array con la media de la imagen
intensidad = unlist(lapply(grises.test, FUN=mean))

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
simetria = unlist(lapply(grises.test, simetria))

# Juntamos los valores obtenidos de simetria e intensidad
datos.test <- cbind(intensidad, simetria)

# Calculamos la recta de regresión que separa ambas clases
recta_regresion02 = Regress_Lin(datos.test, etiquetas.test)

# Mostramos los coeficientes del hiperplano
recta_regresion02

## ------------------------------------------------------------------------
# Dibujamos los puntos
plot(x=intensidad, y=simetria, col=etiquetas.test+3, xlab="Intensidad", 
     ylab="Grado Simetria", main="Recta Regresión")

# Dibujamos la recta
curve(0.1293491*x+0.4701312, add=T)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos los datos aleatoriamente
datos05 <- simula_unif(N = 100, dim = 2, rango = c(-10:10))

# Generamos una recta 
recta04 <- simula_recta(intervalo = c(-10:10))

# Etiquetamos las etiquetas a partir de la función de la recta
etiquetas05 <- sign(datos05[,2] - recta04[1]*datos05[,1] - recta04[2])

# Usamos la recta de regresión creada en los datos de entrenamiento
etiquetas_regresion02 <- sign(datos05[,2] - 
                                recta_regresion02[2]*datos05[,1] -
                                recta_regresion02[2])
# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
Eout = sum(etiquetas05 != etiquetas_regresion02) / length(etiquetas05)

Eout

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)

# Generamos los datos aleatoriamente
datos06 = simula_unif(N=1000, dim = 2, rango=c(-1,1))

# Dibujamos la nube de puntos
plot (datos06, col = 2, xlab = "X", ylab = "Y", xlim =c(-1,1), ylim=c(-1,1), 
      main="Muestra de entrenamiento")

## ------------------------------------------------------------------------
# Aplicamos la función a cada punto y almacenamos el signo del resultado en una lista
etiquetas06 = sign((datos06[,1]+0.2)^2+datos06[,2]^2-0.6)
# con esto acabamos de etiquetar cada punto 

# Introducimos el ruido en las etiquetas
etiquetas_ruido02 = generar_ruido(etiquetas06,10)

# Representamos los puntos
plot (datos06, col = etiquetas_ruido02+3, xlab = "X", ylab = "Y", xlim =c(-1,1), ylim=c(-1,1), 
      main="Muestra de entrenamiento con ruido")

## ------------------------------------------------------------------------
# Añadimos a los datos una columna 
datos07 <- cbind(1, datos06)

# Calculamos los coeficientes del hiperplano
recta_regresion04 = Regress_Lin(datos07, etiquetas_ruido02)

# Los mostramos por pantalla
recta_regresion04

## ------------------------------------------------------------------------
# Dibujamos la nube de puntos
plot(datos06, col=etiquetas_ruido02+3, xlab = "X", ylab = "Y", xlim =c(-1,1), ylim=c(-1,1),
     main="Recta de regresión")

# Dibujamos la recta
curve(-0.008209552*x-0.008209552, add=T)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Etiquetamos los puntos a partir de la recta de regresión
etiquetas_regresion05 <- sign((datos07[,1]+0.2)^2+datos07[,2]^2-0.6)

# Calculamos Ein como las etiquetas que son distintas, partido el total de las etiquetas
ein1 = sum(etiquetas_ruido02 != etiquetas_regresion05) / length(etiquetas_ruido02)

ein1

## ------------------------------------------------------------------------
Regress_Lin1 <- function(datos, label) {
  # Añadimos una columna a los datos, para hacer el producto escalar
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
  
  # Calculamos la pseudoinversa que (X^T) * etiqueta
  pseudoinversa <- xx %*% t(x)
  
  w <- pseudoinversa %*% as.numeric(label)
  
  # Devolvemos los pesos
  c(w)
}

## ------------------------------------------------------------------------
# Establecemos una semilla por defecto, para la generación de números aleatorios
set.seed(3)

# Establecemos eout a cero
e1 = 0
# Establecemos ein a cero
e2 <- 0
 
# Ejecutar el experimento 1000 veces
for (i in seq(1,1000)){
  
  # Ein
  # -------------------------------------------------------------------------
  # Generamos aleatoriamente una lista de números aleatorios
  datos06 = simula_unif(N=1000, dim=2, rango=c(-1,1))
  # Clasificamos los puntos
  etiquetas06 = sign((datos06[,1]+0.2)^2+datos06[,2]^2-0.6)
  # Introducimos el ruido en las etiquetas
  etiquetas_ruido02 = generar_ruido(etiquetas06, 10)
  # Calculamos los coeficientes del hiperplano
  recta_regresion04 = Regress_Lin1(datos07, etiquetas_ruido02)
  # Etiquetamos los puntos a partir de la recta de regresión
  recta = c(-recta_regresion04[1]/recta_regresion04[3], 
            - recta_regresion04[2]/recta_regresion04[3])
  etiquetas_regresion05 <- sign(datos07[,2] - recta[1]*datos07[,1] - recta[2])
  # Calculamos el valor de Ein
  e2 = e2 + sum(etiquetas_ruido02 != etiquetas_regresion05) / length(etiquetas_ruido02)

  # Eout
  # -------------------------------------------------------------------------
  # Generamos aleatoriamente una lista de números aleatorios
  datos06.test = simula_unif(N=1000, dim = 2, rango=c(-1,1))
  # Clasificamos los puntos
  etiquetas07 = sign((datos06.test[,1]+0.2)^2+datos06.test[,2]^2-0.6)
  # Introducimos el ruido en las etiquetas
  etiquetas_ruido03 = generar_ruido(etiquetas07, 10)
  # Etiquetamos los puntos a partir de la recta de regresión ya creada
  etiquetas_regresion05.test <- sign(datos06.test[,2] - 
                                       recta[1]*datos06.test[,1] - recta[2])
  # Calculamos el valor de Eout
  e1 = e1 + sum(etiquetas_ruido03 != etiquetas_regresion05.test) /
    length(etiquetas_ruido03)
}
  
cat ("Ein:", e2/1000)
cat("Eout: ", e1/1000)

## ------------------------------------------------------------------------
ein4 <- 0

# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)
  
# Generamos los datos
datos08 <- simula_unif(N=1000,dim = 2, c(-10,10)) 
  
# Etiquetamos cada punto, partimos de la misma función mencionada en el experimento 1
etiquetas07 = sign((datos08[,1]+0.2)^2+ datos08[,2]^2-0.6)
  
# Introducimos el ruido en las etiquetas
etiquetas_ruido03 = generar_ruido(etiquetas07, 10)
  
# Añadimos a los datos, el vector de caracteristicas mencionado en el enunciado
datos08.train <- cbind(1, datos08[,1], datos08[,2], datos08[,1]*datos08[,2],
                       datos08[,1]^2, datos08[,2]^2)
  
# Calculamos los coeficientes del hiperplano
recta_regresion06 = Regress_Lin1(datos08.train, etiquetas_ruido03)
  
etiquetas_regresion05 <- sign(sum(1*recta_regresion06[2], 
                                  datos08.train[,2]*recta_regresion06[3], 
                                  datos08.train[,3]*recta_regresion06[4],
                                  datos08.train[,4]*recta_regresion06[5],     
                                  datos08.train[,5]*recta_regresion06[6], 
                                  datos08.train[,6]*recta_regresion06[7]))
    
# Medimos el error
ein4 =  0.001*length(which(etiquetas_regresion05 != etiquetas_ruido03))
  
cat("Ein: ", ein4)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3)
  
eout3 <- 0
ein4 <-0

# Ejecutar el experimento 1000 veces
for (i in seq(1,1000)){
  
  # Ein
  # -------------------------------------------------------------------------
  # Generamos los datos
  datos08 <- simula_unif(N=1000,dim = 2, c(-10,10)) 
  # Etiquetamos cada punto, partimos de la misma función mencionada en el experimento 1
  etiquetas07 = sign((datos08[,1]+0.2)^2+ datos08[,2]^2-0.6)
  # Introducimos el ruido en las etiquetas
  etiquetas_ruido03 = generar_ruido(etiquetas07, 10)
  # Añadimos a los datos, el vector de caracteristicas mencionado en el enunciado
  datos08.train <- cbind(1, datos08[,1], datos08[,2], datos08[,1]*datos08[,2], 
                         datos08[,1]^2, datos08[,2]^2)
  # Calculamos los coeficientes del hiperplano
  recta_regresion06 = Regress_Lin1(datos08.train, etiquetas_ruido03)
  etiquetas_regresion05 <- sign(sum(1*recta_regresion06[2],
                                    datos08.train[,2]*recta_regresion06[3], 
                                    datos08.train[,3]*recta_regresion06[4],
                                    datos08.train[,4]*recta_regresion06[5], 
                                    datos08.train[,5]*recta_regresion06[6], 
                                    datos08.train[,6]*recta_regresion06[7]))
  
  # Medimos el error dentro de la muestra
  ein4 = ein4 + 0.001*length(which(etiquetas_regresion05 != etiquetas_ruido03))
  
  # EOUT
  # -------------------------------------------------------------------------
  # Generamos los datos aleatoriamente
  datos09 = simula_unif(N=1000, dim = 2, rango=c(-1,1))
  # Etiquetamos cada punto, partimos de la misma función mencionada en el experimento 1
  etiquetas08 = sign((datos09[,1]+0.2)^2+ datos09[,2]^2-0.6)
  # Introducimos el ruido en las etiquetas
  etiquetas_ruido04 = generar_ruido(etiquetas08, 10)
  # Añadimos a los datos, el vector de caracteristicas mencionado en el enunciado
  datos09.test <- cbind(1, datos09[,1], datos09[,2], datos09[,1]*datos09[,2], 
                        datos09[,1]^2, datos09[,2]^2)
  # Calculamos los coeficientes del hiperplano
  etiquetas_regresion06 <- sign(sum(1*recta_regresion06[2],
                                    datos09.test[,2]*recta_regresion06[3], 
                                    datos09.test[,3]*recta_regresion06[4],  
                                    datos09.test[,4]*recta_regresion06[5], 
                                    datos09.test[,5]*recta_regresion06[6], 
                                    datos09.test[,6]*recta_regresion06[7]))
  
  # Calculamos el error fuera de la muestra
  eout3 = eout3 + 0.001*length(which(etiquetas_regresion06 != etiquetas_ruido04))
}

cat("Ein: ", ein4/1000)
cat("Eout: ", eout3/1000)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
error_in = 0
  
for (i in seq(1,1000)) {
  # Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
  # que al repetir la generación de los números, siempre obtengamos los mismos
  set.seed(3)
  
  # Generamos los datos
  datos09 <- simula_unif(N=100,dim = 2, c(-10,10)) 
  
  # Generamos la recta
  recta05 <- simula_recta(intervalo = c(-10,10)) 
  
  # Generamos las etiquetas dadas por la recta
  etiquetas08 <- sign(datos09[,2] - recta05[1]*datos09[,1] - recta05[2])
  
  # Homgeneizamos datos
  datos10 <- cbind(1, datos09) 
  
  # Obtenemos vector de pesos
  recta_regresion05 <- Regress_Lin(datos10, etiquetas08) 
  
  # Etiquetas dadas por el vector de pesos
  etiquetas_regresion06 <- sign(datos10[,2] - recta_regresion05[1]*datos10[,1] - recta_regresion05[2])
    
  # Medimos el error
  error_in = error_in + 0.01*length(which(etiquetas_regresion06 != etiquetas08))
  }
  
cat("Porcentaje ein: ", error_in/100, "%\n")

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
error_out = 0;
  
for (i in seq(1,1000)) {
  
  # Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
  # que al repetir la generación de los números, siempre obtengamos los mismos
  set.seed(3)  
  
  # Generamos los datos de entrenamiento
  datos11 <- simula_unif(N=100, 2, c(-10,10)) 
  
  # Generamos los datos de test
  datos11.test <- simula_unif(N=1000, 2, c(-10,10)) 
  
  # Generamos recta
  recta06 <- simula_recta(intervalo = c(-10,10)) 
    
  # Etiquetas de la recta para los datos de entrenamiento
  etiquetas09 <- sign(datos11[,2] - recta06[1]*datos11[,1] - recta06[2])  
  
  # Etiquetas de la recta para los datos de test
  etiquetas09.test <- sign(datos11.test[,2] - recta06[1]*datos11.test[,1]-recta06[2])   
    
  # Homogeneizamos datos
  datos12 <- cbind(datos11, 1) 
  datos12.test <- cbind(datos11.test, 1) 
  
  # Calculamos el vector de pesos
  recta_regresion06 <- Regress_Lin(datos12, etiquetas09)
  
  # Etiquetas dadas por el vector de pesos a los datos de test
  etiquetas_regresion.test <- sign(datos11.test[,2] - 
                                     recta_regresion06[1]*datos11.test[,1] - 
                                     recta_regresion06[2])
    
  # Calculamos el error medio
  error_out = error_out + 0.01*length(which(etiquetas_regresion.test != etiquetas09.test)) 
  }

cat("Porcentaje de Eout: ", error_out/1000)

## ------------------------------------------------------------------------
# Después de crear una gráfica o iniciar un apartado paramos la ejecución 3 segundos
Sys.sleep(3)

## ------------------------------------------------------------------------
# Cuando se utilizan números aleatorios, es recomedable establecer una semilla, para 
# que al repetir la generación de los números, siempre obtengamos los mismos
set.seed(3) 

# Gerenamos datos
datos13 <- simula_unif(10, 2, c(-10,10)) 

# Calculamos la recta
recta07 <- simula_recta(intervalo = c(-10,10)) 

# Obtenemos las etiquetas dadas por la muestra
etiquetas10 <- datos13[,2] - recta07[1]*datos13[,1] - recta07[2]  

# Calculamos el vector de pesos dado por la regresion
recta_regresion07 <- Regress_Lin(datos13, etiquetas10)

recta_regresion07

## ------------------------------------------------------------------------
iteraciones = replicate(1000, ajusta_PLA(datos13, etiquetas01, 1000,
                                         c(-0.7866537, -13.9696222, 1)))

cat(mean(iteraciones[3]),"iteraciones para converger.")

