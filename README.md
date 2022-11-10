# CalidadDeAire
Desarrollo

Para este análisis de datos contamos con datos tomados de la base de datos anuales de la REDMA que contiene información de las concentraciones de contaminantes que se muestrean durante 24 horas, cada 6 días.
Los contaminantes que se miden en la REDMA se definen en la siguiente tabla.
 
Como ya se había mencionado anteriormente los datos que trataremos son las Partículas menores a 10 micrómetros (PM10).
Vamos a evaluar el nivel de contaminación de estas medidas de acuerdo con el IMECA.

Preparación de los datos
Comencemos leyendo nuestros datos y analizándolos, utilizaremos los datos del año 2016. Para esto necesitamos de la librería readxl (para leer el archivo).
  > PM10 <- read_excel("D:2016PM10.xls", na = "-99")

Podemos echar un vistazo al contenido de nuestra tabla.
  > head(PM10)
 
La columna Fecha nos indica el día en que las mediciones en cada estación fueron tomadas.
Las demás columnas corresponden a las mediciones las cuales están medidas en µg/m3 = microgramos/metro cúbico.
Queremos colocar la fecha como nombre de nuestras filas, si lo hacemos en el formato en el que está obtendremos un formato no deseado, por lo que procedemos a convertir la fecha en un tipo estándar de fecha y después convertimos nuestra tabla en un dataframe.
  > PM10$FECHA <- as.Date(PM10$FECHA)
  > 
  > PM10 <- as.data.frame(PM10)
  > 
  > row.names(PM10) <- PM10$FECHA
  > 
  > PM10 <- PM10[-1]

En nuestros datos existen valores perdidos, pero para saber cuántos son y de qué estación podemos hacer uso de la función sapply, nuestro resultado después de aplicarla es el siguiente.
  > sapply(PM10, function(x) sum(is.na(x)))
 
En total tenemos 61 observaciones en todo el año, por lo que es conveniente eliminar la columna de la estación LPR y de la estación SHA, ya que según la cantidad de datos faltantes la estación pudo haber fallado y los datos ya no son confiables.
En el caso de los demás datos faltantes por esta ocasión se llenarán con el promedio de su columna, esto ya porque los datos faltantes son pocos y no afectarán en gran escala el desempeño de nuestro algoritmo. Para realizar esta operación se propone el siguiente tramo de código.
  > for(i in 1:7){
  >
  >   +PM10[,i] <- ifelse(is.na(PM10[,i]),
  >
  >   +mean(PM10[,i], na.rm = TRUE),
  >
  >   +PM10[,i]
  >   
  >   +)
  >   
  >   +}

Lo que se hace es verificar por cada registro de nuestro dataframe si el valor es nulo, de ser así se coloca la media de su columna correspondiente, de lo contrario se asigna el valor que contiene actualmente.
Como siguiente paso vamos a calcular el valor total de PM promediando el valor obtenido por estación en cada una de las fechas registradas, esto con la finalidad de saber qué cantidad de PM existe en la ciudad de México, a este promedio es al que le aplicaremos el cálculo IMECA.
Lo logramos con la siguiente operación.
  > PM10$TOTAL = rowMeans (PM10[, 1 :7])

Para poder calcular la calidad del aire con IMECA creamos la siguiente función.
  >  IMECA <- function(x){
  >
  >    +if(x > 0 && x <= 120)
  >
  >    +return(x*0.833)
  >
  >    +if(x > 121 && x <= 320)
  >
  >    +return((x*0.5)+40)
  >
  >    +if(x > 320)
  >
  >    +return(x*0.625)
  >
  > +}

Esta función recibe como parámetro el valor registrado por cada estación en una fecha específica, después verifica dentro de qué rango se encuentra y en base a eso se opera ese valor como lo indica la norma de calidad.
Aplicamos esta función en nuestra columna de totales recorriendo cada fila de ella y llamando a la función IMECA en cada iteración.
Obtenemos como resultado el siguiente dataframe.
 
Donde las columnas 1-7 son los registros en µg/m3 de las estaciones y la columna 8 es el IMECA en México respecto a las mediciones antes mencionadas.

Análisis propuesto
Una vez preparados nuestros datos procedemos a analizarlos, en este documento se propone la utilización de un algoritmo de clasificación, en específico el algoritmo de regresión lineal múltiple.
Así que vamos a verificar si los datos están correlacionados, lo hacemos de estas dos formas, una visual y otra escrita.
  > pairs(PM10)

Y obtenemos la siguiente gráfica, en la que podemos observar si nuestros datos tienen una tendencia lineal respecto a los demás. 
  > cor(PM10)

Con esta función obtenemos qué tan relacionados están nuestros datos, mientras más relacionados estén el algoritmo será más eficaz.
 
La mayoría de los datos están relacionados entre sí y por supuesto con la variable de salida (Total).

Con el análisis hecho hasta ahora podemos decir que utilizar regresión lineal múltiple para interpretar y realizar predicciones sobre nuestros datos es la decisión correcta.
Para llevar a cabo la creación de nuestro modelo elegimos una muestra de nuestros datos, la cual será llamada conjunto de entrenamiento y constará del 60% de nuestros datos, para hacer nuestras predicciones y validar la efectividad de nuestro modelo utilizaremos el 40% restante de nuestros datos.

Así que creamos nuestro modelo. 
  > regresion <- lm(PM10.train$TOTAL ~ .,data = PM10.train)
  >
  > summary(regresion)
 
El coeficiente de determinación (es decir, el coeficiente de correlación al cuadrado) mide la bondad del ajuste de la recta a los datos. A partir de la salida anterior, vemos que su valor en este caso es de 1, además el valor de p está muy por debajo de 0, lo que hace de nuestro modelo un modelo útil.

Pruebas
Ahora bien, para saber si debemos ocupar todas las variables para el armado de nuestro modelo, utilizamos la búsqueda exhaustiva con ayuda de la función regsubsets de la librería leaps.
La idea aquí es evaluar todos los subconjuntos de predictores. Dado que el número de subconjuntos incluso para valores moderados de p es muy grande, después de que el algoritmo crea los subconjuntos y ejecuta todos los modelos, necesitamos alguna forma de examinar los subconjuntos más prometedores y seleccionarlos. 
  > search <- regsubsets(PM10.train$TOTAL ~ .,data = PM10.train, nbest = 1, nvmax = dim(PM10.train)[2], method = "exhaustive")
  >
  > sum <- summary(search)
  >
  > sum$adjr2
 
El R2adj aumenta con la utilización de los 7 predictores, no encontramos un punto de estabilización ya que los 7 predictores son los óptimos para el modelo, por lo que concluimos en que utilizar todos los predictores con los que contamos es lo mejor.

 Para probar nuestro modelo vamos a predecir sobre nuestro conjunto de datos de validación, que consta del 40 por ciento de nuestros datos originales, vamos a medir la precisión del algoritmo con la función RMSE, de la librería caret, que mide el error promedio realizado por el modelo al predecir el resultado de una observación. Matemáticamente, el RMSE es la diferencia cuadrática promedio entre los valores de salida reales observados y los valores predichos por el modelo. Cuanto menor sea el RMSE, mejor será el modelo.
  > RMSE(predict(regresion, PM10.valid), PM10.valid$TOTAL)
  
  [1] 1.484436e-14

Obtenemos un error demasiado bajo, por lo que nuestro modelo es óptimo.

Podemos ocupar otro conjunto de datos para predecir el IMECA, en este caso utilizaremos lo datos tomados del año 2020.
  > predict(regresion, PM20)
 
Podemos observar que para los datos tomados hasta septiembre del año en curso, en el mes de Enero se mantuvo en un IMECA total de 51.051 lo cual cae en el rango regular.
