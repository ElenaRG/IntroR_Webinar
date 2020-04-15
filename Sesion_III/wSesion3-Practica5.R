# ==================  S E S I Ó N  3   - P R A C T I C A 5 ================== #

# =========================================================================== #
# == Análisis Exploratorio de Datos (EDA)
# =========================================================================== #

# == Instalamos el paquete funModeling
# install.packages("funModeling")

# == Corremos las librerías que usaremos en esta practica
library(funModeling)
library(tidyverse)

# == Cargamos la base de datos mtcars
data("mtcars")

# == Comenzamos con el análisis exploratorio

# =========================================================================== #
# == Paso 1: Es necesario ver cómo está compuesta la base que estudiaremos,
# ==         ya que esta composición es muy importante para saber qué tipo
# ==         de herramientas es más conveniente usar
# =========================================================================== #

# == A continuación veremos cuantas observaciones (filas) y 
# == varaibles (columnas) tiene nuestro dataframe
dim(mtcars) 

# == Ahora mandaremos llamar los nombres de las variables que contiene nuestro
# == dataframe
names(mtcars)

# == Con esta sentencia podemos ver una descripción de nuestro dataFrame
str(mtcars)

# ============ Descripción del nombre de cada una de las variables =========== #
# == mpg = millas por galón -> variable numérica 
# == cyl = cilindros 
# == hp = caballos de fuerza 
# == disp = mide el volumen del motor y representa el poder que genera
# ==        el motor 
# == wt = peso (1000 lbs) 
# == qsec = 1/4 milla de tiempo (Tiempo que se demora el carro en recorrer 
# ==                             1/4 de milla) 
# == vs = motor (0= en V, 1= en Línea) 
# == am = tipo de trasmisión (0=automático, 1=manual) 
# == gear = número de engranajes de la trasmisión 
# == carb = número de carburadores

# == Mandamos llamar los 6 primeros datos de nuestro dataframe
head(mtcars)

# =========================================================================== #
# == Paso 2: Una vez que hemos observado el tipo de datos con el que se cuenta
# ==         podemos proceder a explorar la variable por variable, para ver su 
# ==         comportamiento
# =========================================================================== #

# == Correremos la sentencia summary que nos dará un resumen de cada una de las
# == variables que contiene el dataframe
summary(mtcars)

# == Nota: Al contar con puras variables numéricas, tendremos un resumen en 
# ==       el cual, podremos observar, el mínimo, 1 cuartil, mediana, promedio, 
# ==       3 cuartil y el máximo, de cada una de las variables que contiene 
# ==       este dataframe

# == Tras observar los resultados anteriores, nos surgen preguntas sobre
# == cual sería la dispersión que tienen los datos de cada una de las 
# == variables

# == Dado lo anterior calcularemos la Varianza para cada una de las
# == variables que contiene nuestro dataframe
apply(mtcars, 2 , var)

# == También calcularemos, su respectiva desviación estándar
apply(mtcars, 2 , sd)


# == Ahora usaremos la función rbind, para visualizar en un mismo resumen
# == el summary anterior, la varianza y su desviación estándar
rbind(
  summary(mtcars),
  apply(mtcars, 2 , function(x) paste0('Var    :' ,round(var(x),2))),
  apply(mtcars, 2 , function(x) paste0('sd     :' ,round(sd(x),2)))
)

# == Visualizaremos la tabla de correlaciones para cada par de variables
# == en nuestro dataframe
cor(mtcars)

# =========================================================================== #
# == Paso 3: Ahora comenzaremos a visualizar el comportamiento de nuestro
# ==         dataframe
# =========================================================================== #

# == Con esta función  podremos ver de manera gráfica la correlación existente entre
# == cada uno de los pares de variables que contiene nuestro dataframe
pairs(mtcars)

# == En el caso quisiéramos visualizar son un número determinado variables,
# == modificaremos la función de la siguiente manera
pairs(~mpg+disp+drat+wt,
      data=mtcars,
      main="Matriz de dispersión Principales Variables 'mtcars'")

# == Con esta función de la librería "funModeling" visualizaremos de manera 
# == rapida un histograma por cada una de las variables de nuestro dataframe
plot_num(mtcars)

# == Nota: recuera que siempre podrás ver la documentación de cada una de las 
# ==       funciones usando ? + NombreFunción

# =========================================================================== #
# == Paso 4: Una vez que tenemos una mejor compresión de nuestros datos,
# ==         siempre es recomendable, generar cortes por variables lo cual nos
# ==         ayudara a tener un mejor entendimiento de la información  
# =========================================================================== #

# == A continuación observaremos la media, varianza y desviación estándar de
# == la variable caballos de fuerza, misma que contrastaremos el total y su 
# == apertura usando la variable cilindraje

# == A Total
mtcars %>%
  summarise(mean = mean(hp),
            var = var(hp),
            sd = sd(hp))

# == Cortada por la variable cilindraje
mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(hp),
            var = var(hp),
            sd = sd(hp))

# ==  Ahora veremos de manera gráfica el histograma de la variable "disp = mide 
# ==  el volumen del motor y representa el poder que genera" y la contrastaremos
# ==  contra el cilindraje que tiene

# == Crearemos un histograma general y lo guardaremos en el siguiente objeto
grafDisp <- mtcars %>%  
  ggplot(aes(x=disp)) +
  geom_histogram(color='blue', fill='blue') +
  xlab("Volumen del motor") + 
  ylab("Frecuencia")

# == Ahora imprimiremos el objeto grafDisp a total  
grafDisp +
  theme_bw() +
  ggtitle("Histograma de la potencia del motor a total")

# == Ahora imprimiremos el objeto grafDisp cortado por la variable cyl
grafDisp +
  theme_bw() +
  facet_wrap(~ cyl) +
  ggtitle("Histograma de la potencia del motor por número de cilindros")

# == Ahora imprimiremos el objeto grafDisp cortado por la variable cyl y vs  
grafDisp +
  theme_bw() +
  facet_grid(vs ~ cyl) +
  ggtitle("Histograma de la potencia del motor por número de cilindros y tipo de motor")