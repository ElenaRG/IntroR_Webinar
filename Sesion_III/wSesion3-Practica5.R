# ==================  S E S I � N  3   - P R A C T I C A 5 ================== #

# =========================================================================== #
# == An�lisis Exploratorio de Datos (EDA)
# =========================================================================== #

# == Instalamos el paquete funModeling
# install.packages("funModeling")

# == Corremos las librer�as que usaremos en esta practica
library(funModeling)
library(tidyverse)

# == Cargamos la base de datos mtcars
data("mtcars")

# == Comenzamos con el an�lisis exploratorio

# =========================================================================== #
# == Paso 1: Es necesario ver c�mo est� compuesta la base que estudiaremos,
# ==         ya que esta composici�n es muy importante para saber qu� tipo
# ==         de herramientas es m�s conveniente usar
# =========================================================================== #

# == A continuaci�n veremos cuantas observaciones (filas) y 
# == varaibles (columnas) tiene nuestro dataframe
dim(mtcars) 

# == Ahora mandaremos llamar los nombres de las variables que contiene nuestro
# == dataframe
names(mtcars)

# == Con esta sentencia podemos ver una descripci�n de nuestro dataFrame
str(mtcars)

# ============ Descripci�n del nombre de cada una de las variables =========== #
# == mpg = millas por gal�n -> variable num�rica 
# == cyl = cilindros 
# == hp = caballos de fuerza 
# == disp = mide el volumen del motor y representa el poder que genera
# ==        el motor 
# == wt = peso (1000 lbs) 
# == qsec = 1/4 milla de tiempo (Tiempo que se demora el carro en recorrer 
# ==                             1/4 de milla) 
# == vs = motor (0= en V, 1= en L�nea) 
# == am = tipo de trasmisi�n (0=autom�tico, 1=manual) 
# == gear = n�mero de engranajes de la trasmisi�n 
# == carb = n�mero de carburadores

# == Mandamos llamar los 6 primeros datos de nuestro dataframe
head(mtcars)

# =========================================================================== #
# == Paso 2: Una vez que hemos observado el tipo de datos con el que se cuenta
# ==         podemos proceder a explorar la variable por variable, para ver su 
# ==         comportamiento
# =========================================================================== #

# == Correremos la sentencia summary que nos dar� un resumen de cada una de las
# == variables que contiene el dataframe
summary(mtcars)

# == Nota: Al contar con puras variables num�ricas, tendremos un resumen en 
# ==       el cual, podremos observar, el m�nimo, 1 cuartil, mediana, promedio, 
# ==       3 cuartil y el m�ximo, de cada una de las variables que contiene 
# ==       este dataframe

# == Tras observar los resultados anteriores, nos surgen preguntas sobre
# == cual ser�a la dispersi�n que tienen los datos de cada una de las 
# == variables

# == Dado lo anterior calcularemos la Varianza para cada una de las
# == variables que contiene nuestro dataframe
apply(mtcars, 2 , var)

# == Tambi�n calcularemos, su respectiva desviaci�n est�ndar
apply(mtcars, 2 , sd)


# == Ahora usaremos la funci�n rbind, para visualizar en un mismo resumen
# == el summary anterior, la varianza y su desviaci�n est�ndar
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

# == Con esta funci�n  podremos ver de manera gr�fica la correlaci�n existente entre
# == cada uno de los pares de variables que contiene nuestro dataframe
pairs(mtcars)

# == En el caso quisi�ramos visualizar son un n�mero determinado variables,
# == modificaremos la funci�n de la siguiente manera
pairs(~mpg+disp+drat+wt,
      data=mtcars,
      main="Matriz de dispersi�n Principales Variables 'mtcars'")

# == Con esta funci�n de la librer�a "funModeling" visualizaremos de manera 
# == rapida un histograma por cada una de las variables de nuestro dataframe
plot_num(mtcars)

# == Nota: recuera que siempre podr�s ver la documentaci�n de cada una de las 
# ==       funciones usando ? + NombreFunci�n

# =========================================================================== #
# == Paso 4: Una vez que tenemos una mejor compresi�n de nuestros datos,
# ==         siempre es recomendable, generar cortes por variables lo cual nos
# ==         ayudara a tener un mejor entendimiento de la informaci�n  
# =========================================================================== #

# == A continuaci�n observaremos la media, varianza y desviaci�n est�ndar de
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

# ==  Ahora veremos de manera gr�fica el histograma de la variable "disp = mide 
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
  ggtitle("Histograma de la potencia del motor por n�mero de cilindros")

# == Ahora imprimiremos el objeto grafDisp cortado por la variable cyl y vs  
grafDisp +
  theme_bw() +
  facet_grid(vs ~ cyl) +
  ggtitle("Histograma de la potencia del motor por n�mero de cilindros y tipo de motor")