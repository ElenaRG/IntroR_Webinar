## INTRODUCCIÓN a R


################################  S E S I Ó N   1 #############################################


###############################################################################################
## ASIGNACIÓN DE VARIABLES
###############################################################################################

# asignación del valor 4 al objeto 'n'
n <- 4

# reasignación del valor 7 al objeto 'n'
7 -> n

# asignación del resultado de una operación a la variable n1
n1 <- 5 + 3

(8*2)-1

# mostrar los objetos en memoria
ls()

# borrar la variable n1
rm(n1)

###############################################################################################
## TIPO DE DATOS
###############################################################################################

# la función class() se usa para saber de qué tipo es un dato
class("hola")
class(3)
class(9>8)

# también puede verificarse si un dato es de un tipo específico
is.integer(6.4)
is.numeric(6.4)
is.character("hola")

# ejemplos de coerción de datos
as.character(5)
as.numeric("dos")
as.factor(1)
as.factor("hombre")
as.numeric(TRUE)
as.numeric(FALSE)


###############################################################################################
## OPERADORES
###############################################################################################

## ARITMÉTICOS
#  + 	adición
#  - 	sustracción
#  *	multiplicación
#  / 	división
#  ^	potencia

# Adición
2+3
# Sustracción
3-7
# Multiplicación
6*4
# División
93/4

# cualquier operación aritmética con un dato tipo NA devuelve NA
NA - 15


## RELACIONALES
#  <	menor que
#  >	mayor que
#  <=	menor o igual que
#  >= 	mayor o igual que
#  ==	igual	a
#  !=	diferente de

# Menor que
7<2
# Mayor que
4>3
# Menor o igual que
5<=5
# Mayor o igual que
9>=1
# Igual a
5==7
# Diferente a
5!=7


## LÓGICOS
#  X & Y		Y lógico
#  X | Y 		O lógico
#  ! X		 NO lógico

# Y lógico
7<2 & 5>3
2<7 & 5>3
# O lógico
7<2 | 5<3
7<2 | 5>3
7>2 | 5>3
# NO lógico
!7<2  
!5>3

# NOTA: los resultados se muestran en la consola pero no se guardan en memoria 
#       a menos que se asignen a un objeto
  

###############################################################################################
## VECTORES
###############################################################################################

# asignación del objeto (vector) x1
x1 <- c(1,2,"a","b",10:16)
# visualización del vector x1
x1
# ¿qué tipo de vector es x1?
class(x1)

# vector de valores repetidos
x2 <- rep(1, 10)
x3 <- rep(c(1,4), c(3,2))
x4 <- rep(c(-2,7), each=4)

# vector de valores secuenciales
x5 <- seq(1:10)
x6 <- seq(2,  10, by=2)
x7 <- seq(1, -20, by=-5)

# concatenación de vectores
x8 <- c(x1,x7)

# longitud del objeto
length(x5)
length(x6)

#  cuarta coordenada de x1
x1[4]

# un índice negativo implica ignorar el elemento o elementos correspondientes
x1[-3]

# coordenada 2, 5 y 11 de x1
x1[c(2,5,11)]
#  coordenada 2 a la 5 de x1
x1[2:5]
# vector x1 menos los elementos 1, 10 y 11
x1[-c(1,10,11)]
x1[c(-1,-10,-11)]

# muestra de x1, tamaño 4, CON repetición   
# Nota personal: correr varias veces
sample(x1,size=4,replace=TRUE)

# muestra de x1, tamaño 5, SIN repetición   
sample(x1,size=5,replace=FALSE)


###############################################################################################
## OPERACIONES y FUNCIONES CON VECTORES
###############################################################################################

x5
x6

length(x5)
length(x6)

# multiplicación por un escalar
x5*2

# suma/resta de un vector y un escalar
x6-3
x6+2

# suma de vectores
x5+x6

# producto de vectores
x5*x6

# potencia de un vector
w <- x6^2

# raiz cuadrada de un vector
sqrt(w)
w^(1/2)

# ¿si las longitudes son diferentes y no son multiplos?
x <- 1:8
y <- -2:3
length(x)
length(y)
x+y
x*y

# existen funciones predeterminadas para realizar ciertas operaciones.

