# ============================  S E S I � N   2 ============================= #

# =========================================================================== #
# == Paso 1 - Selecci�n de Cran Mirror (URL donde se van a descargar los
# ==          paquetes), nostros recomendamos la de RStudio, aunque en 
# ==          M�xico tambi�n tenemos un par como la de la UNAM y el ITAM
# == Paso 2 - Instalaci�n de Paquetes
# == Paso 3 - Instalaci�n de Paquetes
# =========================================================================== #

# == Paso 1 (Este paso solo se tiene que setear una sola vez o cuando se desea
#            cambiar de mirror por alguna raz� especifica)
options(repos="https://cran.rstudio.com")

# == Paso 2 (Este paso solo se realiza una vez, que es cuando se carga el 
# ==         paquete por primera vez)
install.packages("readxl")

# == Paso3 (Este paso se tiene que correr una sola vez en el archivo de 
# ==        trabajo cuando se quiere usar la libreria que contiene las
# ==        las funciones que se usar�n para trabajar)
library("readxl")

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Establecer ruta de trabajo y cargar una base
# =========================================================================== #

rutaLocal <- 'C:/webinar/'
rutaOnline <- 'https://raw.githubusercontent.com/ElenaRG/IntroR_Webinar/master/DataFrames/'

# == Establecer una ruta de trabajo local
setwd(rutaLocal)

# == Verificar la ruta de trabajo en la que se est� trabajando
getwd()

# == Cargar las librer�as necesarias para importar una base
# == Los paquetes o librer�as son una colecci�n de funciones con una estructura bien definida 
# == dise�adas para resolver una tareas espec�ficas. 
library("readxl")
library("foreign")

# == Cargar una base CSV despu�s de establecer la ruta de trabajo
base1 <- read.csv('cars.csv', header = TRUE)

# == Cargar una base si no se ha establecido la ruta de trabajo
base1.1 <- read.csv(paste0(rutaOnline, 'cars.csv'), header = TRUE)
base1.2 <- read.csv(file.choose(), header=TRUE)

# == Cargar una base en Excel
# == el segundo par�metro indica la hoja que va a importarse
base2 <- read_excel(paste0(rutaLocal, 'flores.xlsx'), 1)

# == Cargar una base en archivo de texto
tv <- read.table(paste0(rutaOnline, 'tv.txt'), header = TRUE)

# == Ejemplo de bases preexistentes en R
library(help = "datasets")
auto <- mtcars
flor <- iris
survivor <- Titanic

rm(list=ls(pat="b"))

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Revisi�n preliminar de datos
# =========================================================================== #

# == mostrar la dimensi�n o tama�o de la base
dim(auto)
dim(tv)
dim(flor)

# == verificar los nombres de las variables o campos en una base
names(auto)
names(tv)
names(flor)

# == verificar estructura de los campos, de qué tipo son los datos
str(auto)

library("tibble")
glimpse(tv)

# == si la base no tiene muchos campos se puede visualizar una muestra de ella
head(auto)
tail(flor)
head(tv, 5)
tail(auto, 3)

# == resumen de las variables de una base de datos
# == si algun campo no es n�merico devuelve las frecuencias
summary(auto)
summary(tv)

# == tambi�n aplica para resumir una variable
# == para acceder a una variable la sintaxis es: base + $ + nombre de la variable
# == ej. auto$gear   mostrar� el contenido de la variable "gear" de la base "auto"
summary(auto$gear)

# == an�lisis de frecuencias de una variable
table(auto$gear)
as.data.frame(table(auto$gear))

# == si se desea mostrar las frecuencias como proporciones entonces usar prop.table
prop.table(table(auto$gear))

# == suma de los elementos de un vector
sum(tv$ninos)
cumsum(tv$ninos)

# == m�nimo, m�ximo, media, varianza, desviaci�n est�ndar, de un vector
min(auto$cyl)
max(auto$gear)
mean(auto$hp)
var(auto$mpg)
sd(auto$mpg)

# == creaci�n de nuevos vectores (campos) en una base de datos
# == Ej. variable "kpg" = kil�metros por gal�n
# == para ello se convierten las millas por gal�n a kil�metros 
auto$kpg <- auto$mpg*1.60934
auto$vs <- as.logical(mtcars$vs)
head(auto)

# == guardar la base en una ruta establecida
getwd()
write.csv(auto, "auto_modif.csv", row.names=FALSE)

# == si se desea guardar en una ruta diferente habr� de especificarse
write.csv(auto,"C:/webinar/auto_modif.csv", row.names=FALSE)

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Revisi�n preliminar de datos usando gr�ficos
# =========================================================================== #

setwd('C:/webinar')
getwd()
auto <- mtcars

str(auto)
auto$car <- row.names(auto)
str(auto)

# ====================== GR�FICOS B�SICOS

# == Gr�fico de Puntos 
plot(auto$mpg)

# == gr�fico de puntos para comparar el cilindraje vs caballos de fuerza
plot(auto$cyl, auto$hp)

plot(auto$cyl, auto$hp, col="red")

plot(auto$cyl, auto$hp,
     col  = "red",
     xlab = "cilindros", 
     ylab = "caballos de fuerza",
     main = "Relaci�n de cilindros y millas por gal�n")

# == CONCLUSI�N: visualmente parece haber una asociaci�n positiva entre el 
# ==             n�mero de cilindros y los caballos de fuerza del auto

# == gr�fico de puntos para comparar los caballos de fuerza vs 
# == las millas por gal�n
plot(auto$hp, auto$mpg,
     col  = "blue",
     xlab = "Caballos de fuerza", 
     ylab = "Millas por gal�n",
     main = "Caballos de fuerza vs millas por gal�n")

# == CONCLUSI�N: visualmente parece haber una asociaci�n negativa entre 
# ==             los caballos de fuerza y el rendimiento del combustible

# == Gr�fico de Barras de las frecuencias de autos seg�n su n�mero 
# == de velocidades
barplot(table(auto$gear))

barplot(table(auto$gear), col="blue")

barplot(table(auto$gear), col=c(5:7))

barplot(table(auto$gear), 
        col=c("blue","red","grey"), 
        main = "Gr�fico de barras",
        xlab = "num casos",
        ylab = "Velocidades", 
        horiz = TRUE)
# == CONCLUSI�N: en la base de datos la mayor�a de los autos son 
# ==             de 3 velocidades

# == Histograma de millas por gal�n
hist(auto$mpg)

# == Boxplot o Gr�fica de caja y brazos
# == gr�fica de caja y brazos para los caballos de fuerza
boxplot(auto$hp,
        xlab="Caballos de fuerza",
        main="Boxplot de la variable hp en la base 'auto'",
        col = "yellow",
        horizontal = TRUE)

# == CONCLUSI�N: la mitad de los datos esta entre 50 y 120 caballos de 
# ==             fuerza aprox. existe mayor dispersi�n en los valores 
# ==             altos de la variable existe un dato at�pico en los valores 
# ==             altos de la variable

# == gr�fica de caja y brazos para comparar las millas por gal�n para cada 
# == n�mero de cilindros
boxplot(auto$mpg~auto$cyl,
        xlab = "N�mero de cilindros",
        ylab = "Millas por gal�n",
        main="Millas por gal�n seg�n el n�mero de cilindros")

# == CONCLUSI�N: los autos de menor n�mero de cilindros tienen mayor 
# ==             rendimiento de combustible los autos de 4 cilindros tienen
# ==             mayor variabiliad en las millas por galos, los que menos 
# ==             dispersi�n tienen son los de 6 cilindros, para los autos de
# ==             8 cilindros hay un dato at�pico en los valores peque�os

# == Combinar varios gr�ficos en una pantalla
# == la primer coordenada corresponde al n�mero de filas en que se divide la 
# == pantalla y la segunda al n�mero de columnas

par(mfrow=c(2,1))  
boxplot(auto$mpg~auto$cyl, main="Millas por gal�n seg�n el n�mero de cilindros")
boxplot(auto$hp~auto$cyl, main="Caballos de fuerza seg�n el n�mero de cilindros")

par(mfrow=c(1,2))
boxplot(auto$mpg~auto$cyl, main="Millas por gal�n seg�n el n�mero de cilindros")
boxplot(auto$hp~auto$cyl, main="Caballos de fuerza seg�n el n�mero de cilindros")

par(mfrow=c(1,1))  # para regresar la pantalla a un gr�fico

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Estad�sticos b�sicos
# =========================================================================== #

# == gr�ficamente se observ� una asociaci�n negativa entre los cilindros y 
# == las millas por gal�n
# == gr�ficamente se observ� una asociaci�n positiva entre los cilindros y 
# == las millas por gal�n
# == �realmente hay asociaci�n? ¿en caso afirmativo que tan fuerte es?  
# == Para verificar la correlaci�n entre variables se usa la funci�n cor()

cor(auto$cyl, auto$mpg)
# == CONCLUSI�N: efectivamente existe una fuerte asociaci�n negativa entre el
# ==             n�mero de cilindros y el rendimiento del combustible

cor(auto$cyl, auto$hp)
# == CONCLUSI�N: efectivamente existe una fuerte asociaci�n positiva entre el 
# ==             n�mero de cilindros y el rendimiento del combustible

# == en el boxplot de n�mero de cilindros vs rendimiento de combustible
# == se observ� un dato at�pico 
boxplot(auto$mpg~auto$cyl,
        xlab = "N�mero de cilindros",
        ylab = "Millas por gal�n",
        main="Millas por gal�n seg�n el n�mero de cilindros",
        col = c("red", "blue", "yellow"))
# == �cu�les son los outliers (mpg) para los autos de 8 cilindros?

# == visualizar la base "autos" para aquellos de 8 cilindros
head(auto)
auto[auto$cyl==8,]

# == visualizar los campos "car" y "mpg" de la base "autos" 
library("dplyr")
select(auto, 1, 2)
select(auto, mpg, cyl)
auto[, c(1,2)]

# == visualizar los campos "car", "mpg" y "cyl" de la base "autos" para 
# == aquellos de 8 cilindros
select(auto[auto$cyl==8,], 1:3)
auto[auto$cyl==8, c(1,2,4)]

# == calcular el promedio de las millas por gal�n para toda la base de datos
mean(auto$mpg)

# == calcular el promedio de las millas por gal�n para los autos de 8 cilindros
mean(auto[auto$cyl==8,]$mpg)

# == visualizar/filtrar la base "autos" para aquellos de 8 cilindros y mpg 
# == menor al promedio
filter(auto, auto$cyl==8 & auto$mpg<15.1)
auto[auto$cyl==8 & auto$mpg<15.1, ]

# == ordenar la base "auto" por la variable "mpg"  (ascendente)
arrange(auto, mpg)

# == ordenar la base "auto" por la variable "mpg"  (descendente)
arrange(auto, desc(mpg))

# == ordenar la base "auto" por la variable "mpg" para aquellos autos con 8
# == cilindros
arrange(auto[auto$cyl==8,],mpg)

# == CONCLUSI�N: ahora es m�s f�cil ver que los autos "Cadillac Fleetwood" y 
# ==             "Lincoln Continental" son los autos de 8 cilindros con 
# ==             rendimiento de combustible at�pico


# == MUTATE para modificar la estructura de una base de datos

# == convertir las millas por gal�n a kil�metros y agregar la nueva variable a
# == la base "autos"
auto$kpg <- auto$mpg*1.60934
head(auto)

# utilizando la funci�n mutate
auto <- mutate(auto, kpg2=mpg*1.60934)
head(auto)

# estandarizar la variable kpg
mutate(auto, kpg_est = (kpg - mean(kpg)) / sd(kpg) )
head(auto)

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Creaci�n de reportes
# =========================================================================== #

# ============= GROUP_BY  &  SUMMARIZE

# == hacer un resumen del promedio de "mpg" y "hp" por cilindraje

#  == 1. agrupar la base por el n�mero de cilindros
resumen <- group_by(auto, cyl)

# == 2. calcular la media de las variables deseadas por cilindraje
summarise(resumen, mean(mpg), mean(hp))

# == 3. resumen renombrando las variables al generarlas
resumen <- summarise(resumen, 
                     casos    = n(),
                     prom_mpg = mean(mpg), 
                     prom_hp  = mean(hp), 
                     prom_kpg = mean(mpg*1.60934))
resumen
as.data.frame(resumen)

# == 4. guardar el reporte en un archivo
getwd()
write.csv(as.data.frame(resumen), "resumen.csv", row.names=TRUE)