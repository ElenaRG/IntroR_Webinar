# ============================  S E S I Ó N   2 ============================= #

# =========================================================================== #
# == Paso 1 - Selección de Cran Mirror (URL donde se van a descargar los
# ==          paquetes), nostros recomendamos la de RStudio, aunque en 
# ==          México también tenemos un par como la de la UNAM y el ITAM
# == Paso 2 - Instalación de Paquetes
# == Paso 3 - Instalación de Paquetes
# =========================================================================== #

# == Paso 1 (Este paso solo se tiene que setear una sola vez o cuando se desea
#            cambiar de mirror por alguna razó especifica)
options(repos="https://cran.rstudio.com")

# == Paso 2 (Este paso solo se realiza una vez, que es cuando se carga el 
# ==         paquete por primera vez)
install.packages("readxl")

# == Paso3 (Este paso se tiene que correr una sola vez en el archivo de 
# ==        trabajo cuando se quiere usar la libreria que contiene las
# ==        las funciones que se usarán para trabajar)
library("readxl")

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Establecer ruta de trabajo y cargar una base
# =========================================================================== #

rutaLocal <- 'C:/webinar'
rutaOnline <- 'https://github.com/ElenaRG/IntroR_Webinar/tree/master/DataFrames'

# == Establecer una ruta de trabajo local
setwd(rutaLocal)

# == Verificar la ruta de trabajo en la que se está trabajando
getwd()

# == Cargar las librerías necesarias para importar una base
# == Los paquetes o librerías son una colección de funciones con una estructura bien definida 
# == diseñadas para resolver una tareas específicas. 
library("readxl")
library("foreign")

# == Cargar una base CSV después de establecer la ruta de trabajo
base1 <- read.csv('cars.csv', header = TRUE)

# == Cargar una base si no se ha establecido la ruta de trabajo
base1.1 <- read.csv(paste0(rutaOnline, 'cars.csv'), header = TRUE)
base1.2 <- read.csv(file.choose(), header=TRUE)

# == Cargar una base en Excel
# == el segundo parámetro indica la hoja que va a importarse
base2 <- read_excel(paste0(rutaOnline, 'flores.xlsx'), 1)

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
# == Revisión preliminar de datos
# =========================================================================== #

# == mostrar la dimensión o tamaño de la base
dim(auto)
dim(tv)
dim(flor)

# == verificar los nombres de las variables o campos en una base
names(auto)
names(tv)
names(flor)

# == verificar estructura de los campos, de quÃ© tipo son los datos
str(auto)

library("tibble")
glimpse(tv)

# == si la base no tiene muchos campos se puede visualizar una muestra de ella
head(auto)
tail(flor)
head(tv, 5)
tail(auto, 3)

# == resumen de las variables de una base de datos
# == si algun campo no es númerico devuelve las frecuencias
summary(auto)
summary(tv)

# == también aplica para resumir una variable
# == para acceder a una variable la sintaxis es: base + $ + nombre de la variable
# == ej. auto$gear   mostrará el contenido de la variable "gear" de la base "auto"
summary(auto$gear)

# == análisis de frecuencias de una variable
table(auto$gear)
as.data.frame(table(auto$gear))

# == si se desea mostrar las frecuencias como proporciones entonces usar prop.table
prop.table(table(auto$gear))

# == suma de los elementos de un vector
sum(tv$ninos)
cumsum(tv$ninos)

# == mínimo, máximo, media, varianza, desviación estándar, de un vector
min(auto$cyl)
max(auto$gear)
mean(auto$hp)
var(auto$mpg)
sd(auto$mpg)

# == creación de nuevos vectores (campos) en una base de datos
# == Ej. variable "kpg" = kilómetros por galón
# == para ello se convierten las millas por galón a kilómetros 
auto$kpg <- auto$mpg*1.60934
auto$vs <- as.logical(mtcars$vs)
head(auto)

# == guardar la base en una ruta establecida
getwd()
write.csv(auto, "auto_modif.csv", row.names=FALSE)

# == si se desea guardar en una ruta diferente habrá de especificarse
write.csv(auto,"C:/webinar/auto_modif.csv", row.names=FALSE)

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Revisión preliminar de datos usando gráficos
# =========================================================================== #

setwd('C:/webinar')
getwd()
auto <- mtcars

str(auto)
auto$car <- row.names(auto)
str(auto)

# ====================== GRÁFICOS BÁSICOS

# == Gráfico de Puntos 
plot(auto$mpg)

# == gráfico de puntos para comparar el cilindraje vs caballos de fuerza
plot(auto$cyl, auto$hp)

plot(auto$cyl, auto$hp, col="red")

plot(auto$cyl, auto$hp,
     col  = "red",
     xlab = "cilindros", 
     ylab = "caballos de fuerza",
     main = "Relación de cilindros y millas por galón")

# == CONCLUSIÓN: visualmente parece haber una asociación positiva entre el 
# ==             número de cilindros y los caballos de fuerza del auto

# == gráfico de puntos para comparar los caballos de fuerza vs 
# == las millas por galón
plot(auto$hp, auto$mpg,
     col  = "blue",
     xlab = "Caballos de fuerza", 
     ylab = "Millas por galón",
     main = "Caballos de fuerza vs millas por galón")

# == CONCLUSIÓN: visualmente parece haber una asociación negativa entre 
# ==             los caballos de fuerza y el rendimiento del combustible

# == Gráfico de Barras de las frecuencias de autos según su número 
# == de velocidades
barplot(table(auto$gear))

barplot(table(auto$gear), col="blue")

barplot(table(auto$gear), col=c(5:7))

barplot(table(auto$gear), 
        col=c("blue","red","grey"), 
        main = "Gráfico de barras",
        xlab = "num casos",
        ylab = "Velocidades", 
        horiz = TRUE)
# == CONCLUSIÓN: en la base de datos la mayoría de los autos son 
# ==             de 3 velocidades

# == Histograma de millas por galón
hist(auto$mpg)

# == Boxplot o Gráfica de caja y brazos
# == gráfica de caja y brazos para los caballos de fuerza
boxplot(auto$hp,
        xlab="Caballos de fuerza",
        main="Boxplot de la variable hp en la base 'auto'",
        col = "yellow",
        horizontal = TRUE)

# == CONCLUSIÓN: la mitad de los datos esta entre 50 y 120 caballos de 
# ==             fuerza aprox. existe mayor dispersión en los valores 
# ==             altos de la variable existe un dato atípico en los valores 
# ==             altos de la variable

# == gráfica de caja y brazos para comparar las millas por galón para cada 
# == número de cilindros
boxplot(auto$mpg~auto$cyl,
        xlab = "Número de cilindros",
        ylab = "Millas por galón",
        main="Millas por galón según el número de cilindros")

# == CONCLUSIÓN: los autos de menor número de cilindros tienen mayor 
# ==             rendimiento de combustible los autos de 4 cilindros tienen
# ==             mayor variabiliad en las millas por galos, los que menos 
# ==             dispersión tienen son los de 6 cilindros, para los autos de
# ==             8 cilindros hay un dato atípico en los valores pequeños

# == Combinar varios gráficos en una pantalla
# == la primer coordenada corresponde al número de filas en que se divide la 
# == pantalla y la segunda al número de columnas

par(mfrow=c(2,1))  
boxplot(auto$mpg~auto$cyl, main="Millas por galón según el número de cilindros")
boxplot(auto$hp~auto$cyl, main="Caballos de fuerza según el número de cilindros")

par(mfrow=c(1,2))
boxplot(auto$mpg~auto$cyl, main="Millas por galón según el número de cilindros")
boxplot(auto$hp~auto$cyl, main="Caballos de fuerza según el número de cilindros")

par(mfrow=c(1,1))  # para regresar la pantalla a un gráfico

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Estadísticos básicos
# =========================================================================== #

# == gráficamente se observó una asociación negativa entre los cilindros y 
# == las millas por galón
# == gráficamente se observó una asociación positiva entre los cilindros y 
# == las millas por galón
# == ¿realmente hay asociación? Â¿en caso afirmativo que tan fuerte es?  
# == Para verificar la correlación entre variables se usa la función cor()

cor(auto$cyl, auto$mpg)
# == CONCLUSIÓN: efectivamente existe una fuerte asociación negativa entre el
# ==             número de cilindros y el rendimiento del combustible

cor(auto$cyl, auto$hp)
# == CONCLUSIÓN: efectivamente existe una fuerte asociación positiva entre el 
# ==             número de cilindros y el rendimiento del combustible

# == en el boxplot de número de cilindros vs rendimiento de combustible
# == se observó un dato atípico 
boxplot(auto$mpg~auto$cyl,
        xlab = "Número de cilindros",
        ylab = "Millas por galón",
        main="Millas por galón según el número de cilindros",
        col = c("red", "blue", "yellow"))
# == ¿cuáles son los outliers (mpg) para los autos de 8 cilindros?

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

# == calcular el promedio de las millas por galón para toda la base de datos
mean(auto$mpg)

# == calcular el promedio de las millas por galón para los autos de 8 cilindros
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

# == CONCLUSIÓN: ahora es más fácil ver que los autos "Cadillac Fleetwood" y 
# ==             "Lincoln Continental" son los autos de 8 cilindros con 
# ==             rendimiento de combustible atípico


# == MUTATE para modificar la estructura de una base de datos

# == convertir las millas por galón a kilómetros y agregar la nueva variable a
# == la base "autos"
auto$kpg <- auto$mpg*1.60934
head(auto)

# utilizando la función mutate
auto <- mutate(auto, kpg2=mpg*1.60934)
head(auto)

# estandarizar la variable kpg
mutate(auto, kpg_est = (kpg - mean(kpg)) / sd(kpg) )
head(auto)

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Creación de reportes
# =========================================================================== #

# ============= GROUP_BY  &  SUMMARIZE

# == hacer un resumen del promedio de "mpg" y "hp" por cilindraje

#  == 1. agrupar la base por el número de cilindros
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