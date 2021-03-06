# ==================  S E S I Ó N  2   - P R A C T I C A 3 ================== #

# =========================================================================== #
# == MANEJO DE BASES DE DATOS
# == Tidyverse
# =========================================================================== #

# == Este c�digo solo corremos la primera vez instalamos el paquete tydiverse
# == y la librer�a reshape
# == Nota: si ya cuentas con estos paquetes instalados, solo comenta las
# ==       siguientes lineas y corre carga las librer�as a usar
install.packages("tidyverse")
install.packages("reshape") 

# == Aqu� vamos a cargar las librerias que necesitaremos para trabajar
library(tidyverse)
library(reshape)

# == iniciamos una variable que contendr� la ruta en la cual estaremos
# == tendremos nuestros archivos de trabajo e imprimiremos todas nuestras
# == salidas
rutaLocal <- 'C:/webinar/'

# == aqu� seteamos la ruta de donde se va a estar trabajando y comprobamos
# == que es correcta
setwd(rutaLocal)
getwd()

# == Mandamos a llamar la base precargada en R llamada mtcars y la guardamos 
# == como un objeto con el nombre autos
auto <- mtcars


# == Pipe: %>%
# == es un operador que permite encadenar funciones. 
# == Toma la salida de una funci�n y la pasa como entrada de la siguiente.
auto %>% count(cyl)
auto %>% count(cyl, hp)

# =========================================================================== #
# == PROBLEMA: �cu�les son los outliers (mpg) para los autos de 8 cilindros?
# =========================================================================== #

# == Paso 1: se seleccionaron los campos deseados para el an�lisis
auto %>%
  select(mpg, cyl, hp)
  
# == Paso 2: se crearon nuevas variables
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp)

# == Paso 3: se filtr� la base para mostrar aquellos autos de 8 cilindros y mpg
# ==         menor al promedio
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp) %>%
  filter(cyl==8 & mpg<15.1)

# == Paso 4: se orden� la base por la variable "mpg"
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp) %>%
  arrange(desc(mpg))

# == Paso 5: se agrup� la base por la variable cyl para hacer un reporte
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp) %>%
  filter(cyl==8 & mpg<15.1) %>%
  arrange(desc(mpg)) %>%
  group_by(cyl)

# == Paso 6: se operaciones por los datos de agrupaci�n
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp) %>%
  filter(cyl==8 & mpg<15.1) %>%
  arrange(desc(mpg)) %>%
  group_by(cyl)  %>%
  summarise(casos    = n(),
            prom_mpg = mean(mpg), 
            prom_hp  = mean(hp), 
            prom_kpg = mean(mpg*1.60934))

# == Paso 7: se export� el resumen a un archivo csv.
auto %>%
  mutate(car = toupper(str_trim(row.names(auto), side = "both")),
         kpg = mpg*1.60934,
         kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
  select(car, mpg, cyl, hp) %>%
  filter(cyl==8 & mpg<15.1) %>%
  arrange(desc(mpg)) %>%
  group_by(cyl)  %>%
  summarise(casos    = n(),
            prom_mpg = mean(mpg), 
            prom_hp  = mean(hp), 
            prom_kpg = mean(mpg*1.60934)) %>%
  write.csv("resumen.csv", row.names=TRUE)

# =========================================================================== #
# ==  Con la siguiente sentencia y usando tidyverse, se especifica que se 
# == crear� el objeto "nueva_base", el cual se constuye aplicando las 
# == siguientes funciones a la base "autos":
# ==    1. Se agregan los campos: car, kpg y kpg_est,
# ==    2. Se seleccionan los campos car, mpg, cyl y hp,
# ==    3. Se filtra la base para los autos de 8 cilindros y rendimiento menor
# ==       a 15.1 (comentado)
# ==    4. Se ordena la informaci�n por la variable mpg de forma descentente
# ==    5. Se agrupan los datos por el número de cilindros
# ==    6. Se crea un resumen con el número de casos y el promedio de las
# ==       variables mpg, hp y kpg
# =========================================================================== #

nueva_base <- auto %>%
              mutate(car = toupper(str_trim(row.names(auto), side = "both")),
                     kpg = mpg*1.60934,
                     kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
              select(car, mpg, cyl, hp) %>%
              arrange(desc(mpg)) %>%
              group_by(cyl)  %>%
              summarise(casos    = n(),
                        prom_mpg = mean(mpg), 
                        prom_hp  = mean(hp), 
                        prom_kpg = mean(mpg*1.60934))

nueva_base  
