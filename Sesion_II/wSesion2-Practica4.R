# ==================  S E S I Ó N  2   - P R A C T I C A 4 ================== #

# == Este código solo corremos la primera vez instalamos el paquete tydiverse
# == y la librería reshape
# == Nota: si ya cuentas con estos paquetes instalados, solo comenta las
# ==       siguientes lineas y corre carga las librerías a usar
# install.packages("tidyverse")
# install.packages("reshape") 

# == Aquí vamos a cargar las librerias que necesitaremos para trabajar
library(tidyverse)
library(reshape)

# == iniciamos una variable que contendrá la ruta en la cual estaremos
# == tendremos nuestros archivos de trabajo e imprimiremos todas nuestras
# == salidas
rutaLocal <- 'C:/webinar/'

# == aquí seteamos la ruta de donde se va a estar trabajando y comprobamos
# == que es correcta
setwd(rutaLocal)
getwd()

# == Mandamos a llamar la base precargada en R llamada mtcars y la guardamos 
# == como un objeto con el nombre autos
auto <- mtcars

# =========================================================================== #
# == Introducción a GGPLOT2
# =========================================================================== #

# == De la base de datos autos que creamos anteriormente seleccionaremos las
# == variables cyl y mpg, para crear un gráfico de caja y brazos.
# == Como podrás darte cuenta cuando usamos ggplot la notación cambia, ya que
# == cuando llegamos a las sentencias de relacionadas con el grafico en lugar
# == de usar pipes usaremos el signo "+"

# == Recuerda que toda la primera parte de este código se usaran pipes para
# == para hacer la selección necesaria de la data que se usará para generar el 
# == grafico
auto %>%
  select(cyl, mpg) %>%
  mutate(cyl = as.factor(cyl)) %>%
  # == Apartir de este punto usaremos "+" en lugar de pipes
  ggplot(aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot(alpha=0.5, notch=FALSE, outlier.colour="red", outlier.size=3) +
  labs(x     = "Número de cilindros", 
       y     = "Millas por galón",
       title = "Millas por galón según el número de cilindros") +
  theme(legend.position = "none",
        panel.background = element_blank())

# == Nota: recuerda que todos los gráficos que hagas al estar usando tydiverse,
# ==       contaras con todas las bondades de las librerías que este framework
# ==       ofrece, como por ejemplo, lo seria poder trabajar con los pipes y "+".

# == Todos los gráficos, también los puedes guardar como un objeto y a este poderlo
# == mandarlo llamar cada vez que se necesite
grafBoxplot <- auto %>%
  select(cyl, mpg) %>%
  mutate(cyl = as.factor(cyl)) %>%
  # == Apartir de este punto usaremos "+" en lugar de pipes
  ggplot(aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot(alpha=0.5, notch=FALSE, outlier.colour="red",
               outlier.size=3) +
  labs(x     = "Número de cilindros", 
       y     = "Millas por galón",
       title = "Millas por galón según el número de cilindros") +
  theme(legend.position = "none",
        panel.background = element_blank())

# == Cuando se tienen objetos de gráficos y se mandan llamar, estos pueden ser
# == modificados con sentencias adicionales. Esto es de gran utilidad, ya que puedes
# == tener un gráfico base al cual solo le modificares el estilo cada vez que se
# == requiera
grafBoxplot + theme_test()


# == Al igual que los dataframes, los gráficos los puedes exportar con formato de
# == imagen, usando la siguientes sentencias
jpeg('boxplot.jpg', units="in", width=12, height=8, res=500) # Instanciamos el tipo de imagen
grafBoxplot + 
  theme_test() # Corremos la grafica
dev.off() # Imprimimos y exportamos


# =========================================================================== #
# == Ahora crearemos un gráfico de brarras, poniendo en práctica lo aprendido
# == anteriormente
# == Ejercicio: Crear un gráfico de barras horizontales que muestre la 
# ==            frecuencia de la variable "gear", este grafico debe tener
# ==            un tema minimal y se debe exportar como un jpg con el nombre
# ==            de barplot
# =========================================================================== #

# == Paso 1: Creamos las frecuencias de la variable gear
auto %>%
  mutate(gear = as.character(gear)) %>%
  count(gear, sort=TRUE)

# == Paso 2: Crear el grafico base y guardar lo en un objeto
garfBarplot  <- auto %>%
  mutate(gear = as.character(gear)) %>%
  count(gear, sort=TRUE) %>%
  ggplot(aes(x=gear, y=n)) +
  geom_bar(stat="identity", width=0.75, colour="black", 
           fill=c("blue","red","yellow")) +
  xlab("Número de velocidades") + 
  ylab("Frecuencias") +
  ggtitle("Gráfico de barras \n con ggplot2") + 
  geom_text(aes(label=n), hjust=-0.2 , vjust=-0.1,
            color="black", size=3.5)

# == Paso 3: Usar el tema minimal, rotar la gráfica y exportar el objeto
jpeg('barplot.jpg', units="in", width=12, height=8, res=500)
garfBarplot +
  theme_minimal() +
  coord_flip() 
dev.off()

# == Nota: Recuerda que no es necesario seguir todos estos pasos, ya que podrías
# ==       llegar al mismo resultado en un solo paso. Sin embargo siempre es
# ==       recomendable ir armando bloques que sean fáciles de leer e interpretar
# ==       por cualquier persona que lea tu código, además de que modularizar siempre
# ==       hará que cuando se necesite un cambio sea más fácil identificar donde
# ==       se tiene que hacer y cambiarlo una sola vez y no varias (ya que esto
# ==       no es eficiente)
