# ==================  S E S I Ó N  2   - P R A C T I C A 4 ================== #

# == Este c�digo solo corremos la primera vez instalamos el paquete tydiverse
# == y la librer�a reshape
# == Nota: si ya cuentas con estos paquetes instalados, solo comenta las
# ==       siguientes lineas y corre carga las librer�as a usar
# install.packages("tidyverse")
# install.packages("reshape") 

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

# =========================================================================== #
# == Introducci�n a GGPLOT2
# =========================================================================== #

# == De la base de datos autos que creamos anteriormente seleccionaremos las
# == variables cyl y mpg, para crear un gr�fico de caja y brazos.
# == Como podr�s darte cuenta cuando usamos ggplot la notaci�n cambia, ya que
# == cuando llegamos a las sentencias de relacionadas con el grafico en lugar
# == de usar pipes usaremos el signo "+"

# == Recuerda que toda la primera parte de este c�digo se usaran pipes para
# == para hacer la selecci�n necesaria de la data que se usar� para generar el 
# == grafico
auto %>%
  select(cyl, mpg) %>%
  mutate(cyl = as.factor(cyl)) %>%
  # == Apartir de este punto usaremos "+" en lugar de pipes
  ggplot(aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot(alpha=0.5, notch=FALSE, outlier.colour="red", outlier.size=3) +
  labs(x     = "Número de cilindros", 
       y     = "Millas por gal�n",
       title = "Millas por gal�n según el número de cilindros") +
  theme(legend.position = "none",
        panel.background = element_blank())

# == Nota: recuerda que todos los gr�ficos que hagas al estar usando tydiverse,
# ==       contaras con todas las bondades de las librer�as que este framework
# ==       ofrece, como por ejemplo, lo seria poder trabajar con los pipes y "+".

# == Todos los gr�ficos, también los puedes guardar como un objeto y a este poderlo
# == mandarlo llamar cada vez que se necesite
grafBoxplot <- auto %>%
  select(cyl, mpg) %>%
  mutate(cyl = as.factor(cyl)) %>%
  # == Apartir de este punto usaremos "+" en lugar de pipes
  ggplot(aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot(alpha=0.5, notch=FALSE, outlier.colour="red",
               outlier.size=3) +
  labs(x     = "Número de cilindros", 
       y     = "Millas por gal�n",
       title = "Millas por gal�n según el número de cilindros") +
  theme(legend.position = "none",
        panel.background = element_blank())

# == Cuando se tienen objetos de gr�ficos y se mandan llamar, estos pueden ser
# == modificados con sentencias adicionales. Esto es de gran utilidad, ya que puedes
# == tener un gr�fico base al cual solo le modificares el estilo cada vez que se
# == requiera
grafBoxplot + theme_test()


# == Al igual que los dataframes, los gr�ficos los puedes exportar con formato de
# == imagen, usando la siguientes sentencias
jpeg('boxplot.jpg', units="in", width=12, height=8, res=500) # Instanciamos el tipo de imagen
grafBoxplot + 
  theme_test() # Corremos la grafica
dev.off() # Imprimimos y exportamos


# =========================================================================== #
# == Ahora crearemos un gr�fico de brarras, poniendo en pr�ctica lo aprendido
# == anteriormente
# == Ejercicio: Crear un gr�fico de barras horizontales que muestre la 
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
  ggtitle("Gr�fico de barras \n con ggplot2") + 
  geom_text(aes(label=n), hjust=-0.2 , vjust=-0.1,
            color="black", size=3.5)

# == Paso 3: Usar el tema minimal, rotar la gr�fica y exportar el objeto
jpeg('barplot.jpg', units="in", width=12, height=8, res=500)
garfBarplot +
  theme_minimal() +
  coord_flip() 
dev.off()

# == Nota: Recuerda que no es necesario seguir todos estos pasos, ya que podr�as
# ==       llegar al mismo resultado en un solo paso. Sin embargo siempre es
# ==       recomendable ir armando bloques que sean f�ciles de leer e interpretar
# ==       por cualquier persona que lea tu c�digo, adem�s de que modularizar siempre
# ==       har� que cuando se necesite un cambio sea m�s f�cil identificar donde
# ==       se tiene que hacer y cambiarlo una sola vez y no varias (ya que esto
# ==       no es eficiente)