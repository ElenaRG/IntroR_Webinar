
################################  S E S I Ó N  3  #############################################

###############################################################################################
## MANEJO DE BASES DE DATOS
## Tidyverse
###############################################################################################

install.packages("tidyverse")
library(tidyverse)
library(reshape)

setwd('C:/IntroR/')
getwd()
auto <- mtcars

# Pipe: %>%
# es un operador que permite encadenar funciones. 
# Toma la salida de una función y la pasa como entrada de la siguiente.

auto %>% count(cyl)
auto %>% count(cyl, hp)


# PROBLEMA: ¿cuáles son los outliers (mpg) para los autos de 8 cilindros?

# se seleccionaron los campos deseados para el análisis
# se crearon nuevas variables
# se filtró la base para mostrar aquellos autos de 8 cilindros y mpg menor al promedio
# se ordenó la base por la variable "mpg"
# se agrupó la base por la variable cyl para hacer un reporte
# se exportó el resumen a un archivo csv.

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

# todas las acciones se realizan a partir de la base auto pero no modifica este objeto
head(auto)

nueva_base <- auto %>%
              mutate(car = toupper(str_trim(row.names(auto), side = "both")),
                     kpg = mpg*1.60934,
                     kpg_est = (kpg - mean(kpg)) / sd(kpg)) %>%
              select(car, mpg, cyl, hp) %>%
              #filter(cyl==8 & mpg<15.1) %>%
              arrange(desc(mpg)) %>%
              group_by(cyl)  %>%
              summarise(casos    = n(),
                        prom_mpg = mean(mpg), 
                        prom_hp  = mean(hp), 
                        prom_kpg = mean(mpg*1.60934)) %>% 
              as.data.frame()

nueva_base

# con la siguiente sentencia y usando tidyverse, se especifica que se creará el objeto
# "nueva_base", el cual se constuye aplicando las siguientes funciones a la base "autos":
# se agregan los campos: car, kpg y kpg_est,
# se seleccionan los campos car, mpg, cyl y hp,
# se filtra la base para los autos de 8 cilindros y rendimiento menor a 15.1 (comentado)
# se ordena la información por la variable mpg de forma descentente
# se agrupan los datos por el número de cilindros
# se crea un resumen con el número de casos y el promedio de las variables mpg, hp y kpg
# el resumen se convierte a data frame


## GGPLOT2
# gráfica de caja y brazos para comparar las millas por galón para cada número de cilindros

jpeg('boxplot.jpg', units="in", width=12, height=8, res=500)
ggplot(auto, aes(x=as.factor(cyl), y=mpg, fill=cyl)) +
  geom_boxplot(alpha=0.5, notch=FALSE, outlier.colour="red", outlier.size=3) +
  labs(x     = "Número de cilindros", 
       y     = "Millas por galón",
       title = "Millas por galón según el número de cilindros") +
  theme(legend.position = "none",
        panel.background = element_blank()) #+
  #coord_flip()
dev.off()


# gráfica de barras para la frecuencia del número de velocidades
jpeg('barplot.jpg', units="in", width=12, height=8, res=500)
ggplot(as.data.frame(table(auto$gear)), aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", width=0.75, colour="green", fill=c("blue","red","yellow")) +
    xlab("Número de velocidades") + 
    ylab("Frecuencias") +
    ggtitle("Gráfico de barras \n con ggplot2") + 
    geom_text(aes(label=Freq), hjust=-0.2 , vjust=-0.1, color="black", size=3.5) +
    theme(panel.background = element_rect(colour = "orange", fill = "white"),
          plot.title   = element_text(size=14, hjust=0.5 , face="bold.italic"),
          axis.title.x = element_text(color="purple", size=12, face="bold")) +
    coord_flip() 
dev.off()