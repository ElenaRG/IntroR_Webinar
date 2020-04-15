Sesion 3 - Practica 6
================
<estadisticalemon@delarivagroup.com>

## Creacion de reportes - R Markdown

En este reporte exploraremos la base de datos “mtcars”

|                   |  mpg | cyl | disp |  hp | drat |    wt |  qsec | vs | am | gear | carb |
| ----------------- | ---: | --: | ---: | --: | ---: | ----: | ----: | -: | -: | ---: | ---: |
| Mazda RX4         | 21.0 |   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |  0 |  1 |    4 |    4 |
| Mazda RX4 Wag     | 21.0 |   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |  0 |  1 |    4 |    4 |
| Datsun 710        | 22.8 |   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |  1 |  1 |    4 |    1 |
| Hornet 4 Drive    | 21.4 |   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |  1 |  0 |    3 |    1 |
| Hornet Sportabout | 18.7 |   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |  0 |  0 |    3 |    2 |
| Valiant           | 18.1 |   6 |  225 | 105 | 2.76 | 3.460 | 20.22 |  1 |  0 |    3 |    1 |

Resumen de datos tabla mtcars

## Vizualizacion de la informacion

A continuacion generaremos un grafico de dispersion para visualizar el
comportamiento de las variables de las base de datos mtcars

![](wSesion3-Practica6_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Adicional al grafico imprimiremos una tabla con el coeficiente de
correlacion de cada variable de la base de datos mtcars

``` r
cor(mtcars) %>%
  knitr::kable(digits = 3, caption = 'Coeficiente de Correlacion "mtcars"')
```

|      |     mpg |     cyl |    disp |      hp |    drat |      wt |    qsec |      vs |      am |    gear |    carb |
| ---- | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: | ------: |
| mpg  |   1.000 | \-0.852 | \-0.848 | \-0.776 |   0.681 | \-0.868 |   0.419 |   0.664 |   0.600 |   0.480 | \-0.551 |
| cyl  | \-0.852 |   1.000 |   0.902 |   0.832 | \-0.700 |   0.782 | \-0.591 | \-0.811 | \-0.523 | \-0.493 |   0.527 |
| disp | \-0.848 |   0.902 |   1.000 |   0.791 | \-0.710 |   0.888 | \-0.434 | \-0.710 | \-0.591 | \-0.556 |   0.395 |
| hp   | \-0.776 |   0.832 |   0.791 |   1.000 | \-0.449 |   0.659 | \-0.708 | \-0.723 | \-0.243 | \-0.126 |   0.750 |
| drat |   0.681 | \-0.700 | \-0.710 | \-0.449 |   1.000 | \-0.712 |   0.091 |   0.440 |   0.713 |   0.700 | \-0.091 |
| wt   | \-0.868 |   0.782 |   0.888 |   0.659 | \-0.712 |   1.000 | \-0.175 | \-0.555 | \-0.692 | \-0.583 |   0.428 |
| qsec |   0.419 | \-0.591 | \-0.434 | \-0.708 |   0.091 | \-0.175 |   1.000 |   0.745 | \-0.230 | \-0.213 | \-0.656 |
| vs   |   0.664 | \-0.811 | \-0.710 | \-0.723 |   0.440 | \-0.555 |   0.745 |   1.000 |   0.168 |   0.206 | \-0.570 |
| am   |   0.600 | \-0.523 | \-0.591 | \-0.243 |   0.713 | \-0.692 | \-0.230 |   0.168 |   1.000 |   0.794 |   0.058 |
| gear |   0.480 | \-0.493 | \-0.556 | \-0.126 |   0.700 | \-0.583 | \-0.213 |   0.206 |   0.794 |   1.000 |   0.274 |
| carb | \-0.551 |   0.527 |   0.395 |   0.750 | \-0.091 |   0.428 | \-0.656 | \-0.570 |   0.058 |   0.274 |   1.000 |

Coeficiente de Correlacion “mtcars”

#### Principales salidas de reportes que se pueden crear en RMarkdown

  - beamer\_presentation
  - github\_document
  - html\_document
  - ioslides\_presentation
  - latex\_document
  - md\_document
  - odt\_document
  - pdf\_document
  - powerpoint\_presentation
  - rtf\_document
  - slidy\_presentation
  - word\_document
