Sintaxis Basica - RMarkdown
================
<estadisticalemon@delarivagroup.com>

    Los tamanios de encabezado en Markdown van del 1 al 6, donde 1 es el mayor o 
    el principal, estos se denotan con # "Texto deseado"

    # Encabezado 1

# Encabezado 1

    ## Encabezado 2

## Encabezado 2

    ### Encabezado 3

### Encabezado 3

    #### Encabezado 4

#### Encabezado 4

    ##### Encabezado 5

##### Encabezado 5

    ###### Encabezado 6

###### Encabezado 6

    Formato de texto
    ------------------------------------------------------------
    
    *cursiva*   _cursiva_
    **negrita**   __negrita__
    `code`
    superscript^2^ y subscript~2~

## Formato de texto

*cursiva* *cursiva* <br> <!--Tambien podemos usar tags de HTML-->
**negrita** **negrita** <br> `code` <br> superscript<sup>2</sup> y
subscript<sub>2</sub> <br>

    Tipos de Listas
    ------------------------------------------------------------
    
    #### Listas no enumeradas
    
    *   Elemento 1 en lista no enumerada
    
    *   Elemento 2
    
        * Elemento 2a
    
        * Elemento 2b
        
    #### Listas enumeradas
    
    1.  Elemento 1 en lista enumerada
    
    1.  Elemento 2. La numeracion se incrementa automaticamente en el output.

## Tipos de Listas

#### Listas no enumeradas

  - Elemento 1 en lista no enumerada

  - Elemento 2
    
      - Elemento 2a
    
      - Elemento 2b

#### Listas enumeradas

1.  Elemento 1 en lista enumerada

2.  Elemento 2. La numeracion se incrementa automaticamente en el
    output.

<!-- end list -->

    Enlaces e imagenes
    ------------------------------------------------------------
    
    <www.rutaDelEnlace.com>
    
    [texto del enlace](rutaDelEnlace)
    
    ![texto de la imagen](rutaDeLaImagen)

## Enlaces e imagenes

<estadisticalemon@delarivagroup.com>

[texto del enlace](rutaDelEnlace)

![Lorem Picsum](https://picsum.photos/100)

    Tablas 
    ------------------------------------------------------------
    
    Primer encabezado     | Segundo encabezado
    --------------------- | ---------------------
    Contenido de la celda | Contenido de la celda
    Contenido de la celda | Contenido de la celda

## Atajos en RMarkdown

| Task                       | Windows & Linux  | macOS               |
| -------------------------- | ---------------- | ------------------- |
| Insert R chunk             | Ctrl+Alt+I       | Command+Option+I    |
| Preview HTML               | Ctrl+Shift+K     | Command+Shift+K     |
| Knitr document (knitr)     | Ctrl+Shift+K     | Command+Shift+K     |
| Compile Notebook           | Ctrl+Shift+K     | Command+Shift+K     |
| Compile PDF                | Ctrl+Shift+K     | Command+Shift+K     |
| Run all chunks above       | Ctrl+Alt+P       | Command+Option+P    |
| Run current chunk          | Ctrl+Alt+C       | Command+Option+C    |
| Run current chunk          | Ctrl+Shift+Enter | Command+Shift+Enter |
| Run next chunk             | Ctrl+Alt+N       | Command+Option+N    |
| Run all chunks             | Ctrl+Alt+R       | Command+Option+R    |
| Go to next chunk/title     | Ctrl+PgDown      | Command+PgDown      |
| Go to previous chunk/title | Ctrl+PgUp        | Command+PgUp        |
| Show/hide document outline | Ctrl+Shift+O     | Command+Shift+O     |
| Build book, website, .     | Ctrl+Shift+B     | Command+Shift+B     |
