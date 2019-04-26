Instalación
===========

Hace falta instalar `stack`, disponible en <https://docs.haskellstack.org/en/stable/README/> 

Una vez hecho esto puede ejecutarse `stack build` para instalar y compilar.

**Windows**: En Windows hay que modificar `stack.yaml` y reemplazar `convert_template.sh` por `convert_template.bat`.

Ejecución
=========

Para ejecutar un programa el comando a ejecutar es `stack exec <nombre>`.

`diagrams`
-------------

Si es llamado sin argumentos produce diagramas en pdf de los circuitos especificados.

`deutsch`
-----------

El binario generado tiene dos modos de ejecución

1.  Si se llama *sin argumentos* lee de la entrada estándar una tabla de
    verdad formateada como un fichero csv a dos columnas sin comillas
    que define una función f:{0,1}ⁿ → {0,1} a la que aplicar el
    algoritmo de Deutsch-Jozsa.
2.  Si se llama con *un argumento* `input.csv` lee la tabla de verdad
    del fichero `input.csv`.

Se incluyen dos ejemplos en la carpeta `oracles`. El programa comprueba
que la tabla de verdad esté bien formateada pero **no** comprueba que la
función sea balanceada (esto es, que la mitad de las imágenes sean 0 y
la otra mitad 1) o constante, en cuyo caso el algoritmo de Deutsch-Jozsa
tiene un comportamiento indeterminado.

Código
======

El código se organiza en las carpetas `deutsch`, `diagrams` y `src`, el resto de ficheros son necesarios para la compilación. En concreto

-   El código del algoritmo de Deutsch-Jozsa está disponible en el
    fichero `deutsch/Deutsch.hs` y
-   El código de generación de oráculos a partir de su tabla de verdad
    está disponible en el fichero `src/Oracle.hs`.
