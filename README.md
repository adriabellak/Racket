# Reporte

### Reflexiona sobre la solución planteada, los algoritmos implementados y sobre el tiempo de ejecución de estos.

No implementamos ningun algoritmo porque no lo consideramos necesario, el archivo fuente se lee línea por línea y luego se genera el código 
html conforme se va leyendo cada palabra. Nuestra solución solo recorre las listas que usamos para almacenar información secuencialmente y se utilizan
expresiones regulares para separar y clasificar cada palabra. Utilizamos una máquina de estados para utilizar la expresión regular correspondiente para cada palabra.
Descubrimos que en css hay 3 caracteres especiales que organizan el código. Las llaves "{}" separan los bloques de código de los selectores el,
punto y coma ";" separa cada atributo del resto y los dos puntos ":" separan la propiedad de su valor. Utilizando expresiones regulares para separar el código
buscando estos elementos nos permitió saber que tipo de dato deberíamos esperar en cada sección. Antes de una llave abierta va un selector, despues va una propiedad,
despues dos puntos, luego un valor, un punto y coma y despues podrá haber una llave cerrada u otra propiedad.

El tiempo de ejecución

### Calcula la complejidad de tu algoritmo basada en el número de iteraciones y contrástala con el tiempo obtenido en el punto 7.
