# Calificaciones

## Practicas

### Practica 1

Reduce: no hace la reducción de la derecha a la izquierda.

Bien por hacer pruebas distintas a las definidas en la practica.

**Calificación: 10**

### Practica 2
Su implementación de setvalueA esta incompleta debían lanzar
un error cuando el indice dado esta fuera de rango del arreglo.
Su implementación de printML es erronea no están manejando
el caso cuando tienen listas MCons anidadas.
Les estoy contando su punto extra.

**Calificación: 10.5**

### Practica 3
Muy bien, solamente no commenten sus lineas de prueba
mas bien usen la función

(print-only-errors true)

**Calificación: 10**

### Practica 4
Muy bien, aunque su interprete tiene un bug en los with multi parametricos.

**Calificación: 10**

## Tareas

### Tarea 1
Problema I: Bien, pero no es evidente que su algoritmo alternativo es 
mas eficiente que un stack, es evidente que era O(n^2) y hubiera sido
mas facil mostrar que su version es mejor que esta complejidad.
Problema II: Muy bien.
Problema III: Su forma Brujin es incorrecta, y no escribieron los resultados
parciales de la corrida.

**Calificación: 8**

### Tarea 2

Problema 1: En realidad que el lenguaje anfitrión sea Racket/Scheme no nos dice
nada sobre las capacidades del lenguaje objetivo, lo que tenian que indicar
es que FAE es equivalente a calculo lambda sin tipos y en este se puede definir
los conceptos de verdad y falsedad y los condicionales.

Problema 2: Tenian que dar el ejemplo en un archivo aparte al PDF, aparte su ejemplo hace uso de un efecto lateral como la impresión aun en Haskell se 
tiene el mismo resultado que siendo glotón. Su error es usar operaciones logicas
estas tienen semantica de short-circuit que son ortogonales a que el lenguaje
sea perezoso o glotón.

Problema 3: Muy bien.

Problema 4: Bien, les sugeri usar la sintaxis de estado del libro de Shriram pero tienes la idea correcta..

**Calificación: 9**