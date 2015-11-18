from sys import argv

script, nombreArchivo = argv

print "Archivo prueba sacado de internet"

print "Se borra %r." % nombreArchivo
print "Si no se desea borrar , presionar CTRL-C (^C)."
print "Si quieres continuar presiona ENTER"


raw_input("?")

print "Se intenta abrir el archivo..."
target = open(nombreArchivo, 'w')

print "Borrar el archivo,  Goodbye!"
target.truncate()

print "Se piden 3 lineas."

linea1 = raw_input("linea 1: ")
linea2 = raw_input("linea 2: ")
linea3 = raw_input("linea 3: ")

print "Se escriben en el archivo."

target.write(linea1)
target.write("\n")
target.write(linea2)
target.write("\n")
target.write(linea3)
target.write("\n")

print "Y finalmente se cierra."
target.close()
