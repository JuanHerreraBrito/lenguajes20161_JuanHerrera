from sys import argv

script, nombreArchivo = argv

print "Se borra %r." % nombreArchivo
print "Si no se desea borrar , presionar CTRL-C (^C)."
print "Si quieres continuar presiona ENTER"


print "Se intenta abrir el archivo..."
target = open(nombreArchivo, 'r')

print "Se leen en el archivo."

print target.read();
print target.read();
print target.read();
print "Y finalmente se cierra."
target.close()
