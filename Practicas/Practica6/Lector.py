from sys import argv
import Edge
import Graph
import Vertex

script, nombreArchivo = argv

""" Para json, se debe meter en metodo y hacer clase en general """
jsonFile = open(nombreArchivo, 'r')

s = jsonFile.read();

arr = s.split(',', 1)
""" cuidar caso con espacio y sin espacio"""
"""print arr[0].split(':', 1)[1]"""
""" bueno en el caso actual """
print "Esto es el '1' DIRIGIDO, '0' NoDIRIGIDO"
print arr[0].split(': ', 1)[1]


vertices = arr[1].split('[')[1]
print "Estos son los vertices"
print vertices.split(']')[0]

"""for x in range(3, arr[1].split('[').__len__() - 1):
	print arr[1].split('[')[x].split(']')[0]
"""
print "La primer Arista"
print arr[1].split('[')[3].split(']')[0]
jsonFile.close()

def prueba(a,b,c):
	print a+" "+b+" "+c
a=arr[1].split('[')[4].split(']')[0]
sep = a.split(',')
#Crear las aristas
print "Arista numero 2"
prueba(sep[0],sep[1],sep[2])
