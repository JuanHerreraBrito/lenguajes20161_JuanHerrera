from sys import argv

script, nombreArchivo = argv

class GraphReader(object):
    
    directed = -1
    vertices = []
    aristas = []

    def __init__(self, nombre = None):
        self.nombre = nombre

    def read_file(self, name):
        """ Para json, se debe meter en metodo y hacer clase en general """
        jsonFile = open(name, 'r')
        s = jsonFile.read()
        arr = s.split(',', 1)
        """ cuidar caso con espacio y sin espacio"""
        """print arr[0].split(':', 1)[1]"""
        """ bueno en el caso actual """
        self.directed = int(arr[0].split(': ', 1)[1])
        verticesSucio = arr[1].split('[')[1]
        self.vertices = verticesSucio.split(']')[0].split(', ')

        for i in range(0, self.vertices.__len__()):
            self.vertices[i] = self.vertices[i].split('"')[1]

        j = 0
	self.aristas = [None]* (arr[1].split('[').__len__() - 3)
        for x in range(3, arr[1].split('[').__len__()):
            respaldo = arr[1].split('[')[x].split(']')[0].split(', ')
            self.aristas[j] = [respaldo[0].split('"')[1],respaldo[1].split('"')[1],int(respaldo[2])]
            j += 1 

        jsonFile.close()

    def get_directed(self):
        return self.directed

    def get_vertices(self):
        return self.vertices

    def get_aristas(self):
        return self.aristas

cosa = GraphReader()
cosa.read_file(nombreArchivo)
print cosa.get_directed()
print cosa.get_vertices()
print cosa.get_aristas()
