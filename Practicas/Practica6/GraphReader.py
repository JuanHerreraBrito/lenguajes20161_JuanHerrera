from sys import argv

script, nombreArchivo = argv

class GraphReader(object):

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
        print arr[0].split(': ', 1)[1]
        vertices = arr[1].split('[')[1]
        print vertices.split(']')[0]
        for x in range(3, arr[1].split('[').__len__() - 1):
            print arr[1].split('[')[x].split(']')[0]

        jsonFile.close()

cosa = GraphReader()
cosa.read_file(nombreArchivo)
