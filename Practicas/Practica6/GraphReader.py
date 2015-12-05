

class GraphReader(object):
    
    directed = -1
    vertices = []
    aristas = []

    def __init__(self, nombre = None):
        self.nombre = nombre

    def get_directed(self):
        return self.directed

    def get_vertices(self):
        return self.vertices

    def get_aristas(self):
        return self.aristas

class ReadJson(GraphReader):
    def read_file(self, name):
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

class ReadCsv(GraphReader):
    def read_file(self, name):
        csvFile = open(name, 'r')
        s = csvFile.read()
        self.directed = s[7]
        arr = s[9:]
        arr = arr.split('\n')
        vertices = ""
        self.aristas = [None]* arr.__len__()
        for i in range(0, arr.__len__()):
            aux = arr[i].split(', ')
            self.aristas[i] = [aux[0].split('"')[1], aux[1].split('"')[1], int(aux[2])]
            vertices += aux[0].split('"')[1] + aux[1].split('"')[1]
        self.vertices = cleanVertex(vertices)
        csvFile.close()
    
    global cleanVertex
    def cleanVertex(string):
        stringResult = string[0]
        while (string != "" ):
            strn = string.split(string[0])
            stringAux = ""
            for i in range(0, strn.__len__()):
                stringAux += strn[i]
            string = stringAux
            if (string != ""):
                stringResult += string[0]
        return list(stringResult)

class ReadXml(GraphReader):
    def read_file(self, name):
        xmlFile = open(name, 'r')
        s = xmlFile.read()
        arr = s.split('\n',3)
	directed = arr[2]
        self.directed = int(directed.split('"')[1])
        arr = arr[3]
        vertex = arr.split('edge',1)[0]
	vertex = vertex.split('="')
	self.vertices = [None]* (vertex.__len__() - 1)
        for i in range(1, vertex.__len__()):
            self.vertices[i - 1] = vertex[i].split('"')[0]
        
        edge = arr.split('edge',1)[1].split('\n')
        self.aristas = [None]* (edge.__len__() - 1)
        #print edge
        for j in range(0, edge.__len__() - 1):
            edgeAux = edge[j].split('"')
            self.aristas[j] = [edgeAux[1], edgeAux[3], int(edgeAux[5])]
         
        xmlFile.close()

#cosa1 = ReadJson()
#cosa1.read_file("petersen.json")
#print cosa1.get_directed()
#print cosa1.get_vertices()
#print cosa1.get_aristas()

#cosa2 = ReadCsv()
#cosa2.read_file("petersen.csv")
#print cosa2.get_directed()
#print cosa2.get_vertices()
#print cosa2.get_aristas()

#cosa3 = ReadXml()
#cosa3.read_file("petersen.xml")

#print cosa3.get_directed()
#print cosa3.get_vertices()
#print cosa3.get_aristas()
