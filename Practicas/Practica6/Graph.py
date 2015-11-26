from Vertex import *
from Edge import *


class Graph(object):
	"""docstring for Graph"""
	def __init__(self, vertices, aristas, dirigida):
		super(Graph, self).__init__()
		self.dirigida = dirigida
		self.vertices = [None]* vertices.__len__()
                for i in range(0, vertices.__len__()):
                    self.vertices[i] = Vertex(vertices[i])
		self.aristas = [None]* aristas.__len__()
                for i in range(0, aristas.__len__()):
                    self.aristas[i] = Edge(aristas[i][0],aristas[i][1],aristas[i][2])


	def directed(self):
		if self.dirigida==0:
			return false
		else :
			return true

	def vertices(self):
		return self.vertices
	
	def edges(self):
		return self.aristas

	def has_cycles():
                return 3

g = Graph(['a', 'd'], [['a', 'd', 1], ['a', 'b', 1]], 1)
for i in g.vertices:
    print i.id
