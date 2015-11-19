class Vertex(object):
	"""docstring for Vertex"""
	def __init__(self, id):
		super(Vertex, self).__init__()
		self.neighbours
		self.id= id
		self.grado= 0
	
	def addNeighbour(self, newNeighbour):
		self.neighbours[grado]=newNeighbour
		self.grado= grado + 1 

		#Metodo que regresa los vertices adyacentes
	def neighbours(self):
			return self.neighbours
		#Metodo que regresa el grado del vertice
	def degree(self):
			return self.grado
	def describe(self):
		return "Vertice: "+ self.id+"\nGrado: "+self.grado