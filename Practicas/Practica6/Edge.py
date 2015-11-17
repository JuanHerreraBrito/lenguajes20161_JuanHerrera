class Edge(object):
	"""docstring for Edge"""
	def __init__(self, inicio, fin, peso):
		super(Edge, self).__init__()
		self.inicio = inicio
		self.fin= fin
		self.peso=peso
	
	def weight(self):
		return self.peso

	def svertex(self):
		return self.inicio

	def tvertex(self):
		return self.fin
	


		