class Graph(object):
	"""docstring for Graph"""
	def __init__(self, vertices, aristas, dirigida):
		super(Graph, self).__init__()
		self.vertices = vertices
		self.aristas= aristas
		self.dirigida=dirigida
	def directed(self):
		if self.dirigida==0:
			return false
		else :
			return true
	
		