from Vertex import *
from Edge import *
import Queue


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
                self.q = Queue.Queue()
                self.auxVertices = 0

	def directed(self):
		if self.dirigida==0:
			return False
		else :
			return True

	def vertices(self):
		return self.vertices
	
	def edges(self):
		return self.aristas

	def has_cycles(self):
                self.auxVertices = [False]*(self.vertices.__len__())
                #marcar visitado vertice y encolar
                if self.dirigida:
                    tieneCiclos = False
                    for i in range (0, self.vertices.__len__()):
                        print "Unicio nuevo arbol"
                        print self.vertices[i].id
                        tieneCiclos = busquedaProfundidad(self, i, self.vertices[i].id)
                        print " "
                        if tieneCiclos:
                            break
                    if tieneCiclos:
                        print "Con ciclos d"
                    else:
                        print "Sin ciclos d"
                    return tieneCiclos
                else:
                    for i in range (0, self.vertices.__len__()):
                        if self.auxVertices[i] == False:
                            self.q.put(i)
                            self.auxVertices[i] = True
                            self.vertices[i].visitado = True
                            if not got_a_tree(self):
                                print "con ciclos" 
                                return True
                    print "sin ciclos"
                    return False

        global got_a_tree
	def got_a_tree(self):
            if self.q.empty():
                #regresar sin ciclos y reiniciar todo
                print "No tiene ciclos falta reiniciar cosas, ampliar para direccional"
                return True
            else:
                #regresar proceso para ciclos
                iA = self.q.get()
                nombre = self.vertices[iA].id
                #print nombre + "   nombre"
                #marcar visitado aristas , vertices adyacentes y encolar los ultimos
                for i in range(0, self.aristas.__len__()):
                    #print self.aristas[i].inicio + " " + self.aristas[i].fin + " " + nombre
                    if (self.aristas[i].fin == nombre) and ( not self.dirigida):
                        esAristaFinal = True
                    else:
                        esAristaFinal = False

                    if (self.aristas[i].inicio == nombre) or esAristaFinal:
                        if self.aristas[i].usada == False:
                            #marcar como visitado
                            if (self.aristas[i].inicio == nombre):
                                vecino = indiceVecino(self, self.aristas[i].fin)
                            else:
                                vecino = indiceVecino(self, self.aristas[i].inicio)
                            #con vecino checamos si vecino esta visitado, si no visitamos, si si acabamos con un ciclo
                            self.aristas[i].usada = True
                            if self.vertices[vecino].visitado == True:
                                print "tiene ciclos"
                                return False
                            if not self.dirigida:
                                self.vertices[vecino].visitado = True
                                self.auxVertices[vecino] = True
                            self.q.put(vecino)
                            #print self.vertices[vecino].id + " vecino"
                return got_a_tree(self)

        global indiceVecino
	def indiceVecino(self, nombre):
            for i in range(0, self.vertices.__len__()):
                if self.vertices[i].id == nombre:
                    return i
            print "No se encuentra id"
            return -1
        
        global busquedaProfundidad
        def busquedaProfundidad(self, i, nombre):
            for ii in range(0, self.aristas.__len__()):
                self.aristas[ii].usada = False
            for j in range(0, self.vertices.__len__()):
                self.vertices[j].visitado = False
            #self.q.flush()
            #self.vertices[i].visitado = True
            self.q.put(i)
            return busquedaProfundidadAux(self, nombre)

        global busquedaProfundidadAux
        def busquedaProfundidadAux(self, nombre):
            if self.q.empty():
                #regresar sin ciclos y reiniciar todo
                print "false"
                return False
            else:
                iA = self.q.get()
                print iA 
                actual = self.vertices[iA].id
                print actual + " actual"
                print self.vertices[iA].visitado
                if self.vertices[iA].visitado == False:
                    self.vertices[iA].visitado = True
                    for i in range(0, self.aristas.__len__()):
                        if (self.aristas[i].inicio == actual):
                            if(self.aristas[i].fin == nombre):
                                print "True"
                                return True
                            self.aristas[i].usada = True
                            vecino = indiceVecino(self, self.aristas[i].fin)
                            print "entra"
                            print self.vertices[vecino].id
                            self.q.put(vecino)
            return busquedaProfundidadAux(self, nombre)
            
#con ciclos
#g = Graph(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'], [['a', 'b', 11], ['a', 'e', 1], ['a', 'f', 4], ['b', 'c', 8], ['b', 'g', 8], ['c', 'd', 3], ['c', 'h', 3], ['d', 'e', 3], ['d', 'i', 2], ['e', 'j', 1], ['f', 'h', 9], ['f', 'i', 7], ['g', 'i', 4], ['g', 'j', 1], ['h', 'j', 9], ['i', 'c', 9]] , 1)
#, ['i', 'c', 9]
#sin ciclos
g = Graph(['a', 'b', 'c', 'd', 'e', 'f'], [['a', 'b', 11], ['a', 'd', 1], ['a', 'e', 4], ['a', 'f', 8], ['b', 'c', 8]] , 1)

#for i in g.vertices:
#    print i.id
#print "\n"

#for i in g.aristas:
#    print i.inicio
#    print i.fin
#    print i.peso
#    print "\n"
print g.directed()

g.has_cycles()
