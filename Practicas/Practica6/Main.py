#from  import *
from Graph import *
from GraphReader import *
#from ReadJson import *
#from ReadCsv import *
#from ReadXml import *

print " "
pete1 = ReadJson()
pete1.read_file("petersen.json")
print "petersen.json"
#print pete1.get_directed()
print "Vertices"
print pete1.get_vertices()
print "Aristas"
print pete1.get_aristas()

g1 = Graph(pete1.get_vertices() ,pete1.get_aristas() ,pete1.get_directed())
print "Tiene Ciclos"
print g1.has_cycles()

print " "
pete2 = ReadCsv()
pete2.read_file("petersen.csv")
print "petersen.csv"
#print pete2.get_directed()
print "Vertices"
print pete2.get_vertices()
print "Aristas"
print pete2.get_aristas()

g2 = Graph(pete2.get_vertices() ,pete2.get_aristas() ,pete2.get_directed())
print "Tiene Ciclos"
print g2.has_cycles()

print " "

#if (pete1.get_vertices() == pete2.get_vertices()):
#    print "igual"
#else:
#    print "diferente"

#if (pete1.get_aristas() == pete2.get_aristas()):
#    print "igual"
#else:
#    print "diferente"

#if (pete2.get_directed() == pete2.get_directed()):
#    print "igual"
#else:
#    print "diferente"
print"Problemas con orden diferente de aristas? en CSV  de petersen"

print " "
pete3 = ReadXml()
pete3.read_file("petersen.xml")
print "petersen.xml"
#print pete3.get_directed()
print "Vertices"
print pete3.get_vertices()
print "Aristas"
print pete3.get_aristas()

g3 = Graph(pete3.get_vertices() ,pete3.get_aristas() ,pete3.get_directed())
print "Tiene Ciclos"
print g3.has_cycles()

print " "
gra4 = ReadJson()
gra4.read_file("graph.json")
print "graph.json"
#print gra4.get_directed()
print "Vertices"
print gra4.get_vertices()
print "Aristas"
print gra4.get_aristas()

g4 = Graph(gra4.get_vertices() ,gra4.get_aristas() ,gra4.get_directed())
print "Tiene Ciclos"
print g4.has_cycles()

print " "
gra5 = ReadCsv()
gra5.read_file("graph.csv")
print "graph.csv"
#print gra5.get_directed()
print "Vertices"
print gra5.get_vertices()
print "Aristas"
print gra5.get_aristas()

g5 = Graph(gra5.get_vertices() ,gra5.get_aristas() ,gra5.get_directed())
print "Tiene Ciclos"
print g5.has_cycles()

print " "
gra6 = ReadXml()
gra6.read_file("graph.xml")
print "graph.xml"
#print pete6.get_directed()
print "Vertices"
print gra6.get_vertices()
print "Aristas"
print gra6.get_aristas()

g6 = Graph(gra6.get_vertices() ,gra6.get_aristas() ,gra6.get_directed())
print "Tiene Ciclos"
print g6.has_cycles()
