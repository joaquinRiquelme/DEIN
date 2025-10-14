# Codigo para generar grafos dirigidos a partir de una matriz de adyacencia.
# Se calcula la matriz potencia y los valores propios de cada variable

# Cargar y/o instalar librerias
library(igraph)
library(readxl)

# Leer datos de matriz de adyacencia

m.adj <- t(as.matrix(read_excel("Matriz de Adyacencia Limpia.xlsx", 
                    range = "B1:Q17")))
m.adj
# m.adj<-matrix(sample(16^2,x= c(-1,0,1), replace=TRUE), nrow = 16)

w.m.adj <- abs(m.adj)

# crear grafo desde la matriz de adyacencia
graph.adj <- igraph::graph_from_adjacency_matrix(m.adj, 
                                                 weighted = TRUE,
                                                 mode = "directed")
graph.abs<- igraph::graph_from_adjacency_matrix(w.m.adj, 
                                                 weighted = TRUE,
                                                 mode = "directed")

E(graph.adj)

# Asignar nombres a los nodos (variables)
V(graph.adj)$name <- paste("V", 1:vcount(graph.adj), sep = "")

# Configurar el color de las aristas
# Si el peso es positivo (1), la arista es azul.
# Si el peso es negativo (-1), la arista es roja.
colores_aristas <- ifelse(E(graph.adj)$weight > 0, "blue", "red")

# Configurar el ancho de las aristas
# Las aristas con peso negativo pueden ser más delgadas o punteadas para mayor diferenciación
anchos_aristas <- ifelse(E(graph.adj)$weight!=0,2,0)# * 2  # Multiplicamos por 2 para que sean visibles

# Opciones de diseño del grafo
layout_grafo <- layout_with_fr(graph.abs)

# Plotear el grafo con las opciones personalizadas
plot(graph.adj,
     vertex.color = "lightgreen",        # Color de los nodos
     vertex.size = 20,                   # Tamaño de los nodos
     vertex.label.color = "black",       # Color de las etiquetas de los nodos
     vertex.label.cex = 0.8,             # Tamaño de la fuente de las etiquetas
     edge.color = colores_aristas,       # Colores de las aristas definidos anteriormente
     edge.width = anchos_aristas,        # Ancho de las aristas
     edge.arrow.size = 0.5,              # Tamaño de las puntas de flecha
     edge.curved = 0.2,                  # Curvatura de las aristas para evitar superposición
     layout = layout_grafo               # El diseño del grafo
)

# Agregar una leyenda para los colores de las aristas
legend("topright",
       legend = c("Relación Positiva (+1)", "Relación Negativa (-1)"),
       col = c("blue", "red"),
       lwd = 2,
       title = "Tipo de Relación")

## Calcular las variables de centralidad de cada variable ----
centralidad_grado <- degree(graph.abs)

cercania_centralidad <- closeness(graph.abs)

intermediacion_centralidad <- betweenness(graph.abs)

centralidad_propia <- eigen(m.adj)$values

V(graph.abs)$grado <- centralidad_grado
V(graph.abs)$cercania <- cercania_centralidad
V(graph.abs)$intermediacion <- intermediacion_centralidad
V(graph.abs)$centralidad_propia <- centralidad_propia

plot(graph.abs, vertex.size = V(graph.abs)$grado * 5, vertex.label = 1:vcount(graph.abs))
plot(graph.abs, vertex.size = V(graph.abs)$cercania * 5, vertex.label = 1:vcount(graph.abs))
plot(graph.abs, vertex.size = V(graph.abs)$intermediacion * 2, vertex.label = 1:vcount(graph.abs))
plot(graph.abs, vertex.size = V(graph.abs)$centralidad_propia, vertex.label = 1:vcount(graph.abs))

# La centralidad que calcule es para grafos dirigidos			

#Identificar Feedback		

#Elevar la matriz a potencia		

#Calcular autovalores (leer capitulo) de una matriz y 

#calcular la inversa de una matriz