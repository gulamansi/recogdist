library(igraph)
library(readr)
library(tidyr)
library(RColorBrewer)
library(networkD3)

sfl3 <- "C:/Users/User/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021BigramYESonly.csv"
sfl3 <- "C:/Users/User/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021BigramNOonly.csv"
sfl3 <- "C:/Users/User/Desktop/Text Mining Life/COGDIST/September 2022/Dataset2021BigramAll.csv"


######create network
#nodes <- read.csv(file.choose(), header=T)
nodes <- read.csv(sfl3, header=T)
g <- graph.data.frame(cbind(nodes$word1, nodes$word2), directed=T)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

#Use Induced Subgraph Function if you want to select subset of the graph
g2 <- induced_subgraph(g, V(g)$degree > 300)

#histogram
hist(V(g2)$degree,
     col = "green",
     main = "Histogram of Node Degree",
     ylab = "Frequency",
     xlab = "Degree of Vertices")
#network diagram
set.seed(222)
plot(g2,
     vertex.color = rainbow(52),
     vertex.size = V(g2)$degree*0.05,
     edge.arrow.size = 0.1,
     edge.color = "pink",
     edge.width = 0.1,
     layout = layout.auto
)


#network measures

gorder(g) #number of vertices
gsize(g) #number of edges
#centrality
centr_degree(g, mode = "all")
centr_degree(g, mode = "in")
centr_degree(g, mode = "out")
graph.density(g) #density
reciprocity(g) #reciprocity
transitivity(g) #transitivity
#diameter
diam <- get_diameter(g, directed=T)
diam
mean_distance(g) #mean distance
ecount(g)/(vcount(g)*(vcount(g)-1)) #edgeCount #vertexCount
closeness(g, mode="all", weight=NA)
betweenness(g, directed=T, weights=NA)
#hubs and authorities
hs <- hub_score(g, weights=NA)$vector
as <- authority_score(g, weights=NA)$vector
par(mfrow=c(1,2))
plot(g, vertex.label=NA, main="Hubs")
plot(g, vertex.label=NA, main="Authorities")
#

library(networkD3)
#netw <- data.frame(cbind(nodes$word1, nodes$word2))
#simpleNetwork(netw)
wc <- cluster_walktrap(g2)
members <- membership(wc)
# Convert to object suitable for networkD3
g2d3 <- igraph_to_networkD3(g2, group = members)
# Create force directed network plot
forceNetwork(Links = g2d3$links, Nodes = g2d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 20, zoom = TRUE
             )

