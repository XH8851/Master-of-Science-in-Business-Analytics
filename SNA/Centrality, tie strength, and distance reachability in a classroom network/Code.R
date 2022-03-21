library(igraph)
library(data.table)
library(reshape)
library(tidyr)
library(reshape2)
library(expm)
library(plyr)
setwd('...')



# Load the Data ----------

all_edges = fread("social_and_task_network.csv", header = TRUE)
nonzero_edges = all_edges[social_tie > 0 | task_tie > 0]
full_network = graph.data.frame(nonzero_edges)



# 1 Consider the social and task ties as two separate networks ------

# Delete the ties whose values are 0
network_social = delete.edges(full_network, E(full_network)[get.edge.attribute(full_network,name = "social_tie")==0])
network_task = delete.edges(full_network, E(full_network)[get.edge.attribute(full_network,name = "task_tie")==0])

# Delete the isolated nodes
network_social = delete.vertices(network_social, V(network_social)[degree(network_social)==0])
network_task = delete.vertices(network_task, V(network_task)[degree(network_task)==0])


## 1.A. Indegree, outdegree, closeness, betweenness, and PageRank centrality -------

# Use a matrix to store in-degree, out-degree, closeness, betweenness and pagerank centrality
corr_social = data.frame(
  in_degree = degree(network_social, mode = "in"),
  out_degree = degree(network_social, mode = "out"),
  closeness = closeness(network_social, mode = "out"),
  betweenness = betweenness(network_social),
  pagerank = page.rank(network_social)$vector
)

corr_task = data.frame(
  in_degree = degree(network_task, mode = "in"),
  out_degree = degree(network_task, mode = "out"),
  closeness = closeness(network_task, mode = "out"),
  betweenness = betweenness(network_task),
  pagerank = page.rank(network_task)$vector
)


## 1.B. Correlations of the five centrality measures ------

# The two networks contain different nodes. Find the common nodes and calculate correlations based on them.
common_node = intersect(rownames(corr_social),rownames(corr_task))

corr_matrix = cor(corr_social[common_node,], corr_task[common_node,], method="spearman", use="complete.obs")



# 2 Strong Triadic Closure? -------

# Calculate the average tie value
mean_social = mean(all_edges$social_tie[all_edges$social_tie != 0])
mean_task = mean(all_edges$task_tie[all_edges$task_tie != 0])

# Label the edge to be strong if the value > average, weak otherwise. 
# sw_edges: strong/weak edges
sw_edges = nonzero_edges
sw_edges$type[sw_edges$social_tie > mean_social | sw_edges$task_tie > mean_task] = "strong"
sw_edges$type[sw_edges$social_tie <= mean_social & sw_edges$task_tie <= mean_task] = "weak"


## 2.A. Visually in a Plot -------

# Color the weak and strong ties
sw_network = graph.data.frame(d=sw_edges, directed=F)
E(sw_network)$color <-  "blue"
E(sw_network)$color[E(sw_network)$type == "strong"] <- "red"

# Plot the graph
plot.igraph(sw_network, layout=layout.fruchterman.reingold, vertex.label.color="black", edge.color=E(sw_network)$color, vertex.size = 12, edge.arrow.size=.3, edge.curved=FALSE)

# Suppose it satisfies. From the graph, node 19 has strong ties with node 16 and 20, but node 16 and 20 do not have strong or weak ties between, which violated the Strong Triadic Closure.Therefore, the network does not satisfy Strong Triadic Closure.


## 2.B. Computationally -------

# Find the strong ties and make them a new network
strong_network = delete.edges(sw_network, E(sw_network)[get.edge.attribute(sw_network,name = "type")=='weak'])
strong_network = delete.vertices(strong_network, V(strong_network)[degree(strong_network)==0])

# violation records how many ties should exist to satisfy Strong Triadic Closure but do not.
violation = 0

# Visit each node in the network with only strong ties
for (i in V(strong_network)$name){

  # Use network with only strong ties to find how many strong ties node i connects
  strong_neighbor = unique(V(strong_network)$name[neighbors(strong_network, i, mode = "total")])
  
  # Only one strong tie to one node is not considered for Strong Triadic Closure
  if(length(strong_neighbor)==1) next
  
  # According to the closure, these neighbors should connect to each other in the sw_network
  ideal_ties = t(combn(strong_neighbor, 2))
  
  # The reality is stored in real_ties. Indirected network still store the edges in a directed way, so rbind() produces the reversed edge for any directed edge in case direction affects the result. 
  real_ties = as.data.frame(as_edgelist(sw_network))
  real_ties = rbind(real_ties[,c(2,1)],real_ties)
  
  violation = violation + nrow(ideal_ties) - nrow(match_df(as.data.frame(ideal_ties),real_ties)) 

}

# Violation is 18. Violation > 0 means that the network violates Strong Triadic Closure, consistent with the conclusion in question 2.A. It also means 18 more ties should exist to satify Strong Triadic Closure.



# 3 Continue ------
## 3.A Edge-level Betweenness -----

# edge-level betweenness of social ties
sw_social_edges = sw_edges[sw_edges$social_tie > 0]
sw_social_edges$social_bet = edge_betweenness(network_social, e = E(network_social), directed = TRUE)

# edge-level betweenness of task ties
sw_task_edges = sw_edges[sw_edges$task_tie > 0]
sw_task_edges$task_bet = edge_betweenness(network_task, e = E(network_task), directed = TRUE)


## 3.B Strong/Weak ties with High/Low Betweenness-----

# Calculate the average betweenness of social/weak ties
mean_social = mean(edge_betweenness(network_social, e = E(network_social), directed = TRUE))
mean_task = mean(edge_betweenness(network_task, e = E(network_task), directed = TRUE))

# Label strong/weak ties: ties are strong if betweenness > average, weak otherwise.
sw_edges_and_bet = merge(sw_social_edges,sw_task_edges,by=c("ego", "alter"), all = T)
sw_edges_and_bet$type_bet = "weak"
sw_edges_and_bet$type_bet[sw_edges_and_bet$social_bet > mean_social | sw_edges_and_bet$task_bet > mean_task] = "strong"

# sw_type_bet: 4 columns, including ego, alter, type (strong/weak) generated in question 2, and type (strong/weak) based on betweenness
sw_type_bet = sw_edges_and_bet[,c(1,2,5,9,11)]
sw_type_bet[which(is.na(sw_type_bet$type.x))]$type.x = sw_type_bet[which(is.na(sw_type_bet$type.x))]$type.y
sw_type_bet = sw_type_bet[,-"type.y"]

# table3 stores the number of ties which are strong/weak in question 2 and strong/weak based on betweenness.
table3 = data.frame(
  strong_ties = c(length(which(sw_type_bet$type.x == "strong" & sw_type_bet$type_bet == "strong")),
                  length(which(sw_type_bet$type.x == "strong" & sw_type_bet$type_bet == "weak"))
  ),
  weak_ties = c(length(which(sw_type_bet$type.x == "weak" & sw_type_bet$type_bet == "strong")),
                length(which(sw_type_bet$type.x == "weak" & sw_type_bet$type_bet == "weak"))
  )
)
rownames(table3) = c("high_betweenness", "low_betweenness")

# From table3, among edges with high betweenness, only 17 are strong edges, and other 34 are weak edges. Based on frequency, edges with high betweenness tend to be weak edges.

# It makes sense, because high betweenness indicates the edge is like a local bridge to connect to other nodes. Many nodes in the graph are connected only by weak ties, e.g., node 2, 6, 9, 12, 14, which means the local bridges tend to be weak edges.



# 4 Pairs of Nodes with no Walks between -----

walk_edges = nonzero_edges[,c(1,2)]
walk_edges$walk = 1
walk_network = graph.data.frame(walk_edges)

# Set a matrix of edges 
table4 = dcast(walk_edges, ego ~ alter,fill = 0)[,-1]
rownames(table4) = colnames(table4)

# The longest distance would be 21 among 22 nodes, so 21 matrix multiplication is enough.
table4 = as.matrix(table4) %^% 21

# Set the distance for diagonal elements 1 to ignore them when calculating pairs with no walks between.
diag(table4)=1

# In manual calculation, zeros in the table means no walks. Note that the pair needs division.
sum(rowSums(table4 == 0, ))/2

# In distances(), inf means no walks. Both sum() returns 38 pairs with no walks between.
sum(rowSums(distances(walk_network, mode = "out") == Inf))/2

# In conclusion, 38 pairs of nodes do not have walks between one another.



# 5 Network-level Measure of Degree Centrality -----

# Network-level Measure of Degree Centrality = 1 means all other nodes connect and only connect to one node. 
# The degree centrality = (n-1) * (n-1 - 1) / (n-1)(n-2) = 1.

graph5_1 = graph(edges = c("1", "2", "1", "3", "1", "4", "1", "5"), directed = FALSE)

# Suppose the network-level measure of centrality = sum of (max_closeness - closeness) / (n-1)(n-2)
closeness_51 = closeness(graph5_1)
closeness_centrality_51 = sum(max(closeness_51) - closeness_51) / ((length(closeness_51)-1)*(length(closeness_51)-2))

# Suppose the network-level measure of centrality = sum of (max_betweenness - betweenness) / (n-1)(n-2)
betweenness_51 = betweenness(graph5_1)
betweenness_centrality_51 = sum(max(betweenness_51) - betweenness_51) / ((length(betweenness_51)-1)*(length(betweenness_51)-2))

# The closeness centrality is 0.036. The betweenness centrality is 2. The relationship does not hold true.

# However, if I use centr_clo(graph5_1)$centralization and centr_betw(graph5_1)$centralization, the results are both 1, which means the relationship holds true. I am uncertain how the functions calculates the centrality, and thus decide not to use them.


# Network-level Measure of Degree Centrality = 0 means each node has the same degrees, so that the numerator is 0.
graph5_2 = graph(edges = c("1", "2", "1", "3", "1", "4", "1", "5", "2","3","3","4","4","5","5","2","2","4","3","5"), directed = FALSE)

closeness_52 = closeness(graph5_2)
closeness_centrality_52 = sum(max(closeness_52) - closeness_52) / ((length(closeness_52)-1)*(length(closeness_52)-2))

betweenness_52 = betweenness(graph5_2)
betweenness_centrality_52 = sum(max(betweenness_52) - betweenness_52) / ((length(betweenness_52)-1)*(length(betweenness_52)-2))

# The closeness centrality is 0. The betweenness centrality is 0. centr_clo(graph5_2)$centralization and centr_betw(graph5_2)$centralization return 0 too. The relationship holds true!


