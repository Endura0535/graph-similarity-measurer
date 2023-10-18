rm(list = ls())

library(igraph)

# load graph data
g1 <- read.graph("g1.csv", directed = FALSE)
g2 <- read.graph("g2.csv", directed = FALSE)

igraph::vcount(g1)
igraph::ecount(g1)
igraph::vcount(g2)
igraph::ecount(g2)
#plot(g2)

## setting G ##
V(g1)$name <- V(g1)
V(g2)$name <- V(g2)


## cal centrality 
# G1
cntr <- degree(g1, V(g1), normalized = TRUE) #degree, closeness, betweenness로 변경해서 결과 도출
cntr <- data.frame("node" = V(g1)$name, "value" = cntr)
#cntr <- cntr[order(-cntr$value),]

# G2
cntr2 <- degree(g2, V(g2), normalized = TRUE) #degree, closeness, betweenness로 변경해서 결과 도출
cntr2 <- data.frame("node" = V(g2)$name, "value" = cntr2)
#cntr2 <- cntr2[order(-cntr2$value),]


## top N% node ##
topN1 <- vector("list", 9)
topN2 <- vector("list", 9)
for(i in 1:9)
{
  topN1[[i]] <- subset(cntr$node, cntr$value > quantile(cntr$value, prob = 1 - i/10), select = cntr$node)
  topN2[[i]] <- subset(cntr2$node, cntr2$value > quantile(cntr2$value, prob = 1 - i/10), select = cntr$node)
}

## top N% Union g1, g2 ##
tot_len <- length(union(V(g1)$name, V(g2)$name))
tot_len
topNSum <- vector("list", 9)
Nper <- c()
for(i in 1:9)
{
  topNSum[[i]] <- union(topN1[[i]], topN2[[i]])
  Nper[i] <- length(topNSum[[i]])/tot_len
}
Nper # real N%


## get GED g1, g2##
adj1 <- as_adjacency_matrix(g1)
adj2 <- as_adjacency_matrix(g2)
o_ged <- sum(xor(adj1, adj2)) / (nrow(adj1) * ncol(adj2))
o_ged

ged <- c()
diff <- c()
for(i in 1:9)
{
  # get subgraph
  sub1 <- induced.subgraph(g1, intersect(topNSum[[i]], V(g1)$name))
  sub2 <- induced.subgraph(g2, intersect(topNSum[[i]], V(g2)$name))
  
  # get subgraph's adjacency matrix
  subadj1 <- as_adjacency_matrix(sub1)
  subadj2 <- as_adjacency_matrix(sub2)

  # cal GED (edge insert = 1, delete = 1)
  diff[i] <- (sum(xor(subadj1, subadj2)))
  ged[i] <- diff[i] / (nrow(subadj1) * ncol(subadj2))
}
ged

## get diff original vs subgraph
err <- c()
for(i in 1:9)
{
  err[i] <- abs(o_ged - ged[i])/o_ged * 100
}
err

## result 
#cntr # G1's centrality
#cntr2 # G2's centrality
o_ged # G1-G2 GED
ged # 10~90% subgraph's GED
err #error
Nper
