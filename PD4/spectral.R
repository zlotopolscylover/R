library(Rcpp)
sourceCpp('cpp_Mnn.cpp')

#macierz G

macierz_G <- function(S){
  G <- matrix(0, ncol= nrow(S), nrow=nrow(S))
  for(i in 1:nrow(G)){
    G[i,unique(S[i,])] <- 1
  }
  return (G)
}

#uspójnienie macierzy G

library(igraph)
uspojnienie <-  function(G){
graf <-  graph_from_adjacency_matrix(G)
d <- bfs(graf,root=1,neimode="all", father = TRUE)
v <- which(is.na(d$father)==TRUE)
for(i in 2:length(v)){
  G[v[1],v[i]] <- 1
}
return(G)
}

#laplasjan

Laplacian_eigen <- function(G,k){
  stopifnot(k>1)
  D_matrix <- matrix(0,nrow=nrow(G),ncol=ncol(G))
  for(i in 1:nrow(G)){
    D_matrix[i,i] <- sum(G[i,])
  }
  L_matrix <- D_matrix - G
  E_matrix <- eigen(L_matrix)$vectors[,(nrow(G)-1):(nrow(G)-k)]
  
  return(E_matrix)
}

#uspójniony graf 

Mnn_Graf <- function(G){
  g <- macierz_G(G)
  G <- uspojnienie(g)
  return(G)
}

#f. spectral clustering

spectral_clustering <- function(X,k,M){
  Mnn <- Mnn(X,M)
  G <- MnnGraf(Mnn)
  L <- Laplacian_eigen(G,k)
  k1 <- kmeans(L, k)
  return(k1)
}






