require(igraph)

# Build graph based on directed adjacency matrix
adjacencyModel <- function(adj) {
  # Get list of events
  elt_list <- colnames(adj)
  # Build arrow flow
  flow = c()
  for (i in 1:dim(adj)[1]) {
    for (j in 1:dim(adj)[2]) {
      if (adj[i, j]) {
        flow <- c(flow, elt_list[i], elt_list[j])
      }
    }
  }
  # Find any isolated events
  isolated <- elt_list[rowSums(adj) == 0 & colSums(adj) == 0]
  graph(flow, isolates = isolated)
}

adjacencyLoopModel <- function(adj) {
  
}