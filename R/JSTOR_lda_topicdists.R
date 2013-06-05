#' Calculate Euclidean distances between topics 
#' 
#' @description Generates a dendrogram of topic clusters, a network plot of topic-topic relationships, and a graphml file to open with Gephi. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param lda the object returned by the function JSTOR_lda.
#' @return Returns plots of the topic clusters and network and a graphml file in the working directory that can be opened with Gephi
#' @examples 
#' ## JSTOR_lda_topicdists(lda = lda150) 



JSTOR_lda_topicdists <- function(lda){
  
  # unpack output from JSTOR_lda
  topic.props <- lda[[1]]  
  # if want to take logs, so adjust zeros to avoid -Inf
  # topic.props[topic.props == 0] <- 0.0000000000001
  
  #### Euclidean distance matrix on topics
  library(cluster)
  topic.props.dists1 <-  as.matrix(daisy(t((topic.props[, !(colnames(topic.props) %in% c("ID","year"))])), metric =  "euclidean", stand = TRUE))
  # Change row values to zero if less than row minimum plus row standard deviation
  # This is how Jockers subsets the distance matrix to keep only 
  # closely related documents and avoid a dense spagetti diagram 
  # that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
  topic.props.dists2 <- topic.props.dists1
  topic.props.dists1[ sweep(topic.props.dists1, 1, (apply(topic.props.dists1,1,min) + apply(topic.props.dists1,1,sd) )) > 0 ] <- 0
  
  ## dendrogram
  plot(hclust(dist(topic.props.dists2)), xlab = "topic clusters", sub = "", main = "")
  ## network plot
  library(igraph)
  g <- as.undirected(graph.adjacency(topic.props.dists1))
  layout1 <- layout.fruchterman.reingold(g, niter=500)
  plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1,  vertex.color= "grey", edge.arrow.size = 0.1, vertex.label.dist=0.5, vertex.label = NA)
  
  write.graph(g, file="topics.graphml", format="graphml") 
  message(paste0("The topics.graphml file for Gephi can be found in ", getwd()))
  return(topic.props.dists1)
}

