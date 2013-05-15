#' Cluster documents by similarities in word frequencies
#' 
#' @description Generates plots visualizing a clustering of the documents. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them using the JSTOR_removestopwords function.
#' @param x object returned by the function JSTOR_unpack.
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents with stopwords removed.
#' @param word The word to subset the documents by, ie. use only documents containing this word in the cluster analysis
#' @param f A scalar value to filter the total number of words used in the cluster analyses. For each document, the count of each word is divided by the total number of words in that document, expressing a word's frequency as a proportion of all words in that document. This parameter corresponds to the summed proportions of a word in all documents (ie. the column sum for the document term matrix). If f = 0.01 then only words that constitute at least 1.0% of all words in all documents will be used for the cluster analyses.
#' @return Returns plots of clusters of documents, and dataframes of affinity propogation clustering, k-means and PCA outputs
#' @examples 
#' ## cl1 <- JSTOR_clusterbywords(unpack, corpus, "pirates")
#' ## cl2 <- JSTOR_clusterbywords(unpack, corpus, c("pirates", "privateers"))


JSTOR_clusterbywords <- function(x, corpus, word, f = 0.01){ 
library(tm)
message("converting corpus to document term matrix...")
dtm <- DocumentTermMatrix(corpus)
message("done")
# get only docs with a certain word
dtm_mat <- as.matrix(dtm)

# get doc numbers that contain the word of interest
docs <- unname(which(dtm_mat[, intersect(colnames(dtm_mat), word) ] >= 1))

# subset the corpus to get only those docs containing the word of interest
corpus_subset <- corpus[docs]
# make a new dtm of those subsetted docs
dtm_subset <- DocumentTermMatrix(corpus_subset)
dtm_subset_mat <- as.matrix(dtm_subset)

# explore some clustering
# inspect distribution of document lengths
# message("making a histogram of words per document...")
# hist(apply(dtm_subset_mat, 1, sum), xlab="Number of Terms in Term-Document Matrix",
#     main="Number of Words Per Document")
# Because the lengths of our documents vary so wildly 
# we may want to row-standardize our document matrix 
# (divide each entry by the number of terms in that document).
# We can perform this in R using the following code:
rowTotals <- apply(dtm_subset_mat, 1, sum)  # Find the sum of words in each Document
input <- dtm_subset_mat/rowTotals           # Divide each row by those totals
input <- input[, which(colSums(input) > f)] # subset in a arbitrary way...
rownames(input) <- names(corpus_subset)

### Various clustering methods
# get a sense of how many clusters suit the data
# using Affinity propagation (AP) clustering
# see http://dx.doi.org/10.1126/science.1136800
require(apcluster)
d.apclus <- apcluster(negDistMat(r=2), input)
k <-  length(d.apclus@clusters)

aggres1 <- aggExCluster(x=d.apclus)
message("making a cluster dendrogram of clusters...")
cl_plot <- plot(aggres1, showSamples=F, main = "Document clusters")
message("done")

require(ggplot2)
require(ggdendro)

#convert cluster object to use with ggplot
message("making a cluster dendrogram of documents...")
dendr <- dendro_data(as.dendrogram(aggres1), showSamples=TRUE, main = "Document clusters", type="rectangle")
print(ggdendrogram(dendr, rotate=TRUE, size = 3) + 
        labs(title="Document clusters") + 
        # not 100% sure these are the correct labels... best to inspect the cluster output...
        geom_text(data=label(dendr), aes(x=x, y=y), label=names(unlist(aggres1@clusters[[1]])), hjust=0, size=3))
message("done")
# k-means
cl <- kmeans(input,      # Our input term document matrix
             centers=k,  # The number of clusters
             nstart=25)  # The number of starts chosen by the algorithm


# get the top twenty words in each cluster, using the k-means output
# modified from Brandon M. Stewart
message("calculating top words per k-means cluster...")
x2 <- vector("list", k)
x3 <- vector("list", 4)
   for (i in 1:length(cl$withinss)) {
  #For each cluster, this defines the documents in that cluster
  inGroup <- which(cl$cluster==i)
  within <- dtm_subset[inGroup,]
  if(length(inGroup)==1) within <- t(as.matrix(within))
  out <- dtm_subset[-inGroup,]
  words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
  names(x2)[i] <- paste0("Cluster_", i)
  labels <- order(words, decreasing=T)[1:20] #Take the top 20 Labels
  x2[[i]] <- paste0((names(words)[labels]) )#From here down just labels
  if(i==length(cl$withinss)) {
    x3[[1]] <- ("Cluster Membership")
    x3[[2]] <- (table(cl$cluster))
    x3[[3]] <- ("Within cluster sum of squares by cluster")
    x3[[4]] <- (cl$withinss)
  }
  x4 <- c(x2,x3)
}
message("done")

message("calculating PCA...")
# PCA
require(FactoMineR,quietly = TRUE)
res.pca <- PCA(input, graph = FALSE)
# extract some parts for plotting
PC1 <- res.pca$ind$coord[,1]
PC2 <- res.pca$ind$coord[,2]
PClabs <- rownames(res.pca$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- PClabs #gsub("[[:punct:]]", "", labs)
#
# Just showing the individual samples...
library(ggplot2)
fun <- function(PCs, PClabs){
p <- ggplot(PCs, aes(PC1,PC2)) + 
  geom_text(size = 2, label = PClabs) +
  theme(aspect.ratio=1) + theme_bw(base_size = 20)
p
}
p <- fun(PCs, PClabs)
print(p)

#
fun <- function(df){
pv <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# no data so there's nothing to plot
# put a faint circle there, as is customary
pv <- pv + geom_path(aes(x, y), data = df, colour="grey70") 
pv
}
angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
pv <- fun(df)
print(pv)
#
# add on arrows and variable labels
library(grid)
fun <- function(res.pca){
  
  # Now extract variables
  #
  vPC1 <- res.pca$var$coord[,1]
  vPC2 <- res.pca$var$coord[,2] 
  vPCs <- data.frame(vPC1=vPC1,vPC2=vPC2)
  rownames(vPCs) <- rownames(res.pca$var$coord) 
 
  #
  # and plot them
  
pv <- pv + geom_text(data=vPCs, aes(x=vPC1,y=vPC2), label=rownames(vPCs), size=4) + xlab("PC1") + ylab("PC2") 
pv <- pv + geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30")
pv
}
pv <- fun(res.pca)
print(pv)
# plot docs and words side by side
library(gridExtra)
message("plotting PCA output...")
grid.arrange(p,pv,nrow=1)
return(list(cluster = aggres1, kmeans = x4, PCA = res.pca))
message("done")

}
