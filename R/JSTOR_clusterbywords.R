#' Cluster documents by similarities in word frequencies
#' 
#' @description Generates plots visualizing a clustering of the documents. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them using the JSTOR_removestopwords function.
#' @param x object returned by the function JSTOR_unpack.
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents with stopwords removed.
#' @param word The word to subset the documents by, ie. use only documents containing this word in the cluster analysis
#' @return Returns plots of clusters of documents, and dataframes of k-means and PCA output
#' @examples 
#' ## cl1 <- JSTOR_clusterbywords(unpack, corpus, "pirates")
#' ## cl2 <- JSTOR_clusterbywords(unpack, corpus, c("pirates", "privateers"))


JSTOR_clusterbywords <- function(x, corpus, word){ 
library(tm)
message("converting corpus to document term matrix...")
dtm <- DocumentTermMatrix(corpus)
# get only docs with a certain word
dtm_mat <- as.matrix(dtm)
# vector of words to subset dtm by
# ie. keep only docs with these words
word <- c("gender")

# get doc numbers that contain the word of interest
docs <- unname(which(dtm_mat[, intersect(colnames(dtm_mat), word) ] >= 1))

# subset the corpus to get only those docs containing the word of interest
corpus_subset <- corpus[docs]
# make a new dtm of those subsetted docs
dtm_subset <- DocumentTermMatrix(corpus_subset)
dtm_subset_mat <- as.matrix(dtm_subset)

# explore some clustering
# inspect distribution of document lengths
hist(apply(dtm_subset_mat, 1, sum), xlab="Number of Terms in Term-Document Matrix",
     main="Number of Terms Per Article")
# Because the lengths of our documents vary so wildly 
#we may want to row-standardize our document matrix 
# (divide each entry by the number of terms in that document).
# We can perform this in R using the following code:
rowTotals <- apply(dtm_subset_mat, 1, sum) # Find the sum of words in each Document
input <- dtm_subset_mat/rowTotals          # Divide each row by those totals
input <- input[, which(colSums(input) > 0.01)] # subset in a arbitrary way...
rownames(input) <- names(corpus_subset)

### Various clustering methods
# get a sense of how many clusters suit the data
# using Affinity propagation (AP) clustering
# see http://dx.doi.org/10.1126/science.1136800
library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), input)
k <-  length(d.apclus@clusters)

aggres1 <- aggExCluster(x=d.apclus)

cl_plot <- plot(aggres1, showSamples=TRUE, main = "Document clusters", nodePar=list(pch=NA, lab.cex=0.4))
# how to get DOI as names here?

library(ggplot2)
library(ggdendro)

#convert cluster object to use with ggplot
dendr <- dendro_data(cl_plot, type="rectangle") 

labs <- names(unlist(aggres1@clusters[[1]]))
#your own labels are supplied in geom_text() and label=labs
ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x=x, y=y, label=labs, hjust=0), size=3) +
  coord_flip() + 
  scale_y_reverse(expand=c(0.2, 0)) + 
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())


# k-means
cl <- kmeans(input,           # Our input term document matrix
             centers=k,  # The number of clusters
             nstart=25)  # The number of starts chosen by the algorithm


# get the top twenty words in each cluster, using the k-means output
# from Brandon M. Stewart
for (i in 1:length(cl$withinss)) {
  #For each cluster, this defines the documents in that cluster
  inGroup <- which(cl$cluster==i)
  within <- dtm_subset[inGroup,]
  if(length(inGroup)==1) within <- t(as.matrix(within))
  out <- dtm_subset[-inGroup,]
  words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
  print(c("Cluster", i), quote=F)
  labels <- order(words, decreasing=T)[1:20] #Take the top 20 Labels
  print(names(words)[labels], quote=F) #From here down just labels
  if(i==length(cl$withinss)) {
    print("Cluster Membership")
    print(table(cl$cluster))
    print("Within cluster sum of squares by cluster")
    print(cl$withinss)
  }
}

# PCA
library(FactoMineR)
res.pca <- PCA(input, graph = FALSE)
# extract some parts for plotting
PC1 <- res.pca$ind$coord[,1]
PC2 <- res.pca$ind$coord[,2]
labs <- rownames(res.pca$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- labs
#
# Just showing the individual samples...
library(ggplot2)
p <- ggplot(PCs, aes(PC1,PC2, label=rownames(PCs))) + 
  geom_text(size = 2) +
  theme(aspect.ratio=1) + theme_bw(base_size = 20)
# Now extract variables
#
vPC1 <- res.pca$var$coord[,1]
vPC2 <- res.pca$var$coord[,2]
vlabs <- rownames(res.pca$var$coord)
vPCs <- data.frame(cbind(vPC1,vPC2))
rownames(vPCs) <- vlabs
colnames(vPCs) <- colnames(PCs)
#
# and plot them
#
pv <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# no data so there's nothing to plot
# put a faint circle there, as is customary
angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
pv <- pv + geom_path(aes(x, y), data = df, colour="grey70") 
#
# add on arrows and variable labels
library(grid)
pv <- pv + geom_text(data=vPCs, aes(x=vPC1,y=vPC2,label=rownames(vPCs)), size=4) + xlab("PC1") + ylab("PC2") 
pv <- pv + geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30")

# plot docs and words side by side
library(gridExtra)
grid.arrange(p,pv,nrow=1)

}
