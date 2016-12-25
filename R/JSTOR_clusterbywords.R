#' Cluster documents by similarities in word frequencies
#' 
#' @description Generates plots visualizing the results of different clustering methods applied to the documents. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them using the JSTOR_removestopwords function.
#' @param nouns the object returned by the function JSTOR_dtmofnouns. A corpus containing the documents with stopwords removed.
#' @param word The word or vector of words to subset the documents by, ie. use only documents containing this word (or words) in the cluster analysis
#' @param f A scalar value to filter the total number of words used in the cluster analyses. For each document, the count of each word is divided by the total number of words in that document, expressing a word's frequency as a proportion of all words in that document. This parameter corresponds to the summed proportions of a word in all documents (ie. the column sum for the document term matrix). If f = 0.01 then only words that constitute at least 1.0 percent of all words in all documents will be used for the cluster analyses.
#' @param custom_stopwords character vector of stop words to use in addition to the default set supplied by the tm package
#' @return Returns plots of clusters of documents, and dataframes of affinity propogation clustering, k-means and PCA outputs. The plots can be accessed and displayed using the $ function, for example with: cl1$p or plot(cl1$cl_plot) etc.
#' @examples 
#' ## cl1 <- JSTOR_clusterbywords(nouns, "pirates")
#' ## cl2 <- JSTOR_clusterbywords(nouns, c("pirates", "privateers"))
#' @import slam apcluster ggplot2 ggdendro FactoMineR grid gridExtra

JSTOR_clusterbywords <- function(nouns, word, custom_stopwords=NULL, f = 0.01){ 


y <- nouns
# get dtm that only has the word of interest (to minimize memory burden)
y1 <- y[,y$dimnames$Terms == word]

# get matrix of frequencies of that word over all docs
y2 <- as.matrix(y1[,dimnames(y1)$Terms %in% word])
# subset full dtm to keep only docs with the word of interest
# plus all the other words in those docs
y3 <- y[ y$dimnames$Docs %in% names(y2[ y2 >= 1, ]), ]

message("removing stopwords...")
y3 <- y3[, !(y3$dimnames$Terms %in% c(custom_stopwords, stopwords(kind = "en"))) ]


message("standardising word counts in the document term matrix...")
# explore some clustering
# inspect distribution of document lengths
# message("making a histogram of words per document...")
# hist(apply(dtm_subset_mat, 1, sum), xlab="Number of Terms in Term-Document Matrix",
#     main="Number of Words Per Document")
# Because the lengths of our documents vary so wildly 
# we may want to row-standardize our document matrix 
# (divide each entry by the number of terms in that document).
# We can perform this in R using the following code:

# Find the sum of all words in each Document
rowTotals <-  row_sums(y3)      
# Divide each row by those totals
input <- do.call(rbind, lapply(1:length(rowTotals), function(i) as.vector(y3[  y3$dimnames$Docs[[i]], ]/rowTotals[[i]]) ))
# subset in a arbitrary way...
rownames(input) <- y3$dimnames$Docs
colnames(input) <- y3$dimnames$Terms
input <- input[, which(colSums(input) > f)] 
message("done")

### Various clustering methods
# get a sense of how many clusters suit the data
# using Affinity propagation (AP) clustering
# see http://dx.doi.org/10.1126/science.1136800
message("calculating affinity propagation clustering...")

{{

d.apclus <- apcluster(negDistMat(r=2), input)
k <-  length(d.apclus@clusters)
}}

aggres1 <- aggExCluster(x=d.apclus)
message("done")
message("making a cluster dendrogram of clusters...")
# prevent the plot function from drawing when the function is run
png("NULL") 
cl_plot <- (plot(aggres1, showSamples=F, main = "Document clusters"))
dev.off() 
message("done")


#convert cluster object to use with ggplot
message("making a cluster dendrogram of documents...")
png("NULL") 
dendr <- dendro_data(as.dendrogram(aggres1), showSamples=TRUE, main = "Document clusters", type="rectangle")
ggden <- print(ggdendrogram(dendr, rotate=TRUE, size = 3) + 
        labs(title="Document clusters") + 
        # not 100% sure these are the correct labels... best to inspect the cluster output...
        geom_text(data=label(dendr), aes(x=x, y=y), label=names(unlist(aggres1@clusters[[1]])), hjust=0, size=3))
dev.off()

message("done")

message("calculating k-means clusters...")
# k-means
cl <- kmeans(input,      # Our input term document matrix
             centers=k,  # The number of clusters
             nstart=25)  # The number of starts chosen by the algorithm
message("done")


# get the top twenty words in each cluster, using the k-means output
# modified from Brandon M. Stewart
message("calculating top words per k-means cluster...")
x2 <- vector("list", k)
x3 <- vector("list", 4)
   for (i in 1:length(cl$withinss)) {
  cat(paste0("analysing ", i," of ", k, " clusters\n")) 
  #For each cluster, this defines the documents in that cluster
  inGroup <- which(cl$cluster==i)
  within <- y3[inGroup,]
  if(length(inGroup)==1) within <- t(as.matrix(within))
  out <- y3[-inGroup,]
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
  cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
}
message("done")

message("calculating PCA...")
# PCA

res.pca <- PCA(data.frame(input), graph = FALSE)
# extract some parts for plotting
PC1 <- res.pca$ind$coord[,1]
PC2 <- res.pca$ind$coord[,2]
PClabs <- rownames(res.pca$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- PClabs #gsub("[[:punct:]]", "", labs)
#
# Just showing the individual samples...

fun <- function(PCs, PClabs){
  ggplot(PCs, aes(PC1,PC2)) + 
  geom_text(size = 2, label = PClabs) +
  theme(aspect.ratio=1) + theme_bw(base_size = 20)
}

png("NULL") 
p <- fun(PCs, PClabs)
dev.off()


#
fun <- function(df){
pv <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# no data so there's nothing to plot
# put a faint circle there, as is customary
pv <- pv + geom_path(aes(x, y), data = df, colour="grey70") 
}

angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 

png("NULL") 
pv <- fun(df)
dev.off()

#
# add on arrows and variable labels
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
}

png("NULL") 
pv <- fun(res.pca)
dev.off()


# plot docs and words side by side
message("plotting PCA output...")
png("NULL") 
arranged <-  arrangeGrob(p,pv,nrow=1)
dev.off()


message("plotting affinity propagation clustering output...")
png("NULL") 
apc <- plot(d.apclus, as.matrix(cbind(PC1, PC2)))
dev.off()

message("plotting k-means clustering output...")
df <- data.frame(as.matrix(cbind(PC1, PC2)))
df$cluster = factor(cl$cluster)

# The following graph color codes the points by cluster
# 
png("NULL") 
q <- ggplot(data=df, aes(x=PC1, y=PC2, color=cluster)) + 
  # geom_point() +
  stat_ellipse() +
  geom_text(aes(label=cluster), size=5)
dev.off()
  

return(list(cluster = aggres1, kmeans = x4, PCA = res.pca, cl_plot = cl_plot, ggden = ggden, p = p, pv = pv, q = q, arranged = arranged, apc = apc ))

}
