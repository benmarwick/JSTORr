#' Subset 1gram object
#' 
#' 
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param word The word or vector of words to subset the documents by, ie. use only documents containing this word (or words) in the cluster analysis
#' @description Subsets 1 grams 
#' @export





JSTOR_subset1grams <- function(unpack1grams, word){

y <- unpack1grams$wordcounts
biblio <- unpack1grams$bibliodata

# get articles with that word
y1 <- y[,y$dimnames$Terms == word]
# get matrix of frequencies of that word over all docs
y2 <- as.matrix(y1[,dimnames(y1)$Terms %in% word])
# subset full dtm to keep only docs with the word of interest
# plus all the other words in those docs
y3 <- y[ y$dimnames$Docs %in% names(y2[ y2 >= 1, ]), ]

biblio_word <- biblio[(biblio$x %in% y3$dimnames$Docs), ]

return(list("wordcounts" = y3, "bibliodata" = biblio_word))

}
