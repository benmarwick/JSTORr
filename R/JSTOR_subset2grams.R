#' Subset 2gram object
#' 
#' @description Subsets bigrams 





JSTOR_subset2grams <- function(unpack2grams, bigram){

y <- unpack2grams$bigrams
biblio <- unpack2grams$bibliodata

# get articles with that word
y1 <- y[,y$dimnames$Terms == bigram]
# get matrix of frequencies of that word over all docs
y2 <- as.matrix(y1[,dimnames(y1)$Terms %in% bigram])
# subset full dtm to keep only docs with the word of interest
# plus all the other words in those docs
y3 <- y[ y$dimnames$Docs %in% names(y2[ y2 >= 1, ]), ]

biblio_word <- biblio[(as.character(biblio$x) %in% as.character(y3$dimnames$Docs)), ]

return(list("bigrams" = y3, "bibliodata" = biblio_word))

}
