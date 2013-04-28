#' Divide documents up into chunks of n words 
#' 
#' @description Divides documents up into chunks of n words, ready for topic modelling. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents.
#' @param n the number of words that each chunk should contain
#' @return Returns a list of character vectors. Each vector is a chunk of n words. The vectors have names corresponding to the first column of the bibliodata data frame.
#' @examples 
#' ## chunk1 <- JSTOR_chunk(x = unpacked, corpus = corpus, n = 1000) 


JSTOR_chunk <- function(x, corpus, n)
  {
# convert corpus to list of character vectors
message("converting corpus to list of vectors...")
listofwords <- vector("list", length(corpus))
for(i in 1:length(corpus))
  {
  listofwords[[i]] <- corpus[[i]]
  }
message("done")
# divide each vector into chunks of n words
# from http://stackoverflow.com/q/16232467/1036500
f <- function(x) 
{
 y <- unlist(strsplit(x, " "))
 ly <- length(y)
 split(y, gl(ly%/%n+1, n, ly))
}
message("splitting documents into chunks...")
listofnwords1 <- sapply(listofwords, f)
listofnwords2 <- unlist(listofnwords1, recursive = FALSE)
message("done")
# append IDs to list items so we can get bibliographic data for each chunk
lengths <- sapply(1:length(listofwords), function(i) length(listofnwords1[[i]]))
names(listofnwords2) <- unlist(lapply(1:length(lengths), function(i) rep(x$bibliodata$x[i], lengths[i])))
names(listofnwords2) <- paste0(names(listofnwords2), "_", unlist(lapply(lengths, function(x) seq(1:x))))
return(listofnwords2)
}





