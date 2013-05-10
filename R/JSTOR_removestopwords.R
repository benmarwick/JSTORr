#' Removes stopwords from a corpus
#' 
#' @description This function removes stopwords from a corpus (it is a simple wrapper for tm::removeWords). Note that the function JSTOR_corpusofnouns contains this stopword removal function also, but it can be very slow due to the part-of-speech tagging. As a convenience, the stopword removal function is provided separately to enable quick repeats of the stopword removal process as the stopword list is updated and other functions are re-run.  This function uses the stopword list in the tm package. The location of tm's English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat")
#' @param corpus object returned by the function JSTOR_corpusofnouns or any corpus produced by the tm package.
#' @return Returns a corpus containing documents with stopwords removed, ready for more advanced text mining and topic modelling.  
#' @examples 
#' ## mycorpus <- JSTOR_removestopwords(corpus) 




JSTOR_removestopwords <- function(corpus){


library(tm)
# clean and simplify the text
message("removing stopwords...")
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
mycorpus.clean <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
message("done")
return(mycorpus.clean)
}




