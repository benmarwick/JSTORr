#' Build corpus and remove all words except non-name nouns
#' 
#' @description This function does part-of-speech tagging and removes all parts of speech that are not non-name nouns. It also removes punctuation, numbers, words with less than three characters, stopwords and unusual characters (characters not in ISO-8859-1, ie non-latin1-ASCII). For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @return Returns a corpus containing documents, ready for more advanced text mining and topic modelling.  
#' @examples 
#' ## mycorpus1 <- JSTOR_corpusofnouns(unpack) 




JSTOR_corpusofnouns <- function(x){

wordcounts <- x$wordcounts
bibliodata <- x$bibliodata
library(tm)
message("converting vectors to a corpus...")
mycorpus <-  Corpus(VectorSource(wordcounts))
message("done")
# clean and simplify the text
message("removing stopwords, punctuation, white spaces, numbers...")
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
mycorpus.clean <- tm_map(mycorpus, FUN = tm_reduce, tmFuns = funcs)
message("done")

#### do POS tagging and remove non-nouns
# install packages
message("applying part of speech tags to words...")
library("openNLP", "openNLPmodels.en")
# apply POS tags, this can take a very long time...
mycorpus.clean.POStag <- list(vector, length(mycorpus.clean))
for(i in 1:length(mycorpus.clean)){
  cat(paste0("tagged ", i," of ", length(mycorpus.clean), " documents\n")) 
  mycorpus.clean.POStag[[i]] <- tmTagPOS(mycorpus.clean[[i]])
  invisible(gc(verbose = FALSE)) # force empty RAM between each iteration
  cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
}
message("done")
message("extracting non-name nouns and discarding the rest...")
# split the word/tag strings to make a dataframe with words in 
# one col and tags in another col
mycorpus.POStag.split <- llply(1:length(mycorpus.clean.POStag), function(i)  read.table(textConnection(gsub(" ", "\n", mycorpus.clean.POStag[[i]])), sep="/", stringsAsFactors=FALSE), .progress = "text", .inform = FALSE)
# just keep NNs
mycorpus.nouns <- llply(1:length(mycorpus.POStag.split), function(i) mycorpus.POStag.split[[i]][mycorpus.POStag.split[[i]]$V2 == "NN" | mycorpus.POStag.split[[i]]$V2 == "NNS",], .progress = "text", .inform = FALSE)
# remove words with less than 3 characters
mycorpus.nouns <- llply(1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][nchar(mycorpus.nouns[[i]]$V1)  > 3, ], .progress = "text", .inform = FALSE)
# remove common human names http://www.fakenamegenerator.com/ http://www.census.gov/genealogy/www/data/2000surnames/index.html
babynames <- tolower(read.csv("https://raw.github.com/hadley/data-baby-names/master/baby-names.csv")[,2])
mycorpus.nouns  <- llply(1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][!mycorpus.nouns[[i]]$V1 %in% babynames, ], .progress = "text", .inform = FALSE)
# make character string of NNs for topic modelling
mycorpus.noun.strings <- llply(1:length(mycorpus.nouns), function(i) paste(mycorpus.nouns[[i]][,1], collapse = ", "), .progress = "text", .inform = FALSE)
# remove odd characters
mycorpus.noun.strings <- llply(1:length(mycorpus.noun.strings), function(j) iconv(mycorpus.noun.strings[[j]], "latin1", "ASCII", sub=""), .progress = "text", .inform = FALSE)
# remove punctuation
mycorpus.noun.strings <- llply(1:length(mycorpus.noun.strings), function(j) gsub("[[:punct:]]", "",  mycorpus.noun.strings[[j]]), .progress = "text", .inform = FALSE)
message("done")
# put in corpus
message("converting vectors back into a corpus")
mycorpus <- Corpus(VectorSource(mycorpus.noun.strings))
message("all done")
return(mycorpus)
}




