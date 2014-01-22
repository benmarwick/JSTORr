#' Make a Document Term Matrix containing only nouns
#' 
#' @description This function does part-of-speech tagging and removes all parts of speech that are not non-name nouns. It also removes punctuation, numbers, words with less than three characters, stopwords and unusual characters (characters not in ISO-8859-1, ie non-latin1-ASCII). For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). This function uses the stoplist in the tm package. The location of tm's English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat") Note that the part-of-speech tagging can result in the removal of words of interest. To prevent the POS tagger from removing these words, edit the tagdict file and add the word(s) with a NN tag. To find the tagdict file, enter this at the R prompt: at the R prompt: paste0(.libPaths()[1], "/openNLPmodels.en/models/parser/tagdict") and edit with a text editor.
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param word Optional word or vector of words to subset the documents by, ie. make a document term matrix containing only documents in which this word (or words) appears at least once.
#' @param sparse A numeric for the maximal allowed sparsity, default is one (ie. no sparsing applied). Removes sparse terms from a term-document matrix, see help(removeSparseTerms) for more details. Values close to 1 result in a sparse matrix, values close to zero result in a dense matrix. It may be useful to reduce sparseness if the matrix is too big to manipulate in memory or if processing times are long.
#' @param parallel  logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may actually be slower if you have one core, limited memory or if 
#' the data set is small due to communication of data between the cores.
#' @param POStag logical Do part-of-speech tagging to identify and remove non-nouns. Default is True, but the option is here to speed things up when working interactively with large numbers of documents. 
#' @return Returns a Document Term Matrix containing documents, ready for more advanced text mining and topic modelling.  
#' @examples 
#' ## nouns <- JSTOR_dtmofnouns(unpack1grams) 




JSTOR_dtmofnouns <- function(unpack1grams, word=NULL, parallel=FALSE, sparse=1, POStag=TRUE){

y <- unpack1grams$wordcounts

if (length(word) == 1) {
  
# if we are subsetting for documents that contain a specific word...
  
# get dtm that only has the word of interest (to minimize memory burden)
y1 <- y[,y$dimnames$Terms == word]
# get matrix of frequencies of that word over all docs
y2 <- as.matrix(y1[,dimnames(y1)$Terms %in% word])
# subset full dtm to keep only docs with the word of interest
# plus all the other words in those docs
y3 <- y[ y$dimnames$Docs %in% names(y2[ y2 >= 1, ]), ]
y <- y3
# clean up
rm(y1, y2, y3)

} else {
  # just return the full set if no word is specified for subsetting
  y <- unpack1grams$wordcounts
}


# somehow docs got to be a factor... fix this
# and why do I have to subset the Docs like this?
# something to do with how the DTM is made? I bet
# the CSV file is dodgy... 
y$dimnames$Docs <- as.character(y$dimnames$Docs)
y <- y[unique(as.character(y$dimnames$Docs[1:nrow(y)])), ]

# reduce size of DTM in case of memory or speed issues
if (sparse == 1){
  # don't reduce the size
} else {
  # do reduce the size
  y <- removeSparseTerms(y, sparse)
}


library(tm)
# reduce the size of the dtm... select the dtm directly
# https://stat.ethz.ch/pipermail/r-help/2011-May/278202.html


message("removing stopwords...")
y <- y[, !(y$dimnames$Terms %in% stopwords(kind = "en")) ]
message("done")

message("discarding words with <3 characters (probably OCR errors)...")
y <- y[,nchar(y$dimnames$Terms) > 3]
message("done")

message("discarding words with >2 consecutive characters (probably OCR errors)...")
y <- y[,!grepl("(.)\\1{2,}", y$dimnames$Terms)]
message("done")

message("discarding non-ASCII characters...")
y <- y[,(y$dimnames$Terms %in% iconv(y$dimnames$Terms, "latin1", "ASCII", sub=""))]
message("done")

# message("discarding common first names...")
# # remove common human names http://www.fakenamegenerator.com/ http://www.census.gov/genealogy/www/data/2000surnames/index.html
# babynames <- tolower(data.table::fread("http://raw.github.com/hadley/data-baby-names/master/baby-names.csv"))
# bn <-   gsub("[[:punct:]]", "", unlist(unlist((strsplit(babynames[2], split=", ")))) )
# y <- y[,!(y$dimnames$Terms %in% bn)]
# message("done")
if (POStag == TRUE) {
message("keeping only non-name nouns...")
# openNLP changed, so we need this replacement for tagPOS...
library(NLP)
tagPOS <-  function(x) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  PTA <- Maxent_POS_Tag_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, PTA, a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}



library(openNLP)

if(parallel) {
  
  # parallel version
  y1 <- strsplit(y$dimnames$Terms,",")
  require(parallel); library(openNLPmodels.en)
  cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
  clusterEvalQ(cl, {
    tagPOS
    library(openNLPmodels.en)
  })
  clusterExport(cl, varlist = "y1", envir=environment())
  
  pos <- parLapply(cl, seq_along(y1), function(i) {
    x <- tagPOS(y1[[i]], language = "en")
    if (i%%10==0) invisible(gc())
    x
  })
  
  
  stopCluster(cl)
  
  
 } else { # non-parallel method


  pos <- JSTORr:::tagPOS(y$dimnames$Terms)

}

y <- y[,grep("/NN", unlist(strsplit(pos$POStagged, " ,/, ")))]
} else { 
  # don't do POS tagging 
}

# clean up
invisible(gc())
message("all done")
return(y)
}





