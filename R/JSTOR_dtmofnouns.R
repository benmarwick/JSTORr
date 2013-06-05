#' Make a Document Term Matrix containing only nouns
#' 
#' @description This function does part-of-speech tagging and removes all parts of speech that are not non-name nouns. It also removes punctuation, numbers, words with less than three characters, stopwords and unusual characters (characters not in ISO-8859-1, ie non-latin1-ASCII). For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). This function uses the stoplist in the tm package. The location of tm's English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat") Note that the part-of-speech tagging can result in the removal of words of interest. To prevent the POS tagger from removing these words, edit the tagdict file and add the word(s) with a NN tag. To find the tagdict file, enter this at the R prompt: at the R prompt: paste0(.libPaths()[1], "/openNLPmodels.en/models/parser/tagdict") and edit with a text editor.
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param sparse A numeric for the maximal allowed sparsity, default is 0.999. Removes sparse terms from a term-document matrix, see help(removeSparseTerms) for more details. Values close to 1 result in a sparse matrix, values close to zero result in a dense matrix. It may be useful to reduce sparseness if the matrix is too big to manipulate in memory or if processing times are long.
#' @param parallel  logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may actually be slower if you have one core, limited memory or if 
#' the data set is small due to communication of data between the cores.
#' @return Returns a Document Term Matrix containing documents, ready for more advanced text mining and topic modelling.  
#' @examples 
#' ## nouns <- JSTOR_dtmofnouns(unpack1grams) 




JSTOR_dtmofnouns <- function(unpack1grams, parallel=FALSE, sparse=0.999){

y <- unpack1grams$wordcounts



# somehow docs got to be a factor... fix this
# and why do I have to subset the Docs like this?
# something to do with how the DTM is made? I bet
# the CSV file is dodgy... 
y$dimnames$Docs <- as.character(y$dimnames$Docs)
y <- y[unique(as.character(y$dimnames$Docs[1:nrow(y)])), ]

# reduce size of DTM in case of memory or speed issues
y <- removeSparseTerms(y, sparse)


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

message("keeping only non-name nouns...")
library(openNLP)

if(parallel) {
  
  # parallel version
  y1 <- strsplit(y$dimnames$Terms,",")
  require(parallel); library(openNLPmodels.en)
  cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
  clusterEvalQ(cl, {
    openNLP::tagPOS
    library(openNLPmodels.en)
  })
  clusterExport(cl, varlist = "y1", envir=environment())
  
  pos <- parLapply(cl, seq_along(y1), function(i) {
    x <- openNLP::tagPOS(y1[[i]], language = "en")
    if (i%%10==0) invisible(gc())
    x
  })
  
  
  stopCluster(cl)
  
  
 } else { # non-parallel method

  pos <- tagPOS(y$dimnames$Terms)

}

y <- y[,grep("/NN", pos)]


# clean up
invisible(gc())
message("all done")
return(y)
}




