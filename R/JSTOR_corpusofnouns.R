#' Remove all words except non-name nouns
#' 
#' @description This function does part-of-speech tagging and removes all parts of speech that are not non-name nouns. It also removes punctuation, numbers, words with less than three characters, stopwords and unusual characters (characters not in ISO-8859-1, ie non-latin1-ASCII). For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). This function uses the stoplist in the tm package. The location of tm's English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat") Note that the part-of-speech tagging can result in the removal of words of interest. To prevent the POS tagger from removing these words, edit the tagdict file and add the word(s) with a NN tag. To find the tagdict file, enter this at the R prompt: at the R prompt: paste0(.libPaths()[1], "/openNLPmodels.en/models/parser/tagdict") and edit with a text editor.
#' @param x object returned by the function JSTOR_unpack.
#' @param parallel if TRUE, apply function in parallel, using the parallel library
#' @return Returns a Document Term Matrix containing documents, ready for more advanced text mining and topic modelling.  
#' @examples 
#' ## nouns <- JSTOR_corpusofnouns(unpack, parallel = TRUE) 




JSTOR_corpusofnouns <- function(x, parallel=FALSE){

wordcounts <- x$wordcounts

  
# Full-text method
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
rm("mycorpus", "funcs", "skipWords"); invisible(gc())
message("done")

#### do POS tagging and remove non-nouns

if(parallel) { # if parallel == TRUE, do this top set...
  
  message("applying part of speech tags to words, this may take some time...")
  
  # parallel method adapted from https://github.com/trinker/qdap/blob/master/R/pos.R
  tagger <- function(x, gc.rate=10){ 
    n <- length(x)
    require(parallel); require(tm); require(openNLP)
    cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
    clusterExport(cl=cl, varlist=c("x", "n", "gc.rate", "tmTagPOS"), 
                  envir=environment())
    x <- parLapply(cl, seq_len(n), function(i) {
      x <- tmTagPOS(x[[i]])
      if (i%%gc.rate==0) gc()
      return(x)
    }
    )
    stopCluster(cl)  #stop the cluster
    return(x)
  }
  mycorpus.clean.POStag <- tagger(mycorpus.clean)
  
} else { # if parallel == FALSE, do this top set...

message("applying part of speech tags to words...")
library("openNLP")
# apply POS tags, this can take a very long time...
mycorpus.clean.POStag <- vector("list", length = length(mycorpus.clean))

for(i in 1:length(mycorpus.clean)){
  cat(paste0("tagged ", i," of ", length(mycorpus.clean), " documents\n")) 
  mycorpus.clean.POStag[[i]] <- tmTagPOS(mycorpus.clean[[i]])
  if (i%%10) invisible(gc(verbose = FALSE)) # force empty RAM evry 100 iterations
  cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
}

}
# clean up
rm("mycorpus.clean"); if ( exists("tagger")) {rm("tagger") ; invisible(gc())} else {invisible(gc()) } 

###### end POS-tagging ####

###### now do a bunch of small data-cleaning jobs...

if(parallel) { # if parallel == TRUE, do this top set...

message("done")
message("extracting non-name nouns and discarding the rest...")
# parallel setup
library("plyr"); require(parallel)
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.clean.POStag"), envir=environment())

# split the word/tag strings to make a dataframe with words in 
# one col and tags in another col
mycorpus.POStag.split <- parLapply(cl, 1:length(mycorpus.clean.POStag), function(i)  read.table(textConnection(gsub(" ", "\n", mycorpus.clean.POStag[[i]])), sep="/", stringsAsFactors=FALSE))
stopCluster(cl); rm(cl)

# just keep NNs
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.POStag.split"), envir=environment())
mycorpus.nouns <- parLapply(cl, 1:length(mycorpus.POStag.split), function(i) mycorpus.POStag.split[[i]][mycorpus.POStag.split[[i]]$V2 == "NN" | mycorpus.POStag.split[[i]]$V2 == "NNS",])
stopCluster(cl); rm(cl)

message("done")
message("discarding words with <3 characters...")

# remove words with less than 3 characters
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.nouns"), envir=environment())
mycorpus.nouns <- parLapply(cl, 1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][nchar(mycorpus.nouns[[i]]$V1)  > 3, ])
stopCluster(cl); rm(cl)

message("done")
message("discarding words with >2 consecutive characters (probably OCR errors...")

# remove words with more than two consequtive characters (probably OCR errors)
# also converts data frames to vectors
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.nouns"), envir=environment())
mycorpus.nouns <- parLapply(cl, 1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]]$V1[!grepl("(.)\\1{2,}", mycorpus.nouns[[i]]$V1)])
stopCluster(cl); rm(cl)

message("done")
message("discarding common first names...")

# remove common human names http://www.fakenamegenerator.com/ http://www.census.gov/genealogy/www/data/2000surnames/index.html
babynames <- tolower(data.table::fread("https://raw.github.com/hadley/data-baby-names/master/baby-names.csv")[,2])

cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.nouns","babynames"), envir=environment())
mycorpus.nouns  <- parLapply(cl, 1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][!mycorpus.nouns[[i]] %in% babynames ])
stopCluster(cl); rm(cl)

# make character string of NNs for topic modelling
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.nouns"), envir=environment())
mycorpus.noun.strings <- parLapply(cl, 1:length(mycorpus.nouns), function(i) paste(mycorpus.nouns[[i]], collapse = ", "))
stopCluster(cl); rm(cl); rm("mycorpus.nouns")

message("done")
message("discarding non-ASCII characters...")

# remove odd characters
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.noun.strings"), envir=environment())
mycorpus.noun.strings <- parLapply(cl, 1:length(mycorpus.noun.strings), function(j) iconv(mycorpus.noun.strings[[j]], "latin1", "ASCII", sub=""))
stopCluster(cl); rm(cl)

message("done")
message("discarding punctuation...")

# remove punctuation
cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
clusterExport(cl=cl, varlist=c("mycorpus.noun.strings"), envir=environment())
mycorpus.noun.strings <- parLapply(cl, 1:length(mycorpus.noun.strings), function(j) gsub("[[:punct:]]", "",  mycorpus.noun.strings[[j]]))
stopCluster(cl); rm(cl)

} else {   # if parallel == FALSE, do this bottom set...
 
 library(plyr)
 # not parallel
 
 message("done")
 message("extracting non-name nouns and discarding the rest...")
 
 # just keep NNs
 mycorpus.POStag.split <- llply(1:length(mycorpus.clean.POStag), function(i)  read.table(textConnection(gsub(" ", "\n", mycorpus.clean.POStag[[i]])), sep="/", stringsAsFactors=FALSE), .progress = "text", .inform = FALSE)
 mycorpus.nouns <- llply(1:length(mycorpus.POStag.split), function(i) mycorpus.POStag.split[[i]][mycorpus.POStag.split[[i]]$V2 == "NN" | mycorpus.POStag.split[[i]]$V2 == "NNS",], .progress = "text", .inform = FALSE)
 
 
 message("done")
 message("discarding words with <3 characters...")
 
 # remove words with less than 3 characters
 mycorpus.nouns <- llply(1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][nchar(mycorpus.nouns[[i]]$V1)  > 3, ], .progress = "text", .inform = FALSE)
 
 
 message("done")
 message("discarding words with >2 consecutive characters (probably OCR errors...")
 
 # remove words with more than two consequtive characters (probably OCR errors), also converts data frames to vectors
 mycorpus.nouns <- llply(1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]]$V1[!grepl("(.)\\1{2,}", mycorpus.nouns[[i]]$V1)], .progress = "text", .inform = FALSE)
 
 message("done")
 message("discarding common first names...")
 
 # remove common human names http://www.fakenamegenerator.com/ http://www.census.gov/genealogy/www/data/2000surnames/index.html
 babynames <- tolower(data.table::fread("https://raw.github.com/hadley/data-baby-names/master/baby-names.csv")[,2])
 mycorpus.nouns  <- llply(1:length(mycorpus.nouns), function(i) mycorpus.nouns[[i]][!mycorpus.nouns[[i]] %in% babynames ], .progress = "text", .inform = FALSE)
 # make character string of NNs for topic modelling
 mycorpus.noun.strings <- llply(1:length(mycorpus.nouns), function(i) paste(mycorpus.nouns[[i]], collapse = ", "), .progress = "text", .inform = FALSE)
 # remove odd characters
 mycorpus.noun.strings <- llply(1:length(mycorpus.noun.strings), function(j) iconv(mycorpus.noun.strings[[j]], "latin1", "ASCII", sub=""), .progress = "text", .inform = FALSE)
 # remove punctuation
 mycorpus.noun.strings <- llply(1:length(mycorpus.noun.strings), function(j) gsub("[[:punct:]]", "",  mycorpus.noun.strings[[j]]), .progress = "text", .inform = FALSE)
 
}

message("done")
# put in corpus
message("converting vectors back into a corpus...")
mycorpus <- Corpus(VectorSource(mycorpus.noun.strings))
names(mycorpus) <- names(wordcounts)


# clean up
invisible(gc())
message("all done")
return(mycorpus)
}




