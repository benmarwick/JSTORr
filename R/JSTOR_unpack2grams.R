#' Unpack JSTOR journal articles and bibliographic data to a Document Term Matrix of 2-grams
#' 
#' @description Import journal articles and bibliographic data from the downloaded zipfile and reshape ready for simple text mining. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). 
#' @param path full path to directory containing 'bigrams' folder and the citations.CSV file. These are obtained after unzipping the file downloaded from DfR (you should unzip the file before running this function). Default is the working directory.
#' @param parallel if TRUE, apply function in parallel, using the parallel library. Default is FALSE as this is typically faster for smaller datasets (ie. ~5000 articles) due to communication overhead.
#' @return Returns a list of two items. First is "bigrams", a Document Term Matrix of 2-grams, and second is 'bibliodata', a data frame of bibliographic information for all articles. 
#' @examples 
#' ## unpack2grams <- JSTOR_unpack2grams(path = "C:/Users/marwick/Downloads/JSTOR") # then follow prompts to navigate to the location of the zipfile




JSTOR_unpack2grams <- function(parallel=FALSE, path=getwd()){
 
  # set working directory to newly created folder
  # (within working directory) with lots of CSV files
  
  
  ## trim off the last forward slash, if there was one.
  if  (substr(path, nchar(path), nchar(path)) == "/") {
    
    path <- substr(path, 1, nchar(path)-1) # trim last char
    
  } else {
    
    path <- path # don't trim
    
  }
       
  # change directory to 1-gram CSV files    
  setwd(paste0(path,"/bigrams"))
  
  #### get list of data, the CSV files of wordcounts in dropbox folder
  message("reading 2-grams into R...")
  myfiles <- dir(pattern = "\\.(csv|CSV)$", full.names = TRUE)
  # read CSV files into a R data.table object
  # fread is 10x faster than read.csv...
  suppressMessages(library(data.table))
  library(plyr)
  read_csv2dt <- function(x) data.table(fread(x, sep = ",", stringsAsFactors=FALSE))
#   read_csv2dt <- function(x) data.table(read.csv(x, sep = ",", stringsAsFactors=FALSE, 
#                                                  colClasses = c("character", "integer"), 
#                                                  comment.char = "", header = TRUE ))
  if(parallel) {
    
    suppressMessages(library(snow)); suppressMessages(library(parallel))
    cl <- makeCluster(detectCores(), type = "SOCK")
    clusterExport(cl, c("myfiles", "read_csv2dt"), envir=environment())
    clusterEvalQ(cl, library(data.table))
    aawc <- parLapplyLB(cl, myfiles, read_csv2dt)
    stopCluster(cl); invisible(gc(verbose = FALSE))
    
  } else {
    
    library(plyr)
    aawc <-  llply(myfiles, read_csv2dt, .progress = "text", .inform = FALSE)
  }
  

  # assign file names to each dataframe in the list
  names(aawc) <- myfiles
  message("done")
  
  #### convert DfR format to dtm for each doc
  message("reshaping the 2-grams into a document term matrix...")
  
  # custom version of tm::DocumentTermMatrix for 1-grams
  library(slam); library(tm)
  my_dtm_2gram <- function(x){ 
    y <- as.integer(x$WEIGHT)
    names(y) <- x$BIGRAMS
    v =  unname(y)          # num
    i = rep(1, length(y))   # int
    j = seq(1:length(y))    # int
    z <- simple_triplet_matrix(v = v,   # num
                          i = i,        # int
                          j = j,        # int
                          nrow = max(i),
                          ncol = max(j), 
                          dimnames =                          
                            list(                              
                              Docs = deparse(substitute(x)),
                              Terms = names(y)))
  zz <- as.DocumentTermMatrix(z, weighting = weightTf)
  return(zz)
  }
  
  # get all tables into dtms
  if(parallel) {
    
    suppressMessages(library(snow)); suppressMessages(library(parallel))
    cl <- makeCluster(detectCores(), type = "SOCK")
    clusterExport(cl, c("aawc", "my_dtm_2gram"), envir=environment())
    clusterEvalQ(cl, list(library(tm), library(slam)))
    aawc1 <- parLapplyLB(cl, 1:length(aawc), function(i) my_dtm_2gram(aawc[[i]]) )
    stopCluster(cl); rm("aawc"); invisible(gc(verbose = FALSE))
    
   } else {
     
     library(plyr)
     aawc1 <- llply(1:length(aawc), function(i) my_dtm_2gram(aawc[[i]]), .progress = "text", .inform = FALSE)
  }
  
  
  library(stringr)
  names(aawc1) <- str_extract(basename(myfiles), "[^bigrams_].+[^.CSV]")
 
  message("done")


  
 
  
  #### bring in citations file with biblio data for each paper
  setwd(path) # change this to the location of the citations.csv file
  
  # get the colclasses to speed up the reading of a large CSV
  # this comes from http://rforwork.info/2012/10/11/know-your-dataset-ffdf/
  headset = read.csv("citations.CSV", header = TRUE, nrows = 50)
  headclasses = sapply(headset, class)
  headclasses[grep("factor", headclasses)] = "character"
  
  # now read in file
  cit <- read.csv("citations.CSV", row.names = NULL, comment.char = "", header = TRUE, 
                  stringsAsFactors = FALSE, colClasses=headclasses, quote = "")
  # replace for-slash with underscore to make it match the filenames
  # and replace odd \t that was added during import 
  library(stringr)
  cit$id <- str_extract(chartr('/', '_', cit$id), ".*[^\t]")
  # limit list of citations to full length articles only 
  # note that citation type is not in the correct column
  # and that we need \t in there also
  citfla <- cit[cit$publisher == 'fla\t',]
  # subset from the wordcount data only the full length articles
 
  # subset items in the list of wordcount data whose names are in 
  # the list of fla citation IDs
  bigrams <- aawc1[which(names(aawc1) %in%  citfla$id)]
  
  
  # put citation IDs in the same order with wordcount data names
  # which is the same order as myfiles
  bibliodata <- (merge(names(bigrams), citfla, by.x=1, by.y="id"))
  # create a variable that holds the year of publication for
  # each article
  bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]+{4}")
  
  # clean up a little
  rm(aawc1, cit, citfla, myfiles, headclasses); invisible(gc(verbose = FALSE))
  
  # make one giant dtm with all docs (rather slow...)
  
  
  if(parallel) {
    
    suppressMessages(library(snow)); suppressMessages(library(parallel))
    cl <- makeCluster(detectCores(), type = "SOCK")
    clusterExport(cl, c("bigrams"), envir=environment())
    clusterEvalQ(cl, library(tm) )
    bigrams <- do.call(tm:::c.DocumentTermMatrix, bigrams)
    stopCluster(cl); invisible(gc(verbose = FALSE))
    
  } else {
    
    library(plyr)
      bigrams <- do.call(tm:::c.DocumentTermMatrix, bigrams)

  }

  
  # give docs their DOI as names
  bigrams$dimnames$Docs <- as.character(bibliodata$x)

# somehow docs got to be a factor... fix this
# and why do I have to subset the Docs like this?
# something to do with how the DTM is made? I bet
# the CSV file is dodgy... 
  bigrams <- bigrams[unique(as.character(bigrams$dimnames$Docs[1:nrow(bigrams)])), ]
  
  
  #### end 2-grams ####
  
  message("finished with 2-grams")
  
  return(list("bigrams" = bigrams, "bibliodata" = bibliodata))
  
}
  