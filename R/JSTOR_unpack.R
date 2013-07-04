#' Unpack JSTOR journal articles and bibliographic data 
#' 
#' @description Import and reshape journal articles and bibliographic data from the downloaded zipfile and reshape ready for the MALLET functions. Currently this is only useful for smaller collections because it generates large data objects in memory. As a rough guide, <5000 articles should be ok. 
#' @param parallel if TRUE, apply function in parallel, using the parallel library
#' @param path to directory containing 'wordcounts' and 'bigrams' folders and the citations.CSV file. These are obtained after unzipping the file downloaded from DfR (you should unzip the file before running this function).
#' @return Returns a list of three items. First is "wordcounts", a list of character vectors where each vector contains the words of one article,  second is 'bigrams', as for 'wordcounts' but with 2-grams instead of 1-grams, and third is 'bibliodata', a data frame of bibliographic information for all articles. 
#' @examples 
#' ## unpack <- JSTOR_unpack(path = "C:/Users/marwick/Downloads/JSTOR") 




JSTOR_unpack <- function(parallel=TRUE, path){
  #### get data into the R session 
  ## trim off the last forward slash, if there was one.
  if  (substr(path, nchar(path), nchar(path)) == "/") {
    
    path <- substr(path, 1, nchar(path)-1) # trim last char
    
  } else {
    
    path <- path # don't trim
    
  }
  
  # change directory to 1-gram CSV files    
  setwd(paste0(path,"/wordcounts"))
  
  
  #### Deal with wordcounts (ie. 1-grams) first
  # set working directory to newly created folder
  # (within working directory) with lots of CSV files
  setwd(paste0(getwd(),"/wordcounts"))
  
  #### get list of data, the CSV files of wordcounts in dropbox folder
  message("reading 1-grams into R...")
  myfiles <- dir(pattern = "\\.(csv|CSV)$", full.names = TRUE)
  # read CSV files into a R data.table object
  # fread is 10x faster than read.csv...
  library(data.table)
  library(plyr)
  read_csv2dt <- function(x) data.table(fread(x, sep = ",", stringsAsFactors=FALSE))
  aawc <-  llply(myfiles, read_csv2dt, .progress = "text", .inform = FALSE)
  

  # assign file names to each dataframe in the list
  names(aawc) <- myfiles
  message("done")
  
  #### reshape data
  message("reshaping the 1-grams...")
  untable <- function(x) rep(x$WORDCOUNTS, times = x$WEIGHT)
  if(parallel) {
    
    # do in parallel
    library(parallel)
    library(snowfall)
    sfInit( parallel=TRUE, cpus=detectCores() )
    sfExport( 'aawc', 'untable' )
    
    # `untable' each CSV file into a list of data frames, one data frame per file
    aawc1 <- sfClusterApplyLB(aawc, untable)
    names(aawc1) <- names(myfiles)
    sfStop(); rm("aawc"); invisible(gc(verbose = FALSE))
    
    sfInit( parallel=TRUE, cpus=detectCores() )
    sfExport( 'aawc1')
    # go through each item of the list and randomise the order of the words
    # so they are not in alpha order (which distorts the topic modelling)
    aawc1 <- sfClusterApplyLB(aawc1, function(i) sample(i, length(i)))
    sfStop()
    names(aawc1) <- myfiles
    invisible(gc(verbose = FALSE))
    
  } else { 
  
  # do not do in parallel
  # `untable' each CSV file into a list of data frames, one data frame per file
  aawc1 <- llply(aawc, function(x) {rep(x$WORDCOUNTS, times = x$WEIGHT)}, .progress = "text", .inform = FALSE)
  names(aawc1) <- myfiles
  invisible(gc(verbose = FALSE))
  # go through each item of the list and randomise the order of the words
  # so they are not in alpha order (which distorts the topic modelling)
  aawc1 <- llply(aawc1, function(i) sample(i, length(i)), .progress = "text", .inform = FALSE)
  names(aawc1) <- myfiles
  invisible(gc(verbose = FALSE))
  }
  
  message("done")
  
  #### bring in citations file with biblio data for each paper
  setwd(path) # change this to the location of the citations.csv file
  cit <- read.csv("citations.CSV")
  # replace for-slash with underscore to make it match the filenames
  # and replace odd \t that was added during import 
  library(stringr)
  cit$id <- str_extract(chartr('/', '_', cit$id), ".*[^\t]")
  # limit list of citations to full length articles only 
  # note that citation type is not in the correct column
  # and that we need \t in there also
  citfla <- cit[cit$publisher == 'fla\t',]
  # subset from the wordcount data only the full length articles
  # remove characters from the file names that are not in the citation list
  # to enable matching with citation IDs
  library(stringr)
  names(aawc1) <- str_extract(basename(names(aawc1)), "[^wordcounts_].+[^.CSV]")
  # subset items in the list of wordcount data whose names are in 
  # the list of fla citation IDs
  wordcounts <- aawc1[which(names(aawc1) %in% citfla$id)]
  # put citation IDs in order with wordcount data names  
  bibliodata <- (merge(names(wordcounts), citfla, by.x=1, by.y="id"))
  # create a variable that holds the year of publication for
  # each article
  bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]+{4}")
  
  #### end 1-grams ####
  
  #### Now deal with bigrams (ie. 2-grams) 
  # set working directory to folder with bigrams
  # (within working directory) with lots of CSV files of bigrams
  setwd(path) 
  setwd(paste0(getwd(),"/bigrams"))
  
  #### get list of data, the CSV files of bigrams in dropbox folder
  message("reading the 2-grams into R...")
  myfiles <- dir(pattern = "\\.(csv|CSV)$", full.names = TRUE)
  # read CSV files into a R data.table object
  # fread is 10x faster than read.csv...
  aawc2 <- llply(myfiles, read_csv2dt, .progress = "text", .inform = FALSE)
  # assign file names to each dataframe in the list
  names(aawc2) <- myfiles
  message("done")
  
  #### reshape data
  message("reshaping the 2-grams...")
  if(parallel) {
    
    library(parallel)
    library(snowfall)
    
    sfInit( parallel=TRUE, cpus=detectCores() )
    sfExport( 'aawc2')   
    # `untable' each CSV file into a list of data frames, one data frame per file
    aawc2 <- sfClusterApplyLB(aawc2, function(x) {rep(x$BIGRAMS, times = x$WEIGHT)})
    sfStop()
    names(aawc2) <- myfiles
    invisible(gc(verbose = FALSE))    
    
    sfInit( parallel=TRUE, cpus=detectCores() )
    sfExport( 'aawc2')  
    # go through each item of the list and randomise the order of the words
    # so they are not in alpha order (which distorts the topic modelling
    # if the docs are broken into chunks later on)
    aawc2 <- sfClusterApplyLB(aawc2, function(i) sample(i, length(i)))
    sfStop()
    names(aawc2) <- myfiles
    invisible(gc(verbose = FALSE))
    message("done")
    
  } else { 
  # `untable' each CSV file into a list of data frames, one data frame per file
  aawc2 <- llply(aawc2, function(x) {rep(x$BIGRAMS, times = x$WEIGHT)}, .progress = "text", .inform = FALSE)
  names(aawc2) <- myfiles
  invisible(gc(verbose = FALSE))
  # go through each item of the list and randomise the order of the words
  # so they are not in alpha order (which distorts the topic modelling)
  aawc2 <- llply(aawc2, function(i) sample(i, length(i)), .progress = "text", .inform = FALSE)
  names(aawc2) <- myfiles
  invisible(gc(verbose = FALSE))
  message("done")
  }
  
  #### bring in citations file with biblio data for each paper
  message("reshaping bibliographics data...")
  setwd(path) # change this to the location of the citations.csv file
  cit <- read.csv("citations.CSV")
  # replace for-slash with underscore to make it match the filenames
  # and replace odd \t that was added during import 
  library(stringr)
  cit$id <- str_extract(chartr('/', '_', cit$id), ".*[^\t]")
  # limit list of citations to full length articles only 
  # note that citation type is not in the correct column
  # and that we need \t in there also
  citfla <- cit[cit$publisher == 'fla\t',]
  # subset from the wordcount data only the full length articles
  # remove characters from the file names that are not in the citation list
  # to enable matching with citation IDs
  library(stringr)
  names(aawc2) <- str_extract(basename(names(aawc2)), "[^bigrams_].+[^.CSV]")
  # subset items in the list of wordcount data whose names are in 
  # the list of fla citation IDs
  bigrams <- aawc2[which(names(aawc2) %in% citfla$id)]
                             
  
  #### end bigrams ####
  
  # put citation IDs in order with wordcount data names (order is the same for bigrams)
  bibliodata <- (merge(names(wordcounts), citfla, by.x=1, by.y="id"))
  # create a variable that holds the year of publication for
  # each article
  bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]+{4}")
  # now we have a table of citations with a unique ID for each article
  # that is linked to the year of publication. We can come back to this
  invisible(gc(verbose = FALSE))
  message("done")
  return(list("wordcounts" = wordcounts, "bigrams" = bigrams, "bibliodata" = bibliodata))
 
}

