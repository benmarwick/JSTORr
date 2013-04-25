#' Unpack JSTOR journal articles and bibliographic data 
#' 
#' @description Unzip, import and reshape journal articles and bibliographic data from the downloaded zipfile and reshape ready for simple text mining. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param path  Full path name of the directory containing the zip file obtained from JSTOR's Data for Research tool
#' @param zipfile The name of the zip file obtained from JSTOR's Data for Research tool (include the zip file suffix)
#' @return Returns "wordcounts", a list of character vectors where each vector contains the words of one article, and 'bibliodata', a table of bibliographic information for all articles. 
#' @examples 
#' JSTOR_unpack("C:/Documents", "2013.4.20.FxFmBVYd.zip") # note forward slash, not backslash
#' JSTOR_unpack(getwd(), "2013.4.20.FxFmBVYd.zip")



JSTOR_unpack <- function(path, zipfile){
  #### get data into the R session 
  # set R's working directory
  setwd(path) # change this to where you downloaded the data!
  # Get zip file of CSVs from JSTOR and unzip
  # this may take a few minutes...
  # unzip(zipfile)
  # set working directory to newly created folder
  # (within working directory) with lots of CSV files
  setwd(paste0(getwd(),"/wordcounts"))
  
  #### get list of data, the CSV files of wordcounts in dropbox folder
  myfiles <- dir(pattern = "\\.(csv|CSV)$", full.names = TRUE)
  # read CSV files into a R data object
  library(plyr)
  aawc <-  llply(myfiles, read.csv, .progress = "text", .inform = FALSE)
  # assign file names to each dataframe in the list
  names(aawc) <- myfiles
  
  #### reshape data
  # `untable' each CSV file into a list of data frames, one data frame per file
  aawc1 <- sapply(1:length(aawc), function(x) {rep(aawc[[x]]$WORDCOUNTS, times = aawc[[x]]$WEIGHT)})
  names(aawc1) <- myfiles
  
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
  # now we have a table of citations with a unique ID for each article
  # that is linked to the year of publication. We can come back to this
  wordcounts <<- wordcounts
  bibliodata <<- bibliodata
  invisible(gc())
}
