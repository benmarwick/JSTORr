#' Unpack JSTOR journal articles and bibliographic data to a Document Term Matrix of 2-grams
#' 
#' @description Import journal articles and bibliographic data from the downloaded zipfile and reshape ready for simple text mining. For use with JSTOR's Data for Research datasets (\url{http://dfr.jstor.org/}). 
#' @param path full path to directory containing 'bigrams' folder and the citations.CSV file. These are obtained after unzipping the file downloaded from DfR (you should unzip the file before running this function). Default is the working directory.
#' @return Returns a list of two items. First is "bigrams", a Document Term Matrix of 2-grams, and second is 'bibliodata', a data frame of bibliographic information for all articles. 
#' @examples 
#' ## unpack2grams <- JSTOR_unpack2grams(path = "C:/Users/marwick/Downloads/JSTOR") 
#' # then follow prompts to navigate to the location of the zipfile
#' @import plyr data.table slam tm stringr




JSTOR_unpack2grams <- function( path=getwd()){
 
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

  read_csv2dt <- function(x) data.table(fread(x, sep = ",", stringsAsFactors=FALSE))
                                                
 
    aawc <-  llply(myfiles, read_csv2dt, .progress = "text", .inform = FALSE)
 
  # assign file names to each dataframe in the list
  names(aawc) <- myfiles
  message("done")
  
  #### convert DfR format to dtm for each doc
  message("reshaping the 2-grams into a document term matrix...")
  
  # custom version of tm::DocumentTermMatrix for 1-grams
 
  my_dtm_2gram <- function(x){ 
    y <- as.integer(x$WEIGHT)
    names(y) <- x$BIGRAMS
    v <-  unname(y)          # num
    i <- rep(1, length(y))   # int
    j <- seq(1:length(y))    # int
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

     aawc1 <- llply(1:length(aawc), function(i) my_dtm_2gram(aawc[[i]]), .progress = "text", .inform = FALSE)
 

  names(aawc1) <- str_extract(basename(myfiles), "[^bigrams_].+[^.CSV]")
 
  message("done")


  
 
  
  #### bring in citations file with biblio data for each paper
  setwd(path) # change this to the location of the citations.csv file
  
  # since citation files can be TSV (JSTOR changed to this in 2015) or csv (before 2015)...
  read_citations <- function(i) {if (stringr::str_sub(i, start=-3) == "CSV" | stringr::str_sub(i, start=-3) == "csv") 
  {read.csv(i, quote="", row.names=NULL, comment.char = "", header = TRUE,  stringsAsFactors = FALSE, colClasses="character")}
    else 
    {if (stringr::str_sub(i, start=-3) == "TSV" | stringr::str_sub(i, start=-3) == "tsv") 
    {read.delim(i, row.names = NULL, comment.char = "", header = TRUE, stringsAsFactors = FALSE, colClasses="character", quote = "")}
      else
      {"Citations files cannot be loaded"}  } 
  }
  
  
  cit <- read_citations
  # cit <- read.delim("citations.tsv", row.names = NULL, comment.char = "", header = TRUE, stringsAsFactors = FALSE, colClasses="character", quote = "")
  # replace for-slash with underscore to make it match the filenames
  # and replace odd \t that was added during import 
  cit$id <- str_extract(chartr('/', '_', cit$id), ".*[^\t]")
  # limit list of citations to full length articles only 
  # note that citation type is not in the correct column
  # changed this in case we get a dataset that was not originally all fla
  citfla <- cit[cit$publisher == 'fla',]
  # subset from the wordcount data only the full length articles
 
  # subset items in the list of wordcount data whose names are in 
  # the list of fla citation IDs
  bigrams <- aawc1[which(names(aawc1) %in%  citfla$id)]
  
  
  # put citation IDs in the same order with wordcount data names
  # which is the same order as myfiles
  bibliodata <- (merge(names(bigrams), citfla, by.x=1, by.y="id"))
  # create a variable that holds the year of publication for
  # each article
  bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]{4}")
  
  # clean up a little
  rm(aawc1, cit, citfla, myfiles); invisible(gc(verbose = FALSE))
  
  # make one giant dtm with all docs (rather slow...)
   
      bigrams <- do.call(c, bigrams)

  
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
  