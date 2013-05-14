#' Plot the changes in topic proportions over time
#' 
#' @description Generates plots and data frames showing changes in topic proportions over time. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack.
#' @param topics a range of topic numbers to plot, Default is 1 to 56 (ie. the first 56 topics in the model). A mimimum of two topics must be specified.
#' @return Returns a plot of topics over time and a list of dataframes of the topic proportions per document for further analysis.
#' @examples 
#' ## tot <- JSTOR_MALLET_topicsovertime(x = unpack, topics = 1:100)



JSTOR_MALLET_topicsovertime <- function(x, topics=1:56){
  ## = Ben Marwick's comments
  # = Andrew Goldstone's comments, directly from https://github.com/agoldst/dfr-analysis
  
  ## get bibliodata
  bibliodata <- x$bibliodata  
  
  # Andrew Goldstone's method: get the user to choose the file
  
  message("Select the topic keys file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputtopickeys <- file.choose()
  print(outputtopickeys)
  
  message("Select the topic docs file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputdoctopics <- file.choose()
  print(outputdoctopics)
  
#   # get user to paste in the path to the MALLET output files
#   
#   # from  http://r.789695.n4.nabble.com/url-prep-function-backslash-issue-tp3778530p3779086.html
#   message("please paste in the path to the MALLET output files (no quotes needed):")
#   oldstring1 <- readline() 
#   path <- chartr("\\", "/", oldstring1) 
#   setwd(path)
#   
#   # get user to paste in the name of the topic keys file
#   
#   message("please paste in the name of the topic keys file (no quotes needed):")
#   oldstring2 <- readline() 
#   outputtopickeys <- oldstring2
#   
#   # get user to paste in the name of the topic keys file
#   
#   message("please paste in the name of the topic docs file (no quotes needed):")
#   oldstring3 <- readline() 
#   outputdoctopics <- oldstring3
  
  outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
  outputdoctopicsresult <- read.table(outputdoctopics, header=F, sep="\t")
  
  ## Andrew Goldstone's function to make a matrix where
  ## cols = topics
  ## rows = docs
  sort.topics <- function(df) {
    # width of data frame: 2n + 2
    w <- dim(df)[2]
    n.topics <- (w - 2) / 2
    # number of docs
    n.docs <- dim(df)[1]
    
    # construct matrix of indices by extracting topic numbers from each row
    topic.nums <- df[,seq(from=3,to=w-1,by=2)]
    # thus topic.nums[i,j] is the jth most frequent topic in doc i
    # and df[i,2(j + 2)] is the proportion of topic topic.nums[i,j] in doc i
    # with topics numbered from 0 
    # since this is just permuting the even-numbered rows of
    # t.m <- as.matrix(df[,3:w])
    # it must be expressible as some outer product or something
    # but screw it, time for a for loop
    
    result <- matrix(0,nrow=n.docs,ncol=n.topics)
    for(i in 1:n.docs) {
      for(j in 1:n.topics) {
        result[i,topic.nums[i,j]+1] <- df[i,2 * j + 2] 
      }
    }
    result
  }
  
  ## make the topic.frame using the function above
  topics.matrix <- sort.topics(outputdoctopicsresult)
  
  # reset the 'topics' parameter if number of topics is less
  # than the default for a nice facetted plot (53)
  ifelse(ncol(topics.matrix) < 53, (topics <- 1:ncol(topics.matrix)), (topics <-  1:53))
  
  ## Andrew Goldstone's function
  # create a dataframe with row n, column m giving proportion of topic m in doc m
  # add rows named by doc id
  # Pass in a function for converting filenames stored
  # in mallet's output to id's. The default is the as.id function from metadata.
  # R, but you can pass a different one
  
  read.doc.topics <- function() {
     
    df <- read.table(outputdoctopics,header=FALSE,skip=1,stringsAsFactors=FALSE)
  
    ids <- gsub("\\.txt", "", basename(df$V2))
    
    topics.frame <- as.data.frame(sort.topics(df))
    names(topics.frame) <- paste("topic",sep="",1:length(topics.frame))
    cbind(topics.frame,id=ids,stringsAsFactors=FALSE)
    
    # add the ids again, but on the right 
    cbind(topics.frame,id=ids,stringsAsFactors=FALSE)

  }
  
  ## make it so, using the function above
  doc.topics <- read.doc.topics()
  
  ## Andrew Goldstone's function
  read.keys <- function() {
 
    keys.filename <- outputtopickeys
     
    df <- read.csv(keys.filename,sep="\t",header=FALSE,as.is=TRUE,
                   col.names=c("topic","alpha","keywords"))
    df$topic <- df$topic + 1
    df
  }
  ## make it so, using above function
  keys.frame <- read.keys()
  
  ## Andrew Goldstone's function (I have un-functioned it)
  # top-level input function: make a combined 
  # dataframe of topic proportions and document metadata
  # replace pubdate string with numeric year
  # NB id formats must match in the two frames, since the merge is by id
  
  ## first, get meta.frame
  meta.frame <- x$bibliodata ## read in citations.CSV
  meta.frame$id <- meta.frame$x 
  meta.frame$pubdate <- as.numeric(substr(meta.frame$issue, 1,4))
    
    # merge and
    # clumsily reorder to ensure that subsetting result to nth column
    # will give topic n
    merged <- merge(doc.topics,meta.frame,by="id")
    ids <- merged$id
    topic.model.df <- cbind(subset(merged,select=-id),id=ids)  
  
  ## Andrew Goldstone's function
  # Return the top num.words keywords for topic i
  # not vectorized in topic
  
  topic.keywords <- function(topic,keys.frame,num.words=5) {
    words <- unlist(strsplit(keys.frame$keywords[topic],split=" "))
    words[1:num.words]
  }
  
  ## Andrew Goldstone's function
  # Return a list of keyword-based labels for topics
  # vectorized in topic
  topic.shortnames <- function(topic,keys.frame,num.words=2) {
    words.list <- strsplit(keys.frame$keywords[topic],split=" ")
    sapply(words.list, function (words) {
      paste(words[1:num.words],collapse=" ")
    }
    )
  }
  
  ## Andrew Goldstone's function (slightly modified)
  # Nice R: vectorized in topics and years, gives overall proportion
  # Unnice R: yrs.table, being a table, is indexable by labels, not numbers
  # even though it is a table of numbers
  topic.years.proportion <- function(topic,yrs,df,
                                     yrs.table = table(df$pubdate)
  ) {
    sum(df[df$pubdate %in% yrs,topic]) / # subset rows with just year of interest and cols with topic of interest
      sum(yrs.table[as.character(yrs)])  # is this the same as getting an average value for that topic over that time period?
  }
  
  ## Andrew Goldstone's function (slightly modified)
  # The moving window for averages is 2w + 1 years
  # (not exactly smoothing, since it doesn't just average averages but
  # calculates the average over the whole time-interval)
  # returns a two-column matrix, with the years covered in the first column
  # and the averaged values in the second column
  topic.proportions.by.year1 <- function(topic,df,smoothing.window=0) {
    yi <- range(df$pubdate)
    w <- smoothing.window
    
    years <- seq.int(yi[1]+w,yi[2]-w) 
    result <- matrix(nrow=length(years),ncol=2)
    result[,1] <- years
    result[,2] <- sapply(years, function (y)
      (topic.years.proportion(topic,(y-w):(y+w),df)))
    result
  }
  
  ## Andrew Goldstone's function 
  plot.many.topics.yearly <- function(topics,df,keys.frame,w=2) {
    n <- length(topics) * length(df$id)
    
    to.plot.list <- lapply(as.list(topics), function (i) { 
      to.add <- as.data.frame(topic.proportions.by.year1(i,df,w))
      names(to.add) <- c("year","proportion")
      # The facets will be sorted in alphabetical order
      # so, until I learn how to order them,
      # let's just do this kludge, works for n.topics < 1000
      tnum <- sprintf("%03d",i)
      to.add$topic <- paste(tnum,topic.shortnames(i,keys.frame))
      to.add$alpha <- keys.frame$alpha[i]
      to.add
    }
    )
    to.plot <- do.call(rbind,to.plot.list)
    (qplot(year,proportion,data=to.plot,facets= ~ topic, color=alpha, geom="line"))
  }
  
  ### make it so
  require(ggplot2)
  print(plot.many.topics.yearly(topics,  topic.model.df, keys.frame))
  return(topic.model.df)
  
  
}

