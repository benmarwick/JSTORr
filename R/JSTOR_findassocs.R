#' Plot the words with the strongest correlation with a given word, by time intervals
#' 
#' @description Generates a plot of the top n words in all the documents in ranges of years that positively correlate with a given word. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function after adding common words to the stopword list. To learn more about editing the stopword list, see the help for the JSTOR_dtmofnouns function. 
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param nouns the object returned by the function JSTOR_dtmofnouns. A Document Term Matrix containing the documents.
#' @param n the number years to aggregate documents by. For example, n = 5 (the default value) will create groups of all documents published in non-overlapping five year ranges. Note that high n values combined with high plimit and corlimit values will severly filter the output. For exploratory data analysis it's recommended to start with low n values and work up.
#' @param word The word to calculate the correlations with
#' @param corlimit The lower threshold value of the Pearson correlation statistic (default is 0.4).
#' @param plimit The lower threshold value of the Pearson correlation statistic (default is 0.05).
#' @param topn An integer for the number of top ranking words to plot. For example, topn = 20 (the default value) will plot the top 20 words for each range of years.
#' @param biggest An integer to control the maximum size of the text in the plot
#' @param parallel logical.  If TRUE attempts to run the function on multiple 
#' cores.  Note that this may actually be slower if you have one core, limited memory or if 
#' the data set is small due to communication of data between the cores.
#' @return Returns a plot of the most frequent words per year range, with word size scaled to frequency, and a dataframe with words and counts for each year range
#' @examples 
#' ## findassocs <- JSTOR_findassocs(unpack1grams, nouns, "rouges")
#' ## findassocs <- JSTOR_findassocs(unpack1grams, nouns, n = 10, "pirates", topn = 100)
#' ## findassocs <- JSTOR_findassocs(unpack1grams, nouns, n = 5, "marines", corlimit=0.6, plimit=0.001)


JSTOR_findassocs <- function(unpack1grams, nouns, word, n=5, corlimit=0.4, plimit=0.05, topn=20, biggest=5, parallel=FALSE){
  
  # alert if word is not present
  if ( length( nouns[, dimnames(nouns)$Terms == word ]$dimnames$Terms) == 0  )  {
    stop("This word is not present in the document term matrix, so this function cannot be completed") 
  } else {
  
  
  # get bibliodata ready
  bibliodata <- unpack1grams$bibliodata

  bibliodata$year <- as.numeric(as.character(bibliodata$year))
  uniqueyears <- sort(unique(bibliodata$year))
  
  # split the years vector into chunks that are n years big
  xx <- seq_along(uniqueyears)
  # get list of chunks, each chunk has n years in it
  years <- split(uniqueyears, ceiling(xx/as.numeric(n)))
  # clean up a little
  rm(xx, uniqueyears)

  
  
  # get years of publication on to dtm as doc names
  nouns$dimnames$Docs <- as.character(bibliodata$year[match(nouns$dimnames$Docs, bibliodata$x)])
  # make list of dtms, with each dtm containing docs spanning n years
  message("dividing documents up into groups...")
  
  
  
  # create a list of dtms to iterate over 
  library(plyr)
  dtmlist <- llply(years,  function(x) nouns[ (nouns$dimnames$Docs %in% as.character(x)), ], .progress = "text", .inform = FALSE ) 
  message("done")
  
  # dtmlist <- lapply(1:length(years), function(i) dtm1[[i]] <- dtm[as.numeric(substring(dtm$dimnames$Docs, 1, 4)) %in% unname(unlist(years[i]))])
  # put names on the list of dtm so we can see the years represented by each dtm
  names(dtmlist) <- lapply(years, function(i) paste0(min(i),"-",max(i) ))
  # Here's the heavy lifting...
  
  
  # function from gamlr::corr to calculate correlation between simple triplet matrix
  # and matrix, to avoid filling memory with a giant regular matrix
  cor_slam <- function (x, y)
  {
    if (!inherits(x, "simple_triplet_matrix")) {
      return(cor(x, y))
    }
    n <- nrow(x)
    v <- t(normalize(y))
    r <- tcrossprod_simple_triplet_matrix(t(x)/sdev(x), v)/(nrow(x) - 
                                                              1)
    dimnames(r) <- list(dimnames(x)[[2]], dimnames(y)[[2]])
    return(r)
  }  
  # this one also comes from gamlr...
  normalize <-  function (x, m = NULL, s = sdev(x)) 
  {
    if (!is.null(ncol(x))) 
      if (length(s) != ncol(x)) 
        stop("length(s)!=ncol(x)")
    s[s == 0] <- 1
    if (is.simple_triplet_matrix(x)) {
      x$v <- x$v/s[x$j]
      return(x)
    }
    x <- as.matrix(x)
    if (is.null(m)) 
      m <- col_means(x)
    return(t((t(x) - m)/s))
  }
  
  # and this one also...
  sdev <- function (x) 
  {
    if (!inherits(x, "simple_triplet_matrix")) {
      return(apply(as.matrix(x), 2, sd))
    }
    n <- nrow(x)
    sqrt(col_sums(x^2)/(n - 1) - col_sums(x)^2/(n^2 - n))
    return(sqrt(col_sums(x^2)/(n - 1) - col_sums(x)^2/(n^2 - 
                                                         n)))
  }
  
  # need a function from this one also... can't be bothered copy-pasting...
  library(slam)
  
  # here they are all wrapped up: calculate the correlation, subset by corlimit and put in order
  findAssocsBig <- function(u, word, corlimit){
    suppressWarnings(x.cor <-  cor_slam(          u[ ,!u$dimnames$Terms == word],        
                                                  as.matrix(u[  ,u$dimnames$Terms == word ])  )  )  
    x <- sort(round(x.cor[ (x.cor[ ,word ] > corlimit), ], 3), decreasing = TRUE)
    return(x)
  }
  
  
  # find most correlation and p-value between the keyword and all other words in the chunk
  message("calculating correlations and p-values...")
  suppressMessages(library(data.table))
  # function to calculate p and r values of correlation
  # supply list of dtms as u
  pandr <- function(u) { 
    # seems like I have to make and delete this each time because
    # it's getting the 'years' col evern when I don't explicitly assign it!
    filler <- data.table( x.cor = rep(0, topn),  p = rep(0, topn), words = rep("", topn) )
    
    # check to see if word is present
    if (length(u$dimnames$Terms == word) == 0) {
      # if the word isn't in the dtm, make a table of zeros
      wordcor <- filler
    } else { 
      # if the word is present, carry on with calculations...    
      # get Pearson correlation values
      x.cor <- findAssocsBig(u, word, corlimit)
      
      # get p-values
      df <- nrow(u[, !u$dimnames$Terms == word]) - 2 
      t <- sqrt(df) * x.cor / sqrt(1 - x.cor ^ 2) 
      p <- pt(t, df) 
      p <- 2 * pmin(p, 1 - p) # get p-value
      x.cor <- (cbind(x.cor, p)) # combine r and p-values
      rm(p,t)  # clean up on the way...
      
      ## branching logic...
      # if there are no words that meet the corlimit...
      if (length(x.cor) == 0) {
        wordcor <- filler
      } else {
        
        # if there are no words that meet the plimit...
        if (nrow(x.cor[x.cor[,2] < plimit  , , drop=FALSE]) == 0) {
          wordcor <- filler
        } else {
          
          # if the number of words returned is less than topn...
          if (nrow(x.cor) < topn) {
            # just return all the words...
            wordcor <-  data.table(x.cor[x.cor[,2] < plimit  , , drop=FALSE])
            words <- row.names(x.cor)[1:nrow(wordcor)] 
            wordcor[, words := words ]
            
          } else {
            # otherwise get the topn words
            wordcor <-  data.table( x.cor[x.cor[,2] < plimit  , ][1:topn,])
            words <- row.names(x.cor)[1:nrow(wordcor)][1:topn]
            wordcor[, words := words ]
          }
          
        }
      }  
    }
    setnames(wordcor, c("r", "p", "words"))
    years <- paste0(min(as.numeric(u$dimnames$Docs)), "-", max(as.numeric(u$dimnames$Docs)) )
    rm(filler)
    wordcor[, years := years ]
    return(wordcor)
  }
  
  
  if(parallel) {
    
    # parallel version
    require(parallel)
    cl <- makeCluster(mc <- getOption("cl.cores", detectCores()))
    clusterEvalQ(cl, library(slam))
    clusterExport(cl, varlist = c("dtmlist", "pandr", "cor_slam", "sdev", "normalize"), envir=environment())
    
    wordcor1 <- parLapplyLB(cl, dtmlist, pandr)    
    
    stopCluster(cl)
    
    
  } else { # non-parallel method
    
    wordcor1  <- llply(dtmlist, pandr, .progress = "text", .inform = FALSE )  
    
  }
  
  
  
  # combine list of dataframes into one big dataframe with word, freq and year-range
  suppressWarnings(wordcor1 <- data.table(do.call(rbind, wordcor1)))
  # add column of year ranges for each row
  # get a warning here because sometimes the dtmlist item does not have topn rows...
  # wordcor1$years <- unlist(lapply(1:length(dtmlist), function(i) rep(names(dtmlist[i]), topn)))
   
  
  
  
  
  
  
  message("done")
  
  # plot in a word-cloud-y kind of way, but with more useful information in the 
  # structure of the visualiation... 
  suppressMessages(require(ggplot2)); library("plyr")
  suppressWarnings(print(
    ggplot(wordcor1, aes(factor(years), r)) + 
      geom_text(aes(label = words, size = r, alpha = r), position=position_jitter(h=0.1,w=0.2), subset = .(r > 0)) +
      scale_size(range = c(3, biggest), name = paste0("Correlation value with the word '", word, "'")) +
      scale_alpha(range=c(0.5,1), limits=c(min(wordcor1$r), max(wordcor1$r)), guide = 'none') + 
      xlab("Year range") +
      # inspect bibliodata$year to see min and max year to set axis limits
      scale_x_discrete(limits=c(unique(wordcor1$years)), breaks = c(unique(wordcor1$years))) +
      ylab(paste0("Pearson correlation with '", word, "'"))  +
      ylim(0,1)  +
      theme(
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(), 
        axis.text.x=element_text(angle = 90, hjust = 0))
  ))
  
  return("findassocs" = wordcor1)
  
}
}
