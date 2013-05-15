#' Plot the words with the strongest correlation with a given word, by time intervals
#' 
#' @description Generates a plot of the top n words in all the documents in ranges of years that positively correlate with a given word. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them using the JSTOR_removestopwords function.
#' @param x object returned by the function JSTOR_unpack.
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents.
#' @param n the number years to aggregate documents by. For example, n = 5 (the default value) will create groups of all documents published in non-overlapping five year ranges.
#' @param word The word to calculate the correlations with
#' @param corlimit The lower threshold value of the Pearson correlation statistic (default is 0.4).
#' @param plimit The lower threshold value of the Pearson correlation statistic (default is 0.05).
#' @param topn An integer for the number of top ranking words to plot. For example, topn = 20 (the default value) will plot the top 20 words for each range of years.
#' @param biggest An integer to control the maximum size of the text in the plot
#' @return Returns a plot of the most frequent words per year range, with word size scaled to frequency, and a dataframe with words and counts for each year range
#' @examples 
#' ## findassocs <- JSTOR_findassocs(unpack, corpus, n = 10, "pirates", topn = 100)
#' ## findassocs <- JSTOR_findassocs(unpack, corpus, n = 5, "marines", corlimit=0.6, plimit=0.001)


JSTOR_findassocs <- function(x, corpus, word, n=5, corlimit=0.4, plimit=0.05, topn=20, biggest=5){

  # get bibliodata ready
  bibliodata <- x$bibliodata
  
require(tm)
# convert corpus to dtm, keep words that occur in 5 or more docs
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5,Inf))))
# put year and title fragment as document names
# get first five words of title (without any punctuation marks)
titlefrag <- lapply(1:nrow(bibliodata), function(i) paste(unlist(strsplit(as.character(gsub('[[:punct:]]','', bibliodata$doi)[i]), " "))[1:5], collapse = "."))
dtm$dimnames$Docs <- paste0(bibliodata$year, "_", titlefrag)

bibliodata$year <- as.numeric(as.character(bibliodata$year))
uniqueyears <- sort(unique(bibliodata$year))

# split the years vector into chunks that are n years big
xx <- seq_along(uniqueyears)
# get list of chunks, each chunk has n years in it
years <- split(uniqueyears, ceiling(xx/as.numeric(n)))
# create list of dtms by subsetting the full dtm into seperate
# dtms of documents where publication year matches the chunks
# of years
dtmlist <- vector("list", length = length(years))
for(i in 1:length(years)){
  dtmlist[[i]] <- dtm[as.numeric(substring(dtm$dimnames$Docs, 1, 4)) %in% unname(unlist(years[i]))]
}
  
  
# dtmlist <- lapply(1:length(years), function(i) dtm1[[i]] <- dtm[as.numeric(substring(dtm$dimnames$Docs, 1, 4)) %in% unname(unlist(years[i]))])
# put names on the list of dtm so we can see the years represented by each dtm
names(dtmlist) <- lapply(years, function(i) paste0(min(i),"_",max(i) ))
# Here's the heavy lifting...
# find most correlation and p-value between the keyword and all other words in the chunk
wordcor <- vector("list", length = length(dtmlist))
for(i in 1:length(dtmlist)){
  x <- dtmlist[[i]]
  ind <- word == Terms(x)
  # get Pearson correlation values
  suppressWarnings(x.cor <- cor(as.matrix(x[, ind]), as.matrix(x[, !ind])))
  # get p-values for correlation: http://r.789695.n4.nabble.com/Very-slow-using-double-apply-and-cor-test-to-compute-correlation-p-values-for-2-matrices-tp871312p871316.html
  # massively faster than using cor.test with lapply...
  r  <- x.cor
  df <- nrow(as.matrix(x[, !ind])) - 2 
  t <- sqrt(df) * r / sqrt(1 - r ^ 2) 
  p <- pt(t, df) 
  p <- 2 * pmin(p, 1 - p) 
  x.cor <- rbind(x.cor, p)
  # make sorted dataframe with highest cor values at top
  x.cor1 <- data.frame(round(x.cor[, which(x.cor[1, ]  > corlimit & x.cor[2, ] < plimit)],4), row.names = c("r","p"))
  # deal with dtms where there is zero correlation
  result <- try(wordcor[[i]] <- t(x.cor1[,order(-x.cor1[which(rownames(x.cor1) == 'r'),])][1:topn]));
  ifelse(class(result) == "try-error",    
    # pad out with zeros if there is zero cor
    wordcor[[i]] <- data.frame(r = rep(0, topn), p = rep(0, topn), row.names = NULL),
    # or get the cor and pvals if nonzero cor
    wordcor[[i]] <- t(x.cor1[,order(-x.cor1[which(rownames(x.cor1) == 'r'),])][1:topn])                           
   )
  }

# combine list of dataframes into one big dataframe with word, freq and year-range
suppressWarnings(wordcor1 <- data.frame(do.call(rbind, wordcor), word = row.names(do.call(rbind, wordcor))))
# add column of year ranges for each row
wordcor1$years <- unlist(lapply(1:length(dtmlist), function(i) rep(names(dtmlist[i]), topn)))


# plot in a word-cloud-y kind of way, but with more useful information in the 
# structure of the visualiation... 
require(ggplot2)
suppressWarnings(print(
  ggplot(wordcor1, aes(factor(years), r)) + 
        geom_text(aes(label = word, size = r, alpha = r), position=position_jitter(h=0.1,w=0.2)) +
        scale_size(range = c(3, biggest), name = paste0("Correlation value with the word '", word, "'")) +
        scale_alpha(range=c(0.5,1), limits=c(min(wordcor1$r), max(wordcor1$r)), guide = 'none') + 
        xlab("Year range") +
        ylab(paste0("Pearson correlation with '", word, "'"))  +
        ylim(0,1)  +
    theme(
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
        ))
        
return("findassocs" = wordcor1)

}

