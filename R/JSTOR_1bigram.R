#' Plot the frequency of one bigram over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of a bigram over time. The relative frequency is the frequency of the bigram in a document divided by the total number of bigrams in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param unpack2grams object returned by the function JSTOR_unpack2grams.
#' @param bigram two words, surrounded by standard quote marks, or a vector of bigrams.
#' @param span span of the lowess line (controls the degree of smoothing). Default is 0.4
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ## JSTOR_1bigram(unpack2grams, "pirate booty")
#' ## JSTOR_1bigram(unpack2grams, c("treasure chest", "musket balls", "jolly roger"), span = 0.7)

JSTOR_1bigram <- function(unpack2grams, bigram, span = 0.4){
  #### investigate change in use of certain bigrams of interest over time
  # set working directory to where the bigrams are
  # (within working directory) with lots of CSV files
  y <- unpack2grams$bigrams
  bibliodata <- unpack2grams$bibliodata
  
  # using dtm
  # y <- as.matrix(bigrams)
  # Get total number of word in the article to standarise for different article lengths
  library(slam)
  leng <- row_sums(y)
  # now get total number of word of interest (always lower case)
  bigram1 <- as.matrix(y[,dimnames(y)$Terms %in% bigram])
  # calculate ratio
  bigram_ratio <- bigram1/leng
  
  
#   # full-text method
#   # Get total number of word in the article to standarise for different article lengths
#   leng <- sapply(1:length(bigrams), function(i) length(bigrams[[i]]))
#   # now word of interest (always lower case)
#   bigram1 <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bigram ))
#   # calculate ratio
#   bigram_ratio <- bigram1/leng
  
  
  # get years for each article
  suppressMessages(library(data.table))
  bigram_by_year <- data.table(bigram_ratio, year = as.numeric(as.character(bibliodata$year)))
  setnames(bigram_by_year, c("bigram_ratio", "year"))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # vizualise one word over time
  library(ggplot2)
  suppressWarnings(ggplot(bigram_by_year, aes(year, log(bigram_ratio))) +
                     geom_point(subset = .(bigram_ratio > 0)) +
                     geom_smooth( aes(group=1), method = "loess", span = span, data=subset(bigram_by_year, bigram_ratio>0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of the bigram '", bigram, "'")) +
                     # inspect bibliodata$year to see min and max year to set axis limits
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)))
}