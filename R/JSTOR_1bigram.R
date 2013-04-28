#' Plot the frequency of one word over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of a bigram over time. The relative frequency is the frequency of the bigram in a document divided by the total number of bigrams in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @param bigram two words, surrounded by standard quote marks, or a vector of bigrams.
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ## JSTOR_1bigram(unpack, "pirate booty")
#' ## JSTOR_1bigram(unpack, c("treasure chest", "musket balls", "jolly roger")



JSTOR_1bigram <- function(x, bigram){
  #### investigate change in use of certain bigrams of interest over time
  # set working directory to where the bigrams are
  # (within working directory) with lots of CSV files
  bigrams <- x$bigrams
  bibliodata <- x$bibliodata
  # Get total number of word in the article to standarise for different article lengths
  leng <- sapply(1:length(bigrams), function(i) length(bigrams[[i]]))
  # now word of interest (always lower case)
  bigram1 <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bigram ))
  # calculate ratio
  bigram_ratio <- bigram1/leng
  # get years for each article
  bigram_by_year <- data.frame(bigram_ratio, year = as.numeric(as.character(bibliodata$year)))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # vizualise one word over time
  library(ggplot2)
  suppressWarnings(ggplot(bigram_by_year, aes(year, log(bigram_ratio))) +
                     geom_point(subset = .(bigram_ratio > 0)) +
                     geom_smooth( aes(group=1), method = "loess", span = 0.4, data=subset(bigram_by_year, bigram_ratio>0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of the bigram '", bigram, "'")) +
                     # inspect bibliodata$year to see min and max year to set axis limits
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)))
}

