#' Plot the frequency of one word over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of a word over time. The relative frequency is the frequency of the word in a document divided by the total number of words in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @param oneword One word, surrounded by standard quote marks, or a vector of words
#' @param span span of the lowess line (controls the degree of smoothing)
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ##JSTOR_1word(unpack, "diamonds", span = 0.4)
#' ##JSTOR_1word(unpack, c("diamonds", pearls"), span = 0.4)



JSTOR_1word <- function(x, oneword, span = 0.5){
  #### investigate change in use of certain words of interest over time
  wordcounts <- x$wordcounts
  bibliodata <- x$bibliodata
  # Get total number of word in the article to standarise for different article lengths
  leng <- sapply(1:length(wordcounts), function(i) length(wordcounts[[i]]))
  # now word of interest (always lower case)
  word <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% oneword ))
  # calculate ratio
  word_ratio <- word/leng
  # get years for each article
  word_by_year <- data.frame(word_ratio, year = as.numeric(as.character(bibliodata$year)))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # vizualise one word over time
  library(ggplot2)
  suppressWarnings(ggplot(word_by_year, aes(year, log(word_ratio))) +
                     geom_point(subset = .(word_ratio > 0)) +
                     geom_smooth( aes(group=1), method = "loess", span = span, data=subset(word_by_year, word_ratio>0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of the word '", oneword, "'")) +
                     # inspect bibliodata$year to see min and max year to set axis limits
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)))
}

