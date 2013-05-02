#' Plot the frequency of one bigram against bigram over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of two bigrams over time. The relative frequency is the frequency of the bigram in a document divided by the total number of bigrams in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @param bigram1 two words, surrounded by standard quote marks, or a vector of bigrams.
#' @param bigram2 two words, surrounded by standard quote marks, or a vector of bigrams.
#' @param span span of the lowess line (controls the degree of smoothing). Default is 0.4
#' @retu rn Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ## JSTOR_2bigrams(unpack, "pirate booty", "treasure chest")
#' ## JSTOR_2bigrams(unpack, c("treasure chest", "musket balls"), c("jolly roger"), span = 0.2)



JSTOR_2bigrams <- function(x, bigram1, bigram2, span = 0.4){
  #### investigate change in use of certain bigrams of interest over time
  # set working directory to where the bigrams are
  # (within working directory) with lots of CSV files
  bigrams <- x$bigrams
  bibliodata <- x$bibliodata
  # Get total number of word in the article to standarise for different article lengths
  leng <- sapply(1:length(bigrams), function(i) length(bigrams[[i]]))
  # now word of interest (always lower case)
  bigram1a <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bigram1 ))
  bigram2a <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bigram2 ))
  # calculate ratio
  bigram1_ratio <- bigram1a/leng
  bigram2_ratio <- bigram2a/leng
  # get years for each article
  two_bigrams_by_year <- data.frame(bigram1_ratio, bigram2_ratio,  year = as.numeric(as.character(bibliodata$year)))
  # reshape into a long table to make it easier to work with in ggplt
  library(reshape2)
  two_bigrams_by_year_melt <- melt(two_bigrams_by_year, id.vars = "year")
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # vizualise one word over time
  library(ggplot2)
  suppressWarnings(ggplot(two_bigrams_by_year, aes(year, log(value))) +
                     geom_point(subset = .(value > 0), aes(colour = variable)) +
                     geom_smooth( aes(colour = variable), method = "loess", span = span, subset = .(value > 0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of bigrams")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)) +
                     scale_colour_discrete(labels = c(paste(bigram1, collapse = ", "), paste(bigram2, collapse = ", "))) )
}

