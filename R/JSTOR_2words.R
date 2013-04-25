
#' Plot the frequencies of one word against another word over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of two words (two 1-grams) over time. The relative frequency is the frequency of the word in a document divided by the total number of words in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param word1 One word, surrounded by standard quote marks.
#' @param word2 One word, surrounded by standard quote marks.
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ##JSTOR_2words("diamonds", "pearls")
#' ##JSTOR_2words("milk", "sugar")


JSTOR_2words <- function(word1, word2){
  # Comparing two words of interest (always lower case)
  w1 <- word1
  w2 <- word2
  word1 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% w1))
  word2 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% w2))
  leng <- sapply(1:length(wordcounts), function(i) length(wordcounts[[i]]))
  # calculate ratios
  word1_ratio <- word1/leng
  word2_ratio <- word2/leng
  # get years for each article and make data frame
  twowords_by_year <- data.frame(word1_ratio, word2_ratio, year = as.numeric(as.character(bibliodata$year)))
  # reshape into a long table to make it easier to work with in ggplt
  library(reshape2)
  twowords_by_year_melt <- melt(twowords_by_year, id.vars = "year")
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # visualise
  library(ggplot2)
  suppressWarnings(ggplot(twowords_by_year_melt, aes(year, log(value))) +
                     geom_point(subset = .(value > 0), aes(colour = variable)) +
                     geom_smooth( aes(colour = variable), method = "loess", span = 0.4, subset = .(value > 0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of words")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)) +
                     scale_colour_discrete(labels = c(w1, w2)))
}
