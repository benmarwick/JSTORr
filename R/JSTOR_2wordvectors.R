
#' Plot the frequencies of one set of words against another set of words over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of two set of words (two sets of 1-grams) over time. The relative frequency is the frequency of the set of words in a document divided by the total number of words in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x object returned by the function JSTOR_unpack.
#' @param v1 One vector of words, each word surrounded by standard quote marks.
#' @param v2 A second vector of words, each word surrounded by standard quote marks.
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ##JSTOR_2wordvectors(dat1, c("diamonds", "pearls"), c"milk", "sugar"))
#' ##JSTOR_2wordvectors(dat1, v1 = c("silver", "gold", "platinum"), v2 = c("oil", "gas"))



JSTOR_2wordvectors <- function(x, v1, v2){
  # Comparing vectors of related words of interest (always lower case)
  wordcounts <- x$wordcounts
  bibliodata <- x$bibliodata
  wv1 <- v1
  wv2 <- v2
  wordv1 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% wv1))
  wordv2 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% wv2))
  leng <- sapply(1:length(wordcounts), function(i) length(wordcounts[[i]]))
  # calculate ratios
  wordv1_ratio <- wordv1/leng
  wordv2_ratio <- wordv2/leng
  # get years for each article and make data frame
  twowordvs_by_year <- data.frame(wordv1_ratio, wordv2_ratio, year = as.numeric(as.character(bibliodata$year)))
  # reshape into a long table to make it easier to work with in ggplt
  library(reshape2)
  twowordvs_by_year_melt <- melt(twowordvs_by_year, id.vars = "year")
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # visualise
  library(ggplot2)
  suppressWarnings(ggplot(twowordvs_by_year_melt, aes(year, log(value))) +
                     geom_point(subset = .(value > 0), aes(colour = variable)) +
                     geom_smooth( aes(colour = variable), method = "loess", span = 0.4, subset = .(value > 0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of words")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)) +
                     scale_colour_discrete(labels = c(paste(wv1, collapse = ", "), paste(wv2, collapse = ", "))) )
}
