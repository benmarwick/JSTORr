
#' Plot the change over time of the correlation between one bigram (or set of bigrams) and another bigram (or set of bigrams) in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of two sets of bigrams (two sets of 2-grams, or two sets of multiple bigrams) over time. The relative frequency is the frequency of bigrams in a document divided by the total number of bigrams in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param unpack2 object returned by the function JSTOR_unpack2.
#' @param bigram1 One bigram or a vector of bigrams, each bigram surrounded by standard quote marks.
#' @param bigram2 One bigram or a vector of bigrams, each bigram surrounded by standard quote marks.
#' @param span span of the loess line (controls the degree of smoothing). Default is 0.4
#' @return Returns a ggplot object with publication year on the horizontal axis and Pearson's correlation on the vertical axis. Each point represents all the documents of a single year, point size is inversely proportional to p-value of the correlation.
#' @examples 
#' ## JSTOR_2bigramscor(unpack2, bigram1 = "hot water", bigram2 = "cold water")
#' ## JSTOR_2bigramscor(unpack2, c("hot water", "warm water", "tepid water"), c("cold water", "ice water"))


JSTOR_2bigramscor <- function(unpack2, bigram1, bigram2, span = 0.4){
  ## investigate correlations between bigrams over time
  y <- unpack2$bigrams
  bibliodata <- unpack2$bibliodata
  bg1 <- bigram1
  bg2 <- bigram2
  
  # using dtm
  # y <- as.matrix(wordcounts)
  # Get total number of word in the article to standarise for different article lengths
  library(slam)
  leng <- row_sums(y)
  # now get total numbers of words of interest (always lower case)
  bg1a <- as.matrix(y[,dimnames(y)$Terms %in% bg1])
  bg2a <- as.matrix(y[,dimnames(y)$Terms %in% bg2])
  
  
#   # full-text method
#   bg1a <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bg1))
#   bg2a <- sapply(1:length(bigrams), function(i) sum(bigrams[[i]] %in% bg2))
#   leng <- sapply(1:length(bigrams), function(i) length(bigrams[[i]]))
  
  
  # calculate ratios
  cbg1_ratio <- bg1a/leng
  cbg2_ratio <- bg2a/leng
  # get years for each article and make data frame
  suppressMessages(library(data.table))
  c2bigrams_by_year <- data.table(cbg1 = cbg1_ratio, cbg2 = cbg2_ratio, year = as.numeric(as.character(bibliodata$year)))
  setnames(c2bigrams_by_year, c("cbg1", "cbg2", "year"))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # calculate correlations of the two words per year (and p-values)
  library(plyr)
  corrp <- ddply(c2bigrams_by_year, .(year), summarize, "corr" = cor.test(cbg1, cbg2)$estimate, "pval" = cor.test(cbg1, cbg2)$p.value)
  # visualise
  library(ggplot2)
  suppressWarnings(ggplot(corrp, aes(year, corr)) +
                     geom_point(aes(size = -pval)) +
                     geom_smooth(  method = "loess", span = span, se = FALSE) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     geom_hline(yintercept=0, colour = "red") + 
                     ylim(min(corrp$corr), 1.0) +
                     ylab(paste0("correlation between \'", bg1, "\' and \'", bg2, "\'")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_max+1, 2)) +
                     scale_size_continuous("p-values", breaks = c(-0.75, -0.25, -0.05, -0.001), labels = c(0.75, 0.25, 0.05, 0.001)))
}
