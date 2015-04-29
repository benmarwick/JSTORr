
#' Plot the frequencies of one word against another word over time in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the relative frequency of two words (two 1-grams) over time. The relative frequency is the frequency of the word in a document divided by the total number of words in a document. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param word1 One word (or vector of words), surrounded by standard quote marks.
#' @param word2 One word (or vector of words), surrounded by standard quote marks.
#' @param span span of the lowess line (controls the degree of smoothing). Default is 0.4
#' @param se display standard error of lowess line. Default is no (false).
#' @param yearfrom year to start the x-axis from, the minimum year to display on the plot
#' @param yearto year to end the x-axis at, the maximum year to display on the plot
#' @return Returns a ggplot object with publication year on the horizontal axis and log relative frequency on the vertical axis. Each point represents a single document.
#' @examples 
#' ## JSTOR_2words(unpack1grams, "diamonds", "pearls")
#' ## JSTOR_2words(unpack1grams, word1 = "milk", word2 = "sugar", span = 0.8) +
#' ## scale_y_continuous(trans=log2_trans()) # to diminish the effect of a few extreme values
#' 


JSTOR_2words <- function(unpack1grams, word1, word2, span = 0.4, se = FALSE, yearfrom = NULL, yearto = NULL){
  # Comparing two words of interest (always lower case)
  y <- unpack1grams$wordcounts
  bibliodata <- unpack1grams$bibliodata
  w1 <- word1
  w2 <- word2
  
  # Get total number of word in the article to standarise for different article lengths
  library(slam)
  leng <- row_sums(y)
  # now get total number of word of interest (always lower case)
  word1 <- as.matrix(y[,dimnames(y)$Terms %in% w1])
  word2 <- as.matrix(y[,dimnames(y)$Terms %in% w2])
  
  # alert if word is not present
  if ( (length(dimnames(word1)$Terms) == 0) | (length(dimnames(word2)$Terms) == 0) ) {
    stop("One or both of these words are not present in the document term matrix, so this function cannot be completed") 
  } else {
  
    
    # if vector of words, get row totals, for word1
    if (length(w1) > 1) {
      word1 <- as.matrix(rowSums(word1)) 
    } else {
      word1 <- word1
    }
    
    # if vector of words, get row totals, for word2
    if (length(w2) > 1) {
      word2 <- as.matrix(rowSums(word2)) 
    } else {
      word2 <- word2
    }  
    
    
  # calculate ratios
  word1_ratio <- word1/leng * 1000
  word2_ratio <- word2/leng * 1000
  

   
  # get years for each article and make data frame
  suppressMessages(library(data.table))
  twowords_by_year <- data.table(word1_ratio, word2_ratio, year = as.numeric(as.character(bibliodata$year)))
  # reshape into a long table to make it easier to work with in ggplt
  library(reshape2)
  twowords_by_year_melt <- melt(twowords_by_year, id.vars = "year")
  
  if(is.null(yearfrom)) { # if no value entered by user, take min value of years in dataset
    
    yearfrom <- as.numeric(as.character(min(bibliodata$year)))
    
    
  } else { # if value intered by user, then check it's in the dataset
    
    if(yearfrom %in% as.numeric(as.character(bibliodata$year))) {
     
      yearfrom <- yearfrom # if it is, take the user's value
      
    } else {
    
    stop("The yearfrom value is outside the range of years available for this corpus")  # if not, throw an error 
    
  }
}

# repeat for upper value of years


if(is.null(yearto)) { # if no value entered by user, take max value of years in dataset
  
  yearto <- as.numeric(as.character(max(bibliodata$year)))
  
  
} else { # if value intered by user, then check it's in the dataset
  
  if(yearto %in% as.numeric(as.character(bibliodata$year))) {
    
    yearto <- yearto # if it is, take the user's value
    
  } else {
    
    stop("The yearto value is outside the range of years available for this corpus")  # if not, throw an error 
    
  }
}






  # visualise
  library(ggplot2)
  library(scales)
  g <- suppressWarnings(ggplot(twowords_by_year_melt, aes(year, log(value))) +
                     geom_point(subset = .(value > 0), aes(colour = variable), size = I(3)) +
                     geom_smooth( aes(colour = variable), se = se, method = "loess", span = span, subset = .(value > 0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                           legend.background = element_blank(), legend.key = element_blank(), 
                           panel.background = element_blank(), panel.border = element_blank(), 
                           strip.background = element_blank(), plot.background = element_blank()) +
                     ylab(paste0("log of frequency per 1000 words")) +
                     
                     scale_x_continuous(limits=c(yearfrom, yearto), breaks = seq((yearfrom - 1), (yearto + 1), 2)) +
                     scale_colour_discrete(labels = c(paste(w1, collapse = ", "), paste(w2, collapse = ", "))) +
                     guides(colour=guide_legend(title="words"))) 
}
# put DOIs back on 
twowords_by_year_melt <- cbind(twowords_by_year_melt, as.character(bibliodata$x))
return(list(words_by_year = twowords_by_year_melt, plot = g))

}
