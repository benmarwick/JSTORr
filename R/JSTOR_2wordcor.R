#' Plot the change over time of the correlation between one set of words and another set of words in a JSTOR DfR dataset
#' 
#' @description Function to plot changes in the correlation of two set of words (two sets of 1-grams) over time. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param word1 One word or a vector of words, each word surrounded by standard quote marks.
#' @param word2  One word or a vector of words, each word surrounded by standard quote marks.
#' @param span span of the loess line (controls the degree of smoothing). Default is 0.4
#' @param yearfrom year to start the x-axis from, the minimum year to display on the plot
#' @param yearto year to end the x-axis at, the maximum year to display on the plot
#' @return Returns a ggplot object with publication year on the horizontal axis and Pearson's correlation on the vertical axis. Each point represents all the documents of a single year, point size is inversely proportional to p-value of the correlation.
#' @examples 
#' ## JSTOR_2wordcor(unpack1grams, word1 = "pearls", word2 = "diamonds")
#' ## JSTOR_2wordcor(unpack1grams, c("silver", "gold", "platinum"), c("oil", "gas"), span = 0.3)


JSTOR_2wordcor <- function(unpack1grams, word1, word2, span = 0.4, yearfrom = NULL, yearto = NULL){
  ## investigate correlations between words over time
  y <- unpack1grams$wordcounts
  bibliodata <- unpack1grams$bibliodata
  cw1 <- word1
  cw2 <- word2
  
  # using dtm

  # Get total number of word in the article to standarise for different article lengths
  library(slam)
  leng <- row_sums(y)
  # now get total numbers of words of interest (always lower case)
  cword1 <- as.matrix(y[,dimnames(y)$Terms %in% cw1])
  cword2 <- as.matrix(y[,dimnames(y)$Terms %in% cw2])
  
  # alert if word is not present
  if ( (length(dimnames(cword1)$Terms) == 0) | (length(dimnames(cword1)$Terms) == 0) ) {
    stop("One or both of these words are not present in the document term matrix, so this function cannot be completed") 
  } else {
    
    # if vector of words, get row totals, for word1
    if (length(cw1) > 1) {
      cword1 <- as.matrix(rowSums(cword1)) 
    } else {
      cword1 <- cword1
    }
    
    # if vector of words, get row totals, for word2
    if (length(cw2) > 1) {
      cword2 <- as.matrix(rowSums(cword2)) 
    } else {
      cword2 <- cword2
    }
    
    

  # calculate ratios
  cword1_ratio <- cword1/leng
  cword2_ratio <- cword2/leng
  # get years for each article and make data frame
  suppressMessages(library(data.table))
  ctwowords_by_year <- data.table(ww1 = cword1_ratio, ww2 = cword2_ratio, year = as.numeric(as.character(bibliodata$year)))
  setnames(ctwowords_by_year, c("ww1", "ww2", "year"))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # calculate correlations of the two words per year (and p-values)
  library(plyr)
  corrp <- ddply(ctwowords_by_year, .(year), summarize, "corr" = cor.test(ww1, ww2)$estimate, "pval" = cor.test(ww1, ww2)$p.value)
  
  
  # apply year limits, if any specified
  
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
  g <- suppressWarnings(ggplot(corrp, aes(year, corr)) +
                     geom_point(aes(size = -pval)) +
                     geom_smooth(  method = "loess", span = span, se = FALSE) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     geom_hline(yintercept=0, colour = "red") + 
                     theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                           legend.background = element_blank(), legend.key = element_blank(), 
                           panel.background = element_blank(), panel.border = element_blank(), 
                           strip.background = element_blank(), plot.background = element_blank()) +
                     ylab(paste0("correlation between '",cw1, "' and '", cw2,"'")) +
                     ylim(-1.0, 1.0) +
                     scale_x_continuous(limits=c(yearfrom, yearto), breaks = seq((yearfrom - 1), (yearto + 1), 2)) +
                     scale_size_continuous("p-values", breaks = c(-0.75, -0.25, -0.05, -0.001), labels = c(0.75, 0.25, 0.05, 0.001)))
  }
  
  return(list(corrp = corrp, plot = g))
  }
