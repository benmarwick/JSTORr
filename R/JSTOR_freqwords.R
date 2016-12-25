#' Plot the most frequent words by time intervals
#' 
#' @description Generates a plot of the top n words in all the documents in ranges of years. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them by re-running the JSTOR_dtmofnouns function. The location of the English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat")
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param nouns the object returned by the function JSTOR_dtmofnouns. A Document Term Matrix of nouns.
#' @param n the number years to aggregate documents by. For example, n = 5 (the default value) will create groups of all documents published in non-overlapping five year ranges.
#' @param lowfreq An integer for the minimum frequency of a word to be included in the plot. Default is 300.
#' @param topn An integer for the number of top ranking words to plot. For example, topn = 20 (the default value) will plot the top 20 words for each range of years.
#' @param biggest An integer to control the maximum size of the text in the plot
#' @param custom_stopwords character vector of stop words to use in addition to the default set supplied by the tm package
#' @return Returns a plot of the most frequent words per year, with word size scaled to frequency (accessed via freqwords$plot$plot, yes twice), and a dataframe with words and counts for each year range (accessed via freqwords$freqterms).
#' @examples 
#' ## freqwords <- JSTOR_freqwords(unpack1grams, nouns, n = 2, biggest = 5, lowfreq = 100, topn = 5)
#' ## freqwords <- JSTOR_freqwords(unpack1grams, nouns)
#' @import plyr ggplot2


JSTOR_freqwords <- function (unpack1grams, nouns, custom_stopwords = NULL, n = 5, lowfreq = 300, topn = 20, 
                             biggest = 10 ) 
{
  
  # bibliodata <- unpack_multi$bibliodata
  bibliodata <- unpack1grams$bibliodata
  message("removing stopwords...")
  y <- nouns[, !(nouns$dimnames$Terms %in% c(custom_stopwords, stopwords(kind = "en"))) ]
  
  
  # make sure we have a good match between DOIs and years
  DOIs_in_dtm <- data.frame(DOI = y$dimnames$Docs)
  DOIs_and_years_in_bibliodata <- data.frame(DOI = bibliodata$x,
                                             year = as.numeric(substring(bibliodata$issue, 1, 4)))      

  DOIs_and_years_in_dtm <- join(DOIs_in_dtm, DOIs_and_years_in_bibliodata, by = "DOI")
  # rename docs with year of publication, need to have Docs as character to subset it...
  y$dimnames$Docs <- as.character(DOIs_and_years_in_dtm$year)
  
  # subset bibliodata in case we filtered the nouns by a word
  
  bibliodata_subset <- bibliodata[(bibliodata$year %in% DOIs_and_years_in_dtm$year), ]
  
  
  message("done")
  bibliodata_subset$year <- as.numeric(as.character(bibliodata_subset$year))
  uniqueyears <- sort(unique(bibliodata_subset$year))
  
  message("getting information about the documents...")
  titlefrag <- llply(1:nrow(bibliodata_subset), function(i) paste(unlist(strsplit(as.character(gsub("[[:punct:]]", 
                                                                                                    "", bibliodata_subset$doi)[i]), " "))[1:5], collapse = "."), 
                     .progress = "text", .inform = FALSE)
  
  message("done")
  x <- seq_along(uniqueyears)
  years <- split(uniqueyears, ceiling(x/n))
  message(paste0("splitting the document term matrix into chunks of ", 
                 n, " years..."))
  
  
  
  dtm1 <- vector(mode = "list", length = length(years))
  dtmlist <- llply(1:length(years), function(i) 
    dtm1[[i]] <- y[(y$dimnames$Docs 
                    %in% unname(unlist(years[i]))), ], 
    .progress = "text",  .inform = FALSE)
  
  
  
  message("done")
  message("building x-axis labels...")
  names(dtmlist) <- llply(years, function(i) paste0(min(i), 
                                                    "-", max(i)))
  message("done")
  message(paste0("calculating most frequent words in each ", 
                 n, " year chunk..."))
  freqterms <- llply(dtmlist, function(i) findFreqTerms(i, 
                                                        lowfreq = lowfreq, highfreq = Inf), .progress = "text", 
                     .inform = FALSE)
  message("done")
  message("putting the words in rank order...")
  freqterms1 <- llply(1:length(years), 
                      function(i) colSums(as.matrix(dtmlist[[i]])[,  freqterms[[i]], drop=FALSE]), 
                      .progress = "text", .inform = FALSE)
  
  

  freqterms2 <- llply(1:length(years), function(i) arrange(data.frame(word = names(freqterms1[[i]]), 
                                                                      count = unname(freqterms1[[i]])), desc(count)), .progress = "text", 
                      .inform = FALSE)
  names(freqterms2) <- names(dtmlist)
  freqterms3 <- do.call(rbind, freqterms2)
  freqterms3$year <- gsub("\\..*", "", rownames(freqterms3))
  freqterms3$rank <- as.numeric(gsub(".*\\.", "", rownames(freqterms3)))
  message("done")

png("NULL")
  plot <- suppressWarnings(print(ggplot(freqterms3, 
                                        aes(factor(year), 
                                                rank)) + 
                                   geom_text(aes(label = word, 
                                                 size = count, 
                                                 alpha = count), 
                                             data = subset(freqterms3, 
                                                           rank < topn)) +
                                   scale_y_reverse() + 
                           scale_size(range = c(3, biggest), 
                                      name = "Word count") + 
                           scale_alpha(range = c(0.5, 1), 
                                       limits = c(min(freqterms3$count),
                                                  max(freqterms3$count)), 
                                       guide = "none") + 
                             xlab("Year range") + 
                           ylab("Rank order of word") + 
                             theme(panel.background = element_blank(), 
                                   panel.border = element_blank(), 
                                   panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   plot.background = element_blank(), 
                                   axis.text.x = element_text(angle = 90, 
                                                              hjust = 0))))
  
dev.off()

  return(list(freqterms = freqterms3, plot = plot))
  }



