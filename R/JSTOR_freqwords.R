#' Plot the most frequent words by time intervals
#' 
#' @description Generates a plot of the top n words in all the documents in ranges of years. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). For best results, repeat the function several times after adding common words to the stopword list and excluding them by re-running the JSTOR_dtmofnouns function. The location of the English stopwords list can be found by entering this at the R prompt: paste0(.libPaths()[1], "/tm/stopwords/english.dat")
#' @param unpack1grams object returned by the function JSTOR_unpack1grams.
#' @param nouns the object returned by the function JSTOR_dtmofnouns. A Document Term Matrix of nouns.
#' @param n the number years to aggregate documents by. For example, n = 5 (the default value) will create groups of all documents published in non-overlapping five year ranges.
#' @param lowfreq An integer for the minimum frequency of a word to be included in the plot. Default is 300.
#' @param topn An integer for the number of top ranking words to plot. For example, topn = 20 (the default value) will plot the top 20 words for each range of years.
#' @param biggest An integer to control the maximum size of the text in the plot
#' @return Returns a plot of the most frequent words per year range, with word size scaled to frequency, and a dataframe with words and counts for each year range
#' @examples 
#' ## freqwords <- JSTOR_freqwords(unpack1grams, nouns, n = 2, biggest = 5, lowfreq = 100, topn = 5)
#' ## freqwords <- JSTOR_freqwords(unpack1grams, nouns)


JSTOR_freqwords <- function(unpack1grams, nouns, n=5, lowfreq=300, topn = 20, biggest = 10){
  
  # get bibliodata ready
  bibliodata <- unpack1grams$bibliodata
  message("removing stopwords...")
  y <- nouns[, !(nouns$dimnames$Terms %in% stopwords(kind = "en")) ]
  message("done")
require(plyr)

bibliodata$year <- as.numeric(as.character(bibliodata$year))
uniqueyears <- sort(unique(bibliodata$year))

message("getting information about the documents...")
# put names on the Docs that we can use to get year info from
titlefrag <- llply(1:nrow(bibliodata), function(i) paste(unlist(strsplit(as.character(gsub('[[:punct:]]','', bibliodata$doi)[i]), " "))[1:5], collapse = "."), .progress = "text", .inform = FALSE)
y$dimnames$Docs <- paste0(bibliodata$year, "-", titlefrag)
message("done")

# split the years vector into chunks that are n years big
x <- seq_along(uniqueyears)
# get list of chunks, each chunk has n years in it
years <- split(uniqueyears, ceiling(x/n))
# create list of dtms by subsetting the full dtm into seperate
# dtms of documents where publication year matches the chunks
# of years
message(paste0("splitting the document term matrix into chunks of ",n , " years..."))
dtm1 <- vector(mode = "list", length = length(years) )
dtmlist <- llply(1:length(years), function(i) dtm1[[i]] <- y[as.numeric(substring(y$dimnames$Docs, 1, 4)) %in% unname(unlist(years[i]))], .progress = "text", .inform = FALSE)
message("done")

message("building x-axis labels...")
# put names on the list of dtm so we can see the years represented by each dtm
names(dtmlist) <- llply(years, function(i) paste0(min(i),"-",max(i) ),.progress = "text", .inform = FALSE)
message("done")
 
message(paste0("calculating most frequent words in each ", n, " year chunk..."))
# Here's the heavy lifting...
# find most frequent words per chunk (results are in alpha order, not very useful)
freqterms <- llply(dtmlist, function(i) findFreqTerms(i, lowfreq = lowfreq, highfreq = Inf), .progress = "text", .inform = FALSE)
message("done")
  
message("putting the words in rank order...")
# get frequencies of words to put in order of abundance, rather than alpha
freqterms1 <- llply(1:length(years), function(i) colSums(as.matrix(dtmlist[[i]])[, freqterms[[i]]]), .progress = "text", .inform = FALSE)
# reshape into dataframes and sort with most freq word at the top
require(plyr)
freqterms2 <- llply(1:length(years), function(i) arrange(data.frame(word = names(freqterms1[[i]]), count = unname(freqterms1[[i]])), desc(count)), .progress = "text", .inform = FALSE)
names(freqterms2) <- names(dtmlist)
# combine list of dataframes into one big dataframe with word, freq and year-range
freqterms3 <- do.call(rbind, freqterms2)
# make a year-range col by getting everything before the period in the rowname
freqterms3$year <- gsub("\\..*","", rownames(freqterms3))
freqterms3$rank <- as.numeric(gsub(".*\\.","", rownames(freqterms3)))
message('done')

# plot in a word-cloud-y kind of way, but with more useful information in the 
# structure of the visualiation... 
require(ggplot2)
suppressWarnings(print(ggplot(freqterms3, aes(factor(year), rank)) + 
  geom_text(aes(label = word, size = count, alpha = count), data = subset(freqterms3, rank < topn)) +
  scale_y_reverse() + 
  scale_size(range = c(3, biggest), name = "Word count") +
  scale_alpha(range=c(0.5,1), limits=c(min(freqterms3$count),max(freqterms3$count)), guide = 'none') + 
  xlab("Year range") +
  ylab("Rank order of word") +
 # coord_fixed(ratio = (1/(1+sqrt(5))/2)  ) +
  theme(
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0))
  ))

# return the dataframe in case anything else is wanted with it
return(freqterms3)

}
