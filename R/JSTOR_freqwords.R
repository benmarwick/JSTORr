#' Plot the most frequent words by time intervals
#' 
#' @description Generates a plot of the top n words in all the documents in ranges of years. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents.
#' @param n the number years to aggregate documents by. For example, n = 5 (the default value) will create groups of all documents published in non-overlapping five year ranges.
#' @param lowfreq An integer for the minimum frequency of a word to be included in the plot. Default is 300.
#' @param topn An integer for the number of top ranking words to plot. For example, topn = 20 (the default value) will plot the top 20 words for each range of years.
#' @param biggest An integer to control the maximum size of the text in the plot
#' @return Returns a plot of the most frequent words per year range, with word size scaled to frequency, and a dataframe with words and counts for each year range
#' @examples 
#' ## freqwords <- JSTOR_freqwords(corpus, n = 2, biggest = 40, lowfreq = 100, topn = 5)
#' ## freqwords <- JSTOR_freqwords(corpus)


JSTOR_freqwords <- function(corpus, n=5, lowfreq=300, topn = 20, biggest = 30){

require(tm)
# convert corpus to dtm, keep words that occur in 5 or more docs
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5,Inf))))
# put year and title fragment as document names
# get first five words of title (without any punctuation marks)
titlefrag <- lapply(1:nrow(bibliodata), function(i) paste(unlist(strsplit(as.character(gsub('[[:punct:]]','', bibliodata$doi)[i]), " "))[1:5], collapse = "."))
dtm$dimnames$Docs <- paste0(bibliodata$year, "_", titlefrag)

bibliodata$year <- as.numeric(as.character(bibliodata$year))
uniqueyears <- sort(unique(bibliodata$year))

# split the years vector into chunks that are n years big
x <- seq_along(uniqueyears)
# get list of chunks, each chunk has n years in it
years <- split(uniqueyears, ceiling(x/n))
# create list of dtms by subsetting the full dtm into seperate
# dtms of documents where publication year matches the chunks
# of years
dtmlist <- lapply(1:length(years), function(i) dtm1[[i]] <- dtm[as.numeric(substring(dtm$dimnames$Docs, 1, 4)) %in% unname(unlist(years[i]))])
# put names on the list of dtm so we can see the years represented by each dtm
names(dtmlist) <- lapply(years, function(i) paste0(min(i),"_",max(i) ))
# find most frequent words per chunk (results are in alpha order, not very useful)
freqterms <- lapply(dtmlist, function(i) findFreqTerms(i, lowfreq = lowfreq, highfreq = Inf))
# get frequencies of words to put in order of abundance, rather than alpha
freqterms1 <- lapply(1:length(years), function(i) colSums(as.matrix(dtmlist[[i]])[, freqterms[[i]]]))
# reshape into dataframes and sort with most freq word at the top
require(plyr)
freqterms2 <- lapply(1:length(years), function(i) arrange(data.frame(word = names(freqterms1[[i]]), count = unname(freqterms1[[i]])), desc(count)))
names(freqterms2) <- names(dtmlist)
# combine list of dataframes into one big dataframe with word, freq and year-range
freqterms3 <- do.call(rbind, freqterms2)
# make a year-range col by getting everything before the period in the rowname
freqterms3$year <- gsub("\\..*","", rownames(freqterms3))
freqterms3$rank <- as.numeric(gsub(".*\\.","", rownames(freqterms3)))

# plot in a word-cloud-y kind of way, but with more useful information in the 
# structure of the visualiation... 
print(ggplot(freqterms3, aes(factor(year), rank)) + 
  geom_text(aes(label = word, size = count, alpha = count), data = subset(freqterms3, rank < topn)) +
  scale_y_reverse() + 
  scale_size(range = c(3, biggest), name = "Word count") +
  scale_alpha(range=c(0.5,1), limits=c(min(freqterms3$count),max(freqterms3$count)), guide = 'none') + 
  xlab("Year range") +
  ylab("Rank order of word"))

# return the dataframe in case anything else is wanted with it
return(freqterms3)

}
