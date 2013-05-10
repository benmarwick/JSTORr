\name{JSTOR_freqwords}
\alias{JSTOR_freqwords}
\title{Plot the most frequent words by time intervals}
\usage{
  JSTOR_freqwords(x, corpus, n = 5, lowfreq = 300,
    topn = 20, biggest = 30)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents.}

  \item{n}{the number years to aggregate documents by. For
  example, n = 5 (the default value) will create groups of
  all documents published in non-overlapping five year
  ranges.}

  \item{lowfreq}{An integer for the minimum frequency of a
  word to be included in the plot. Default is 300.}

  \item{topn}{An integer for the number of top ranking
  words to plot. For example, topn = 20 (the default value)
  will plot the top 20 words for each range of years.}

  \item{biggest}{An integer to control the maximum size of
  the text in the plot}
}
\value{
  Returns a plot of the most frequent words per year range,
  with word size scaled to frequency, and a dataframe with
  words and counts for each year range
}
\description{
  Generates a plot of the top n words in all the documents
  in ranges of years. For use with JSTOR's Data for
  Research datasets (http://dfr.jstor.org/). For best
  results, repeat the function several times after adding
  common words to the stopword list and excluding them
  using the JSTOR_removestopwords function.
}
\examples{
## freqwords <- JSTOR_freqwords(unpack, corpus, n = 2, biggest = 40, lowfreq = 100, topn = 5)
## freqwords <- JSTOR_freqwords(unpack, corpus)
}

