\name{JSTOR_findassocs}
\alias{JSTOR_findassocs}
\title{Plot the words with the strongest correlation with a given word, by time intervals}
\usage{
  JSTOR_findassocs(x, corpus, word, n = 5, corlimit = 0.4,
    plimit = 0.05, topn = 20, biggest = 5)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents.}

  \item{n}{the number years to aggregate documents by. For
  example, n = 5 (the default value) will create groups of
  all documents published in non-overlapping five year
  ranges.}

  \item{word}{The word to calculate the correlations with}

  \item{corlimit}{The lower threshold value of the Pearson
  correlation statistic (default is 0.4).}

  \item{plimit}{The lower threshold value of the Pearson
  correlation statistic (default is 0.05).}

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
  in ranges of years that positively correlate with a given
  word. For use with JSTOR's Data for Research datasets
  (http://dfr.jstor.org/). For best results, repeat the
  function several times after adding common words to the
  stopword list and excluding them using the
  JSTOR_removestopwords function.
}
\examples{
## findassocs <- JSTOR_findassocs(unpack, corpus, n = 10, "pirates", topn = 100)
## findassocs <- JSTOR_findassocs(unpack, corpus, n = 5, "marines", corlimit=0.6, plimit=0.001)
}

