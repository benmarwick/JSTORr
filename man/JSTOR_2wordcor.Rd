\name{JSTOR_2wordcor}
\alias{JSTOR_2wordcor}
\title{Plot the change over time of the correlation between one set of words and another set of words in a JSTOR DfR dataset}
\usage{
  JSTOR_2wordcor(x, word1, word2)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{word1}{One word or a vector of words, each word
  surrounded by standard quote marks.}

  \item{word2}{One word or a vector of words, each word
  surrounded by standard quote marks.}
}
\value{
  Returns a ggplot object with publication year on the
  horizontal axis and Pearson's correlation on the vertical
  axis. Each point represents all the documents of a single
  year, point size is inversely proportional to p-value of
  the correlation.
}
\description{
  Function to plot changes in the relative frequency of two
  set of words (two sets of 1-grams) over time. The
  relative frequency is the frequency of the set of words
  in a document divided by the total number of words in a
  document. For use with JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
##JSTOR_2wordcor(dat1, word1 = "pearls", word2 = "diamonds"))
##JSTOR_2wordcor(dat1, c("silver", "gold", "platinum"), c("oil", "gas"))
}

