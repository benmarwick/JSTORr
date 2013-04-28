\name{JSTOR_2bigramscor}
\alias{JSTOR_2bigramscor}
\title{Plot the change over time of the correlation between one bigram (or set of bigrams) and another bigram (or set of bigrams) in a JSTOR DfR dataset}
\usage{
  JSTOR_2bigramscor(x, bigram1, bigram2)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{bigram1}{One bigram or a vector of bigrams, each
  bigram surrounded by standard quote marks.}

  \item{bigram2}{One bigram or a vector of bigrams, each
  bigram surrounded by standard quote marks.}
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
  sets of bigrams (two sets of 2-grams, or two sets of
  multiple bigrams) over time. The relative frequency is
  the frequency of bigrams in a document divided by the
  total number of bigrams in a document. For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
## JSTOR_2bigramscor(dat1, bigram1 = "hot water", bigram2 = "cold water"))
## JSTOR_2bigramscor(dat1, c("hot water", "warm water", "tepid water"), c("cold water", "ice water"))
}

