\name{JSTOR_2bigrams}
\alias{JSTOR_2bigrams}
\title{Plot the frequency of one bigram against bigram over time in a JSTOR DfR dataset}
\usage{
  JSTOR_2bigrams(x, bigram1, bigram2, span = 0.4)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{bigram1}{two words, surrounded by standard quote
  marks, or a vector of bigrams.}

  \item{bigram2}{two words, surrounded by standard quote
  marks, or a vector of bigrams.}

  \item{span}{span of the lowess line (controls the degree
  of smoothing). Default is 0.4}
}
\description{
  Function to plot changes in the relative frequency of two
  bigrams over time. The relative frequency is the
  frequency of the bigram in a document divided by the
  total number of bigrams in a document. For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
## JSTOR_2bigrams(unpack, "pirate booty", "treasure chest")
## JSTOR_2bigrams(unpack, c("treasure chest", "musket balls"), c("jolly roger"), span = 0.2)
}

