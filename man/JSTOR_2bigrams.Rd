\name{JSTOR_2bigrams}
\alias{JSTOR_2bigrams}
\title{Plot the frequency of one bigram against bigram over time in a JSTOR DfR dataset}
\usage{
  JSTOR_2bigrams(unpack2grams, bigram1, bigram2,
    span = 0.4)
}
\arguments{
  \item{unpack2grams}{object returned by the function
  JSTOR_unpack2grams.}

  \item{bigram1}{two words, surrounded by standard quote
  marks, or a vector of bigrams.}

  \item{bigram2}{two words, surrounded by standard quote
  marks, or a vector of bigrams.}

  \item{span}{span of the lowess line (controls the degree
  of smoothing). Default is 0.4}
}
\value{
  Returns a ggplot object with publication year on the
  horizontal axis and log relative frequency on the
  vertical axis. Each point represents a single document.
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
## JSTOR_2bigrams(unpack2grams, "pirate booty", "treasure chest")
## JSTOR_2bigrams(unpack2grams, c("treasure chest", "musket balls"), c("jolly roger"), span = 0.2)
}

