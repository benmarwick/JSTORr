\name{JSTOR_1bigram}
\alias{JSTOR_1bigram}
\title{Plot the frequency of one word over time in a JSTOR DfR dataset}
\usage{
  JSTOR_1bigram(unpack2, bigram, span = 0.4)
}
\arguments{
  \item{unpack2}{object returned by the function
  JSTOR_unpack.}

  \item{bigram}{two words, surrounded by standard quote
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
  Function to plot changes in the relative frequency of a
  bigram over time. The relative frequency is the frequency
  of the bigram in a document divided by the total number
  of bigrams in a document. For use with JSTOR's Data for
  Research datasets (http://dfr.jstor.org/).
}
\examples{
## JSTOR_1bigram(unpack2, "pirate booty")
## JSTOR_1bigram(unpack2, c("treasure chest", "musket balls", "jolly roger"), span = 0.7)
}

