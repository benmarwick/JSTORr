\name{JSTOR_1word}
\alias{JSTOR_1word}
\title{Plot the frequency of one word over time in a JSTOR DfR dataset}
\usage{
  JSTOR_1word(x, oneword, span = 0.5)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{oneword}{One word, surrounded by standard quote
  marks, or a vector of words}

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
  word over time. The relative frequency is the frequency
  of the word in a document divided by the total number of
  words in a document. For use with JSTOR's Data for
  Research datasets (http://dfr.jstor.org/).
}
\examples{
##JSTOR_1word(unpack, "diamonds")
##JSTOR_1word(unpack, c("diamonds", pearls"), span = 0.8)
}

