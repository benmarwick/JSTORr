\name{JSTOR_2wordvectors}
\alias{JSTOR_2wordvectors}
\title{Plot the frequencies of one set of words against another set of words over time in a JSTOR DfR dataset}
\usage{
  JSTOR_2wordvectors(x, v1, v2)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{v1}{One vector of words, each word surrounded by
  standard quote marks.}

  \item{v2}{A second vector of words, each word surrounded
  by standard quote marks.}
}
\value{
  Returns a ggplot object with publication year on the
  horizontal axis and log relative frequency on the
  vertical axis. Each point represents a single document.
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
##JSTOR_2wordvectors(dat1, c("diamonds", "pearls"), c"milk", "sugar"))
##JSTOR_2wordvectors(dat1, v1 = c("silver", "gold", "platinum"), v2 = c("oil", "gas"))
}

