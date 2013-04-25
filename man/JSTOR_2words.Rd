\name{JSTOR_2words}
\alias{JSTOR_2words}
\title{Plot the frequencies of one word against another word over time in a JSTOR DfR dataset}
\usage{
  JSTOR_2words(word1, word2)
}
\arguments{
  \item{word1}{One word, surrounded by standard quote
  marks.}

  \item{word2}{One word, surrounded by standard quote
  marks.}
}
\value{
  Returns a ggplot object with publication year on the
  horizontal axis and log relative frequency on the
  vertical axis. Each point represents a single document.
}
\description{
  Function to plot changes in the relative frequency of two
  words (two 1-grams) over time. The relative frequency is
  the frequency of the word in a document divided by the
  total number of words in a document. For use with JSTOR's
  Data for Research datasets (http://dfr.jstor.org/).
}
\examples{
JSTOR_2words("diamonds", "pearls")
JSTOR_2words("milk", "sugar")
}

