\name{JSTOR_2words}
\alias{JSTOR_2words}
\title{Plot the frequencies of one word against another word over time in a JSTOR DfR dataset}
\usage{
JSTOR_2words(unpack1grams, word1, word2, span = 0.4, se = FALSE)
}
\arguments{
  \item{unpack1grams}{object returned by the function
  JSTOR_unpack1grams.}

  \item{word1}{One word (or vector of words), surrounded by
  standard quote marks.}

  \item{word2}{One word (or vector of words), surrounded by
  standard quote marks.}

  \item{span}{span of the lowess line (controls the degree
  of smoothing). Default is 0.4}

  \item{se}{display standard error of lowess line. Default
  is no (false).}
}
\value{
Returns a ggplot object with publication year on the
horizontal axis and log relative frequency on the vertical
axis. Each point represents a single document.
}
\description{
Function to plot changes in the relative frequency of two
words (two 1-grams) over time. The relative frequency is
the frequency of the word in a document divided by the
total number of words in a document. For use with JSTOR's
Data for Research datasets (http://dfr.jstor.org/).
}
\examples{
## JSTOR_2words(unpack1grams, "diamonds", "pearls")
## JSTOR_2words(unpack1grams, word1 = "milk", word2 = "sugar"), span = 0.8
}

