\name{JSTOR_corpusofnouns}
\alias{JSTOR_corpusofnouns}
\title{Build corpus and remove all words except non-name nouns}
\usage{
  JSTOR_corpusofnouns(x)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}
}
\value{
  Returns a corpus containing documents, ready for more
  advanced text mining and topic modelling.
}
\description{
  This function does part-of-speech tagging and removes all
  parts of speech that are not non-name nouns. It also
  removes punctuation, numbers, words with less than three
  characters, stopwords and unusual characters (characters
  not in ISO-8859-1, ie non-latin1-ASCII). For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/). This function uses the stoplist
  in the tm package. The location of tm's English stopwords
  list can be found by entering this at the R prompt:
  paste0(.libPaths()[1], "/tm/stopwords/english.dat")
}
\examples{
## mycorpus <- JSTOR_corpusofnouns(unpack)
}

