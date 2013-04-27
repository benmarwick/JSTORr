\name{JSTOR_chunk}
\alias{JSTOR_chunk}
\title{Divide documents up into chunks of n words}
\usage{
  JSTOR_chunk(x, corpus, n)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents.}

  \item{n}{the number of words that each chunk should
  contain}
}
\value{
  Returns a list of character vectors. Each vector is a
  chunk of n words. The vectors have names corresponding to
  the first column of the bibliodata data frame.
}
\description{
  Divides documents up into chunks of n words, ready for
  topic modelling. For use with JSTOR's Data for Research
  datasets (http://dfr.jstor.org/).
}
\examples{
## chunk1 <- JSTOR_chunk(x = unpacked, corpus = corpus, n = 1000)
}

