\name{JSTOR_lda}
\alias{JSTOR_lda}
\title{Generate a topic model with K topics using Latent Dirichlet allocation (LDA)}
\usage{
  JSTOR_lda(corpus, K)
}
\arguments{
  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents.}

  \item{K}{the number of topics that the model should
  contain}
}
\value{
  Returns a data frame with documents as rows, topics as
  columns and posterior probabilities as cell values.
}
\description{
  Generates a topic model with K topics using Latent
  Dirichlet allocation (LDA, with the lda package) For use
  with JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
## lda1 <- JSTOR_lda(x = unpacked, corpus = corpus, K = 150)
}

