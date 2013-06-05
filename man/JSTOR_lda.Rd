\name{JSTOR_lda}
\alias{JSTOR_lda}
\title{Generate a topic model with K topics using the implementation of Latent Dirichlet allocation (LDA) in the lda package}
\usage{
  JSTOR_lda(unpack1grams, nouns, K, alpha = 50/K)
}
\arguments{
  \item{unpack1grams}{object returned by the function
  JSTOR_unpack1grams.}

  \item{nouns}{the object returned by the function
  JSTOR_dtmtonouns.}

  \item{K}{the number of topics that the model should
  contain}

  \item{alpha}{The scalar value of the Dirichlet
  hyperparameter for topic proportions. Higher values lead
  to more uniform distributions of topics over documents.
  Default is 50/K}
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
## lda1 <- JSTOR_lda(unpack1grams, nouns, K = 150)
}

