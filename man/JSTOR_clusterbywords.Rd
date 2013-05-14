\name{JSTOR_clusterbywords}
\alias{JSTOR_clusterbywords}
\title{Cluster documents by similarities in word frequencies}
\usage{
  JSTOR_clusterbywords(x, corpus, word)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents
  with stopwords removed.}

  \item{word}{The word to subset the documents by, ie. use
  only documents containing this word in the cluster
  analysis}
}
\value{
  Returns plots of clusters of documents, and dataframes of
  k-means and PCA output
}
\description{
  Generates plots visualizing a clustering of the
  documents. For use with JSTOR's Data for Research
  datasets (http://dfr.jstor.org/). For best results,
  repeat the function several times after adding common
  words to the stopword list and excluding them using the
  JSTOR_removestopwords function.
}
\examples{
## cl1 <- JSTOR_clusterbywords(unpack, corpus, "pirates")
## cl2 <- JSTOR_clusterbywords(unpack, corpus, c("pirates", "privateers"))
}

