\name{JSTOR_clusterbywords}
\alias{JSTOR_clusterbywords}
\title{Cluster documents by similarities in word frequencies}
\usage{
  JSTOR_clusterbywords(x, corpus, word, f = 0.01)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents
  with stopwords removed.}

  \item{word}{The word to subset the documents by, ie. use
  only documents containing this word in the cluster
  analysis}

  \item{f}{A scalar value to filter the total number of
  words used in the cluster analyses. For each document,
  the count of each word is divided by the total number of
  words in that document, expressing a word's frequency as
  a proportion of all words in that document. This
  parameter corresponds to the summed proportions of a word
  in all documents (ie. the column sum for the document
  term matrix). If f = 0.01 then only words that constitute
  at least 1.0% of all words in all documents will be used
  for the cluster analyses.}
}
\value{
  Returns plots of clusters of documents, and dataframes of
  affinity propogation clustering, k-means and PCA outputs
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

