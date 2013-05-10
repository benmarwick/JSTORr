\name{JSTOR_lda_topicdists}
\alias{JSTOR_lda_topicdists}
\title{Calculate Euclidean distances between topics}
\usage{
  JSTOR_lda_topicdists(lda)
}
\arguments{
  \item{lda}{the object returned by the function
  JSTOR_lda.}
}
\value{
  Returns plots of the topic clusters and network and a
  graphml file in the working directory that can be opened
  with Gephi
}
\description{
  Generates a dendrogram of topic clusters, a network plot
  of topic-topic relationships, and a graphml file to open
  with Gephi. For use with JSTOR's Data for Research
  datasets (http://dfr.jstor.org/).
}
\examples{
## JSTOR_lda_topicdists(lda = lda150)
}

