\name{JSTOR_lda_docdists}
\alias{JSTOR_lda_docdists}
\title{Calculate Euclidean distances between documents}
\usage{
JSTOR_lda_docdists(lda)
}
\arguments{
  \item{lda}{the object returned by the function
  JSTOR_lda.}
}
\value{
Returns plots of the document clusters and network and a
graphml file in the working directory that can be opened
with Gephi
}
\description{
Generates a dendrogram of document clusters, a network plot
of document-document relationships, and a graphml file to
open with Gephi. For use with JSTOR's Data for Research
datasets (http://dfr.jstor.org/).
}
\examples{
## JSTOR_lda_docdists(lda = lda150)
}

