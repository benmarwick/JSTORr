\name{JSTOR_MALLET_topicsovertime}
\alias{JSTOR_MALLET_topicsovertime}
\title{Plot the changes in topic proportions over time}
\usage{
  JSTOR_MALLET_topicsovertime(x, topics = 1:56)
}
\arguments{
  \item{x}{the object returned by the function
  JSTOR_unpack.}

  \item{topics}{a range of topic numbers to plot, Default
  is 1 to 56 (ie. the first 56 topics in the model). A
  mimimum of two topics must be specified.}
}
\value{
  Returns a plot of topics over time and a list of
  dataframes of the topic proportions per document for
  further analysis.
}
\description{
  Generates plots and data frames showing changes in topic
  proportions over time. For use with JSTOR's Data for
  Research datasets (http://dfr.jstor.org/).
}
\examples{
## tot <- JSTOR_MALLET_topicsovertime(x = unpack, topics = 1:100)
}
\author{
  Ben Marwick with substantial contributions from Andrew
  Goldstone
}

