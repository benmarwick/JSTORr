\name{JSTOR_unpack}
\alias{JSTOR_unpack}
\title{Unpack JSTOR journal articles and bibliographic data}
\usage{
  JSTOR_unpack(path, zipfile)
}
\arguments{
  \item{path}{Full path name of the directory containing
  the zip file obtained from JSTOR's Data for Research
  tool}

  \item{zipfile}{The name of the zip file obtained from
  JSTOR's Data for Research tool (include the zip file
  suffix)}
}
\value{
  Returns a list of two items. First is "wordcounts", a
  list of character vectors where each vector contains the
  words of one article, and second is 'bibliodata', a data
  frame of bibliographic information for all articles.
}
\description{
  Unzip, import and reshape journal articles and
  bibliographic data from the downloaded zipfile and
  reshape ready for simple text mining. For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
## dat1 <- JSTOR_unpack("C:/Documents", "2013.4.20.FxFmBVYd.zip") # note forward slash, not backslash
## dat2 <- JSTOR_unpack(getwd(), "2013.4.20.FxFmBVYd.zip")
}

