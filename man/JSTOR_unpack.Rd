\name{JSTOR_unpack}
\alias{JSTOR_unpack}
\title{Unpack JSTOR journal articles and bibliographic data}
\usage{
  JSTOR_unpack()
}
\value{
  Returns a list of three items. First is "wordcounts", a
  list of character vectors where each vector contains the
  words of one article, second is 'bigrams', as for
  'wordcounts' but with 2-grams instead of 1-grams, and
  third is 'bibliodata', a data frame of bibliographic
  information for all articles.
}
\description{
  Unzip, import and reshape journal articles and
  bibliographic data from the downloaded zipfile and
  reshape ready for simple text mining. For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/). Function prompts for full path
  name of the directory containing the zip file obtained
  from JSTOR's Data for Research tool and for the name of
  the zip file obtained from JSTOR's Data for Research tool
  (include the zip file suffix)
}
\examples{
## dat1 <- JSTOR_unpack() # then follow prompts to paste in path and the name of the zipfile
## unpack <- JSTOR_unpack() # as above
}

