\name{JSTOR_unpack}
\alias{JSTOR_unpack}
\title{Unpack JSTOR journal articles and bibliographic data}
\usage{
  JSTOR_unpack(parallel = TRUE, path)
}
\arguments{
  \item{parallel}{if TRUE, apply function in parallel,
  using the parallel library}

  \item{path}{to directory containing 'wordcounts' and
  'bigrams' folders and the citations.CSV file. These are
  obtained after unzipping the file downloaded from DfR
  (you should unzip the file before running this
  function).}
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
  Import and reshape journal articles and bibliographic
  data from the downloaded zipfile and reshape ready for
  the MALLET functions. Currently this is only useful for
  smaller collections because it generates large data
  objects in memory. As a rough guide, <5000 articles
  should be ok.
}
\examples{
## unpack <- JSTOR_unpack(path = "C:/Users/marwick/Downloads/JSTOR")
}

