\name{JSTOR_dtmofnouns}
\alias{JSTOR_dtmofnouns}
\title{Make a Document Term Matrix containing only nouns}
\usage{
  JSTOR_dtmofnouns(unpack1grams, word = NULL,
    parallel = FALSE, sparse = 0.999)
}
\arguments{
  \item{unpack1grams}{object returned by the function
  JSTOR_unpack1grams.}

  \item{word}{Optional word or vector of words to subset
  the documents by, ie. make a document term matrix
  containing only documents in which this word (or words)
  appears at least once.}

  \item{sparse}{A numeric for the maximal allowed sparsity,
  default is 0.999. Removes sparse terms from a
  term-document matrix, see help(removeSparseTerms) for
  more details. Values close to 1 result in a sparse
  matrix, values close to zero result in a dense matrix. It
  may be useful to reduce sparseness if the matrix is too
  big to manipulate in memory or if processing times are
  long.}

  \item{parallel}{logical.  If TRUE attempts to run the
  function on multiple cores.  Note that this may actually
  be slower if you have one core, limited memory or if the
  data set is small due to communication of data between
  the cores.}
}
\value{
  Returns a Document Term Matrix containing documents,
  ready for more advanced text mining and topic modelling.
}
\description{
  This function does part-of-speech tagging and removes all
  parts of speech that are not non-name nouns. It also
  removes punctuation, numbers, words with less than three
  characters, stopwords and unusual characters (characters
  not in ISO-8859-1, ie non-latin1-ASCII). For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/). This function uses the stoplist
  in the tm package. The location of tm's English stopwords
  list can be found by entering this at the R prompt:
  paste0(.libPaths()[1], "/tm/stopwords/english.dat") Note
  that the part-of-speech tagging can result in the removal
  of words of interest. To prevent the POS tagger from
  removing these words, edit the tagdict file and add the
  word(s) with a NN tag. To find the tagdict file, enter
  this at the R prompt: at the R prompt:
  paste0(.libPaths()[1],
  "/openNLPmodels.en/models/parser/tagdict") and edit with
  a text editor.
}
\examples{
## nouns <- JSTOR_dtmofnouns(unpack1grams)
}

