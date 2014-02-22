\name{JSTOR_corpusofnouns}
\alias{JSTOR_corpusofnouns}
\title{Remove all words except non-name nouns}
\usage{
JSTOR_corpusofnouns(x, parallel = FALSE)
}
\arguments{
  \item{x}{object returned by the function JSTOR_unpack.}

  \item{parallel}{if TRUE, apply function in parallel,
  using the parallel library}
}
\value{
Returns a Document Term Matrix containing documents, ready
for more advanced text mining and topic modelling.
}
\description{
This function does part-of-speech tagging and removes all
parts of speech that are not non-name nouns. It also
removes punctuation, numbers, words with less than three
characters, stopwords and unusual characters (characters
not in ISO-8859-1, ie non-latin1-ASCII). For use with
JSTOR's Data for Research datasets (http://dfr.jstor.org/).
This function uses the stoplist in the tm package. The
location of tm's English stopwords list can be found by
entering this at the R prompt: paste0(.libPaths()[1],
"/tm/stopwords/english.dat") Note that the part-of-speech
tagging can result in the removal of words of interest. To
prevent the POS tagger from removing these words, edit the
tagdict file and add the word(s) with a NN tag. To find the
tagdict file, enter this at the R prompt: at the R prompt:
paste0(.libPaths()[1],
"/openNLPmodels.en/models/parser/tagdict") and edit with a
text editor.
}
\examples{
## nouns <- JSTOR_corpusofnouns(unpack, parallel = TRUE)
}

