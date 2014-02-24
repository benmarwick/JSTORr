\name{JSTOR_MALLET}
\alias{JSTOR_MALLET}
\title{Generate one or more topic models using MALLET's implementation of Latent Dirichlet allocation (LDA)}
\usage{
JSTOR_MALLET(nouns, MALLET = "C:/mallet-2.0.7",
  JAVA = "C:/Program Files (x86)/Java/jre7/bin", K)
}
\arguments{
  \item{nouns}{the object returned by the function
  JSTOR_dtmofnouns. If you want to use the document
  contents with all other parts of speech, use
  unpack1grams$wordcounts as the first argument here (where
  unpack1grams is the output from JSTOR_unpack1grams)}

  \item{MALLET}{the directory containing MALLET's bin
  directory, ideally "C:/mallet-2.0.7" or similarly close
  to C:/ on a Windows computer.}

  \item{JAVA}{the directory containing java.exe. It will
  probably be something like "C:/Program
  Files/Java/jre7/bin" but you should check}

  \item{K}{the number of topics that the model should
  contain. Can also be a vector of numbers of topics, then
  a model will be generated for each number. Useful for
  comparing diagnostics of different models, but may be
  time consuming.}
}
\value{
Returns a folder of text files (one text file per document)
and a folder of output files from MALLET. The folders are
named with a date-time stamp so they wont be overwritten by
repeated runs.
}
\description{
Generates one or more topic models using MALLET and plots
diagnostics. This is a very basic R wrapper for MALLET on
Windows, see here for something more complete:
http://cran.r-project.org/web/packages/mallet/ For use with
JSTOR's Data for Research datasets (http://dfr.jstor.org/).
}
\examples{
## JSTOR_MALLET(nouns, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = 150) # generate a single model
## JSTOR_MALLET(nouns =  unpack1grams$wordcounts, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = seq(150, 500, 50)) # can also generate multiple models with different numbers of topics
}

