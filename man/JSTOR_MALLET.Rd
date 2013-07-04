\name{JSTOR_MALLET}
\alias{JSTOR_MALLET}
\title{Generate one or more topic models using MALLET's implementation of Latent Dirichlet allocation (LDA)}
\usage{
  JSTOR_MALLET(corpus, MALLET = "C:/mallet-2.0.7",
    JAVA = "C:/Program Files (x86)/Java/jre7/bin", K)
}
\arguments{
  \item{corpus}{the object returned by the function
  JSTOR_corpusofnouns. A corpus containing the documents.}

  \item{MALLET}{the directory containing MALLET's bin
  directory, ideally "C:/mallet-2.0.7" or similarly close
  to C:/ on a Windows computer.}

  \item{JAVA}{the directory containing java.exe. To find
  this directory, type at the R prompt:
  Sys.getenv("JAVA_HOME")}

  \item{K}{the number of topics that the model should
  contain. Can also be a vector of numbers of topics, then
  a model will be generated for each number. Useful for
  comparing diagnostics of different models.}
}
\value{
  Returns plots of diagnostics if more than one number of
  topics is specified. Output files from MALLET can be
  found in the working directory.
}
\description{
  Generates one or more topic models using MALLET and plots
  diagnostics. This is a very basic R wrapper for MALLET on
  Windows, see here for something better:
  http://www.cs.princeton.edu/~mimno/R/ For use with
  JSTOR's Data for Research datasets
  (http://dfr.jstor.org/).
}
\examples{
## JSTOR_MALLET(corpus = corpus, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = 150) # generate a single model
## JSTOR_MALLET(corpus = corpus, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = seq(150, 500, 50)) # can also generate multiple models with different numbers of topics
}

