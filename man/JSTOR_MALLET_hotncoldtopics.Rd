\name{JSTOR_MALLET_hotncoldtopics}
\alias{JSTOR_MALLET_hotncoldtopics}
\title{Plot the top five hot and cold topics in the archive}
\usage{
JSTOR_MALLET_hotncoldtopics(x, pval = 0.05, ma = 5)
}
\arguments{
  \item{x}{the object returned by the function
  JSTOR_unpack1grams (for the bibliographic data only).}

  \item{pval}{p-value of the correlation cutoff for topics
  to include in the top 5 negative/positive list (ie. only
  topics where p<0.01 or 0.001?). Default is 0.05.}

  \item{ma}{ma moving average interval, default is five
  years.}
}
\value{
Returns a plot of the hot topics and plot of the cold
topics and a list of dataframes of the topic proportions
per year. Years as rows, topics as columns and posterior
probabilities as cell values.
}
\description{
Generates plots and data frames of the top five hot and
cold topics. Hot topics are topics with a positive
correlation to year of publication, cold topics have a
negative correlation. For use with JSTOR's Data for
Research datasets (http://dfr.jstor.org/).
}
\examples{
## hotncold <- JSTOR_MALLET_hotncoldtopics(x = unpack1grams)
}

