JSTORr  [![Build Status](https://travis-ci.org/benmarwick/JSTORr.svg?branch=master)](https://travis-ci.org/benmarwick/JSTORr)
======

Simple exploratory text mining and document clustering of journal articles from JSTOR's Data for Research service.

Objective
----
The aim of this package is provide some simple functions in `R` to explore changes in word frequencies over time in a specific journal archive. It is designed to solve the problem of finding patterns and trends in the unstructured text content of a large number of scholarly journals articles from the JSTOR archive.

Currently there are functions to explore changes in:
- a single word (ie. plot the relative frequency of a 1-gram over time)
- two words independantly (ie. plot the relative frequency of two 1-grams over time)
- sets of words (ie. plot the relative frequency of a single group of mulitple 1-grams over time)
- correlations between two words over time (ie. plot the correlation of two 1-grams over time)
- correlations between two sets of words over time (ie. plot the correlation two sets of multiple 1-grams over time)
- all of the above with bigrams (a sequence of two words)
- the most frequent words by n-year ranges of documents (ie. top words in all documents published in 2-5-10 year ranges, whatever you like)
- the top n words correlated a word by n-year ranges of documents (ie. the top 20 words associated with the word 'pirate' in 5 year ranges)
- various methods (k-means, PCA, affinity propagation) to detect clusters in a set of documents containing a word or set of words
- topic models with the `lda` package for full `R` solution or the Java-based MALLET program (if installing that is an option, currently implemented here for Windows only) 

This package will be useful to researchers who want to explore the history of ideas in an academic field, and investigate changes in word and phrase use over time, and between different journals. 

How to install
----
First, make sure you've got Hadley Wickham's excellent devtools package installed. If you haven't got it, you can get it with these lines in your R console:

```
install.packages(pkgs = "devtools", dependencies = TRUE)
```
Then, use the `install_github()` function to fetch this package from github:

```
library(devtools)
# download and install the package (do this only once ever per computer)
install_github("benmarwick/JSTORr")
```
Error messages relating to rJava on Windows can probably be fixed by following exactly the instructions [here][SOrJava]. On OSX, try `R CMD javareconf` at the command line, then R `install.packages("rJava",type='source')`.

----
First, go to JSTOR's [Data for Research service][dfr] and make a request for data. The DfR service makes available large numbers of journal articles in a format that is convenient for text mining. When making a request for data to use with this package, you **must** chose:
- `CSV` as the 'output format', not `XML`, which is the default
- `Word Counts` **and** `bigrams` as the 'Data Type'

Second, once you've downloaded and unzipped the zip file that is the 'full dataset' from DfR then you can start `R` (it's highly recommended to use [RStudio][rstudio] when working with this package, much easier to manage the plot output) and work through the steps in the next section.

Typical workflow
----
Here's how to make use of this package:

First, go to [Data for Research service][dfr] and request data as specified above and download the zip file when it's available (it can take a few hours to days for DfR to prepare your archive). Unzip the file and make a note of its location on your computer (in R, you can unzip like this: `unzip("2013.6.4.usytW8LZ.zip")` with your zip file name in between the quote marks). If you can't, or don't want to, get data from dfr, there is a small dataset included with the package, you can access it with `JSTORr::data(unpack1grams)`, and then skip down to exploring some visualisations.

Second, start [RStudio][rstudio] and run: 

```
# load the package to your current R session (do this every time you start R and use the functions)
library(JSTORr)
# change the path to where you unzipped your file on your computer
unpack1grams <- JSTOR_unpack1grams(path = "C:/Users/marwick/Downloads/JSTOR")
```
but change the path value to suit your system and watch the console progress bars. Then you'll get a data object `unpack1grams`, containing a document term matrix of 1-grams and a data frame of bibliographic data.

Third, now you're ready to explore some visualisations of key words over time with `JSTOR_1word`, `JSTOR_2words`, and correlations of words over time with `JSTOR_2wordcor`, for example:

```
# one word over time
JSTOR_1word(unpack1grams, "pirate")
# two words over time
JSTOR_2words(unpack1grams, "pirate", "navy")
# correlation of words over time
JSTOR_2wordcor(unpack1grams, "pirate", "navy")
```

Fourth, use `JSTOR_dtmofnouns` to create a document term matrix of nouns only, then investigate the most frequent words over time with `JSTOR_freqwords` and analyse correlations over time of all words with a word of interest with `JSTOR_findassocs`.  For example,

```
# subset the words to get nouns only
nouns <-  JSTOR_dtmofnouns(unpack1grams, sparse = 0.75)
# plot and tabulate the most frequent words over time
JSTOR_freqwords(unpack1grams, nouns)
# plot and tabulate words correlated with 'pirate' over time
JSTOR_findassocs(unpack1grams, nouns, "pirate")
```

To optimise the output from these functions you must add words to the stopword list and then repeat these functions a few times over until the results look more relevant. Typically you'll need to add words like 'journal', 'press', 'research' and so on to your stopword list. You can find the location of the stopword list on your computer by running this line `paste0(.libPaths()[1], "/tm/stopwords/english.dat")` Then you can just edit the stopword list in RStudio or any text editor. Be careful not to have any errant spaces after a word on the list.

Fifth, explore document clusters using `JSTOR_clusterbywords` to calculate Affinity Propagation Clustering, K-means Clustering and Principal Components Analysis on a document term matrix. For example, 

```
# plot and tabulate clusters of documents that contain the word 'pirate'
JSTOR_clusterbywords(nouns, 'pirate')
```

Sixth, generate topic models with `JSTOR_lda` (using the `lda` package, it's a lot faster than `topicmodels`). Expore the model output with `JSTOR_lda_docdists` and `JSTOR_lda_topicdists`. Identify hot and cold topics in the corpus with `JSTOR_lda_hotncoldtopics`. Remember that editing the stopwords list may help to make the topics more distinctive. 

```
# generate topic model with 50 topics (an arbitrary choice)
my_model <- JSTOR_lda(unpack1grams, nouns, 50)
# visualise document distances by topics
JSTOR_lda_docdists(my_model)
# plot and tabulate hot and cold topics
JSTOR_lda_hotncoldtopics(my_model)
```

Or if you have MALLET installed, you can run `JSTOR_MALLET` to generate topic models using MALLET. Explore topic models with `JSTOR_MALLET_hotncoldtopics`, `JSTOR_MALLET_topicsovertime` and `JSTOR_MALLET_topicinfo`. See more about MALLET here http://mallet.cs.umass.edu/topics.php and http://programminghistorian.org/lessons/topic-modeling-and-mallet 

Limitations and Disclaimer
----
Currently this package is intended for the exploration of a single journal archive. For example, all of the articles held by JSTOR of one journal or on one subject. It may be useful for other types of DfR archives, but has not yet been widely tested. Also, I am not a programmer, computer scientist, linguist, statistician, lawyer, etc. This software is provided as-is, in the hope that it may be useful, but without any warranty or support, whatsoever, see the [LICENSE](LICENSE) for more details. This is a work in progress and there is currently very little custom error handling (the more cryptic errors are usually due to a search for a word or bigram that does not exist in the archive). Use at your own risk, and fork and share as you like. Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Acknowledgements
----
Many of the best ideas for these functions have come directly from the prolific and creative research and coding of Andrew Goldstone, Jonathan Goodwin, Shawn Graham, Matt Jockers, David Mimno, Ben Schmidt and Ted Underwood. None of them are responsible for the consequences of use of this package, no matter how awful, even if they arise from flaws in it (of course I take full responsibility for the flaws). Magdalena Balazinska provided access to computing resources for development and testing, for which I'm most grateful. Thanks to Ian Kretzler, Jiun-Yu Liu and Joss Whittaker for intensive testing and many useful suggestions.
  
  [dfr]:http://dfr.jstor.org/
  [SOrJava]:http://stackoverflow.com/a/7604469/1036500
  [rstudio]:http://www.rstudio.com/ide/download/
  
