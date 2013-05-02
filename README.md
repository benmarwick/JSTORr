JSTORr
======

Simple exploratory text mining of journal articles from JSTOR's Data for Research service.

Objective
----
The aim of this package is provide some simple functions in `R` to explore changes in word frequencies over time in a specific journal archive. Currently there are functions to explore changes in:
- a single word (ie. plot the relative frequency of a 1-gram over time)
- two words independantly (ie. plot the relative frequency of two 1-grams over time)
- sets of words (ie. plot the relative frequency of a single group of mulitple 1-grams over time)
- correlations between two words over time (ie. plot the correlation of two 1-grams over time)
- correlations between two sets of words over time (ie. plot the correlation two sets of multiple 1-grams over time)
- all of the above with bigrams (a sequence of two words)
- topic models with the `lda` package for full `R` solution or the MALLET Java-based program (if installing that is an option)

How to install
----
First, make sure you've got Hadley Wickham's excellent devtools package installed. If you haven't got it, you can get it with these lines in your R console:

```
install.packages(pkgs = "devtools", dependencies = TRUE)
```
Then, use the `install_github()` function to fetch this package from github:

```
library(devtools)
install_github(repo = "JSTORr", username = "UW-ARCHY-textual-macroanalysis-lab")
```
How to get started
----
First, go to JSTOR's [Data for Research service][dfr] and make a request for data. The DfR service makes available large numbers of journal articles in a format that is convenient for text mining. When making a request for data to use with this package, you **must** chose:
- `CSV` as the 'output format', not `XML`, which is the default
- `Word Counts` **and** `bigrams` as the 'Data Type'

Second, once you've downloaded the zip file that is the 'full dataset' from DfR then you can start `R`, install this package and run this function: 

```
unpack <- JSTOR_unpack() # takes no arguments, but watch for prompts to enter details
```
Third, have fun exploring the other functions in the package!

Typical workflow
----
Here's one way to make use of this package:

First, go to [Data for Research service][dfr] and request data as specified above and download the zip file when it's available (it can take a few hours to days for DfR to prepare your archive). No need to unzip, that's done by the package.

Second, start `R` and run something like `unpack <- JSTOR_unpack()` and paste in the directory and zip file name when prompted. Then you'll get a data object `unpack`, containing 1-grams, 2-grams and bibliographic data.

Third, explore some visualisations of key words over time with `JSTOR_1word`, `JSTOR_2words`, `JSTOR_1bigram`, `JSTOR_2bigrams`, and correlations of words over time with `JSTOR_2wordcor` and `JSTOR_2bigramscor`

Fourth, put the documents into a corpus with `JSTOR_corpusofnouns` and explore further with more complex text analysis methods. The corpus can be changed to a Document Term Matrix using the `tm` package which has many advanced text mining methods. 

Fifth, generate topic models with `JSTOR_lda` (using the `lda` package, it's a lot faster than `topicmodels`) and `JSTOR_MALLET`. The latter function requires MALLET to be installed on your computer. See more about MALLET here http://mallet.cs.umass.edu/topics.php and http://programminghistorian.org/lessons/topic-modeling-and-mallet 

Sixth, visualise and explore the output from the topic models.


Limitations and Disclaimer
----
Currently this package is intended for the exploration of a single journal archive. For example, all of the articles held by JSTOR of one journal or on one subject. It may be useful for other types of DfR archives, but has only been tested on single-journal archives. Also, I am not a programmer, computer scientist, statistician, lawyer, etc. This is a work in progress and there is currently very little custom error handling. Use at your own risk, and fork and share as you like. 

Acknowledgements
----
Many of the ideas for these functions have come directly from the prolific and creative research of Andrew Goldstone, Jonathan Goodwin, Shawn Graham, Matt Jockers, David Mimno, Ben Schmidt and Ted Underwood. 
  
  
  [dfr]:http://dfr.jstor.org/
  