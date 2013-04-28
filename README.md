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
JSTOR_unpack
```
Third, have fun exploring the other functions in the package!

Limitations
----
Currently this package is intended for the exploration of a single journal archive. For example, all of the articles held by JSTOR of one journal. It may be useful for other types of DfR archives, but has only been tested on single-journal archives. Also, I am not a programmer, computer scientist, statistician, lawyer, etc. This is a work in progress, use and share as you like, at your own risk. 
  
  
  [dfr]:http://dfr.jstor.org/
  