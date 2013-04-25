JSTORr
======

Simple text mining of journal articles from JSTOR's Data for Research service.

Objective
----
The aim of this package is provide some functions to explore changes in word frequencies over time in a specific journal archive. 

How to install
----
First, make sure you've got Hadley Wickham's excellent devtools package installed. If you haven't got it, you can get it with these lines in your R console:

```{r}
install.packages(pkgs="devtools", dependencies=TRUE)
```
Then, use the `install_github()` function to fetch this package from github:

```{r}
library(devtools)
install_github("JSTORr", "benmarwick")
```
How to get started
----
First, go to JSTOR's [Data for Research service][dfr] and make a request for data. The DfR service makes available large numbers of journal articles in a format that is convenient for text mining.  

Second, once you've downloaded the zip file that is the 'full dataset' then start `R`, install this package and run this function: 

```{r}
JSTOR_unpack
```
And then you're ready to go with any of the other fuctions.

Limitations
----
Currently this package is for the exploration of a single journal archive. For example, all of the articles held by JSTOR of one journal. 
  
  
  [dfr]:http://dfr.jstor.org/
  