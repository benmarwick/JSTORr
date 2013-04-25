JSTORr
======

Simple text mining of journal articles from JSTOR's Data for Research service.

How to install
======
First, make sure you've got Hadley Wickham's excellent devtools package installed. If you haven't got it, you can get it with these lines in your R console:

```{r}
install.packages(pkgs="devtools", dependencies=TRUE)
```
Then, use the `install_github()` function to fetch this package from github:

```{r}
library(devtools)
install_github("JSTORr", "benmarwick")
```