  ---
  title: 'JSTORr: Simple exploratory text mining and document clustering of journal articles from JSTOR's Data for Research service.
'
  tags:
    - textmining
    - topicmodels
    - R
  authors:
   - name: Ben Marwick
     orcid: 0000-0001-7879-4531
     affiliation: 1
  affiliations:
   - name: University of Washington
     index: 1
  date: 14 December 2016
  bibliography: 
  ---

  # Summary

The aim of this package is provide some simple functions in `R` to explore changes in word frequencies over time in collections of scholarly journal articles. The package is designed specifically to work with journal articles in the [JSTOR](https://www.jstor.org/) archive. Currently there are functions to explore changes in a single word (ie. plot the relative frequency of a 1-gram over time), two words independently (ie. plot the relative frequency of two 1-grams over time), sets of words (ie. plot the relative frequency of a single group of multiple 1-grams over time), correlations between two words over time (ie. plot the correlation of two 1-grams over time), correlations between two sets of words over time (ie. plot the correlation two sets of multiple 1-grams over time), all of the above with bigrams (a sequence of two words), the most frequent words by n-year ranges of documents (ie. top words in all documents published in 2-5-10 year ranges, whatever you like), the top n words correlated a word by n-year ranges of documents (ie. the top 20 words associated with the word 'pirate' in 5 year ranges), various methods (k-means, PCA, affinity propagation) to detect clusters in a set of documents containing a word or set of words, topic models with the `lda` package for full `R` solution or the Java-based MALLET program (if installing that is an option, currently implemented here for Windows only). The package is archived at <https://dx.doi.org/10.6084/m9.figshare.1029396>

  # References
  
Kretzler, Ian, and Ben Marwick. 2015 Investigating Archaeologists’ Engagement Feminist Theory Using Textual Macroanalysis: 25 Years after Chacmool 1989. Proceedings of the 47th Annual Chacmool Archaeological Conference. Chacmool Archaeological Association of the University of Calgary.
  