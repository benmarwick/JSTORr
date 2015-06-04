context("one word against another word over time")

test_that("cluster function removes stopwords", {
  
  library(JSTORr)
  
  # load data
  data(unpack1grams)
  
  # run get nouns
  nouns <- JSTOR_dtmofnouns(unpack1grams, word = "gender")
  
  # set custom stopwords
  custom_stopwords <- c('archaeology', 'university', 'research', 'evidence', 'journal', 'world', 'site', 'cambridge', 'archaeol', 'area', 'region') 
  
  # do cluster analysis
  cluster_result <- JSTOR_clusterbywords(nouns, "gender", custom_stopwords, f = 0.005)
  
  # look for stopwords in clusters
  stop_in_cluster <- lapply(cluster_result$kmeans, function(i) i %in% custom_stopwords)
  vec <- unname(unlist(stop_in_cluster))
  
  # test to see if the stopword were removed
  expect_identical(length(vec == FALSE), length(vec))
  
})