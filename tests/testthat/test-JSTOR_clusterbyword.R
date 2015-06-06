context("one word against another word over time")

test_that("2words gives a plot", {
  
  # load data
  library(JSTORr)
  data(unpack1grams)
  
  # run function
  nouns <- JSTOR_dtmofnouns(unpack1grams, word = "gender")
  JSTOR_clusterbywords_test <- JSTOR_clusterbywords(nouns, "gender")
  
  # get the classes of plots from the test result
  tested_classes <- c(
  dendrogram <- class(JSTOR_clusterbywords_test$cl_plot),
  gg_ggplot1 <- class(JSTOR_clusterbywords_test$ggden$plot),
  gg_ggplot2 <- class(JSTOR_clusterbywords_test$p),
  gg_ggplot3 <- class(JSTOR_clusterbywords_test$pv),
  grob <- class(JSTOR_clusterbywords_test$arranged),
  gg_ggplot4 <- class(JSTOR_clusterbywords_test$q)
  )
  
  # hardcoded classes that I identified when writing the test
  expected_classes <- c("dendrogram", "gg" ,   "ggplot" ,    "gg" ,        "ggplot" ,   
  "gg"   ,      "ggplot",     "arrange",    "ggplot" ,    "gTree"  ,   
  "grob" ,      "gDesc" ,     "gg"   ,      "ggplot")
  
  
  # test to see if we get a plot object class
  expect_equal(tested_classes, expected_classes)
  
})