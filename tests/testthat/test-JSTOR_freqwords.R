context("freqwords over time")

test_that("freqwords gives a plot and a table", {
  
  # load data
  library(JSTORr)
  data(unpack1grams)
  
  # run function
  nouns <- JSTOR_dtmofnouns(unpack1grams, word = "gender")
  JSTOR_freqwords_test <- JSTOR_freqwords(unpack1grams, nouns, "gender")
  
  # get the classes of plots from the test result
  plot_class <- class(JSTOR_freqwords_test$plot$plot)

  # test to see if we get a plot object class
  expect_equal(plot_class, c("gg", "ggplot"))
  
})