context("subset 1grams works")

test_that("subset 1grams works", {
  
  # load data
  data(unpack1grams)
  
  # run function
  JSTOR_subset1grams_test <- JSTOR_subset1grams(unpack1grams, "gender")
  
  words <- JSTOR_subset1grams_test$wordcounts
  docs <- words[, words$dimnames$Terms == 'gender']
  

  # test to see if we get a plot object class
  expect_that(as.numeric(nrow(docs)), equals(102))
  
})