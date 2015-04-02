context("one word against another word over time")

test_that("2words gives a plot", {
  
  # load data
  data(unpack1grams)
  
  # run function
  JSTOR_2words_test <- JSTOR_2words(unpack1grams, "gender", "feminist")
  
  # test to see if we get a plot object class
  expect_is(JSTOR_2words_test$plot, c("gg", "ggplot"))
  
})