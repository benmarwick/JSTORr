context("subset 2grams works")

# test_that("subset 2grams works", {
#   
#   # load data
#   data(unpack2grams)
#   
#   # run function
#   JSTOR_subset2grams_test <- JSTOR_subset2grams(unpack2grams, "we cannot")
#   
#   words <- JSTOR_subset2grams_test$wordcounts
#   docs <- words[, words$dimnames$Terms == 'feminist theory']
#   
# 
#   # test to see if we get a plot object class
#   expect_that(as.numeric(nrow(docs)), equals(???))
#   
# })