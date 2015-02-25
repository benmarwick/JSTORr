context("1 word over time")

test_that("1 word is in the right spot", {
  
  data(unpack1grams)
  
  # quality control: when does 'x' first appear? 
  x <- "gender"
  year_ <- "1993" # first year that 'gender' appears in JAR, that we know from searching JSTOR directly
  
  biblio <- unpack1grams$bibliodata
  
  # to limit by year
  library(dplyr)
  y_ <- biblio %>%
    filter(year == year_)
  
  # to incldue all years
  # y_ <- biblio 
  
  doi_ <- as.character(y_$x)
  
  
  # subset the dtm to get only articls from year
  articles_ <- unpack1grams$wordcounts[doi_, , ]
  # inspect(articles_[1:5,1:5,])
  
  # colSums(as.matrix(articles_[, articles_$dimnames$Terms == x,]))
  
  # inspect(articles_[, articles_$dimnames$Terms == x,])
  
  # did we really get those same DOIs?
  # identical(articles_$dimnames$Docs, doi_)
  
  # find the ones that contain 'x'
  x_ <- as.data.frame(as.matrix(articles_[,articles_$dimnames$Terms == x, ]))
  x_$doi <- rownames(x_)
  x_doi <- x_[ x_[,1] > 0, ]$doi
  
  # find full bilbiodata on those 
  biblio_ <- biblio[biblio$x %in% x_doi , ]
  
  # sort by year
  biblio_  <- biblio_  %>%
    arrange(year)
  
  
  expect_equal(nrow(biblio_), 6)
  
  gender_ <- JSTOR_1word(unpack1grams, "gender")
  
  expect_equal(nrow(gender_$word_by_year), 178)
  
})