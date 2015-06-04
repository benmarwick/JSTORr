
library(JSTORr)

# start with zip file and unzip
test_data <- list.files(pattern = "zip")
unzip(test_data)

unpack2grams_o <- JSTOR_unpack2grams(path = "C:\\Users\\marwick\\Downloads\\2015.4.24.kqSmMTcV_American_Antiquity_bigrams/")

papers_2000_2005 <- unpack2grams_o$bibliodata[ unpack2grams_o$bibliodata$year %in% 2000:2005, ]$x

papers_2000_2005  <- unpack2grams_o$bigrams[ unpack2grams_o$bigrams$dimnames$Docs %in%  papers_2000_2005, ]

papers_2000_2005  <- unpack2grams_o$bigrams[ , unpack2grams_o$bigrams$dimnames$Terms[1:500] ]

unpack2grams <- list("bigrams" = papers_2000_2005, "bibliodata" = unpack2grams_o$bibliodata[ unpack2grams_o$bibliodata$year %in% 2000:2005, ])

save(unpack2grams, file = "../data/unpack2grams.rdata", compress = "bzip2")

unlink(list.files(pattern = "tsv$|txt$", full.names = TRUE))
unlink("bigrams", recursive = TRUE)

# check compression
# tools::checkRdaFiles("../data/unpack1grams.rdata")

# JSTOR_1word(unpack1grams, "horseshoe")
# should be 4 items: 1998, 2000, 2003, 2004

# if working with an older DFR archive that had a CSV file instead of a TSV,
# here's how to convert...
# citations <- read.csv("citations.CSV", row.names = NULL)
# citations <- data.frame(apply(citations, 2, function(i) gsub("\t", "", i)))
# write.table(citations, 'citations.tsv',  quote=FALSE, sep='\t')
# unlink("citations.CSV") # delete CSV
