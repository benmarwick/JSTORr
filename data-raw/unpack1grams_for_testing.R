
library(JSTORr)

# start with zip file and unzip
test_data <- list.files(pattern = "zip")
unzip(test_data)

unpack1grams <- JSTOR_unpack1grams()

save(unpack1grams, file = "../data/unpack1grams.rdata", compress = "bzip2")

unlink(list.files(pattern = "tsv$|txt$", full.names = TRUE))
unlink("wordcounts", recursive = TRUE)

# check compression
# tools::checkRdaFiles("../data/unpack1grams.rdata")

# JSTOR_1word(unpack1grams, "horseshoe")
# should be 4 items: 1998, 2000, 2003, 2004

# if working with an older DFR archive that had a CSV file instead of a TSV,
# here's how to convert...
# citations <- read.csv("citations.CSV")
# citations <- data.frame(apply(citations, 2, function(i) gsub("\t", " ", i)))
# write.table(citations, 'citations.tsv',  quote=FALSE, sep='\t')
