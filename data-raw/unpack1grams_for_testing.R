# This script prepares the unpack1grams.RData object from the ZIP archive. It removes non-ASCII characters so that the pkg build doesn't give any warnings or notes about that. The unpack1grams.RData object is needed for the pkg tests. This script is run manually. 


library(JSTORr)

# start with zip file and unzip
proj_wd <- getwd()
# find zip file
test_data <- normalizePath(list.files(recursive = TRUE, 
                        pattern = "zip",
                        full.names = TRUE))
test_data_file <- gsub(".+/", "", list.files(recursive = TRUE, 
                        pattern = "zip"))
test_data_dir <-  gsub(".zip", "", normalizePath(test_data))
# unzip into data-raw
unzip(test_data, 
      exdir = paste0(proj_wd, 
                     "/data-raw/",
                     gsub(".zip", "", test_data_file)))

# test it to see that it works as-is, it should 
setwd(test_data_dir)
unpack1grams_pretest <- JSTOR_unpack1grams()
setwd(proj_wd)
rm(unpack1grams_pretest)

# clean out non-ascii characters from test data

## quick look for non-ascii characters in CSV files in the pkg:
 files_with_non_ascii <- function() {
   the_bad_files <-  sapply(list.files(paste0(test_data_dir,  
                           "/wordcounts"),
                     recursive = T,
                     full.names = T,
                     include.dirs = T),
          function(i) tools::showNonASCIIfile(i))
   return(the_bad_files)
 }

 # run it to see which files have non-ASCII characters
 the_files_with_non_ascii <- files_with_non_ascii()
   

## replace non-ascii chars
 for(i in seq_along(the_files_with_non_ascii)){

   tmp <- read.csv(names(the_files_with_non_ascii)[i])
   tmp$WORDCOUNTS <-  iconv(tmp$WORDCOUNTS, to = "ASCII//TRANSLIT")
   tmp <- data.frame(WORDCOUNTS = tmp$WORDCOUNTS,
                     WEIGHT = tmp$WEIGHT)
   write.csv(tmp,
             row.names = FALSE,
             names(the_files_with_non_ascii)[i] )
 }
 # check files again with the quick look function above to ensure
 # that the conversion has been successful, should get character(0)
 files_with_non_ascii()


## and do the same for the citations.tsv file:

cits <- read.table(paste0(test_data_dir, 
                          "/citations.tsv"),
                    row.names = NULL,
                    comment.char = "",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    colClasses="character",
                    quote = "",
                    sep = "\t",
                   check.names = FALSE)

# check where offending characters are located
tools::showNonASCIIfile(paste0(test_data_dir,  "/citations.tsv"))
                    

cits1 <- cits
 for(i in seq_len(ncol(cits))){
   tmp1 <- cits[, i]
   for(j in seq_along(tmp1)){
     Encoding(tmp1[j]) <- "latin1"
     cits1[j, i] <- iconv(tmp1[j], to = "ASCII//TRANSLIT")
     # check to see the progress
     print(paste0("col: ", i, ", row: ", j))
   }
 }
 

write.table(cits1, 
            paste0(test_data_dir, 
                   "/citations.tsv"),
            row.names = FALSE,
            quote = FALSE,
            col.names = TRUE,
            sep = "\t")

# have we solved the problem?
tools::showNonASCIIfile(paste0(test_data_dir,
                               "/citations.tsv"))


# then zip it back up and ensure it's in data-raw
unlink(test_data) # delete zip file so we can remake it
zip(zipfile = paste0("./data-raw/", test_data_file), 
    files = paste0("./data-raw/", gsub(".zip", "", test_data_file)))

# unpack from unzipped and cleaned files to create test data objsect

setwd(test_data_dir)
unpack1grams <- JSTOR_unpack1grams()
setwd(proj_wd)

# create RData object
save(unpack1grams, file = "data/unpack1grams.rdata", compress = "bzip2")

# delete unzipped folder and contents
unlink(test_data_dir, recursive = TRUE, force = TRUE)
setwd(proj_wd)



