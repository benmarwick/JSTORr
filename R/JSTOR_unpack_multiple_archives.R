#' Unpacks multiple zip files of JSTOR journal articles and bibliographic data to a Document Term Matrix of 1-grams
#' 
#' @description Import journal articles and bibliographic data from multiple downloaded zipfiles, and reshape ready for simple text mining. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/). 
#' @param mydir path to directory containing multiple zip files dowloaded from dfr.jstor.org (default is the current working directory)
#' @return Returns a list of two items. First is "wordcounts", a Document Term Matrix of 1-grams, and second is 'bibliodata', a data frame of bibliographic information for all articles. 
#' @export
#' @examples 
#' ## multiple_archives <- JSTOR_unpack_multiple_archives(mydir = "~/my_data")
#' @import data.table plyr tm stringr slam



JSTOR_unpack_multiple_archives <- function(mydir = getwd()){

### combine multiple JSTOR DFR archives

# assume we have a folder of several zip files that are
# JSTOR DFR archives

# get file names

zips <- list.files(mydir, full.names = TRUE, pattern = "zip")
dirnames <- gsub(".zip", "", basename(zips))

# loop over the zip files and unzip into their own folder
# (otherwise they'll overwrite each other)
for(i in 1:length(zips)){
  unzip(zips[i], exdir = paste0(mydir, "/", dirnames[i]))
}

# loop over all the 'wordcounts' csv files and combine into
# one mega data table
unzips <- list.files(mydir, full.names = TRUE, recursive = TRUE)
all_wordcounts <- unzips[grep("wordcounts", unzips)]

# library(data.table)
# all_wordcount_csvs_list <- lapply(all_wordcounts,  fread)
# # add DOI
# DOIs <- substr(all_wordcounts, (nchar(all_wordcounts)-19), (nchar(all_wordcounts)-4))
# names(all_wordcount_csvs_list) <- DOIs
# # make into one giant table
# all_wordcount_csvs_dt <- rbindlist(all_wordcount_csvs_list)
# all_wordcount_csvs_dt[, "names" := rep(DOIs, sapply(all_wordcount_csvs_list, function(x) length(x[[1]]), USE.NAMES=FALSE))]
# 
# loop over citation files...
all_citations <- unzips[grep("citation", unzips)]

# since citation files can be TSV (JSTOR changed to this in 2015) or csv...
all_citations_csvs <- lapply(all_citations,  function(i) {if (stringr::str_sub(i, start=-3) == "CSV" | stringr::str_sub(i, start=-3) == "csv") 
                                                            {read.csv(i, quote="", row.names=NULL, comment.char = "", header = TRUE,  stringsAsFactors = FALSE, colClasses="character")}
                                                          else 
                                                            {if (stringr::str_sub(i, start=-3) == "TSV" | stringr::str_sub(i, start=-3) == "tsv") 
                                                            {read.delim(i, row.names = NULL, comment.char = "", header = TRUE, stringsAsFactors = FALSE, colClasses="character", quote = "")}
                                                              else
                                                              {"Citations files cannot be loaded"}  } } )

all_citations_csvs <- rbindlist(all_citations_csvs, fill = TRUE)

###############

read_csv2dt <- function(x) data.table(fread(x, sep = ",", stringsAsFactors=FALSE))
aawc <-  llply(all_wordcounts, read_csv2dt, .progress = "text", .inform = FALSE)
# give the DOIs
names(aawc) <- all_wordcounts
# Identify empty CSV files and exclude them
lens <- sapply(aawc, function(i) i[1]$WEIGHT + i[2]$WEIGHT + i[3]$WEIGHT)
full <- unname(!is.na(lens))
# Subset only CSV files with at least three words...
aawc1 <- aawc[full]

# custom version of tm::DocumentTermMatrix for 1-grams

my_dtm_1gram <- function(x){ 
  y <- as.integer(x$WEIGHT)
  names(y) <- x$WORDCOUNTS
  v =  unname(y)          # num
  i = rep(1, length(y))   # int
  j = seq(1:length(y))    # int
  z <- simple_triplet_matrix(v = v,   # num
                             i = i,        # int
                             j = j,        # int
                             nrow = max(i),
                             ncol = max(j), 
                             dimnames =                          
                               list(                              
                                 Docs = deparse(substitute(x)),
                                 Terms = names(y)))
  zz <- as.DocumentTermMatrix(z, weighting = weightTf)
  return(zz)
}
# use
aawc2 <- llply(1:length(aawc1), function(i) my_dtm_1gram(aawc1[[i]]), .progress = "text", .inform = FALSE)


# assign file names to each dataframe in the list
myfiles1 <- all_wordcounts[full]
names(aawc2) <- str_extract(basename(myfiles1), "[^wordcounts_].+[^.CSV]")

#Now work with biblio data
# replace for-slash with underscore to make it match the filenames
# and replace odd \t that was added during import 

all_citations_csvs$id <- str_extract(chartr('/', '_', all_citations_csvs$id), ".*[^\t]")
all_citations_csvs$publisher <- str_extract(chartr('/', '_', all_citations_csvs$publisher), ".*[^\t]")

# limit list of citations to full length articles only 
# note that citation type is not in the correct column
# and that we need \t in there also
# changed this in case we get a dataset that was not originally all fla
all_citations_fla <- unique(all_citations_csvs[all_citations_csvs$publisher == 'fla',])
# subset from the wordcount data only the full length articles

# subset items in the list of wordcount data whose names are in 
# the list of fla citation IDs
wordcounts_fla <- aawc2[which(names(aawc2) %in%  all_citations_fla$id)]

# drop citations that we dont' have text for
all_citations_fla <- all_citations_fla[which(all_citations_fla$id %in% names(wordcounts_fla))]

# put citation IDs in the same order with wordcount data names
# which is the same order as myfiles
bibliodata <- data.table(merge(names(wordcounts_fla), all_citations_fla, by.x=1, by.y="id"))
# create a variable that holds the year of publication for
# each article
bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]{4}")

# remove duplicate articles (where the same DOI occurs more than once)

# clean up and next step is very memory
keep <- c("bibliodata", "wordcounts_fla")
rm(list=ls()[! ls() %in% keep])

# combine into one giant dtm, rather slow

wordcounts_combined <- do.call(c, wordcounts_fla)

# give docs their DOI as names (this was wrong! should be good now)
# wordcounts_combined and wordcounts_fla are in the same order
# but bibliodata is not
wordcounts_combined$dimnames$Docs <- names(wordcounts_fla)

# delete large object that's no longer needed
rm(wordcounts_fla)

# somehow docs got to be a factor... fix this
# and why do I have to subset the Docs like this?
# something to do with how the DTM is made? I bet
# the CSV file is dodgy... 
wordcounts <- wordcounts_combined[unique(as.character(wordcounts_combined$dimnames$Docs[1:nrow(wordcounts_combined)])), ]
rm(wordcounts_combined)

# put bibliodata in the same order as wordcounts, cf http://stackoverflow.com/a/11977256/1036500
bibliodata <- bibliodata[match(wordcounts$dimnames$Docs, bibliodata$x),]

message("removing stopwords...")
wordcounts <- wordcounts[, !(wordcounts$dimnames$Terms %in% stopwords(kind = "en")) ]
message("done")

message("discarding words with <3 characters (probably OCR errors)...")
wordcounts <- wordcounts[,nchar(wordcounts$dimnames$Terms) > 3]
message("done")

message("discarding words with >2 consecutive characters (probably OCR errors)...")
wordcounts <- wordcounts[,!grepl("(.)\\1{2,}", wordcounts$dimnames$Terms)]
message("done")

message("discarding non-ASCII characters...")
wordcounts <- wordcounts[,(wordcounts$dimnames$Terms %in% iconv(wordcounts$dimnames$Terms, "latin1", "ASCII", sub=""))]
message("done")

unpack_multi <- list("wordcounts" = wordcounts, "bibliodata" = bibliodata)
return(list(wordcounts = wordcounts, bibliodata = bibliodata))
}
