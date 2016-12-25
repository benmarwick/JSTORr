#' @importFrom  grDevices    dev.off    png  
#' @importFrom  stats    aggregate    cor    cor.test    dist    filter  hclust    kmeans    na.omit    pt    sd  
#' @importFrom  utils    head    read.csv    read.delim    read.table  tail    unzip    write.table
globalVariables(c( 
                   "cbg1",
                   "cbg2",
                   "cluster",
                   "corr", 
                   "doc", 
                   "im_max",
                   "proportion",
                   "props...i.",
                   "pval",
                   "r", 
                   "topic",
                   "tops...i.",
                   "value",
                   "variable",
                   "ww1", 
                   "ww2",
                   "x" ))

# testing with r-hub
# rhub::validate_email() 
# rhub::list_validated_emails() 
# rhub::check_on_linux() 
# rhub::check_on_windows() 
# rhub::check_for_cran()

# tools::showNonASCIIfile()

# quick look for non-ascii characters in files in the pkg:
# files_with_non_ascii <- 
#   sapply(list.files("data-raw/2015.2.25.w2Rqrn2E/wordcounts", 
#                     recursive = T,  
#                     full.names = T, 
#                     include.dirs = T), 
#          function(i) tools::showNonASCIIfile(i))
# names(files_with_non_ascii)
# 
# # replace non-ascii chars
# for(i in seq_along(files_with_non_ascii)){
#   
#   tmp <- read.csv(names(files_with_non_ascii)[i])
#   tmp$WORDCOUNTS <- stringi::stri_trans_general(tmp$WORDCOUNTS, "latin-ascii")
#   write.csv(tmp, 
#             names(files_with_non_ascii)[i] )
# }
# 



