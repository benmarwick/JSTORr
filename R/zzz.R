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


# # BUild testing ----------------------------------------
# 
# devtools::build_win()
# 
# # r-hub multiple systems
# 
# devtools::install_github("r-hub/rhub") 
# rhub::validate_email() 
# rhub::list_validated_emails() 
# rhub::check_on_linux() 
# rhub::check_on_windows() 
# rhub::check_for_cran()

