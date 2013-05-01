#' Plot the top five hot and cold topics in the archive
#' 
#' @description Generates plots and data frames of the top five hot and cold topics. Hot topics are topics with a positive correlation to year of publication, cold topics have a negative correlation. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack.
#' @return Returns a plot of the hot topics and plot of the cold topics and a list of dataframes of the topic proportions per year. Years as rows, topics as columns and posterior probabilities as cell values.
#' @examples 
#' ## hotncold <- JSTOR_MALLET_hotncoldtopics(x = unpacked)



JSTOR_MALLET_hotncoldtopics <- function(x){
  
  # get user to paste in the path to the MALLET output files
  
  # from  http://r.789695.n4.nabble.com/url-prep-function-backslash-issue-tp3778530p3779086.html
  message("please paste in the path to the MALLET output files (no quotes needed):")
  oldstring1 <- readline() 
  path <- chartr("\\", "/", oldstring1) 
  setwd(path)
  
  # get user to paste in the name of the topic keys file
  
  message("please paste in the name of the topic keys file (no quotes needed):")
  oldstring2 <- readline() 
  outputtopickeys <- oldstring2
  
  # get user to paste in the name of the topic keys file
  
  message("please paste in the name of the topic docs file (no quotes needed):")
  oldstring3 <- readline() 
  outputdoctopics <- oldstring3
  
  outputtopickeysresult <- read.table(outputtopickeys, header=F, sep="\t")
  outputdoctopicsresult <- read.table(outputdoctopics, header=F, sep="\t")
  
  # manipulate outputdoctopicsresult to be more useful 
  dat <- outputdoctopicsresult
  l_dat <- reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), 
                                                props=colnames(dat[,seq(4, ncol(dat), 2)])), 
                   direction="long")
  library(reshape2)
  topic.props <- dcast(l_dat, V2 ~ V3)
  rm(l_dat) # because this is very big but not longer needed
  # end up with table of docs (rows) and columns (topics)   
  ################### todo: make topic numbers == topic top 5 words
  
  # reorder by document number
  # extract document number from file name
  topic.props$docnum <- as.numeric(gsub("\\D", "", topic.props$V2))
  # sort by that number
  topic.props <- topic.props[with(topic.props, order(topic.props$docnum)), ]
  
  # get bibliodata
  bibliodata <- x$bibliodata
  
  # add 'year' column from bibliodata to topic.props
  # they should both be in the same order, unless you've fiddled with them...
  topic.props$year <- bibliodata$year
  # aggregate topic props to get a mean value per year (exclude filenames and docnum)
  topic.props.agg <- aggregate(formula = . ~ year, data = topic.props[, !(colnames(topic.props) %in% c("V2","docnum"))], FUN = mean)
  # find correlations between topics and year
  year_cors <- data.frame(cor(as.numeric(topic.props.agg$year), topic.props.agg[,-ncol(topic.props.agg)][sapply(topic.props.agg[,-ncol(topic.props.agg)], is.numeric)]))
  which.max(year_cors); max(year_cors)
  which.min(year_cors); min(year_cors)
  
  year_cors_sorted <- sort(year_cors)
  
  # top 5 -ve correlations
  top5_negative <- year_cors_sorted[1:5]
  
  # top 5 +ve correlations
  top5_positive <- year_cors_sorted[(length(year_cors_sorted)-5):length(year_cors_sorted)]
  
  # plot top five +ve
  top5_positive_df <- data.frame(year = topic.props.agg$year, topic.props.agg[as.character(as.numeric(gsub("\\D", "", names(top5_positive))))])
  dat.m.pos <- melt(top5_positive_df, id.vars='year')
  library(ggplot2)
  ggplot(dat.m.pos , aes(year, value, group=variable)) + geom_line(aes(colour=variable))
  
  # plot top five -ve
  top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[as.character(as.numeric(gsub("\\D", "", names(top5_negative))))])
  dat.m.neg <- melt(top5_negative_df, id.vars='year')
  ggplot(dat.m.neg , aes(year, value, group=variable)) + geom_line(aes(colour=variable))
  
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df))
  
  
}

