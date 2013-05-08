#' Plot the top five hot and cold topics in the archive
#' 
#' @description Generates plots and data frames of the top five hot and cold topics. Hot topics are topics with a positive correlation to year of publication, cold topics have a negative correlation. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack.
#' @return Returns a plot of the hot topics and plot of the cold topics and a list of dataframes of the topic proportions per year. Years as rows, topics as columns and posterior probabilities as cell values.
#' @examples 
#' ## hotncold <- JSTOR_MALLET_hotncoldtopics(x = unpacked)



JSTOR_MALLET_hotncoldtopics <- function(x){
  
  
  # Andrew Goldstone's method: get the user to choose the file
  
  message("Select the topic keys file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputtopickeys <- file.choose()
  print(outputtopickeys)
  
  message("Select the topic docs file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputdoctopics <- file.choose()
  print(outputdoctopics)
  
#   # get user to paste in the path to the MALLET output files
#   
#   # from  http://r.789695.n4.nabble.com/url-prep-function-backslash-issue-tp3778530p3779086.html
#   message("please paste in the path to the MALLET output files (no quotes needed):")
#   oldstring1 <- readline() 
#   path <- chartr("\\", "/", oldstring1) 
#   setwd(path)
#   
#   # get user to paste in the name of the topic keys file
#   
#   message("please paste in the name of the topic keys file (no quotes needed):")
#   oldstring2 <- readline() 
#   outputtopickeys <- oldstring2
#   
#   # get user to paste in the name of the topic keys file
#   
#   message("please paste in the name of the topic docs file (no quotes needed):")
#   oldstring3 <- readline() 
#   outputdoctopics <- oldstring3
  
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
  
  # get pearson correlation between topic and year
  year_cors <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor(as.numeric(topic.props.agg$year), 
                                                                                                                                       topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i]))))
  # get p-value for pearson correlation
  year_cor.pval <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor.test(as.numeric(topic.props.agg$year), 
                                                                                                                                       topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i])$p.value)))
  # get five top-ranked words for each topic to use when plotting
  topic_string <- unlist(lapply(1:nrow(outputtopickeysresult), function(i) paste(unlist(strsplit(as.character(outputtopickeysresult[i,"V3"]), " "))[1:5], collapse = ".")))
  
  # make a df of correlations, p-values, topic names and numbers
  years_cor_comb <- data.frame(cor = year_cors, pval = year_cor.pval, topic = topic_string, topicnum = seq(1,length(topic_string),1))
  
  # subset for only topics with p<0.05
  years_cor_comb <- years_cor_comb[years_cor_comb$pval <= 0.1, ]
  # sort the subset
  years_cor_comb <- years_cor_comb[with(years_cor_comb, order(-cor)),]
  
  # get top 5 -ve correlations
  neg <- tail(years_cor_comb[years_cor_comb$cor < 0, ],5)
  
  # get top 5 -ve correlations
  pos <- head(years_cor_comb[years_cor_comb$cor > 0, ],5)
  
  # plot top five +ve
  top5_positive_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% pos$topicnum])
  # get topic numbers in order of column names
  tnu <- intersect(gsub("\\D", "", names(top5_positive_df)), pos$topicnum)
  # get topic names in same order as topic numbers in colnames
  tna <- rep(NA, length(tnu))
  for(i in 1:length(tnu)){
    tna[i] <- as.character(pos[pos$topicnum == tnu[[i]], ]$topic)
  }
  names(top5_positive_df) <- c("year", tna)
  
  dat.m.pos <- melt(top5_positive_df, id.vars='year')
  library(ggplot2)
  print(ggplot(dat.m.pos , aes(year, value, group=variable)) + 
    geom_line(aes(colour=variable)) )
  
  # plot top five -ve
  top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% neg$topicnum])
  # get topic numbers in order of column names
  tnu <- intersect(gsub("\\D", "", names(top5_negative_df)), neg$topicnum)
  # get topic names in same order as topic numbers in colnames
  tna <- rep(NA, length(tnu))
  for(i in 1:length(tnu)){
    tna[i] <- as.character(neg[neg$topicnum == tnu[[i]], ]$topic)
  }
  names(top5_negative_df) <- c("year", tna)
  
  dat.m.neg <- melt(top5_negative_df, id.vars='year')
  print(ggplot(dat.m.neg , aes(year, value, group=variable)) + 
          geom_line(aes(colour=variable)) )
  
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df, "top5_pos_cor" = pos, "top5_neg_cor" = neg))
  
  
}

