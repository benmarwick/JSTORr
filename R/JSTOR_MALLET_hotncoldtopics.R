#' Plot the top five hot and cold topics in the archive
#' 
#' @description Generates plots and data frames of the top five hot and cold topics. Hot topics are topics with a positive correlation to year of publication, cold topics have a negative correlation. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack.
#' @param pval p-value of the correlation cutoff for topics to include in the top 5 negative/positive list (ie. only topics where p<0.01 or 0.001?). Default is 0.05.
#' @param ma ma moving average interval, default is five years.
#' @return Returns a plot of the hot topics and plot of the cold topics and a list of dataframes of the topic proportions per year. Years as rows, topics as columns and posterior probabilities as cell values.
#' @examples 
#' ## hotncold <- JSTOR_MALLET_hotncoldtopics(x = unpacked)



JSTOR_MALLET_hotncoldtopics <- function(x, pval=0.05, ma=5){
  
  
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
  # thanks to http://stackoverflow.com/q/8058402/1036500
  dat <- outputdoctopicsresult
# slow
#   l_dat <- reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), 
#                                                 props=colnames(dat[,seq(4, ncol(dat), 2)])), 
#                    direction="long")
#   library(reshape2)
#   topic.props <- dcast(l_dat, V2 ~ V3)
  
  # faster method
  # make long table
  l_dat1 <- stats::reshape(dat, idvar=1:2, varying=list(topics=colnames(dat[,seq(3, ncol(dat)-1, 2)]), 
                                                              props=colnames(dat[,seq(4, ncol(dat), 2)])), direction="long") 
  # make wide table of docs = rows, and topics = cols, with proportions as cells
  library(reshape2)
  topic.props  <- dcast(l_dat1, l_dat1[,2] ~ l_dat1[,4]) 
  
  rm(l_dat1) # because this is very big but not longer needed
  # end up with table of docs (rows) and columns (topics)   
  

  # get bibliodata
  bibliodata <- x$bibliodata
  
  # match year from bibliodata to DOI in topic output and get year into topics
  topic.props$id <- sub("\\.txt", "", basename(as.character(topic.props[,1]))) # 1st column contains file names
  topic.props <- topic.props[,-1] # delete col with filenames, no longer needed now that we've got the ids
  topic.props <- merge(topic.props, cbind.data.frame(year = bibliodata$year, id = bibliodata$x), by = "id")
  # get mean topic proportions for all docs per year...
  topic.props.agg <- aggregate(formula = . ~ year, data = topic.props[, !(colnames(topic.props) %in% c("id"))], FUN = mean)
  
  # make a n-year moving average to smooth things out a bit (from http://stackoverflow.com/a/4862334/1036500)
  # only looking back: a trailing moving average, ma = years to trail      
  topic.props.agg <- data.frame(na.omit(apply(topic.props.agg, 2, function(x){filter(x,rep(1/ma,ma), sides=1)})))  
  
  # get pearson correlation between topic and year
  year_cors <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor(as.numeric(topic.props.agg$year), 
                                                                                                                              topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i]))))
  # get p-value for pearson correlation
  year_cor.pval <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor.test(as.numeric(topic.props.agg$year), 
                                                                                                                                       topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i])$p.value)))
  # get five top-ranked words for each topic to use when plotting (exclude year)
  topic_string <- paste0("topic_", gsub("\\D", "", colnames(topic.props.agg)[2:ncol(topic.props.agg)]))
  
  # make a df of correlations, p-values, topic names and numbers
  years_cor_comb <- data.frame(cor = year_cors, pval = year_cor.pval, topic = topic_string, topicnum = seq(1,length(topic_string),1))
  
  # stick on the top five words per topic for a bit more information
  words.list <- data.frame(topic = keys.frame$topic, keywords = sapply(strsplit(keys.frame$keywords, split=" "), function (words) paste(words[1:5],collapse=" ")))
  
  years_cor_comb$keywords <-  words.list$keywords[match(years_cor_comb$topicnum, words.list$topic)]
  
  # subset for only topics with p < pval
  years_cor_comb <- years_cor_comb[years_cor_comb$pval <= pval, ]
  # sort the subset
  years_cor_comb <- years_cor_comb[with(years_cor_comb, order(-cor)),]
  
  # get top 5 -ve correlations
  neg <- tail(years_cor_comb[years_cor_comb$cor < 0, ],5)
  
  # get top 5 -ve correlations
  pos <- head(years_cor_comb[years_cor_comb$cor > 0, ],5)
  
  # plot top five +ve
  top5_positive_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% paste0("X", pos$topicnum)])
  
  dat.m.pos <- melt(top5_positive_df, id.vars='year')
  dat.m.pos$topic <- words.list$keywords[match(gsub("\\D", "", dat.m.pos$variable), words.list$topic)]
  library(ggplot2)
  print(ggplot(dat.m.pos , aes(year, value, group=topic)) + 
    geom_line(aes(colour=topic)) )
  
  # plot top five -ve
  top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% paste0("X", neg$topicnum)])
  
  dat.m.neg <- melt(top5_negative_df, id.vars='year')
  dat.m.neg$topic <- words.list$keywords[match(gsub("\\D", "", dat.m.neg$variable), words.list$topic)]
  
  print(ggplot(dat.m.neg , aes(year, value, group=topic)) + 
          geom_line(aes(colour=topic)) )
  
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df, "top5_pos_cor" = pos, "top5_neg_cor" = neg))
  
  
}

