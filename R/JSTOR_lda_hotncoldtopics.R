#' Plot the top five hot and cold topics in the archive
#' 
#' @description Generates plots and data frames of the top five hot and cold topics. Hot topics are topics with a positive correlation to year of publication, cold topics have a negative correlation. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack.
#' @param lda the object returned by the function JSTOR_lda.
#' @return Returns a plot of the hot topics and plot of the cold topics and a list of dataframes of the topic proportions per year. Years as rows, topics as columns and posterior probabilities as cell values.
#' @examples 
#' ## hotncold <- JSTOR_lda_hotncoldtopics(x = unpacked, lda = lda150) 



JSTOR_lda_hotncoldtopics <- function(x, lda){
  
  # unpack output from JSTOR_lda
  topic.props <- lda[[1]]

  
  # get bibliodata
  bibliodata <- x$bibliodata
  
  # add 'year' column from bibliodata to topic.props
  # they should both be in the same order, unless you've fiddled with them...
  topic.props$year <- as.numeric(bibliodata$year)
  topic.props.agg <- aggregate(formula = . ~ year, data = topic.props[, !(colnames(topic.props) %in% c("ID"))], FUN = mean)
  
  # get pearson correlation between topic and year
  year_cors <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor(as.numeric(topic.props.agg$year), 
                                                                                                                              topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i]))))
  # get p-value for pearson correlation
  year_cor.pval <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor.test(as.numeric(topic.props.agg$year), 
                                                                                                                                       topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i])$p.value)))
  # get five top-ranked words for each topic to use when plotting (exclude year)
  topic_string <- colnames(topic.props.agg)[2:ncol(topic.props.agg)]
  
  # make a df of correlations, p-values, topic names and numbers
  years_cor_comb <- data.frame(cor = year_cors, pval = year_cor.pval, topic = topic_string, topicnum = seq(1,length(topic_string),1))
  
  # subset for only topics with p<0.xx
  years_cor_comb <- years_cor_comb[years_cor_comb$pval <= 0.1, ]
  # sort the subset
  years_cor_comb <- years_cor_comb[with(years_cor_comb, order(-cor)),]
  
  # get top 5 -ve correlations
  neg <- tail(years_cor_comb[years_cor_comb$cor < 0, ],5)
  
  # get top 5 -ve correlations
  pos <- head(years_cor_comb[years_cor_comb$cor > 0, ],5)
  
  # plot top five +ve
  top5_positive_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% pos$topic])

  
  dat.m.pos <- melt(top5_positive_df, id.vars='year')
  library(ggplot2)
  print(ggplot(dat.m.pos , aes(year, value, group=variable)) + 
          geom_line(aes(colour=variable)) )
  
  # plot top five -ve
  top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% neg$topic])
  
  dat.m.neg <- melt(top5_negative_df, id.vars='year')
  print(ggplot(dat.m.neg , aes(year, value, group=variable)) + 
          geom_line(aes(colour=variable)) )
  
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df, "top5_pos_cor" = pos, "top5_neg_cor" = neg))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   # OLD METHOD
#   # aggregate topic props to get a mean value per year
#   topic.props.agg <- aggregate(formula = . ~ year, data = topic.props, FUN = mean)
#   # find correlations between topics and year
#   year_cors <- data.frame(cor(as.numeric(topic.props.agg$year), topic.props.agg[,-ncol(topic.props.agg)][sapply(topic.props.agg[,-ncol(topic.props.agg)], is.numeric)]))
#   which.max(year_cors); max(year_cors)
#   which.min(year_cors); min(year_cors)
#   
#   year_cors_sorted <- sort(year_cors)
#   # top 5 -ve correlations
#   top5_negative <- year_cors_sorted[1:5]
#   # top 5 +ve correlations
#   top5_positive <- year_cors_sorted[(length(year_cors_sorted)-5):length(year_cors_sorted)]
#   
#   # plot top five +ve
#   
#   library(ggplot2)
#   
#   top5_positive_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(top5_positive)])
#   dat.m.pos <- melt(top5_positive_df, id.vars='year')
#   print(ggplot(dat.m.pos , aes(year, value, group=variable)) + geom_line(aes(colour=variable)))
#   
#   # plot top five -ve
#   top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[ names(top5_negative)])
#   dat.m.neg <- melt(top5_negative_df, id.vars='year')
#   print(ggplot(dat.m.neg , aes(year, value, group=variable)) + geom_line(aes(colour=variable)))
#   
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df, "top5_pos_cor" = pos, "top5_neg_cor" = neg))
  
  
}

