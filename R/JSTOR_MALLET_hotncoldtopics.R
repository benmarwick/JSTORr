#' Plot the top five hot and cold topics in the archive
#' 
#' @description Generates plots and data frames of the top five hot and cold topics. Hot topics are topics with a positive correlation to year of publication, cold topics have a negative correlation. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param x the object returned by the function JSTOR_unpack1grams (for the bibliographic data only).
#' @param pval p-value of the correlation cutoff for topics to include in the top 5 negative/positive list (ie. only topics where p<0.01 or 0.001?). Default is 0.05.
#' @param ma ma moving average interval, default is five years.
#' @return Returns a plot of the hot topics and plot of the cold topics and a list of dataframes of the topic proportions per year. Years as rows, topics as columns and posterior probabilities as cell values.
#' @examples 
#' ## hotncold <- JSTOR_MALLET_hotncoldtopics(x = unpack1grams)



JSTOR_MALLET_hotncoldtopics <- function(x, pval=0.05, ma=5){
  
  
  # Andrew Goldstone's method: get the user to choose the file
  
  message("Select the topic keys file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputtopickeys <- file.choose()
  print(outputtopickeys)
  
  message("Select the doc topics file")
  ignore <- readline("(press return to open file dialog - it might pop up behind here) ")
  outputdoctopics <- file.choose()
  print(outputdoctopics)
  
  outputdoctopicsresult <- read.table(outputdoctopics, header=F, sep="\t")
  
  # manipulate outputdoctopicsresult to be more useful 
  # thanks to http://stackoverflow.com/q/8058402/1036500
  dat <- data.table::data.table(outputdoctopicsresult)

# reshape using data table

# get document number
docnum <- dat$V1
# get text number
txt <- dat$V2

# remove doc num and text num so we just have topic and props
dat1 <- dat[ ,c("V1","V2", paste0("V", ncol(dat))) := NULL]
# get topic numbers
n <- ncol(dat1) 
tops <- apply(dat1, 1, function(i) i[seq(1, n, 2)])
# get props 
props <- apply(dat1, 1, function(i) i[seq(2, n, 2)])

# put topics and props together
tp <- lapply(1:ncol(tops), function(i) data.frame(tops[,i], props[,i]))
names(tp) <- txt
# make into long table
dt <- data.table::rbindlist(tp)
dt$doc <- unlist(lapply(txt, function(i) rep(i, ncol(dat1)/2)))
dt$docnum <- unlist(lapply(docnum, function(i) rep(i, ncol(dat1)/2)))

# reshape to wide
library(data.table)
setkey(dt, tops...i., doc)
out <- dt[CJ(unique(tops...i.), unique(doc))][, as.list(props...i.), by=tops...i.]
setnames(out, c("topic", as.character(txt)))

# transpose to have table of docs (rows) and columns (topics) 
tout <- data.table(t(out))
setnames(tout, unname(as.character(tout[1,])))
tout <- tout[-1,]
row.names(tout) <- txt  
topic.props <- tout

  # end up with table of docs (rows) and columns (topics)   
  
  # get bibliodata
  bibliodata <- x$bibliodata
  
  # match year from bibliodata to DOI in topic output and get year into topics
  topic.props$id <- sub("\\.txt", "", basename(as.character(row.names(topic.props)))) # row names contains file names
  topic.props <- merge(topic.props, cbind.data.frame(year = bibliodata$year, id = bibliodata$x), by = "id")
  # get mean topic proportions for all docs per year...
topic.props.agg <- topic.props[, !(colnames(topic.props) %in% c("id")), with = FALSE][, lapply(.SD, mean, na.rm=TRUE), by=year ]
  
  # make a n-year moving average to smooth things out a bit (from http://stackoverflow.com/a/4862334/1036500)
  # only looking back: a trailing moving average, ma = years to trail      
  topic.props.agg <- data.frame(na.omit(apply(topic.props.agg, 2, function(x){filter(x,rep(1/ma,ma), sides=1)})))  
  
  # get pearson correlation between topic and year
  year_cors <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor(as.numeric(topic.props.agg$year),                                                                                                                             topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i]))))
  # get p-value for pearson correlation
  year_cor.pval <- as.numeric(unlist(lapply(1:(ncol(topic.props.agg[,!(colnames(topic.props.agg) %in% "year")])), function(i) cor.test(as.numeric(topic.props.agg$year),                                                                                                                           topic.props.agg[,!(colnames(topic.props.agg) %in% "year")][,i])$p.value)))
  # get five top-ranked words for each topic to use when plotting (exclude year)
  topic_string <- paste0("topic_", gsub("\\D", "", colnames(topic.props.agg)[2:ncol(topic.props.agg)]))
  
  # make a df of correlations, p-values, topic names and numbers
  years_cor_comb <- data.frame(cor = year_cors, pval = year_cor.pval, topic = topic_string, topicnum = seq(1,length(topic_string),1))

## Andrew Goldstone's function
read.keys <- function() {
  keys.filename <- outputtopickeys
  df <- read.csv(keys.filename,sep="\t",header=FALSE,as.is=TRUE,
                 col.names=c("topic","alpha","keywords"))
  df$topic <- df$topic + 1
  df
}
## make it so, using above function
keys.frame <- read.keys()
  
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
  
  dat.m.pos <- reshape2::melt(top5_positive_df, id.vars='year')
  dat.m.pos$topic <- words.list$keywords[match(gsub("\\D", "", dat.m.pos$variable), words.list$topic)]
  library(ggplot2)
  print(ggplot(dat.m.pos , aes(year, value, group=topic)) + 
    geom_line(aes(colour=topic)) )
  
  # plot top five -ve
  top5_negative_df <- data.frame(year = topic.props.agg$year, topic.props.agg[names(topic.props.agg) %in% paste0("X", neg$topicnum)])
  
  dat.m.neg <- reshape2::melt(top5_negative_df, id.vars='year')
  dat.m.neg$topic <- words.list$keywords[match(gsub("\\D", "", dat.m.neg$variable), words.list$topic)]
  
  print(ggplot(dat.m.neg , aes(year, value, group=topic)) + 
          geom_line(aes(colour=topic)) )
  
  return(list("top5_positive" = top5_positive_df, "top5_negative" = top5_negative_df, "top5_pos_cor" = pos, "top5_neg_cor" = neg))
  
  
}

