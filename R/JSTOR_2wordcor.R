JSTOR_2wordcor <- function(word1, word2){
  ## FOURTH investigate correlations between words over time
  cw1 <- word1
  cw2 <- word2
  cword1 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% cw1))
  cword2 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% cw2))
  leng <- sapply(1:length(wordcounts), function(i) length(wordcounts[[i]]))
  # calculate ratios
  cword1_ratio <- cword1/leng
  cword2_ratio <- cword2/leng
  # get years for each article and make data frame
  ctwowords_by_year <- data.frame(ww1 = cword1_ratio, ww2 = cword2_ratio, year = as.numeric(as.character(bibliodata$year)))
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # calculate correlations of the two words per year (and p-values)
  library(plyr)
  corrp <- ddply(ctwowords_by_year, .(year), summarize, "corr" = cor.test(ww1, ww2)$estimate, "pval" = cor.test(ww1, ww2)$p.value)
  # visualise
  library(ggplot2)
  suppressWarnings(ggplot(corrp, aes(year, corr)) +
                     geom_point(aes(size = 1/pval)) +
                     geom_smooth(  method = "loess", span = 0.4) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     geom_hline(yintercept=0, colour = "red") + 
                     ylab(paste0("correlation between '",cw1, "'' and '", cw2,"'")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_min+1, 2)) +
                     # this is not quite working properly...
                     scale_size_continuous(name = "p-values", breaks = c(100, 50, 10), labels = c(0.01, 0.05, 0.10)) )
}
