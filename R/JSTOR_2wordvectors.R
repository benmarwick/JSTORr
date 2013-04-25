



JSTOR_2wordvectors <- function(v1, v2){
  # THIRD, try comparing vectors of related words
  # now word of interest (always lower case)
  wv1 <- v1
  wv2 <- v2
  wordv1 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% wv1))
  wordv2 <- sapply(1:length(wordcounts), function(i) sum(wordcounts[[i]] %in% wv2))
  # calculate ratios
  wordv1_ratio <- wordv1/the
  wordv2_ratio <- wordv2/the
  # get years for each article and make data frame
  twowordvs_by_year <- data.frame(wordv1_ratio, wordv2_ratio, year = as.numeric(as.character(bibliodata$year)))
  # reshape into a long table to make it easier to work with in ggplt
  library(reshape2)
  twowordvs_by_year_melt <- melt(twowordvs_by_year, id.vars = "year")
  lim_min <- as.numeric(as.character(min(bibliodata$year)))
  lim_max <- as.numeric(as.character(max(bibliodata$year)))
  # visualise
  library(ggplot2)
  suppressWarnings(ggplot(twowordvs_by_year_melt, aes(year, log(value))) +
                     geom_point(subset = .(value > 0), aes(colour = variable)) +
                     geom_smooth( aes(colour = variable), method = "loess", span = 0.4, subset = .(value > 0)) +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                     ylab(paste0("log of frequency of words")) +
                     scale_x_continuous(limits=c(lim_min, lim_max), breaks = seq(lim_min-1, lim_min+1, 2)) +
                     scale_colour_discrete(labels = c(paste(wv1, collapse = ", "), paste(wv2, collapse = ", "))) )
}
