#' Generate one or more topic models using MALLET
#' 
#' @description Generates one or more topic models using MALLET and plots diagnostics. Works only on Windows. For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param corpus the object returned by the function JSTOR_corpusofnouns. A corpus containing the documents.
#' @param MALLET the directory containing MALLET's bin directory, ideally "C:/mallet-2.0.7" or similarly close to C:/ on a Windows computer.
#' @param JAVA the directory containing java.exe
#' @param K the number of topics that the model should contain. Can also be a vector of numbers of topics, then a model will be generated for each number. Useful for comparing diagnostics of different models.
#' @return Returns plots of diagnostics if more than one number of topics is specified. Output files from MALLET can be found in the working directory.
#' @examples 
#' ## JSTOR_MALLET(corpus = corpus, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = 150) # generate a single model
#' ## JSTOR_MALLET(corpus = corpus, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = seq(150, 500, 50)) # can also generate multiple models with different numbers of topics 


JSTOR_MALLET <- function(corpus, MALLET="C:/mallet-2.0.7" , JAVA = "C:/Program Files (x86)/Java/jre7/bin", K){
  
  # stop if number of topics is less than 2
  if (as.integer(K) != K || as.integer(K) < 2) 
    stop("\nK needs to be an integer of at least 2")
  
  # create new directory to hold the text files that MALLET will use
  setwd(MALLET)
  suppressWarnings(dir.create(paste0(getwd(),"/text_files_for_MALLET")))
  suppressWarnings(setwd(paste0(getwd(),"/text_files_for_MALLET")))
  # delete everything in case that dir was previously used.
  do.call(file.remove,list(list.files(getwd())))
  
  # convert list of character vectors to text files for MALLET to use
  message("creating text files for MALLET to use...")
  sapply(1:length(corpus),
         function (x) write.table(corpus[x][[1]], file=paste(x, "txt", sep="."),
                                  quote = FALSE, row.names = FALSE, eol = " " ))
  message("done")
  
  
  #### Generate topic model with MALLET
  # setup system enviroment for R and MALLET
  # MALLET_HOME <- MALLET # "C:/mallet-2.0.7"                 # location of the bin directory
  # Sys.setenv("MALLET_HOME" = MALLET_HOME)                   # at one point, I needed these to make it work
    Sys.setenv(PATH = JAVA)                                   # but now I can't build the package with them...
                                                              # and it seems to work fine without them
  # configure variables and filenames for MALLET
  ## here using MALLET's built-in example data 
  
  # create list to store output of loop
  loop <- vector("list", length(K))
  
  message("generating topic models...")
  # define loop
  for (i in K) {
    
    # folder containing txt files for MALLET to work on
    importdir <- getwd()
    # name of file for MALLET to train model on
    output <- paste("output.mallet", i, sep = ".")
    # set number of topics for MALLET to use
    ntopics <- i
    # set optimisation interval for MALLET to use
    optint <-  10
    # set number of iterations per model
    iter <- 1000
    # other variables
    alpha <- 50/K # after after Griffiths & Steyvers 2004
    beta  <- 0.01
    # parallel
    # num-threads <- Sys.getenv('NUMBER_OF_PROCESSORS') # difficult to work with because of minus sign...
    
    # set file names for output of model, extensions must be as shown
    outputstate <-     paste("outputstate", i, "gz", sep = ".")
    outputtopickeys <- paste("outputtopickeys", i, "txt", sep = ".")
    outputdoctopics <- paste("outputdoctopics", i, "txt", sep = ".")
    diagnostics <-     paste("diagnostics", i, "xml", sep = ".")
    
    # combine variables into strings ready for windows command line
    cd <- paste0("cd ", MALLET) # location of the bin directory
    import <- paste("bin\\mallet import-dir --input", importdir, "--output", output, "--keep-sequence --remove-stopwords", sep = " ")
    train  <- paste("bin\\mallet run cc.mallet.topics.tui.TopicTrainer --input", output, "--num-topics", ntopics, "--alpha", alpha, "--beta", beta, "--optimize-interval",  optint, "--output-state", outputstate, "--output-topic-keys", outputtopickeys, "--output-doc-topics", outputdoctopics, "--diagnostics-file", diagnostics, "--optimize-burn-in 200", sep = " ")
    
    # send commands to the Windows command prompt and run MALLET from there
    # collect console output in a list
    loop[[i]] <- shell(shQuote(paste(cd, import, train, sep = " && ")), 
                       invisible = FALSE, intern = TRUE)
  }
  message("done")
  
  
  # Output files are in inspect the diagnostics files to see which number of topics is best. 
  message("preparing diagnostics...")
  library(XML)
  setwd(MALLET)
  diagnosticfiles <- list.files(pattern="(diagnostics).*\\.xml$")
  diagnostics <- lapply(1:length(K), function(j) xmlParse(diagnosticfiles[[j]], useInternal = TRUE))
  # function to get topic diagnostics from XML files
  makediagnosticsdataframe <- function(x){
    tmp <-  t(xmlSApply(xmlRoot(x), xmlAttrs))[, -1]
    df <- as.data.frame(tmp, stringsAsFactors = FALSE, row.names = 1:nrow(tmp))
  }
  # apply function to diagnostic files from topic models with different numbers of topics
  # thanks to https://stat.ethz.ch/pipermail/r-help/2009-May/199076.html
  listofdiagnostics <- lapply(1:length(diagnostics), function(i) makediagnosticsdataframe(diagnostics[[i]]))
  # attach topic numbers to the list so we know what data are associated with what number of topics
  names(listofdiagnostics) <- as.numeric(gsub("\\D", "", diagnosticfiles))
  message("done")
  
  # explore diagnostics
  message("plotting diagnostics...")
  library(reshape2)
  suppressWarnings(df <- melt(listofdiagnostics))
  # convert character dataframe to numeric
  df <- data.frame(sapply(df,function(x)as.numeric(as.character(x)))) 
  suppressWarnings(df <- melt(df, id.vars = "L1"))
  # visualize
  library(ggplot2)
  print(ggplot(df, aes(factor(L1), value)) + 
    geom_violin() + 
    geom_jitter(alpha = 0.1) + 
    facet_wrap(~variable, scale = "free"))
  # to assist with interpretation: http://article.gmane.org/gmane.comp.ai.mallet.devel/1483/
  message("done")
  message(paste0("MALLET's output files are in ", getwd()))
  # pop open the folder for the user to see: 
  # from http://stackoverflow.com/a/12135823/1036500
  shell.exec(getwd())
   
  
  # After finishing this I noticed Mimno's own R hook to MALLET: http://www.cs.princeton.edu/~mimno/R/
}
