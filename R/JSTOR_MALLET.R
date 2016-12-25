#' Generate one or more topic models using MALLET's implementation of Latent Dirichlet allocation (LDA) 
#' 
#' @description Generates one or more topic models using MALLET and plots diagnostics. This is a very basic R wrapper for MALLET on Windows, see here for something more complete: http://cran.r-project.org/web/packages/mallet/ For use with JSTOR's Data for Research datasets (http://dfr.jstor.org/).
#' @param nouns the object returned by the function JSTOR_dtmofnouns. If you want to use the document contents with all other parts of speech, use unpack1grams$wordcounts as the first argument here (where unpack1grams is the output from JSTOR_unpack1grams)
#' @param MALLET the directory containing MALLET's bin directory, ideally "C:/mallet-2.0.7" or similarly close to C:/ on a Windows computer.
#' @param JAVA the directory containing java.exe. It will probably be something like "C:/Program Files/Java/jre7/bin" but you should check
#' @param K the number of topics that the model should contain. Can also be a vector of numbers of topics, then a model will be generated for each number. Useful for comparing diagnostics of different models, but may be time consuming.
#' @return Returns a folder of text files (one text file per document) and a folder of output files from MALLET. The folders are named with a date-time stamp so they wont be overwritten by repeated runs. 
#' @examples 
#' ## JSTOR_MALLET(nouns, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = 150) # generate a single model
#' ## JSTOR_MALLET(nouns =  unpack1grams$wordcounts, MALLET = "C:/mallet-2.0.7", JAVA = "C:/Program Files (x86)/Java/jre7/bin", K = seq(150, 500, 50)) # can also generate multiple models with different numbers of topics 


JSTOR_MALLET <- function(nouns, MALLET="C:/mallet-2.0.7" , JAVA = "C:/Program Files (x86)/Java/jre7/bin", K){
  
  # stop if number of topics is less than 2
  if (as.integer(K) != K || as.integer(K) < 2) 
    stop("\nK needs to be an integer of at least 2")
  
  # create new directory to hold the text files that MALLET will use
  setwd(MALLET)
  # get a date-time stamp to make a folder that wont be constantly overwritten
  now <- paste(unlist(strsplit(strftime(Sys.time()), split = " ")), collapse = "_") 
  now <- gsub(":", "", now)
  now <- gsub("-", "", now)
  suppressWarnings(dir.create(paste0(getwd(),"/text_files_for_MALLET_", now)))
  suppressWarnings(setwd(paste0(getwd(),"/text_files_for_MALLET_", now )))
  # delete everything in case that dir was previously used.
  suppressWarnings(do.call(file.remove,list(list.files(getwd()))))
  
  # convert dtm to text files for MALLET to use
  message("creating text files for MALLET to use...")
  
  # write one text file for each document in the dtm
  n <- length(nouns$dimnames$Docs)
  for(i in 1:n){
    cat(paste0("Writing text file ", i," of ", n, " files\n"))
    tmp <- rep(nouns$dimnames$Terms, as.matrix(nouns[i,,]))
    tmp <- paste(tmp, collapse = " ")
    write.table(tmp, file = paste(nouns$dimnames$Docs[i], "txt", sep = "."), quote = FALSE,
                row.names = FALSE, eol = " ")
    cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
  }
  message(paste0("done, text files are in ", getwd()))
  
  
  
  #### Generate topic model with MALLET
setwd(MALLET)
  
  
  
  # setup system enviroment for R and MALLET
  # MALLET_HOME <- MALLET # "C:/mallet-2.0.7"                 # location of the bin directory
  # Sys.setenv("MALLET_HOME" = MALLET_HOME)                   # at one point, I needed these to make it work
    Sys.setenv(PATH = JAVA)                                   # but now I can't build the package with them...
                                                              # and it seems to work fine without them
  # configure variables and filenames for MALLET
  
  # create list to store output of loop
  loop <- vector("list", length(K))
  
  message("generating topic models...")
  # define loop
  for (i in K) {
    
    # folder containing txt files for MALLET to work on
    importdir <- paste0(MALLET,"/text_files_for_MALLET_", now )
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
    outputstate <-     paste("outputstate", i, now, "gz", sep = ".")
    outputtopickeys <- paste("outputtopickeys", i, now, "txt", sep = ".")
    outputdoctopics <- paste("outputdoctopics", i, now, "txt", sep = ".")
    diagnostics <-     paste("diagnostics", i, "xml",now,  sep = ".")
    
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
  
 
#   # to assist with interpretation: http://article.gmane.org/gmane.comp.ai.mallet.devel/1483/
#   message("done")
  message(paste0("MALLET's output files are in ", MALLET))
  # pop open the folder for the user to see: 
  # from http://stackoverflow.com/a/12135823/1036500
  # windows only
  shell.exec(getwd())
   
  
  # After finishing this I noticed Mimno's own R hook to MALLET: http://www.cs.princeton.edu/~mimno/R/
}
