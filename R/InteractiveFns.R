#####################################################
###                 HELPER                        ###
#####################################################

makeSuggList <- function(words, freqTbl){
  good <- hunspell::hunspell_check(words)
  badWords <- words[ good == FALSE ]
  
  hSuggs <- hunspell::hunspell_suggest(badWords)
  hSuggs <- lapply(hSuggs, function(x){ return(ifelse(length(x) > 0, x[[1]], x )) })
  
  fSuggs <- freqSugg(badWords = badWords,
                     freqTbl = freqTbl,
                     sdBoundary = 2)
  
  suggLs <- lapply( seq(1:length(fSuggs)), function(x){
    return( c(fSuggs[[x]], hSuggs[[x]]) ) 
  })
  names(suggLs) <- badWords
  return(suggLs)
}

#####################################################
###                    MAIN                       ###
#####################################################

#' @title Interactively find and replace problematic words in a vector or dataframe
#'
#' @description For each word in the output list from checkSpelling()
#'     find the word in the inputVector and allow the user to enter a
#'     replacement word
#'
#' @param badWords named list of problematic words and suggested replacements
#' @param input dataframe or vector
#' @param outFile filepath for where to append lines of code
#' @export
InteractiveFindReplace <- function(badWords, input, outFile = NULL){
    ret <- input
    
    message("NOTES:")
    message("leaving the replacement field blank means do not replace.")
    message("Entering 'f' uses frequency table suggestion")
    message("Entering 'd' uses dictionary suggestion \n")

    for( nm in names(badWords) ){
        message(paste0("word not found: ", nm))
        message(paste0("Frequency Table Suggestion: ", badWords[[nm]][[1]]))
        message(paste0("Dictionary Suggestion: ", badWords[[nm]][[2]]))
        
        rep <- readline(prompt = paste0("enter replacement for ", nm, ": "))
        
        if(rep == ""){ 
          rep <- nm 
        }else if( rep == "f"){
          rep <- badWords[[nm]][[1]]
        }else if( rep == "d"){
          rep <- badWords[[nm]][[2]]
        }
        
        message("")
        
        if( is.data.frame(input) ){
            ret <- data.frame(lapply(ret, function(x){ 
              gsub(pattern = nm,
                   replacement = rep,
                   x)}))
            colnames(ret) <- colnames(inputDF)
            
            if(!is.null(outFile)){
              codeLn <- paste0("\ndata.frame(lapply(inputDF, function(x){ gsub(pattern = '",
                               nm, "', replacement = '", rep, "', x) }))")
              cat(codeLn, file = outFile, append = TRUE)
            }
        }else{
            ret <- gsub(pattern = nm, replacement = rep, x = ret)
            
            if(!is.null(outFile)){
              codeLn <- paste0("gsub(pattern = '", nm, "', replacement = '", rep, "', x) }))\n")
              cat(codeLn, file = outFile, append = TRUE)
            }
        }
    }

    return(ret)
}


#' @title Interactively check spelling against custom corpus
#'
#' @description Given an input vector or df and output directory, the user may
#'     correct words that are not found in a standard dictionary or a
#'     current list of ImmuneSpace specific terms.
#'
#' @param input dataframe or vector
#' @param name object name or string to use as fileName
#' @param outputDir filepath for where to append lines of code
#' @param freqTbl frequency table of words in a custom corpus
#' @export
interactiveSpellCheck <- function(input, name, outputDir, freqTbl){
    
    if( is.data.frame(input) & is.null(name) ){
        name <- attr(input, "name")
    }
  
    # Want file to be executable
    outFile <- paste0(outputDir, "/", name, ".R")

    # write first lines of file
    header <- paste0("# Changes made to ", name, " using interactiveSpellCheck() \n",
                     "# at ", Sys.time(), "\n")
    cat(header, file = outFile, append = TRUE)
    
    # Parse, run spellcheck, and get suggested replacements
    if( is.data.frame(input) ){
      words <- unique(df2words(input))
    }else{
      words <- unique(vec2Words(input))
    }
    suggLs <- makeSuggList(words, freqTbl)
    
    # do find/Replace
    res <- InteractiveFindReplace(suggLs,
                                  input,
                                  outFile)

    # Add newlines in case same file is updated at a later date
    cat("\n\n", file = outFile, append = TRUE)

    names(res) <- name

    return(res)

}
