#####################################################
###                 HELPER                        ###
#####################################################

makeSuggList <- function(words, freqTbl, metaData = NULL, sdBoundary = 2){

  suggLs <- list()

  # Dictionary Check:
  # rm words present in standard english dictionary using stopwords or hunspell
  good <- hunspell::hunspell_check(words)
  badWords <- words[ good == FALSE ]

  # FreqTbl Check:
  # rm words present in FreqTbl
  badWords <- badWords[ !(badWords %in% names(freqTbl)) ]

  # Suggest replacements for remaining badWords
  if( length(badWords) > 0 ){
      hSuggs <- hunspell::hunspell_suggest(badWords)
      hSuggs <- lapply(hSuggs, function(x){ return(ifelse(length(x) > 0, x[[1]], x )) })

      fSuggs <- freqSugg(badWords = badWords,
                         freqTbl = freqTbl,
                         sdBoundary = sdBoundary)

      suggLs <- lapply( seq(1:length(fSuggs)), function(x){
          return( unique(c(fSuggs[[x]], hSuggs[[x]])) ) # may be same therefore dedupe
      })

      names(suggLs) <- badWords
  }

  # Internal Consistency Check:
  # Find single values and see if they may be very similar in terms of
  # stringdist to more common words within the same vector.

  #TODO ... deal with scenarios based on metaData
  if( !is.null(metaData) ){
      if( metaData$medLength > 100 | metaData$numStrings > 5){
          if(metaData$medLength > 100){
              words <- words[ !(words %in% stopwords::stopwords()) ]
          }
          internalFt <- makeFreqTbl(words)
          singles <- internalFt[ internalFt == 1 ]
          iSuggs <- freqSugg(badWords = names(singles),
                             freqTbl = internalFt,
                             sdBoundary = sdBoundary)
          names(iSuggs) <- names(singles)
          iSuggs <- iSuggs[ names(iSuggs) != iSuggs ] # drop self-referenced

          # return combined list
          suggLs <- c(suggLs, iSuggs)
      }
  }

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

    if(!is.null(outFile) & !file.exists(outFile)){
        file.create(outFile)
    }

    message("NOTES:")
    message("leaving the replacement field blank means do not replace.")
    message("Entering 'f' uses frequency table suggestion")
    message("Entering 'd' uses dictionary suggestion \n")

    for( nm in names(badWords) ){
        message(paste0("word not found: ", nm))
        message(paste0("Frequency Table Suggestion: ", badWords[[nm]][[1]]))
        message(paste0("Dictionary Suggestion: ", badWords[[nm]][[2]]))

        rep <- readline(prompt = paste0("enter replacement for ", nm, ": "))

        if( rep == "f"){
            rep <- badWords[[nm]][[1]]
        }else if( rep == "d"){
            rep <- badWords[[nm]][[2]]
        }

        message("")

        if( rep != ""){
            if( is.data.frame(input) ){
                ret <- data.frame(lapply(ret, function(x){
                    gsub(pattern = nm,
                         replacement = rep,
                         x)}))
                colnames(ret) <- colnames(inputDF)
                codeLn <- paste0("\ndata.frame(lapply(inputDF, function(x){ gsub(pattern = '",
                                 nm, "', replacement = '", rep, "', x) }))")
            }else{
                ret <- gsub(pattern = nm, replacement = rep, x = ret)
                codeLn <- paste0("gsub(pattern = '",
                                 nm, "', replacement = '", rep, "', x)\n")
            }

            if(!is.null(outFile)){
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
#' @importFrom stats median
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

    # Parse
    if( is.data.frame(input) ){
      words <- unique(df2words(input))
    }else{
      words <- unique(vec2Words(input))
    }

    # get metaData
    metaData <- list()
    metaData$medLength <- stats::median(sapply(input, nchar))
    metaData$numStrings <- length(input)

    # Make suggestions
    suggLs <- makeSuggList(words, freqTbl, metaData)

    # do find/Replace
    res <- InteractiveFindReplace(suggLs,
                                  input,
                                  outFile)

    # Add newlines in case same file is updated at a later date
    cat("\n\n", file = outFile, append = TRUE)

    names(res) <- name

    return(res)

}
