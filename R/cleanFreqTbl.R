#' @title clean a word frequency table to remove misspellings
#'
#' @description takes frequency table of words as input, performs
#'     interactive spellchecking function and outputs cleaned table.
#'     R script with changes during interaction is also generated.
#'
#' @param freqTbl frequency table
#' @param outputFile path for output, should be .R
#' @param sdBoundary integer for stringdist max distance to allow for frequencyTbl suggestions
#' @import hunspell
#' @import dplyr
#' @export
cleanFreqTbl <- function(freqTbl, outputFile = NULL, sdBoundary = 2){

    # Transform freqTbl to df for easier manipulation
    df <- data.frame(freqTbl,
                     stringsAsFactors = FALSE)
    colnames(df) <- c("word", "freq")
    df$word <- as.character(df$word)

    # hunspell::hunspell_suggest based on english dictionary
    message("Checking for misspellings according to english dictionary")
    correct <- hunspell_check(df$word)
    incorrect <- df[ !correct, ]
    keep <- df[ correct, ] # for later
    suggs <- hunspell_suggest(incorrect$word)
    dictSuggs <- sapply(suggs, function(x){ ifelse(length(x) > 1, x[[1]], x)})

    # Compare incorrect terms to all freqTbl words to find closest possiblity
    message("Comparing misspellings to other freqTbl words using stringdist")
    freqSuggs <- freqSugg(badWords = incorrect$word,
                          corpusFreqTbl = freqTbl,
                          sdBoundary = sdBoundary)

    # create df of incorrect, dictSuggs, and freqSuggs
    chk <- data.frame(incorrect = incorrect$word,
                      dict = dictSuggs,
                      freq = freqSuggs,
                      stringsAsFactors = FALSE)

    message("Following is a list of words to update. \nTo select the dictionary suggestion, enter 'd'. \nTo select the freqTbl suggestion, enter 'f'. \nOtherwise enter a replacement term.  \nIf the 'enter' button is hit without any entry, the current term will remain.")
    ready <- readline(prompt = "Press 'enter' to begin or type any characters to exit: ")

    if(ready != ""){ return(chk) } # return df in case user wants to see

    # write first lines of outputFile
    if( !file.exists(outputFile) ){ file.create(outputFile) }
    header <- paste0("# Changes made at ", Sys.time(), "\n")
    cat(header, file = outFile, append = TRUE)

    # generate replacement vec
    ret <- sapply(seq(1:nrow(chk)), function(x){
        message("")
        message(paste0("Current Word: ", chk$incorrect[[x]]))
        message(paste0("Dictionary Suggestion: ", chk$dict[[x]]))
        message(paste0("FreqTbl Suggestion: ", chk$freq[[x]]))
        chg <- readline(prompt = "replacement: ")

        if( chg == "f"){
            repl <- chk$freq[[x]]
        }else if( chg == "d"){
            repl <- chk$dict[[x]]
        }else if( chg != ""){
            repl <- chg
        }else{
            repl <- chk$incorrect[[x]]
        }

        if(chg != ""){
            codeLn <- paste0("gsub( ", chk$incorrect[[x]], ", ", repl, ", vec)")
            cat(codeLn, file = outputFile, append = TRUE)
        }

        return(repl)
    })

    # Add newlines to outputFile in case later updates are made
    cat("\n\n", file = outFile, append = TRUE)

    # recombine keepers with new terms
    incorrect$word <- ret
    updated <- rbind(incorrect, keep)

    # summarise frequencies
    updated <- updated %>%
        group_by(word)
        summarise(freq)

    return(updated)
}