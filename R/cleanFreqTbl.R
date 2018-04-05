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
                          freqTbl = freqTbl,
                          sdBoundary = sdBoundary)

    # create df of incorrect, dictSuggs, and freqSuggs
    chk <- data.frame(incorrect = incorrect$word,
                      dict = dictSuggs,
                      freq = freqSuggs,
                      stringsAsFactors = FALSE)

    message("Following is a list of words to update. \nTo select the dictionary suggestion, enter 'd'. \nTo select the freqTbl suggestion, enter 'f'. \nOtherwise enter a replacement term.  \nIf the 'enter' button is hit without any entry, the current term will remain.")
    ready <- readline(prompt = "Press 'enter' to begin or type any characters to exit: ")

    if(ready != ""){ return(chk) } # return df in case user wants to see

    updated <- InteractiveFindReplace(freqSuggs, incorrect$word, outputFile)

    # summarise frequencies
    updated <- updated %>%
        group_by(word)
        summarise(freq)

    return(updated)
}