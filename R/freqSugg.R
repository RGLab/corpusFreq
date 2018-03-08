#' @title provide suggestions for misspelled words from frequency table
#'
#' @description provides suggestions for misspelled words or those that are
#'     less common in the internal frequency table
#'
#' @param badWords misspelled words
#' @param freqTbl frequency table
#' @param sdBoundary integer for stringdist max distance to allow for frequencyTbl suggestions
#' @importFrom stringdist stringdistmatrix
#' @importFrom SnowballC wordStem
#' @export
freqSugg <- function(badWords, freqTbl, sdBoundary = 2){
    cft <- data.frame(freqTbl, stringsAsFactors = F)
    colnames(cft) <- c("word","Freq") # Rename since not sure what they may be coming in

    tmp <- stringdist::stringdistmatrix(badWords, cft$word)
    rownames(tmp) <- badWords
    colnames(tmp) <- cft$word

    result <- sapply(seq(1:length(rownames(tmp))), function(y){
        word <- rownames(tmp)[[y]]
        gWord <- paste0("^", word, "$") # to avoid partial matching
        gWord <- gsub("\\+", "\\\\+", gWord) # grep needs escape char for plus signs
        rowMinus <- tmp[y,][ !grepl(gWord, colnames(tmp)) ] # remove the colname from options
        mostSimilar <- rowMinus[ min(rowMinus) == rowMinus] # find most similar
        nm <- names(mostSimilar)[[1]]

        if(mostSimilar[[1]] > sdBoundary){ return(word) } # return word if outside of stringdist boundary
        # if(wordStem(nm) == wordStem(word)){ return(word)}   # TODO ... use this? lymphocyt returned!

        # return most similar word if most similar word has 2.5x greater frequency.
        # Reason for 2.5 multiplier is that with internal frequency tables, a 2:1 ratio
        # causes the ms word to be suggested when there is very little information to
        # base this rationale off.
        msFreq <- cft$Freq[ cft$word == nm ]
        currFreq <- ifelse(word %in% cft$word, cft$Freq[ cft$word == word ], 0)
        ret <- ifelse(msFreq > (currFreq * 2.5), nm, word)
    })

    return(result)
}
