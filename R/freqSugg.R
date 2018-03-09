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
        mostSim <- rowMinus[ min(rowMinus) == rowMinus] # find most similars
        mostSim <- mostSim[ mostSim <= sdBoundary ]

        if(length(mostSim) == 0){ return(word) } # return word if no options within sdBoundary

        # Subset ms to those with frequency differences of 2.5x current word
        currFreq <- ifelse(word %in% cft$word, cft$Freq[ cft$word == word ], 0)
        msFreq <- cft$Freq[ match(names(mostSim), cft$word) ]
        names(msFreq) <- names(mostSim)
        msFreq <- msFreq[ msFreq > (currFreq * 2.5) ]

        # return word with max frequency
        res <- names(msFreq)[ msFreq == max(msFreq)][[1]]
        ret <- ifelse(length(msFreq > 0), res, word)
    })

    return(result)
}
