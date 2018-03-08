#' @title make a frequency table of words from vector, list, or dataframe
#'
#' @description takes an r-object of vector, list, or dataframe, and
#'     parses this into a frequency table of words
#'
#' @param input R object of vector, list, or dataframe
#' @param deDupe boolean deduplicate
#' @param rmStopWords boolean remove stopwords
#' @export
makeFreqTbl <- function(input, deDupe = FALSE, rmStopWords = FALSE){

    # check input types and parse to words
    if( is.data.frame(input) ){
        input <- apply(input, 2, as.character)
        words <- df2words(input, deDupe, rmStopWords)
    }else if( is.list(input) ){
        input <- lapply(input, as.character)
        words <- list2words(input, deDupe, rmStopWords)
    }else if( is.character(input) ){
        words <- vec2words(input, deDupe, rmStopWords)
    }else{
        stop("`input` is not list, vector, or dataframe")
    }

    # create freqTbl
    ft <- table(words)

    return(ft)
}