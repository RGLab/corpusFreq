##################################
###      PARSING HELPERS       ###
##################################

#' @title parse a character vector to words with specialized handling for scientific terms
#'
#' @description parse a vector to words carefully to ensure that scientific terms
#'     which may include hyphens or plus-signs are not altered
#'
#' @param vec character vector
#' @param deDupe boolean, default TRUE, dedupe vector prior to parsing
#' @param rmStopWords boolean, default TRUE, remove stopwords
#' @importFrom stopwords stopwords
#' @export
vec2words <- function(vec, deDupe = FALSE, rmStopWords = FALSE){

    # De-dupe elements of vector b/c assume copy-pasted and want
    # to base frequencies on typed words.
    if(deDupe){ vec <- unique(vec) }

    # handling punctation before splitting
    vec <- vec[ !is.na(vec) ] # remove NA elements of vector
    vec <- tolower(vec) # lowercase
    vec <- gsub("<\\/|\\\n", " ", vec) # for html elements , e.g. </p>
    vec <- gsub("\\.|\\(|\\)|\\?|=|\\,|>|<|#|;|'|:", " ", vec) # for non-hyphen punct in middle of words

    # punctuation tidying post splitting
    words <- unlist(strsplit(vec, " ")) # make list of words / split on spaces
    words <- gsub("^(\\-|\\*|\\/)|(\\*|\\/)$", "", words) # oddball cases with punct at edge
    words <- gsub("--", "-", words) # double hyphen accidents

    # handle hyphenated terms with multiple letters on both sides
    hyps <- words[ grep("^[[:alpha:]]{2,}-[[:alpha:]]{2,}", words) ] # try to avoid terms like 'b-cell'
    hyps <- unlist(strsplit(hyps, "-"))
    keep <- words[ grep("^[[:alpha:]]{2,}-[[:alpha:]]{2,}", words, invert = T) ]
    words <- c(hyps, keep)

    # handle digits
    words <- words[ grep("^\\d+$", words, invert = T) ] # remove integers
    words <- words[ grep("\\d+[[:punct:]]\\d+", words, invert = T) ] # remove things like '1:1' or '0.123'

    # remove words with only 1 character, possibly due to subs
    words <- words[ nchar(words) > 2 ]

    # remove common non-analytical terms, e.g. 'of' and 'the'
    if(rmStopWords){ words <- words[ !(words %in% stopwords::stopwords()) ] }

    # handle hyphenated datetime words like '2-week' 'year-1'
    words[ grep("minute", words)] <- "minute"
    words[ grep("hour", words)] <- "hour"
    words[ grep("day", words)] <- "day"
    words[ grep("week", words)] <- "week"
    words[ grep("month", words)] <- "month"
    words[ grep("year", words)] <- "year"

    return(words)
}

#' @title parse a df to words with specialized handling for scientific terms
#'
#' @description parse a df to words carefully to ensure that scientific terms
#'     which may include hyphens or plus-signs are not altered
#'
#' @param df dataframe object
#' @param deDupe boolean, default TRUE, dedupe df column elements prior to parsing
#' @param rmStopWords boolean, default TRUE, remove stopwords
#' @export
df2words <- function(df, ...){
    words <- unlist(apply(df, 2, vec2words, ...))
    return(words)
}

#' @title parse a list of character vectors to words with specialized handling for scientific terms
#'
#' @description parse a list to words carefully to ensure that scientific terms
#'     which may include hyphens or plus-signs are not altered
#'
#' @param vec character vector
#' @param deDupe boolean, default TRUE, dedupe list elements prior to parsing
#' @param rmStopWords boolean, default TRUE, remove stopwords
#' @export
list2words <- function(ls, ...){
    words <- unlist(lapply(ls, vec2words, ...))
    return(words)
}

