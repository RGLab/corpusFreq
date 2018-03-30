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
    vec <- gsub("<\\/|\\\n", "", vec) # for html elements , e.g. </p>, remove punct and two-letter or less rm later
    vec <- gsub("\\.|\\(|\\)|\\?|=|\\,|>|<|#|;|'|:|_|\\^|\\/|%|\\*|\\[|\\]|~", " ", vec) # for non-hyphen punct throughout
    vec <- gsub('"', "", vec) # remove double quotes

    # punctuation tidying post splitting
    words <- unlist(strsplit(vec, " ")) # make list of words / split on spaces
    words <- gsub("^(\\-|\\+)", "", words) # oddball cases with leading '+' or '-'
    words <- gsub("--", "-", words) # double hyphen accidents
    words <- gsub("nonascii", "", words) # for issues with non-ascii characters

    # Don't touch CD / IL markers
    cdil <- words[ grep("cd\\d{1,3}|il-\\d{1,2}|il\\d{1,2}", words) ]

    # handle hyphenated terms with multiple letters on both sides and non-CD/IL terms
    noncdil <- words[ !(words %in% cdil) ]
    hyps <- noncdil[ grep("^[[:alnum:]]{1,}-[[:alnum:]]{1,}", noncdil) ] # avoid splitting terms like 'b-cell'
    hyps <- unlist(strsplit(hyps, "-"))
    nohyp <- noncdil[ grep("^[[:alnum:]]{1,}-[[:alnum:]]{1,}", noncdil, invert = T) ]
    nohyp <- gsub("\\+$", "", nohyp) # remove trailing plus from non-hyphenated terms
    nohyp <- unlist(strsplit(nohyp, "+", fixed = T)) # in cases such as 100ug+papain
    noncdil <- c(nohyp, hyps)
    noncdil <- gsub("\\d|-", "", noncdil)

    # rm single letters and empties from anywhere
    words <- c(cdil, noncdil)
    words <- words[ nchar(words) > 2 ]

    # remove common non-analytical terms, e.g. 'of' and 'the'
    if(rmStopWords){ words <- words[ !(words %in% stopwords::stopwords()) ] }

    return(words)
}

#' @title parse a df to words with specialized handling for scientific terms
#'
#' @description parse a df to words carefully to ensure that scientific terms
#'     which may include hyphens or plus-signs are not altered
#'
#' @param df dataframe object
#' @param ... arguments to pass to vec2words
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
#' @param ls list object
#' @param ... arguments to pass to vec2words
#' @export
list2words <- function(ls, ...){
    words <- unlist(lapply(ls, vec2words, ...))
    return(words)
}

