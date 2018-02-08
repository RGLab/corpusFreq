
vec2words <- function(vec, reduce = TRUE, rmStopWords = TRUE){

    # De-dupe elements of vector b/c assume copy-pasted and want
    # to base frequencies on typed words.
    if(reduce){ vec <- unique(vec) }

    vec <- vec[ !is.na(vec) ] # remove NA elements of vector
    vec <- tolower(vec) # lowercase
    vec <- gsub("<\\/|\\\n", " ", vec) # for html elements , e.g. </p>
    vec <- gsub(" [[:punct:]]+|[[:punct:]]+ ", " ", vec) # for all punct at start or end of word
    vec <- gsub("\\.|\\(|\\)|\\?|=|\\,|>|<|#|;|'|:", " ", vec) # for non-hyphen punct in middle of words

    words <- unlist(strsplit(vec, " ")) # make list of words / split on spaces
    words <- gsub("\\b(\\-|\\*|\\/)", "", words) # oddball cases with punct at edge
    words <- words[ grep("^\\d+$", words, invert = T) ] # remove integers
    words <- words[ grep("\\d+[[:punct:]]\\d+", words, invert = T) ] # remove things like '1:1' or '0.123'
    words <- words[ nchar(words) > 2 ] # remove words with only 1 character, possibly due to subs

    if(rmStopWords){ words <- words[ !(words %in% stopwords::stopwords()) ] }

    # handle hyphenated datetime words like '2-week' 'year-1'
    words[ grep("hour", words)] <- "hour"
    words[ grep("day", words)] <- "day"
    words[ grep("week", words)] <- "week"
    words[ grep("month", words)] <- "month"
    words[ grep("year", words)] <- "year"

    return(words)
}

# wrapper for vec2words for dataframe
df2words <- function(df, reduce = TRUE){
    words <- unlist(apply(df, 2, vec2words, reduce))
    return(words)
}

# wrapper for vec2words for list of vectors
list2words <- function(ls, reduce = TRUE){
    words <- unlist(lapply(ls, vec2words, reduce))
    return(words)
}

