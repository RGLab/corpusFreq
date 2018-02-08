#' @title Interactively find and replace problematic words in a vector
#'
#' @description For each word in the output list from checkSpelling()
#'     find the word in the inputVector and allow the user to enter a
#'     replacement word
#'
#' @param misspelledWords named list of problematic words and suggested replacements
#' @param inputVector character vector
#' @param outFile filepath for where to append lines of code
#' @export
InteractiveFindReplace.vector <- function(misspelledWords, inputVector, outFile = NULL){
    ret <- inputVector
    message("NOTE: leaving the replacement field blank means do not replace.")

    for( nm in names(misspelledWords) ){
        message(paste0("word not found: ", nm))
        message("Possible suggestions: ")
        print(misspelledWords[[nm]])
        rep <- readline(prompt = paste0("enter replacement for ", nm, ": "))
        if(rep == ""){ rep <- nm }
        message("")

        ret <- gsub(pattern = nm, replacement = rep, x = ret)

        if(!is.null(outFile)){
            cat(codeLn, file = outFile, append = TRUE)
        }
    }

    return(ret)
}


#' @title Interactively check spelling in vector against custom corpus
#'
#' @description Given an input vector and output directory, the user may
#'     correct words that are not found in a standard dictionary or a
#'     current list of ImmuneSpace specific terms.
#'
#' @param inputVector vector, only character type will be worked on
#' @param vectorName name of vector for use in output R doc
#' @param outFile filepath for where to append lines of code
#' @export
interactiveSpellCheck.vector <- function(inputVector, vectorName, outputDir){

    # skip if not a character vector
    if(typeof(inputVector) != "character"){
        message("skipping non-character vector")
        return(inputVector)
    }

    # Want file to be executable
    outFile <- paste0(outputDir, "/", vectorName, ".R")

    # write first lines of file
    header <- paste0("# Changes made to ", vectorName, " using interactiveSpellCheck() \n",
                     "# at ", Sys.time(), "\n")
    cat(header, file = outFile, append = TRUE)

    # # run regular spell-check first
    # message("---- Running Spell Check ---- \n")
    # misspelledWords <- checkSpelling(inputVector)
    #
    # # do findReplace
    # tmpVec <- InteractiveFindReplace.vector(misspelledWords,
    #                                         inputVector,
    #                                         outFile)
    #
    # # run checkByContext
    # message("---- Running Context Check ---- \n")
    # contextWords <- checkByContext(tmpVec) # fix to not flag regular words like mosquito
    #
    # # do findReplace
    # resVec <- InteractiveFindReplace.vector(contextWords,
    #                                         tmpVec,
    #                                         outFile)

    # Add newlines in case wrapped in sapply statement
    cat("\n\n", file = outFile, append = TRUE)

    names(resVec) <- vectorName

    return(resVec)

}

#' @title Interactively find and replace problematic words in a data frame
#'
#' @description For each word in the output list from checkSpelling()
#'     find the word in the inputVector and allow the user to enter a
#'     replacement word
#'
#' @param misspelledWords named list of problematic words and suggested replacements
#' @param inputDF character vector
#' @param outFile filepath for where to append lines of code
#' @export
InteractiveFindReplace.df <- function(misspelledWords, inputDF, outFile = NULL){
    message("NOTE: leaving the replacement field blank means do not replace.")

    ret <- inputDF
    for(nm in names(misspelledWords)){
        message(paste0("word not found: ", nm))
        message("Possible suggestions: ")
        print(misspelledWords[[nm]])
        rep <- readline(prompt = paste0("enter replacement for ", nm, ": "))
        if(rep == ""){ rep <- nm }
        message("")

        ret <- data.frame(lapply(ret, function(x){ # need to deal with case issues
            gsub(pattern = nm,
                 replacement = rep,
                 x)}))
        colnames(ret) <- colnames(inputDF)

        if(!is.null(outFile)){
            codeLn <- paste0("\ndata.frame(lapply(inputDF, function(x){ gsub(pattern = '",
                             nm, "', replacement = '", rep, "', x) }))")
            cat(codeLn, file = outFile, append = TRUE)
        }
    }

    return(ret)
}

#' @title Interactively check spelling in dataframe against custom corpus
#'
#' @description Given an input dataframe and output directory, the user may
#'     correct words that are not found in a standard dictionary or a
#'     current list of ImmuneSpace specific terms.
#'
#' @param inputDF dataframe, only character type will be worked on
#' @param outputDir filepath for where to append lines of code
#' @param dfName name of df for use in output R doc, default NULL uses "templateName" attribute
#' @export
interactiveSpellCheck.df <- function(input, outputDir, dfName = NULL){

    if(is.null(dfName)){ dfName <- attr(input, "name")}

    # Want file to be executable
    outFile <- paste0(outputDir, "/", dfName, ".R")

    # write first lines of file
    header <- paste0("# Changes made to ", dfName, " using interactiveSpellCheck() \n",
                     "# at ", Sys.time(), "\n")
    cat(header, file = outFile, append = TRUE)

    # get all unique words from entire DF into one vector
    words <- df2words(input)

    # # run regular spell-check first
    # message("---- Running Spell Check ---- \n")
    # misspelledWords <- checkSpelling(words)
    #
    # # do InteractivefindReplace.DF ... creates named list of find:replace pairs and then iterates
    # tmpDF <- InteractiveFindReplace.df(misspelledWords, inputDF, outFile)
    #
    # # run checkByContext to look at words that are in dictionary, but not accurate (e.g. mistyped)
    # message("---- Running Context Check ---- \n")
    # chkdWords <- words[ !(words %in% names(misspelledWords)) ]
    # contextWords <- chkdWords[ !(chkdWords)]
    #
    # # do findReplace
    # resDF <- InteractiveFindReplace.df(contextWords, tmpDF, outFile)

    # Add newlines in case wrapped in sapply statement
    cat("\n\n", file = outFile, append = TRUE)

    names(resDF) <- dfName

    return(resDF)
}