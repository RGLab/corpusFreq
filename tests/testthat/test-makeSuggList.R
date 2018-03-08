# Helpers-------------------------------------------------------
tmp <- c("lymphocyte is a dose of reality in a harsh world.",
         "Doses would be too much.",
         "And is critical for this test.")
tmpFt <- makeFreqTbl(tmp)


# Tests --------------------------------------------------------
context("makeSuggList")

test_that("Many short strings use internalFt with stopwords", {
    vec <- c("1 dose", "2 doses", "5 doses", "3 doses", "1 does", "1 dose", "4 doses")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = tmpFt)

    expect_true("dose" %in% res$does)
})

test_that("Corrects non-freqTbl but regular english word. i.e. 'adn' to 'and'", {
    vec <- c("the fox adn the hen are not friends.")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = tmpFt)

    expect_true("and" %in% res$adn)
})

test_that("Corrects biological word present in freqTbl", {
    vec <- c("Why did the lymphocyt cross the road?")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = tmpFt)

    expect_true("lymphocyte" %in% res$lymphocyt)
})

test_that("Single string with few words, no internalFt work", {
    vec <- c("Why did the lymphocyt cross the road?")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = tmpFt)

    expect_false("why" %in% names(res) )
})
