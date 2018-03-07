# Helper Functions ---------------------------------------------



# Tests --------------------------------------------------------
context("makeSuggList")

test_that("Corrects internally inconsistent words. i.e. 'does' to 'dose'", {
    vec <- c("1 dose", "2 doses", "5 doses", "3 doses", "1 does", "1 dose", "4 doses")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = bioCorpus::allFreqTbl)

    expect_true("dose" %in% res$does)
})

test_that("Corrects non-freqTbl but regular english word. i.e. 'adn' to 'and'", {
    vec <- c("the fox adn the hen are not friends.")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = bioCorpus::allFreqTbl)

    expect_true("and" %in% res$adn)
})

test_that("Corrects word in freqTbl but not regular english word. i.e. 'lymphocyt' to 'lymphocyte'", {
    vec <- c("Why did the lymphocyt cross the road?")

    # TODO: parse vec to words then generate results from method in a named
    # list format
    words <- vec2words(vec)
    res <- corpusFreq:::makeSuggList(words, freqTbl = bioCorpus::allFreqTbl)

    expect_true("lymphocyte" %in% res$lymphocyt)
})