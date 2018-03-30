# Helper Functions ---------------------------------------------



# Tests --------------------------------------------------------
context("vec2words")

test_that("Study argument is not NULL", {
    vec <- 'cd4+ CD4-CD8- il-2 -cd4 <html></br> 1.1 1:1 2-day month-2 mis--take this.sentence cd4+tbet+ cd34-selected avonexnonasciinonascii study"" anti-cd3 ckit+ hg il2-rapa +ova 288_stool 36% 26ng/ul 100ug+papain 21st [ada] wnv-1000 129p2 45yrs 65gad65 72e 6months 10^7 17 organ12 2ki-300'
    expected <- c("cd4+", "cd4-cd8-", "il-2", "cd4", "html", "day", "month", "mis", "take", "this", "sentence", "cd4+tbet+", "cd34-selected", "avonex", "study", "anti-cd3", "ckit", "il2-rapa", "ova", "stool", "papain", "ada", "wnv", "organ", "gad", "months", "yrs")

    res <- vec2words(vec)
    res <- res[ order(res) ]

    expected <- expected[ order(expected) ]
    expect_true( all.equal(expected, res) )
})