# Helper Functions ---------------------------------------------



# Tests --------------------------------------------------------
context("vec2words")

test_that("Study argument is not NULL", {
    vec <- "cd4+ CD4-CD8- il-2 -cd4 <html></br> 1.1 1:1 2-day month-2 mis--take this.sentence"
    expected <- c("cd4+", "cd4-cd8-", "il-2", "cd4", "day", "month", "mis", "take", "this", "sentence")

    res <- vec2Words(vec)
    res <- res[ order(res) ]

    expected <- expected[ order(expected) ]
    expect_true( all.equal(expected, res) )
})