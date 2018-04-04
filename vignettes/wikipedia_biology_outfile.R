# work done on 2018-04-04 10:56:23
spellCheckRes <- function(x){
	x <- gsub(pattern = 'baad', replacement = 'bad', x, ignore.case = TRUE)
	x <- gsub(pattern = 'biologickal', replacement = 'biological', x, ignore.case = TRUE)
	x <- gsub(pattern = 'terme', replacement = 'term', x, ignore.case = TRUE)
	x <- gsub(pattern = 'varietions', replacement = 'variations', x, ignore.case = TRUE)
	x <- gsub(pattern = 'selectiv', replacement = 'selective', x, ignore.case = TRUE)
}