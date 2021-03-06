% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freqSugg.R
\name{freqSugg}
\alias{freqSugg}
\title{provide suggestions for misspelled words from frequency table}
\usage{
freqSugg(badWords, freqTbl, sdBoundary = 2)
}
\arguments{
\item{badWords}{misspelled words}

\item{freqTbl}{frequency table}

\item{sdBoundary}{integer for stringdist max distance to allow for frequencyTbl suggestions}
}
\description{
provides suggestions for misspelled words or those that are
    less common in the internal frequency table
}
