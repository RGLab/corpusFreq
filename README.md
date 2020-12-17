# corpusFreq

 <!-- badges: start -->
 [![R-CMD-check](https://github.com/RGLab/corpusFreq/workflows/R-CMD-check/badge.svg)](https://github.com/RGLab/corpusFreq/actions)
 [![Codecov test coverage](https://codecov.io/gh/RGLab/corpusFreq/branch/master/graph/badge.svg)](https://codecov.io/gh/RGLab/corpusFreq?branch=master)
 [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
  <!-- badges: end -->

A utility package for creating word-frequency tables from various data types and then performing interactive spellchecking with these frequency tables.

The parsing functions are specifically designed with Immunological data in mind and focus on use cases such as manuscript abstracts or clinical forms.

## Installation

The package can be downloaded from the RGLab repo:

``` r
# install.packages("remotes")
remotes::install_github("RGLab/corpusFreq")
```

## Usage

The general idea is for users to create a corpus, or canonical frequency table, to use in spellchecking other data in an R session.  Besides being biology-focused, the main difference between `corpusFreq` and other spellchecking packages, e.g. `refinr` or `hunspell`, is that the interactive methods here use both string-distance and frequency of words in the corpus to determine the most likely correct replacement.  Other spellchecking packages do not take into account the frequency of words, rather they focus on a variation of string distance, stemming, or ngrams to compare incorrect words with possible replacements.

The "hello world" example:

``` r
library(corpusFreq)

# Make frequency table from large text document
myData <- read.table("myText.tsv", sep = "\t", stringsAsFactors = FALSE)
ft <- makeFreqTbl(myData)

# Use frequency to spellcheck other texts
myAbstract <- "This is my stuby loooking at CD4+ cells, but not CD4-CD8- ones."
result <- interactiveSpellCheck(input = myAbstract,
                                name = "CD4_study",
                                outputDir = "home/CD4_work",
                                freqTbl = ft)
print(result)
"This is my study looking at CD4+ cells, but not CD4-CD8- ones."
```

## Use with related package [`BioCorpus`](https://github.com/RGLab/BioCorpus)

If you are working on biological text and want to use a frequency table that has already been curated, you can use data in the `BioCorpus` data package.  There are four frequency tables representing different public or soon-to-be-public database: ImmPort, GEO, Center for AIDS Vaccine Data, and ONB.  The combined version is also found in the package and can be loading by doing the following:

``` r
remotes::install_github("RGLab/BioCorpus")
library(BioCorpus)
ft <- BioCorpus::allDataFT
```

## Examples & Documentation

For more advanced examples and detailed documentation, see the package vignettes.
