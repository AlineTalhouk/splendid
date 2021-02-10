
<!-- README.md is generated from README.Rmd. Please edit that file -->

# splendid

[![Travis build
status](https://travis-ci.org/AlineTalhouk/splendid.svg?branch=master)](https://travis-ci.org/AlineTalhouk/splendid)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/AlineTalhouk/splendid?branch=master&svg=true)](https://ci.appveyor.com/project/AlineTalhouk/splendid)
[![Coverage
status](https://codecov.io/gh/AlineTalhouk/splendid/branch/master/graph/badge.svg)](https://codecov.io/github/AlineTalhouk/splendid?branch=master)

## Overview

The goal of `splendid` is to provide a supervised learning pipeline that
implements major components of a multiclass classification problem. We
guide the user through fitting a classifier, obtaining predictions, and
ultimately evaluating performance using metrics and visualizations.

## Installation

You can install splendid from github with:

``` r
# install.packages("devtools")
devtools::install_github("AlineTalhouk/splendid")
```

## Example

The following example shows how to use the main function of the package,
`splendid()`. A data matrix `hgsc` contains a subset of gene expression
measurements of High Grade Serous Ovarian Carcinoma patients from the
Cancer Genome Atlas publically available datasets. Samples as rows,
features as columns. The function below runs the package through the
`splendid()` function. First we extract the reference class labels (by
TCGA) from the row names of `hgsc`. Then we fit the random forest and
extreme gradient boosting classifiers to one bootstrapped replicate of
the data.

``` r
library(splendid)
data(hgsc)
class <- attr(hgsc, "class.true")
sl_result <- splendid(data = hgsc, class = class, n = 1,
                      algorithms = c("rf", "xgboost"), seed_boot = 5)
str(sl_result, max.level = 2)
#> List of 8
#>  $ models       :List of 2
#>   ..$ rf     :List of 1
#>   ..$ xgboost:List of 1
#>  $ preds        :List of 2
#>   ..$ rf     :List of 1
#>   ..$ xgboost:List of 1
#>  $ evals        :List of 2
#>   ..$ rf     :'data.frame':  34 obs. of  1 variable:
#>   .. ..- attr(*, "err_632plus")= num 0.433
#>   ..$ xgboost:'data.frame':  34 obs. of  1 variable:
#>   .. ..- attr(*, "err_632plus")= num 0.858
#>  $ bests        : Named chr [1:2] "rf" "rf"
#>   ..- attr(*, "names")= chr [1:2] "1" "X1"
#>  $ ensemble_algs: chr "rf"
#>  $ ensemble_mods:List of 1
#>   ..$ :List of 18
#>   .. ..- attr(*, "class")= chr "randomForest"
#>  $ seq_mods     : NULL
#>  $ seq_preds    : NULL
```
