# splendid (development version)

## New Features

* use `yardstick` package for most evaluation metrics

* pass `seeds` to `caret::trainControl()` for reproducible tuning (#48)

* add `roc_plot()` for plotting multi-class ROC curves

* add custom print method for objects returned from `prediction()`. The output was previously not informative and too long

* add parameter `seed_samp` to `splendid()` to allow setting random seed before subsampling

* `splendid_convert()` is now defunct. Use `splendid_process()` for a more comprehensive data pre-processing step. The new function can `convert` categorical variables to dummy variables as before. Added the ability to `standardize` continuous variables and apply `sampling` techniques to deal with class imbalance. Subsampling can only occur on the training set.

* add parameter `stratify` to allow stratified bootstrap sampling on training set

* use standard convention for confusion matrices: predicted in rows, reference in columns

* replace `MLmetrics::MultiLogLoss()` with `ModelMetrics::mlogLoss()` in `logloss()` since it handles the case when the truth has a category with 0 counts but is represented in the probability matrix

* add NPV and specificity to `evaluation()`

## Minor Changes

* increase minimum R version to 3.6.0

* move packages used for classification to `Suggests` to reduce the number of dependencies and are used conditionally

* remove deprecated `context()` from tests

* update roxygen and docs

* internal functions deprecated and imported from new packages as needed

* update vignette parameter descriptions

## Bug Fixes

* put the macro and micro averaged ROC curves at the end of legend in `roc_plot()`

* suppress warnings in call to `multiROC::multi_roc()` after updates to `stats:::regularize.values()` in R-3.6.0 passes `warn.collapsing = TRUE` if there is no value for `ties` in `stats::approx()`

* in sequential method, remove bootstrap iterations with an undefined F1-measure from average calculation

* increase `perc.over` and `perc.under` in SMOTE subsampling to ensure the second to smallest class has > 0 cases

* decrease `minsplit` in `adaboost` so fewer observations are needed to split a node in the rpart classifier

* fix `num_class` in `xgboost`: number of classes should be taken from factor levels (some might be dropped from training set)

* fix factor order in `class_threshold()` to take from column order of associated probability matrix

# splendid 0.1.0

* Default seed parameter value `NULL` does not invoke `set.seed()`

* Extend random seed parameter to more algorithms

* Extended categorical variable conversion to `classification()`

* Reinstate tidy evaluation semantics after package dependencies updated

* Improved RFE interface

* Added AdaBoost.M1 algorithm

* Added a `NEWS.md` file to track changes to the package.
