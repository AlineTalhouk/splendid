# splendid (development version)

## New Features

* add custom print method for objects returned from `prediction()`. The output was previously not informative and too long

* add parameter `seed_samp` to `splendid()` to allow setting random seed before subsampling

* `splendid_convert()` is now defunct. Use `splendid_process()` for a more comprehensive data pre-processing step. The new function can `convert` categorical variables to dummy variables as before. Added the ability to `standardize` continuous variables and apply `sampling` techniques to deal with class imbalance. Subsampling can only occur on the training set.

* add parameter `stratify` to allow stratified bootstrap sampling on training set

* use standard convention for confusion matrices: predicted in rows, reference in columns

* replace `MLmetrics::MultiLogLoss()` with `ModelMetrics::mlogLoss()` in `logloss()` since it handles the case when the truth has a category with 0 counts but is represented in the probability matrix

* add NPV and specificity to `evaluation()`

## Minor Changes

* internal functions deprecated and imported from new packages as needed

* update vignette parameter descriptions

## Bug Fixes

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
