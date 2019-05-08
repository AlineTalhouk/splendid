# splendid (development version)

* use standard convention for confusion matrices: predicted in rows, reference in columns

* internal functions deprecated and imported from new packages as needed

* replace `MLmetrics::MultiLogLoss()` with `ModelMetrics::mlogLoss()` in `logloss()` since it handles the case when the truth has a category with 0 counts but is represented in the probability matrix

* add NPV and specificity to `evaluation()`

# splendid 0.1.0

* Default seed parameter value `NULL` does not invoke `set.seed()`

* Extend random seed parameter to more algorithms

* Extended categorical variable conversion to `classification()`

* Reinstate tidy evaluation semantics after package dependencies updated

* Improved RFE interface

* Added AdaBoost.M1 algorithm

* Added a `NEWS.md` file to track changes to the package.
