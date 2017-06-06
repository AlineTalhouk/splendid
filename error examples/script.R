library(tidyverse)
library(magrittr)
library(splendid)

s <- 212
class <- readRDS("error examples/FinalR.rds")
npcp <- readRDS("error examples/npcp.rds")

data.train <- npcp %>%
  dplyr::select(-c(CC, Pr)) %>%
  diceR::prepare_data(scale = TRUE, type = "conventional", min.var = 0.5) %>%
  as.data.frame()
class.train <- class[, 1]

res <- splendid_model(data.train, class.train, n = 1, seed = s,
                      algorithms = "rf", rfe = TRUE)
str(res, max.level = 3)
# List of 3
# $ models:List of 1
# ..$ rf:List of 1
# .. ..$ :List of 17
# .. .. ..- attr(*, "class")= chr "rfe"
# $ preds :List of 1
# ..$ rf:List of 1
# .. ..$ : Factor w/ 4 levels "1","2","3","4": 1 1 3 4 4 1 3 1 4 1 ...
# .. .. ..- attr(*, "prob")='data.frame':	572 obs. of  4 variables:
#   .. .. ..- attr(*, "class.true")= Factor w/ 4 levels "1","2","3","4": 1 1 1 4 4 1 3 1 4 1 ...
# .. .. ..- attr(*, "class.thres")= Factor w/ 5 levels "1","2","3","4",..: 1 1 3 4 4 1 3 1 4 1 ...
# .. .. ..- attr(*, "class.prop")= num 0.844
# $ evals :List of 1
# ..$ rf:'data.frame':	24 obs. of  1 variable:
#   .. ..$ 1: num [1:24] NA 0.981 0.892 0.908 0.884 ...
