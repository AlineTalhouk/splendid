library(splendid)
library(tidyverse)
library(magrittr)

data(hgsc)
class <- stringr::str_split_fixed(rownames(hgsc), "_", n = 2)[, 2]

tr.ind <- sample(1:nrow(hgsc),300,replace = FALSE)
data.train <- hgsc[tr.ind,]
class.train <- class[tr.ind]
data.test <- hgsc[-tr.ind,]
class.test <- class[-tr.ind]

fit<- splendid_model(data.train,class.train,n=10)
se <- splendid_ensemble(fit,data.train, class.train,top = 10)
top <- classification(data.train,class.train,algs = se$bests[1])
probs <- predict(top, data.test, probability = TRUE) %>% attr("prob")
str(probs)

Discrim_Plot(probs, class.test)
Calib_Plot(probs, class.test)
library(glmnet)
load("MultinomialExample.RData")

fit <-nnet::multinom(class.test ~ probs, MaxNWts = 2000,
                 trace = FALSE)

str(, max.level = 2)
str(coef(fit)[[1]])
names(fit)
fit$a0

coef(fit)
test <- fit
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
probs2 <- predict(fit,probs,type="probs")
Calib_Plot(probs2, class.test)
Discrim_Plot(probs, class.test)
