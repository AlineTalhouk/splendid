library(splendid)
library(tidyverse)
library(magrittr)
setwd("/Users/atalhouk/Repositories/splendid/error examples")
s<-114
algs<- "ldaRfe"
class <-  readRDS("FinalR.rds")
npcp <- readRDS("npcp.rds")

data.train <- npcp %>%
  select(-c(CC,Pr))%>%
  diceR::prepare_data(.,scale = TRUE,type = "conventional", min.var = 0.5) %>%
  as.data.frame(.)

class.train <-class[,1]

splendid_model(data.train,class.train, n=1, seed = s,algorithms = "pam",
               rfe=TRUE)
