# Obtain raw hgsc data file from diceR package
if (!file.exists("data-raw/hgsc-raw.rda")) {
  library(diceR)
  data(hgsc)
  save(hgsc, file = "data-raw/hgsc-raw.rda")
}

# Transpose while keeping sample ID and gene names
library(magrittr)
raw <- get(load(file = "data-raw/hgsc-raw.rda"))
hgsc <- raw %>% 
  set_rownames(.$UNIQID) %>% 
  extract(-1) %>% 
  t() %>% 
  as.data.frame()
devtools::use_data(hgsc)