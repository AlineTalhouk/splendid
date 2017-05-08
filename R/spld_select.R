spld_select <- function(bt, algorithms=NULL){
algs <- names(bt$evals)
a <- abind::abind(bt$evals, along = 3)

# Rank Aggregation
bests <- 	apply(a,2,function(x){x %>% 
                          t() %>% 
                          data.frame() %>% 
                           purrr::map_df(~ algs[order(rank(-.x, ties.method = "random"))]) %>% 
                           t() %>% 
                           RankAggreg::RankAggreg(., ncol(.), method = "GA", 
                                                  verbose = FALSE, maxIter = 1000)%>% 
                           magrittr::use_series("top.list")})

bestofbests <- bests[1,]  %>% 
  table() %>% 
  which.max() %>% 
  names()

# Highest median Bootstrap
df <- map(bt$evals, ~apply(.x,1,median)) %>% 
  data.frame() %>%
  t() %>% 
  apply (.,2, function(x){algs[order(rank(-x, ties.method = "random"))]}) %>% 
  t()%>% 
  RankAggreg::RankAggreg(., ncol(.), method = "GA", 
                         verbose = FALSE, maxIter = 1000)
}

