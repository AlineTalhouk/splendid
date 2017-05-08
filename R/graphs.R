

Discrim_Plot<- function(probs, class){
# probs is a data.frame with rownames are sample names and column names are class names containing the predicted probabilities
# class is the true class label
# This is a 1 vs all approach
  
prev <- prop.table(table(class)) # The prevalence of each class
par(mfrow=c(1,length(unique(class))))
for(i in seq_along(unique(class))){
boxplot(probs[,i]~class, main=colnames(probs)[i], las=2, pch=17)
abline(h=prev[i], col="Gray")
}
}

Calib_Plot <- function(probs,class){
  # probs is a data.frame with rownames are sample names and column names are class names containing the predicted probabilities
  # class is the true class label
  # This is a 1 vs all approach

n <- length(unique(class))
cols <- rainbow(n)
plot(-1,xlim=c(0,1), ylim=c(0,1), xlab="Mean Prediction", ylab="Observed Fraction", main="Reliability Plot")
lines(c(0,1),c(0,1), col="grey")
legend("topleft",unique(class),
       lty=c(1,1), 
       lwd=rep(3,4),col=cols,bty="n")


for(i in seq_along(unique(class))){
  prob <- probs[,i]
  cl <- ifelse(class.test==colnames(probs)[i],1,0)
  bin.pred <- cut(prob,10)
  
  k <- plyr::ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(cl[idx]) / length(cl[idx]), mean(prob[idx]))
  })
  
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]  
  lines(c(0,1),c(0,1), col="grey")
  lw1 <- lowess(k$V2,k$V1)
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]  
  
  points(lw1$x,lw1$y,lwd=3,col=cols[i], type = "l")
}
}