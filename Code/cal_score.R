cal_score <- function(y, x, intercept=T, seed=123, rep_num=500){
  # evaluate the subset
  set.seed(seed)
  mmse <- matrix(rep(0,2*rep_num) , ncol = 2)
  n <- length(y)
  for (i in 1:rep_num) {
    l <- bootstrap(n,n)
    l1 <- l[[1]]
    l2 <- l[[2]]
    if(intercept){
      m <- lm(y~. , data = x)
    }else{
      m <- lm(y~-1+., data  = x)
    }
    ytrue <- y[l2]
    yhat <- predict(m,x[l2,,drop = F])
    mmse[i , 1] <- sum((ytrue-yhat)^2)
    mmse[i , 2] <- length(ytrue)
  }
  return(sum(mmse[,1])/sum(mmse[,2]))
}