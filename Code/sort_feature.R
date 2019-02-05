sort_feature <- function(y, x, ne = 2000, na = 7, nb = 250){
  # sort the features
  # ne: number of subsets
  # na: number of maximum features
  # nb: number of bootstrap steps
  feature <- names(x)
  nf <- length(feature)
  n <- length(y)
  score <- list()
  for (i in 1:ne) {
    # loop for different subsets
    vselect <- sample(0:1, nf, replace = T)
    while (sum(vselect) > na && sum(vselect)  == 0) {
      vselect <- sample(0:1, nf, replace = T)
    }
    findex <- which(vselect == 1)
    s <- matrix(rep(0,2*nb) , ncol = 2)
    for (j in 1:nb) {
      # loop for bootstrap samples
      l <- bootstrap(n, n)
      l1 <- l[[1]]
      l2 <- l[[2]]
      if(length(unique(l1)) < 3 | length(l2) < 3) next
      ytrain <- y[l1]
      ytest <- y[l2]
      xtrain <- x[l1,findex, drop = F]
      xtest <- x[l2,findex, drop = F]
      m <- lm(ytrain~., data = xtrain)
      yhat <- predict(m, xtest)
      s[j , 1] <- sum((yhat-ytest)^2)
      s[j , 2] <- length(l2)
    }
    for (j in findex) {
      f <- feature[j]
      score[[f]] <- c(score[[f]], sum(s[,1])/sum(s[,2]))
    }
  }
  result <- c()
  for (i in feature) {
    result <- c(result , mean(score[[i]]))
  }
  names(result) = feature
  return(sort(result, decreasing = F))
}
