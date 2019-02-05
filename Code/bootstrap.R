bootstrap <- function(n , m){
  # get m bootstrap samples from 1 to n
  l <- 1:n
  l1 <- sample(l, m, replace = T) # bootstrap samples
  l2 <- l[!l %in% l1] # rest of the list 
  return(list(l1,l2))
}