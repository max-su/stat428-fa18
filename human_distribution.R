human_distribution <- function(n){
  setwd('.')
  data = as.vector(read.csv(file="data.csv",check.names=FALSE, header=FALSE))

  estimators = numeric(10)
  for(i in 1:10){
    estimators[i] = sum(data <= i) / length(data)
  }
  estimators = c(0, estimators)

  sample = c()
  for(i in 1:n){
    x = runif(n = 1, min = 0, max = 1)
    sample = c(sum(estimators < x),sample)
  }
  return(sample)
}

q = human_distribution(10000)
table(q)
