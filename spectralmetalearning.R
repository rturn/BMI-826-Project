eigensml = function(predictors) {
  e = eigen(predictors%*%t(predictors))
  accuracies = e$vectors[,1]
  total = sum(accuracies)
  weights = accuracies / total
  prediction = weights%*%predictors
  return(as.vector(prediction))
}

svdsml = function(predictors) {
  s = svd(predictors)
  accuracies = s$u[,1]
  total = sum(accuracies)
  weights = accuracies / total
  prediction = weights%*%predictors
  return(as.vector(prediction))
}

plotaccuracies = function(predictors) {
  s = svd(predictors)
  accuracies = abs(s$u[,1])
  plot(accuracies, ylab = "Balanced Accuracy of Predictor", main = "Eigenplot of Predictor Matrix")
}