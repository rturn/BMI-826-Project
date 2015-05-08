#' Take the SML prediction of multiple predictions using the eigenvectors method. 
#' 
#' @param predictors A matrix of predictors, with the group as the row, and predictions as the columns
#' 
#' @return The SML combined prediction of the predictors
#' @examples
#' \dontrun{prediction = eigensml(multiplePredictions)}
#' 
#' @seealso \code{\link{svdsml}}
#' @export
#' 
eigensml = function(predictors) {
  stopifnot(nrow(predictors) < ncol(predictors))
  e = eigen(predictors%*%t(predictors))
  accuracies = e$vectors[,1]
  total = sum(accuracies)
  weights = accuracies / total
  prediction = weights%*%predictors
  return(as.vector(prediction))
}

#' Take the SML prediction of multiple predictions using the SVD method. 
#' 
#' @param predictors A matrix of predictors, with the group as the row, and predictions as the columns
#' 
#' @return The SML combined prediction of the predictors
#' @examples
#' \dontrun{prediction = svdsml(multiplePredictions)}
#' 
#' @seealso \code{\link{eigensml}}
#' @export
#'
svdsml = function(predictors) {
  stopifnot(nrow(predictors) < ncol(predictors))
  s = svd(predictors)
  accuracies = s$u[,1]
  total = sum(accuracies)
  weights = accuracies / total
  prediction = weights%*%predictors
  return(as.vector(prediction))
}

#' Plot the balanced accuracies of the predictors, as estimated by the SML
#' 
#' @param predictors A matrix of predictors, with the group as the row, and predictions as the columns
#' 
#' @return A plot of the predictors accuracies
#' @examples
#' \dontrun{plotaccuracies(multiplePredictions)}
#' 
#' @export
#'
plotaccuracies = function(predictors) {
  stopifnot(nrow(predictors) < ncol(predictors))
  s = svd(predictors)
  accuracies = abs(s$u[,1])
  plot(accuracies, ylab = "Balanced Accuracy of Predictor", main = "Eigenplot of Predictor Matrix")
}