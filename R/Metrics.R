#' Square Confusion Matrix
#' @description  Returns a square confusion matrix
#' @export
#' @param Y A numeric vector for the ground truth labels
#' @param Y_hat A numeric vector for the predicted Labels
#' @return A confusion table
#' @examples
#' squareConfusionTable(Y = sample(1:2, size = 10, replace = TRUE), Y_hat = rep(1, 10))
squareConfusionTable <- function(Y, Y_hat) {
  Y <- factor(Y)
  Y_hat <- factor(Y_hat, levels = levels(Y))
  table(Y, Y_hat)
}

#' Get Metrics
#' @description  Get all possible task names
#' @export
#' @param A valid task, can be 'MultClass' or 'BinClass'.
#' @return All metrics names for that taks
#' @examples
#' GetPossibleTasks()
GetMetrics <- function(task){
  if(task == 'MultClass'){
    return(MultiClassMetricsNames())
  }

  if(task == 'BinClass'){
    return(BinClassMetricsNames())
  }

  if(task == 'Regression'){
    return(RegressionMetricsNames())
  }

  stop('Invalid task argument')

  return(NULL)
}

##### Binnary Classification

#' Binary Classification Metrics
#' @export
#' @description Returns a character string vector containning all binary classification metrics.
#' @return character string vector with all possible binary classification metrics.
#' @examples
#' BinClassMetricsNames()
BinClassMetricsNames <- function(){
  metricsNames <- c('Accuracy',
                    'Precision',
                    'Recall',
                    'F1 score',
                    'Log Loss',
                    'AUC' )
  metricsNames
}

#' Accuracy
#' @export
#' @description Returns the Accuracy for a classification problem.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @return A numeric value corresponding to the Accuracy of a classification problem
#' @examples
#' Y = sample(x = c(1,2), size = 10, replace = TRUE)
#' Y_hat = sample(x = c(1,2), size = 10, replace = TRUE)
#' Accuracy(Y = Y, Y_hat = Y_hat)
Accuracy <- function(Y, Y_hat) {
  Acc <- sum(Y == Y_hat) / length(Y)
  return(Acc)
}

#' AUC
#' @export
#' @description Returns the Area Under the Curve for a binarry classification problem.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @return A numeric value corresponding to the AUC of binary classification problem
#' @examples
#' Y = sample(x = c(1,2), size = 10, replace = TRUE)
#' Y_hat = sample(x = c(1,2), size = 10, replace = TRUE)
#' AUC(Y = Y, Y_hat = Y_hat)
AUC <- function(Y, Y_hat) {
  rank <- rank(Y_hat)
  nPos <- as.double(sum(Y == unique(Y)[1]))
  nNeg <- as.double(sum(Y == unique(Y)[2]))
  AUC <- (sum(rank[Y == 1]) - nPos * (nPos + 1) / 2) / ((nPos * nNeg))
  return(AUC)
}

#' LogLoss
#' @export
#' @description Returns the Logarithmic Loss for classification problem.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @return A numeric value corresponding to the LogLoss of binary classification problem
#' @examples
#' Y = sample(x = c(1,2), size = 10, replace = TRUE)
#' Y_hat = sample(x = c(1,2), size = 10, replace = TRUE)
#' LogLoss(Y = Y, Y_hat = Y_hat)
LogLoss <- function(Y, Y_hat) {
  eps <- 1e-15
  Y_hat <- pmax(pmin(Y_hat, 1 - eps), eps)
  logLoss <- -mean(Y * log(Y_hat) + (1 - Y) * log(1 - Y_hat))
  return(logLoss)
}

#' BinClassMetricsNames
#' @export
#' @description Returns a binaryResultList with binary classificaiton metrics.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @param MetricsNames can be found at BinClassMetricsNames()
#' @return A binaryResultList with results
#' @examples
#' Y = sample(x = c(1,2), size = 10, replace = TRUE)
#' Y_hat = sample(x = c(1,2), size = 10, replace = TRUE)
#' BinClassMetrics(Y = Y, Y_hat = Y_hat, MetricsNames = BinClassMetricsNames())
BinClassMetrics <- function(Y, Y_hat,
                            MetricsNames = BinClassMetricsNames()){
  if(length(unique(Y)) != 2){
    stop('Not binary classification problem !!! \nMore then 2 different types of values in Y vector!!!')
  }

  if(length(Y) != length(Y_hat)){
    stop('Y and Y_hat have different sizes!!!')
  }

  if( any( (unique(Y_hat) %in% unique(Y)) == FALSE) ){
    stop('Invalid values for Y_hat !!!')
  }

  if( any( (MetricsNames %in% BinClassMetricsNames()) == FALSE) ){
    stop('Invalid values for MetricsNames !!!')
  }

  resultList <- list()
  eps <- 1e-15

  ### MICRO
  confusionMatrix <- squareConfusionTable(Y = Y, Y_hat = Y_hat)
  accuracy <- sum(diag(confusionMatrix) / ( sum(confusionMatrix) + eps))
  precision <- diag(confusionMatrix) / (rowSums(confusionMatrix) + eps)
  recall <- diag(confusionMatrix) / (colSums(confusionMatrix) + eps)
  f1 <- diag(confusionMatrix) / (colSums(confusionMatrix) + eps)
  logLoss <- LogLoss(Y = Y, Y_hat = Y_hat)
  AUC <- AUC(Y = Y, Y_hat = Y_hat)

  if("Accuracy" %in% MetricsNames){
    resultList[['Accuracy']] <- accuracy
  }

  if("Precision" %in% MetricsNames){
    resultList[['Precision']] <- precision
  }

  if("Recall" %in% MetricsNames){
    resultList[['Recall']] <- recall
  }

  if("F1" %in% MetricsNames){
    resultList[['F1']] <- f1
  }

  if('Log Loss' %in% MetricsNames){
    resultList[['Log Loss']] <- logLoss
  }

  if("AUC" %in% MetricsNames){
    resultList[['AUC']] <- AUC
  }

  class(resultList) <- append(class(resultList), 'binaryResultList')
  resultList
}

##### MultClass Classification
#' Multi Class Classification Metrics
#' @export
#' @description Returns a character string vector containning all multi class classification metrics.
#' @return character string vector with all possible multi class classification metrics.
#' @examples
#' MultiClassMetricsNames()
MultiClassMetricsNames <- function(){
  metricsNames <- c('Accuracy Macro',
                    'Accuracy Micro',
                    'Precision Macro',
                    'Precision Micro',
                    'Recall Macro',
                    'Recall Micro',
                    'F1 Macro',
                    'F1 Micro')
  metricsNames
}

#' MultiLogLoss
#' @export
#' @description Returns the Logarithmic Loss for multi class classification problem.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @return A numeric value corresponding to the LogLoss of binary classification problem
#' @examples
#' Y = sample(x = c(1,2), size = 10, replace = TRUE)
#' Y_hat = sample(x = c(1,2), size = 10, replace = TRUE)
#' MultiLogLoss(Y = Y, Y_hat = Y_hat)
MultiLogLoss <- function(Y, Y_hat) {
  if (is.matrix(Y) == FALSE) {
    Y <- model.matrix(~ 0 + ., data.frame(as.character(Y)))
  }
  eps <- 1e-15
  n <- nrow(Y_hat)
  Y_hat <- pmax(pmin(Y_hat, 1 - eps), eps)
  multiLogLoss <- (-1 / n) * sum(Y * log(Y_hat))
  multiLogLoss
}

#' MultClassMetrics
#' @export
#' @description Returns a multiClassResultList with multi class classificaiton metrics.
#' @param Y Ground truth numeric vector.
#' @param Y_hat Predicted Labels numeric vector.
#' @param MetricsNames Metrics names, avilable can be found with MultiClassMetricsNames() .
#' @return A multiClassResultList with results
#' @examples
#' Y = sample(x = c(1,2, 3), size = 20, replace = TRUE)
#' Y_hat = sample(x = c(1, 2, 3), size = 20, replace = TRUE)
#' MultClassMetrics(Y = Y, Y_hat = Y_hat, MetricsNames = MultiClassMetricsNames())
MultClassMetrics <- function(Y, Y_hat,
                             MetricsNames){

  if(length(Y) != length(Y_hat)){
    stop('Y and Y_hat have different sizes!!!')
  }

  if( any( (unique(Y_hat) %in% unique(Y)) == FALSE) ){
    stop('Invalid values for Y_hat !!!')
  }

  if( any( (MetricsNames %in% MultiClassMetricsNames()) == FALSE) ){
    stop('Invalid values for MetricsNames !!!')
  }

  resultList <- list()
  eps <- 1e-15

  ### MICRO
  confusionMatrix <- squareConfusionTable(Y = Y, Y_hat = Y_hat)
  macroAccuracy <-  mean(sum(diag(confusionMatrix)) / sum(confusionMatrix))
  macroPrecision <-  mean(diag(confusionMatrix) / (rowSums(confusionMatrix) + eps))
  macroRecall <-  mean(diag(confusionMatrix) / (colSums(confusionMatrix) + eps))
  macroF1 <- mean((2 * macroPrecision * macroRecall) / (macroPrecision + macroRecall + eps))

  ### MACRO
  nObs <- sum(confusionMatrix)
  nVars <- nrow(confusionMatrix) # number of classes
  nObsPerClass <- apply(confusionMatrix, 1, sum)
  nPredPerClass <- apply(confusionMatrix, 2, sum)
  oneVsAll <- lapply(1 : nVars,
                     function(i){
                       result = c(confusionMatrix[i,i],
                                  nObsPerClass[i] - confusionMatrix[i,i],
                                  nPredPerClass[i] - confusionMatrix[i,i],
                                  nObs-nObsPerClass[i] - nPredPerClass[i] + confusionMatrix[i,i])
                       return( matrix(result, nrow = 2, byrow = T) )
                     })
  microConfusionMatrix <- Reduce('+', oneVsAll)
  microTruePositive <- microConfusionMatrix[1, 1]
  microFalseNegative <- microConfusionMatrix[2, 1]
  microFalsePositive <- microConfusionMatrix[1, 2]
  microTrueNegative <- microConfusionMatrix[2, 2]

  microAccuracy <- (microTruePositive + microTrueNegative) / (sum(microConfusionMatrix) + eps)
  microPrecision <- (microTruePositive) / (microTruePositive + microFalsePositive + eps)
  microRecall <- (microTruePositive) / (microTruePositive + microFalseNegative + eps)
  microAccuracy <- (microTruePositive + microTrueNegative) / (sum(microConfusionMatrix) + eps)
  microF1 <- (2 * microPrecision * microRecall) / (microPrecision + microRecall + eps)


  if("Accuracy Macro" %in% MetricsNames){
    resultList[['Accuracy Macro']] <- macroAccuracy
  }

  if("Accuracy Micro" %in% MetricsNames){
    resultList[['Accuracy Micro']] <- microAccuracy
  }

  if("Precision Macro" %in% MetricsNames){
    resultList[['Precision Macro']] <- macroPrecision
  }

  if("Precision Micro" %in% MetricsNames){
    resultList[['Precision Micro']] <- microPrecision
  }

  if("Recall Macro" %in% MetricsNames){
    resultList[['Recall Macro']] <- macroRecall
  }

  if("Recall Micro" %in% MetricsNames){
    resultList[['Recall Micro']] <- microRecall
  }

  if("F1 Macro" %in% MetricsNames){
    resultList[['F1 Macro']] <- macroF1
  }

  if("F1 Micro" %in% MetricsNames){
    resultList[['F1 Micro']] <- microF1
  }

  class(resultList) <- append(class(resultList), 'multiClassResultList')
  resultList
}
