require('e1071')

### Classification

#' K Nearest Neighbours
#' @description  It is the K Nearest Neighbours method
#' @export
#' @import class
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param K An integer as a parameter for the knn method.
#' @return predicted labels
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- as.numeric( X_train[, 1] ** 2 - X_train[, 2] > 0)
#' Y_test <- as.numeric(X_test[, 1] ** 2 - X_test[, 2] > 0)
#' K <- 5
#' Y_predicted <- knn(X_train = X_train, Y_train = Y_train, X_test = X_test, K = K)
#' print(table(Y_test, Y_predicted))
knn <- function(X_train, Y_train, X_test,
                K){
  Y_hat <- class::knn(train = X_train, X_test, as.factor(Y_train))
  Y_hat <- as.numeric(as.character(Y_hat))
  Y_hat
}
#' Linear Support Vector Machines
#' @description  It is the Support Vector Machines without a kernel
#' @export
#' @import e1071
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param C A numeric value that represents the cost of constraints violation of the regularization term in the Lagrange formulation.
#' @return predicted labels
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- as.numeric( X_train[, 1] ** 2 - X_train[, 2] > 0)
#' Y_test <- as.numeric(X_test[, 1] ** 2 - X_test[, 2] > 0)
#' C <- 5
#' Y_predicted <- svm_linear(X_train = X_train, Y_train = Y_train, X_test = X_test, C = C)
#' print(table(Y_test, Y_predicted))
svm_linear <- function(X_train, Y_train, X_test,
                       C){
  model <- e1071::svm(x = X_train, y = as.factor(Y_train),
                      kernel = 'linear',
                      cost = C)
  Y_hat <- as.numeric(as.character(predict(model, X_test)))
  Y_hat
}

#' Support Vector Machines with Polinomial Kernel
#' @description  It is the Support Vector Machines with a polinomial kernel
#' @export
#' @import e1071
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param degree A integer that represents the kernel polynomial degree
#' @param gamma A numeric value as the kernel coefficient.
#' @param coef0 A numeric value for kernel independent term.
#' @param C A numeric value that represents the cost of constraints violation of the regularization term in the Lagrange formulation.
#' @return predicted labels
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- as.numeric( X_train[, 1] ** 2 - X_train[, 2] > 0)
#' Y_test <- as.numeric(X_test[, 1] ** 2 - X_test[, 2] > 0)
#' C <- 5
#' coef0 <- 0
#' degree <- 5
#' gamma <- 0.5
#' Y_predicted <- svm_poli(X_train = X_train, Y_train = Y_train, X_test = X_test, C = C, coef0 = coef0, degree = degree, gamma = gamma)
#' print(table(Y_test, Y_predicted))
svm_poli <- function(X_train, Y_train, X_test,
                     degree, gamma, coef0, C){
  model <- e1071::svm(x = X_train, y = as.factor(Y_train),
                      kernel = 'polynomial',
                      degree = degree,
                      gamma = gamma,
                      coef0 = coef0,
                      cost = C)
  Y_hat <- as.numeric( predict(model, X_test) )
  Y_hat
}

#' Support Vector Machines with Radial Kernel
#' @description  It is the Support Vector Machines with a radial kernel
#' @export
#' @import e1071
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param gamma A numeric value as the kernel coefficient.
#' @param C A numeric value that represents the cost of constraints violation of the regularization term in the Lagrange formulation.
#' @return predicted labels
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- as.numeric( X_train[, 1] ** 2 - X_train[, 2] > 0)
#' Y_test <- as.numeric(X_test[, 1] ** 2 - X_test[, 2] > 0)
#' C <- 5
#' gamma <- 0.5
#' Y_predicted <- svm_radial(X_train = X_train, Y_train = Y_train, X_test = X_test, C = C, gamma = gamma)
#' print(table(Y_test, Y_predicted))
svm_radial <- function(X_train, Y_train, X_test,
                       gamma, C){
  model <- e1071::svm(x = X_train, y = as.factor(Y_train),
                      kernel = 'radial',
                      gamma = gamma,
                      cost = C)
  Y_hat <- as.numeric( predict(model, X_test) )
  Y_hat
}


#' Create Algorithm
#' @description  It is an auxiliary function to help creating new algorithms in the package standarts.
#' @export
#' @param algoName A character string that represents the algorithm name.
#' @param algoFun A function class object.
#' @param task A character string vector, cointaning 'MultClass' or/and 'BinClass' and/or 'Regression'.
#' @param paramList A list of all parameters and their values to be tested.
#' @return An object of class algorithm.
#' @examples
#' algoName <- 'myAlgo'
#' algoFun <- function(X_train, Y_train, X_test, param1){
#'  set.seed(param1)
#'  sample(Y_train, size = nrow(X_test), replace = TRUE)}
#' task <- c('MultClass', 'BinClass')
#' paramList = list(param1 = 1:3)
#' CreateAlgo(algoName = algoName, algoFun = algoFun, task = task, paramList = paramList)
CreateAlgo <- function(algoName,
                       algoFun,
                       task,
                       paramList){
  if(class(algoName) != 'character'){
    stop('Invalid algoName class !!!')
  }

  if(class(algoFun) != 'function'){
    stop('Invalid function class !!!')
  }

  if(any( (task %in% c('MultClass', 'BinClass', 'Regression')) == FALSE)){
    stop('Invalid task !!!')
  }

  if(!anyNA(paramList)){
    if(class(paramList) != 'list'){
      stop('Invalid paramList !!!')
      }
    }

  algo <- list(algoName = algoName,
               algoFun = algoFun,
               task = task,
               paramList = paramList)
  class(algo) <- append(class(x = algo), 'algorithm')
  algo
}

#' Generates all MultClass Classification Alogrithms
#' @description  It is an auxiliary that allows to load all of the packages multclass classification alogrithms.
#' @export
#' @return Returns a list with all MultClass Classification Alogrithms
#' @examples
#' GetAllMultClassAlgo()
GetAllMultClassAlgo <- function(){
  allMultClassAlgo <- list()
  allMultClassAlgo[[1]] <- CreateAlgo(algoName = 'knn',
                                      algoFun = knn,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(K = 1:10))

  allMultClassAlgo[[2]] <- CreateAlgo(algoName = 'svm_linear',
                                      algoFun = svm_linear,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(C = c(1, 10, 100)))

  allMultClassAlgo[[3]] <- CreateAlgo(algoName = 'svm_poli',
                                      algoFun = svm_poli,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(C = c(1, 10, 100),
                                                       degree = 2:5,
                                                       gamma = c(0.1, 0.01, 0.001),
                                                       coef0 = 0))
  allMultClassAlgo[[4]] <- CreateAlgo(algoName = 'svm_radial',
                                      algoFun = svm_radial,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(C = c(1, 10, 100),
                                                       gamma = c(0.1, 0.01, 0.001)))

  allMultClassAlgo
}
