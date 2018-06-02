require('e1071')

### Regression

#' Linear Regression
#' @description  Performs the multiple linear regression model.
#' @export
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @return predicted values
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' Y <- 3*X[, 1] - 2.5*X[, 2] + 0.2 * runif(100)
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- Y[pos]
#' Y_test <- Y[-pos]
#' Y_predicted <- linear_regression(X_train = X_train, Y_train = Y_train, X_test = X_test)
#' plot(x = (1:100)[-pos], y = Y_test, col = 'red')
#' points(x = (1:100)[-pos], y = Y_predicted, col = 'blue')
linear_regression <- function(X_train, Y_train, X_test){
  aux_data <- data.frame(X = X_train, Y = Y_train)
  model <- lm(formula = Y ~ ., data = aux_data)
  Y_hat <- predict(model, data.frame(X = X_test))
  Y_hat
}

#' Ridge Regression
#' @description  Performs the ridge linear regression model.
#' @export
#' @import ridge
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param lambda The penalty parameter.
#' @return predicted values
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' Y <- 3*X[, 1] - 2.5*X[, 2] + 0.2 * runif(100)
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- Y[pos]
#' Y_test <- Y[-pos]
#' lambda <- 0.25
#' Y_predicted <- ridge_regression(X_train = X_train, Y_train = Y_train, X_test = X_test, lambda = lambda)
#' plot(x = (1:100)[-pos], y = Y_test, col = 'red')
#' points(x = (1:100)[-pos], y = Y_predicted, col = 'blue')
ridge_regression <- function(X_train, Y_train, X_test, lambda){
  aux_data <- cbind(as.data.frame(X_train), Y = Y_train)
  model <- ridge::linearRidge(formula = Y ~ ., lambda = lambda, data = aux_data)
  Y_hat <- as.numeric( predict (model, as.data.frame(X_test)) )
  Y_hat
}

#' Multilayer Perceptron
#' @description Performs a multilayer perceptron regression.
#' @export
#' @import nnet
#' @param X_train A Matrix of trainning observations.
#' @param Y_train A numeric vector of classes or values of the trainning observations.
#' @param X_test A Matrix of testing observations.
#' @param n_neurons Number of neurons in the hidden layer.
#' @param weight_decay Weigth decay parameter for neural network.
#' @param max_iter Maximun number of trainning iterations.
#' @return predicted values
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' Y <- 3*X[, 1] - 2.5*X[, 2] + 0.2 * runif(100)
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- Y[pos]
#' Y_test <- Y[-pos]
#' n_neurons <- 20
#' Y_predicted <- MLP_regression(X_train = X_train, Y_train = Y_train, X_test = X_test, n_neurons = n_neurons)
#' plot(x = (1:100)[-pos], y = Y_test, col = 'red')
#' points(x = (1:100)[-pos], y = Y_predicted, col = 'blue')
MLP_regression <- function(X_train, Y_train, X_test, n_neurons, weight_decay = 0, max_iter = 1000){
  model <- nnet::nnet(x = X_train,
                      y = Y_train,
                      linout = TRUE,
                      size = n_neurons,
                      decay = weight_decay,
                      maxit = max_iter, trace = FALSE, MaxNWts = Inf)
  Y_hat <- predict(model, X_test)[, 1]
  Y_hat
}


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


#' Multilayer Perceptron
#' @description Performs a multilayer perceptron classification
#' @export
#' @import nnet
#' @param X_train A Matrix of trainning observations.
#' @param Y_train  A numeric vector of classes or values of the trainning observations
#' @param X_test A Matrix of testing observations.
#' @param n_neurons Number of neurons in the hidden layer.
#' @param weight_decay Weigth decay parameter for neural network.
#' @param max_iter Maximun number of trainning iterations.
#' @return predicted values
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' pos <- sample(100, 70)
#' X_train <- X[pos, ]
#' X_test <- X[-pos, ]
#' Y_train <- as.numeric( X_train[, 1] ** 2 - X_train[, 2] > 0)
#' Y_test <- as.numeric(X_test[, 1] ** 2 - X_test[, 2] > 0)
#' n_neurons <- 50
#' Y_predicted <- MLP_classification(X_train = X_train, Y_train = Y_train, X_test = X_test, n_neurons = n_neurons)
#' table(Y_test, Y_predicted)
MLP_classification <- function(X_train, Y_train, X_test, n_neurons, weight_decay = 0, max_iter = 1000){
  Y_train_matrix <- factor2BinMatrix(Y_train)
  model <- nnet::nnet(x = X_train,
                      y = Y_train_matrix,
                      linout = FALSE,
                      size = n_neurons,
                      decay = weight_decay,
                      maxit = max_iter, trace = FALSE)
  Y_hat <- BinMatrix2Vector(Y_matrix = predict(model, X_test), levels_val = unique(Y_train))
  Y_hat
}

#' Create Algorithm
#' @description  It is an auxiliary function to help creating new algorithms in the package standarts.
#' @export
#' @param algoName A character string that represents the algorithm name.
#' @param algoFun A function class object.
#' @param task A character string vector, cointaning 'MultClass' or/and 'BinClass' and/or 'Regression'.
#' @param paramList A list of all parameters and their values to be tested, NULL if none.
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

  if(!is.null(paramList)){
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

#' Generates all Binary Classification Alogrithms
#' @description  It is an auxiliary that allows to load all of the packages binary classification alogrithms.
#' @export
#' @return Returns a list with all Binary Classification Alogrithms
#' @examples
#' GetAllBinClassAlgo()
GetAllBinClassAlgo <- function(){
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
  allMultClassAlgo[[5]] <- CreateAlgo(algoName = 'MLP',
                                      algoFun = MLP_classification,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(n_neurons = 10**(1:3),
                                                       weight_decay = 0,
                                                       max_iter = 1000))
  allMultClassAlgo
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
  allMultClassAlgo[[5]] <- CreateAlgo(algoName = 'MLP',
                                      algoFun = MLP_classification,
                                      task = c('MultClass', 'BinClass'),
                                      paramList = list(n_neurons = 10**(1:3),
                                                       weight_decay = 0,
                                                       max_iter = 1000))
  allMultClassAlgo
}

#' Generates all Regression Alogrithms
#' @description  It is an auxiliary that allows to load all of the packages regression alogrithms.
#' @export
#' @return Returns a list with all regression Alogrithms
#' @examples
#' GetAllRegressionAlgo()
GetAllRegressionAlgo <- function(){
  allMultClassAlgo <- list()
  allMultClassAlgo[[1]] <- CreateAlgo(algoName = 'linear_regression',
                                      algoFun = linear_regression,
                                      task = c('Regression'),
                                      paramList = NULL)

  allMultClassAlgo[[2]] <- CreateAlgo(algoName = 'ridge_regression',
                                      algoFun = ridge_regression,
                                      task = c('Regression'),
                                      paramList = list(lambda = c(0.05, 0.1, 0.15, 0.2)))

  allMultClassAlgo[[3]] <- CreateAlgo(algoName = 'MLP',
                                      algoFun = MLP_regression,
                                      task = 'Regression',
                                      paramList = list(n_neurons = c(10, 20),
                                                       weight_decay = 0,
                                                       max_iter = 1000))
  allMultClassAlgo
}


