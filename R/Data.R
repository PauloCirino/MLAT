#' Get Datasets Names
#' @description  Get all available datasets names
#' @export
#' @param task A character string with desired task, for possible tasks check GetPossibleTasks().
#' @return A character string vector with all possible datasets names.
#' @examples
#' GetDataSetsNames(task = 'MultClass')
GetDataSetsNames <- function(task){
  possibleTasks <- NULL

  if(task == 'MultClass'){
    possibleTasks <- c("DNA",
                       "Glass",
                       "Ionosphere",
                       "Iris",
                       "LetterRecognition",
                       "PimaIndiansDiabetes",
                       "Shuttle",
                       "Sonar",
                       "Vehicle",
                       "Vowel")
  }

  if(task == 'BinClass'){
  possibleTasks <- c('Ionosphere',
                     'PimaIndiansDiabetes',
                     'Sonar')
  }

  if(task == 'Regression'){
    possibleTasks <- c('Housing',
                       'AutoSweden')
  }

  stop('Invalid task !!!')

  return(possibleTasks)
}


#' Create DataSet
#' @description  Creates a DataSet object type, to be used with the models provided with this package
#'
#' @export
#' @param X A Matrix.
#' @param Y A numeric vector of classes or values.
#' @param Name A character string, as dataset name.
#' @param type A character string, the types of values for X (numeric or integer).
#' @param task A character string vector of task to be performed, check GetPossibleTasks().
#' @return A DataSet object type.
#' @examples
#' X <- as.matrix(cbind(runif(n = 100), runif(n = 100)))
#' Y <- sample(x = c(1, 2), size = 100, replace = TRUE)
#' Name <- 'randomData'
#' type <- 'numeric'
#' task <- 'BinClas'
#' newData <- CreateDataSet(X = X, Y = Y, Name = Name, type = type, task = task)
CreateDataSet <- function(X, Y, Name,
                          type, task){
  if(class(X) != 'matrix'){
    stop("X is not a matrix!")
  }

  if(class(Y) != 'numeric'){
    stop("Y is not a numeric vector!")
  }

  if(class(Name) != "character"){
    stop('Invalid Name!')
  }

  if(type != 'numeric' && type != 'integer'){
    stop('Invalid type!')
  }

  if(any(task %in% GetPossibleTasks() == FALSE) ){
    stop('Invalid task!')
  }

  if('regression' %in% task){
    if(length(task) != 1){
      stop('Invalid Task !!! \nRegression cannot be combined with other tasks!')
    } else{
      nClasses <- 1
    }
  } else if( any(task %in% GetPossibleTasks()) ){
    nClasses <- length(unique(Y))
    if( nClasses == 2){
      task <- c("MultClass", "BinClas")
    } else{
      task <- c("MultClass")
    }
  } else{
    stop("Invalid task!")
  }

  resultList <- list(X = X,
                     Y = Y,
                     Name = Name,
                     nCols = ncol(x = X),
                     nRows = nrow(x = X),
                     nClasses = nClasses,
                     type = type,
                     task = task,
                     any_na = anyNA(x = X))
  class(resultList) <- append(class(resultList), 'DataSet')
  resultList
}

#' Get DataSet by name
#' @description Get a dataset by name
#'
#' @export
#' @param datasetName A character string, as DataSet name.
#' @param seed For the traint and test split.
#' @param splitPerc Percentage for train of all dataSet, between 0 and 1.
#' @return A trainTestDataSet object type.
#' @examples
#' GetData(datasetName = 'Iris', seed = 123, splitPerc = 0.7)
GetData <- function(datasetName, seed, splitPerc){
  set.seed(seed)

  datasetName <- as.character(datasetName)
  data(list = datasetName )
  Data <- eval(expr = parse(text = datasetName))

  pos <- sample(Data$nRows, round(Data$nRows * splitPerc))
  Data <- append(Data,
                 list(X_train = Data$X[pos, ],
                      X_test = Data$X[-pos, ],
                      Y_train = Data$Y[pos],
                      Y_test = Data$Y[-pos] ))
  class(Data) <- append(class(Data), 'trainTestDataSet')
  Data
}
