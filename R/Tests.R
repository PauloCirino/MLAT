#' Get Possible Tasks
#' @description  Get all possible task names
#' @export
#' @return A character string vector with all possible task names.
#' @examples
#' GetPossibleTasks()
GetPossibleTasks <- function(){
  possibleTasks <- c('MultClass',
                     'BinClas',
                     'Regression')
  possibleTasks
}

EvaluateResults <- function(Y, Y_hat,
                            task,
                            metrics){
  if(!(task %in% GetPossibleTasks())){
    stop('\nPlease choose a valid task!!
         For available options run GetPossibleTasks()')
  }
  #
  #   if(is.na(metrics)){
  #     metrics <- GetMetrics(task = task)
  #   } else{
  #     checkMetrics <- any(metrics %in% GetMetrics(task = task) == FALSE)
  #
  #     stopStr <- paste('Invalid metric option was entered!!!\nPossible options are :',
  #                      paste(GetMetrics(task = task), collapse = ', '))
  #     stop(stopStr)
  #   }

  if(task == 'MultClass'){
    return(MultClassMetrics(Y = Y,
                            Y_hat = Y_hat,
                            MetricsNames = metrics))
  }

  if(task == 'BinClass'){
    return(BinClassMetrics(Y = Y,
                           Y_hat = Y_hat,
                           MetricsNames = metrics))
  }

  if(task == 'Regression'){
    return(RegMetrics(Y = Y,
                      Y_hat = Y_hat,
                      MetricsNames = metrics))
  }

  stop('An unexpected error has occurred !!!!')
}

## NO EXPORT
GetFuncCall <- function(paramNames){
  funcCall <- paste('testFunc(',
                    'X_train = iterData$X_train',
                    ', Y_train = iterData$Y_train',
                    ', X_test = iterData$X_test',
                    sep = '')

  for(paramName in paramNames ){
    funcCall <- paste(funcCall,
                      ', ',
                      paramName,
                      ' = params$',
                      paramName,
                      sep = '' )
  }

  funcCall <- paste(funcCall, ')', sep = '')
  funcCall <- parse(text = funcCall)
  funcCall
}

#' Run Single Test
#' @description Runs single test
#' @export
RunSingleTest <- function(testFunc,
                          paramList,
                          task,
                          dataSetNames,
                          metrics,
                          nTestsPerParam,
                          splitPerc,
                          verbose = TRUE){
  if(is.null(paramList)){
    paramNames <- NULL
    nParams <- 0
    paramList <- list(dataSetName    = dataSetNames,
                      seed           = 1:nTestsPerParam,
                      splitPerc      = splitPerc)
  } else{
    nParams <- length(paramList)
    paramNames <- names(paramList)
    paramList <- append(paramList,
                        list( dataSetName    = dataSetNames,
                              seed           = 1:nTestsPerParam,
                              splitPerc      = splitPerc) )
  }

  nMetrics <- length(metrics)
  paramTable <- expand.grid(paramList)
  nTotalTests <- nrow(paramTable)

  globalResultTable <- as.data.frame( matrix(nrow = nTotalTests,
                                             ncol = nMetrics) )
  colnames(globalResultTable) <- metrics
  error_tests_pos <- numeric()
  for(i in 1:nTotalTests){
    if(verbose){
      cat('Running test', i, '/', nTotalTests,
          'at', as.character(Sys.time()), '\n')
    }

    params <- paramTable[i, ] ### params for the iter
    iterData <- GetData(datasetName = params$dataSetName,  ### data for the iter
                        seed = params$seed,
                        splitPerc = params$splitPerc)

    funcCall <- GetFuncCall(paramNames = paramNames) ### func text
    Y_hat <- try( expr = eval(expr = funcCall ), silent = TRUE) ### runs the func with params values
    if(class(Y_hat) == "try-error"){
      cat('Error at :', params$dataSetName, 'dataset' , '\n')
      error_tests_pos <- append(error_tests_pos, i)
    } else{
      iter_results <- EvaluateResults(Y = iterData$Y_test,
                                      Y_hat = Y_hat,
                                      task = task,
                                      metrics = metrics)

      globalResultTable[i, ] <- as.numeric( iter_results )
    }
  }

  if(length(error_tests_pos) == nTotalTests){
    cat('All Errors in this call !!!\n')
    return(NULL)
  }

  if(length(error_tests_pos) > 0){
    globalResultTable <- globalResultTable[-error_tests_pos, ]
  }

  colnames(globalResultTable) <- metrics
  globalResultTable <- cbind(paramTable[, ((nParams+1):ncol(paramTable))],
                             globalResultTable)
  if(nParams == 0){
    paramID <- NA
  } else if(nParams > 0){
    paramID <- paste(paramNames[1], '=', paramTable[, 1])
    j <- 2
    while(j < nParams){
      paramID <- paste(paramID, ' - ', paramNames[j], '=', paramTable[, j], sep = '')
      j <- j + 1
    }
  }
  globalResultTable <- cbind(paramID = paramID, globalResultTable)
  globalResultTable
}

#' Run Tests
#' @description  Runs the tests given the methods and datasets
#' @export
#' @param cmpTestsFuncsList is a list of
#' @param task A character string with 'MultClass' or 'BinClas'
#' @param dataSetNames A character string vector with valid dataset names, for options check
#' @param metrics character string vector with all testing metrics or NA for all available metrics
#' @param nTestsPerParam FOOO
#' @param splitPerc TODO
#' @param verbose TRUE/FALSE value for printing partial test
#' @return TODO
#' @examples
#' cmpTestsFuncsList <- GetAllMultClassAlgo()
#' task <- 'MultClass'
#' dataSetNames <- c('Iris', 'PimaIndiansDiabetes')
#' myResult <- RunTests(cmpTestsFuncsList = GetAllMultClassAlgo(),
#'                      task = 'MultClass',
#'                      dataSetNames = c('Iris', 'PimaIndiansDiabetes'))
#'
#'cmpTestsFuncsList <- GetAllRegressionAlgo()
#'task <- 'Regression'
#'dataSetNames <- GetDataSetsNames(task = task)
#'myResult <- RunTests(cmpTestsFuncsList = cmpTestsFuncsList,
#'                     task = task,
#'                     dataSetNames = dataSetNames)
RunTests <- function(cmpTestsFuncsList = cmpTestsFuncsList,
                     task = task,
                     dataSetNames = dataSetNames,
                     metrics = NA,
                     nTestsPerParam = 10,
                     splitPerc = 0.7,
                     verbose = TRUE){

  if(splitPerc >= 1 | splitPerc <= 0){
    stop('splitPerc must be between 0 and 1!!! Default = 0.7')
  }

  if(anyNA(metrics)){
    metrics <- GetMetrics(task = task)
  }

  globalResult <- data.frame()
  for(i in 1:length(cmpTestsFuncsList)){
    algoInfo <- cmpTestsFuncsList[[i]]

    cat('\n\n\nALGORITHM = ', algoInfo$algoName, '\n')
    iterResult <- RunSingleTest(testFunc       = algoInfo$algoFun,
                                paramList      = algoInfo$paramList,
                                task           = task,
                                dataSetNames   = dataSetNames,
                                metrics        = metrics,
                                nTestsPerParam = nTestsPerParam,
                                splitPerc      = splitPerc,
                                verbose        = verbose)
    if(is.null(iterResult)){
      cat('All Errors at ', algoName, '!!!\n')
    } else{
      globalResult <- rbind(globalResult,
                            cbind(algorithm = algoInfo$algoName, iterResult))
    }
  }

  class(globalResult) <- append(class(globalResult), 'testResultTable')
  globalResult
}
