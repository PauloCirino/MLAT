#' Reduce Results
#' @description Reduce Results
#' @import dplyr
#' @description Reduces result table by parameter
#' @param resultTable Table resulted from a MLAT::RunTests() call
#' @param reductionMetric A decision metric, must have been selected in MLAT::RunTests() function call.
#' @param reductionMethod A reduction method, can be 'mean' or 'median'.
#' @param reductionOrder The reduction Order of selected values, max or min.
#' @return A reduced Table of type testResultTable .
#' @examples
#' cmpTestsFuncsList <- GetAllMultClassAlgo()
#' task <- 'MultClass'
#' dataSetNames <- c('Iris', 'PimaIndiansDiabetes')
#' myResult <- RunTests(cmpTestsFuncsList = GetAllMultClassAlgo(),
#'                      task = 'MultClass',
#'                      dataSetNames = c('Iris', 'PimaIndiansDiabetes'))
#'reducedTable <- ReduceResults(resultTable = myResult,
#'                              reductionMetric = 'Accuracy Macro',
#'                              reductionMethod = 'mean',
#'                              reductionOrder = 'max' )
#'
#'
#'
ReduceResults <- function(resultTable,
                          reductionMetric,
                          reductionMethod,
                          reductionOrder = 'max'){
  if(! "testResultTable" %in% class(resultTable)){
    stop('resultTable of wrong type !!!')
  }

  if(! reductionMethod %in% c('mean', 'median')){
    stop('Wrong value for reductionMethod!!!\nValid options are mean or median !!!')
  }

  if(! reductionOrder %in% c('max', 'min')){
    stop('Wrong value for reductionOrder!!!\nValid options are max or min !!!')
  }

  if(! reductionMetric %in% colnames(resultTable)){
    stop('resultTable of wrong type !!!')
  }

  originalColnames <- colnames(myResult)

  auxTable <- resultTable %>%
    dplyr::select(algorithm, paramID, reductionMetric, dataSetName)

  columnNames <- colnames(auxTable)
  columnNames[columnNames == reductionMetric] <- 'reductionMetric'
  colnames(auxTable) <- columnNames


  auxTable <- auxTable %>%
    dplyr::group_by(algorithm, paramID, dataSetName)

  if(reductionMethod == 'mean'){
    auxTable <- auxTable %>%
      dplyr::summarise(reducedValue = min(reductionMetric))
  }

  if(reductionMethod == 'median'){
    auxTable <- auxTable %>%
      dplyr::summarise_all(reducedValue = median(reductionMetric))
  }

  auxTable <- auxTable %>%
    dplyr::ungroup() %>%
    dplyr::group_by(algorithm, dataSetName)

  if(reductionOrder == 'max'){
    auxTable <- auxTable %>%
      dplyr::arrange(-reducedValue)
  }

  if(reductionOrder == 'min'){
    auxTable <- auxTable %>%
      dplyr::arrange(reducedValue)
  }

  auxTable <- auxTable%>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dataSetName, algorithm, paramID)

  resultTable <- resultTable %>%
    dplyr::inner_join(auxTable) %>%
    data.frame()

  colnames(resultTable) <- originalColnames

  class(resultTable) <- append(class(resultTable), 'testResultTable')
  resultTable
}

#' Hypoteses Testing
#' @description Hypoteses Testing
#' @import dplyr
#' @import tidyr
#' @import scmamp
#' @export
#' @description Performs Statistical Tests
#' @param resultTable Table resulted from a MLAT::RunTests() call
#' @param testingMetric A decision metric, must have been selected in MLAT::RunTests() function call.
#' @param reductionMethod A reduction method, can be 'mean' or 'median'.
#' @param reductionOrder The reduction Order of selected values, max or min.
#' @param alpha Test significance level.
#' @return A list of type hypotesesTest.
#' @examples
#' cmpTestsFuncsList <- GetAllMultClassAlgo()
#' task <- 'MultClass'
#' dataSetNames <- MLAT::GetDataSetsNames(task = task)
#' myResult <- RunTests(cmpTestsFuncsList = GetAllMultClassAlgo(),
#'                      task = task,
#'                      dataSetNames = dataSetNames)
#'hypotessesTesting(resultTable = myResult,
#'                  testingMetric = 'F1 Macro',
#'                  method = 'friedman',
#'                  reductionMethod = 'mean',
#'                  reductionOrder = 'max',
#'                  alpha = 0.05)
hypotessesTesting <- function(resultTable,
                              testingMetric,
                              method = 'friedman',
                              reductionMethod = 'mean',
                              reductionOrder = 'max',
                              alpha = 0.05){

  if(! "testResultTable" %in% class(resultTable)){
    stop('resultTable of wrong type !!!')
  }

  if(! reductionMethod %in% c('mean', 'median', 'min', 'max')){
    stop('Wrong value for reductionMethod!!!\nValid options are mean or median !!!')
  }

  if(! reductionOrder %in% c('max', 'min')){
    stop('Wrong value for reductionOrder!!!\nValid options are max or min !!!')
  }

  if(! testingMetric %in% colnames(resultTable)){
    stop('resultTable of wrong type !!!')
  }

  if(! method %in% c('anova', 'friedman')){
    stop('Invalid value for method !!!')
  }

  originalColnames <- colnames(resultTable)

  auxTable <- resultTable %>%
    dplyr::select(algorithm, paramID, testingMetric, dataSetName)

  columnNames <- colnames(auxTable)
  columnNames[columnNames == testingMetric] <- 'testingMetric'
  colnames(auxTable) <- columnNames


  auxTable <- auxTable %>%
    dplyr::group_by(algorithm, paramID, dataSetName)

  ## Reduz por parametro
  if(reductionMethod == 'mean'){
    auxTable <- auxTable %>%
      dplyr::summarise(reducedValue = min(testingMetric))
  }else if(reductionMethod == 'median'){
    auxTable <- auxTable %>%
      dplyr::summarise_all(reducedValue = median(testingMetric))
  }else if(reductionMethod == 'max'){
    auxTable <- auxTable %>%
      dplyr::summarise_all(reducedValue = max(testingMetric))
  } else if(reductionMethod == 'min'){
    auxTable <- auxTable %>%
      dplyr::summarise_all(reducedValue = min(testingMetric))
  }
  reducedTable <- auxTable %>% ungroup()

  ## Filtra melhor parametro
  auxTable <- reducedTable %>%
    dplyr::group_by(algorithm, dataSetName)

  if(reductionOrder == 'max'){
    auxTable <- auxTable %>%
      dplyr::arrange(-reducedValue)
  } else if(reductionOrder == 'min'){
    auxTable <- auxTable %>%
      dplyr::arrange(reducedValue)
  }

  auxTable <- auxTable%>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dataSetName, algorithm, paramID)

  reducedTable <- reducedTable %>%
    dplyr::inner_join(auxTable) %>%
    dplyr::select(algorithm, reducedValue, dataSetName) %>%
    tidyr::spread(algorithm, reducedValue)

  testResult <- NULL
  if(method == 'anova'){
    testResult <- anovaTest(reducedTable, reductionOrder, alpha)
  }else if(method == 'friedman'){
    testResult <- friedmanTest(reducedTable, reductionOrder, alpha)
  }

  testResult[['reducedTable']] <- reducedTable %>% as.data.frame()
  testResult
}


anovaTest <- function(reducedTable,
                      reductionOrder,
                      alpha){

  reducedTable <- reducedTable %>%
    dplyr::select(-dataSetName) %>%
    as.data.frame()

  pValueTable <- round( scmamp::tukeyPost(data = reducedTable), 4)
  signDiffTable <- pValueTable < alpha

  auxData <- reducedTable %>%
    tidyr::gather(algorithm, metric) %>%
    dplyr::mutate(algorithm = as.factor(algorithm)) %>%
    as.data.frame()

  ANOVA <- aov(metric ~ algorithm, data = auxData)
  Tukey <- TukeyHSD(ANOVA, ordered = TRUE)

  pdf(NULL)
  dev.off()
  try(dev.control(displaylist = "enable"), silent = TRUE)
  plot(Tukey)
  image <- recordPlot()
  invisible(dev.off())
  plot.new()

  resultTable <- list(image = image,
                      pValueTable = pValueTable,
                      signDiffTable = signDiffTable,
                      test = 'anova',
                      alpha = alpha)
  class(resultTable) <- append(class(resultTable), 'hypotesesTest')
  resultTable

}


friedmanTest <- function(reducedTable,
                         reductionOrder,
                         alpha){
  if(reductionOrder == 'max'){
    decreasing <- TRUE
  } else{
    decreasing <- FALSE
  }

  reducedTable <- reducedTable %>%
                  dplyr::select(-dataSetName) %>%
                  as.data.frame()

  pdf(NULL)
  dev.off()
  try(dev.control(displaylist = "enable"), silent = TRUE)
  plotCD(reducedTable, alpha = alpha, decreasing = decreasing)
  image <- recordPlot()
  invisible(dev.off())
  plot.new()

  pValueTable <- round( scmamp::friedmanPost(data = reducedTable), 4)
  signDiffTable <- pValueTable < alpha

  resultTable <- list(image = image,
                      pValueTable = pValueTable,
                      signDiffTable = signDiffTable,
                      test = 'friedman',
                      alpha = alpha)
  class(resultTable) <- append(class(resultTable), 'hypotesesTest')
  resultTable
}
