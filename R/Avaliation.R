#' Reduce Results
#' @description Reduce Results
#' @import dplyr
#' @export
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

  class(resultTable) <- append(class(resultTable), 'testResultTable')
  resultTable
}

