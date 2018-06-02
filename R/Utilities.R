#' Factor Vector to Binary matrix of dummies
#' @description  Transforms a factor vector into a binary matrix of dummies.
#' @export
#' @param Y a numeric, factor or character vector.
#' @param levels_val levels to be considered, default is NULL for automatic levels.
#' @return matrix
#' @examples
#' Y <- sample(c(1,2,3), size = 100, replace = TRUE)
#' factor2BinMatrix(Y)
#' factor2BinMatrix(as.factor(Y))
#' factor2BinMatrix(Y, levels_val = 1:5)
#' factor2BinMatrix(as.factor(Y), levels_val = 1:5)
factor2BinMatrix <- function(Y, levels_val = NULL){
  if(class(Y) != 'factor'){
    if(class(Y) %in% c('numeric', 'character')){
      if(is.null(levels_val)){
        Y <- as.factor(x = Y)
        levels_val <- levels(Y)
      } else{
        Y <- factor(x = Y, levels = levels_val)
      }
    } else{
      stop('Invalid Y type !!!')
    }
  } else{
    if(is.null(levels_val)){
      levels_val <- levels(Y)
    }
  }
  matrix <- t( sapply(Y, function(x) {as.numeric(x == levels_val)}) )
  matrix
}

#' Matrix of Dummies to Vector
#' @description  Transforms a matrix of dummies into a numeric vector.
#' @export
#' @param Y_matrix a numeric, factor or character vector.
#' @param levels_val levels to be considered, default is NULL for automatic levels.
#' @return A numeric vector
#' @examples
#' Y <- sample(c(1,4,3), size = 100, replace = TRUE)
#' Y_matrix <- factor2BinMatrix(Y)
#' BinMatrix2Vector(Y_matrix = Y_matrix)
#' BinMatrix2Vector(Y_matrix = Y_matrix, levels_val = unique(Y))
#' BinMatrix2Vector(Y_matrix = Y_matrix, levels_val = c('a', 'e', 'i'))
BinMatrix2Vector <- function(Y_matrix, levels_val = NULL){
  Y_hat <- apply(Y_matrix, 1, which.max )
  if(!is.null(levels_val)){
    Y_hat <- levels_val[Y_hat]
  }
  Y_hat
}

