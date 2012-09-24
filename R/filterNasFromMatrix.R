#' removes rows and or columns from a matrix that contain NA values
#'
#' @author Adam Margolin
#' @export

filterNasFromMatrix <- function(dataMatrix, filterBy = "rows"){
  if (filterBy == "rows"){
    rowsWithoutNas <- rowSums(is.na(dataMatrix)) == 0
    dataMatrix <- dataMatrix[rowsWithoutNas,]
  }else if (filterBy == "columns"){
    columnsWithoutNas <- colSums(is.na(dataMatrix)) == 0
    dataMatrix <- dataMatrix[, columnsWithoutNas]
  }else if (filterBy == "both"){
    rowsWithoutNas <- rowSums(is.na(dataMatrix)) == 0
    columnsWithoutNas <- colSums(is.na(dataMatrix)) == 0
    dataMatrix <- dataMatrix[rowsWithoutNas, columnsWithoutNas]
  }else{
    message(paste("Not filtering anything because unknown filterBy argument:", filterBy))
  }
  
  return(dataMatrix)
}