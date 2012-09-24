#' createFeatureAndResponseDataList
#'
#' Prepares data for input to predictive modeling by matching column names of feature and response data and creating
#' feature and response matrices with columns in the same order
#'
#' @param featureData a matrix of feature data with samples on rows and features on columns
#' @param responseData a matrix or AnnotatedDataFrame of pheno data with samples on rows and features on columns
#' @return a list containing two items: the feature data and the response data with intersecting sample names
#' 
#' @author Adam Margolin
#' @export

createFeatureAndResponseDataList <- function(featureData, responseData){
  if(is(responseData, "AnnotatedDataFrame")) {
    responseData <- pData(responseData)
  }
  
  if (is.data.frame(responseData) || is.matrix(responseData)){
    commonRowNames <- intersect(rownames(featureData), rownames(responseData))
    featureAndResponseDataList <- list(featureData = featureData[commonRowNames,], responseData = responseData[commonRowNames,])
  }else if (is.numeric(responseData)){
    commonRowNames <- intersect(rownames(featureData), names(responseData))
    featureAndResponseDataList <- list(featureData = featureData[commonRowNames,], responseData = responseData[commonRowNames])
  }else{
    print("error: uknown responseData type")
  }
  
  return(featureAndResponseDataList)
}
