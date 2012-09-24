#' filterPredictiveModelData filters
#'
#' Filters feature and response data as specificied by the users based on rows or columns that contain NAs.
#' features with low variance, or features not correlated with response.
#' 
#' @param featureData
#' @param responseData
#' @return a list of the filtered feature and response data
#' @author Adam Margolin
#' @export
filterPredictiveModelData <- 
		function(featureData, responseData, filterResponseNas = TRUE, filterFeatureNasBy = "columns", featureVarianceThreshold=0, corPValThresh = NULL)
{    
    if(filterResponseNas){
      isNas <- is.na(responseData)
      featureData <- featureData[!isNas,]
      responseData <- responseData[!isNas]
    }
    
    if(filterFeatureNasBy == "columns"){
      featureData <- filterNasFromMatrix(dataMatrix=featureData, filterBy = filterFeatureNasBy)
    }else if(filterFeatureNasBy == "rows" || filterFeatureNasBy == "both"){
     ###after filtering, need to reassign response data to match rows retained in feature data
      featureData <- filterNasFromMatrix(dataMatrix=featureData, filterBy = filterFeatureNasBy)
      responseData <- responseData[rownames(featureData)]
    }
    
    if(!is.na(featureVarianceThreshold) && !is.null(featureVarianceThreshold) ){
      variances <- apply(featureData, 2, var)
      if(any(variances<=featureVarianceThreshold | is.na(variances) )){
        featureData <- featureData[, -which(variances<=featureVarianceThreshold | is.na(variances))] 
      }
    }
  
    if(!is.null(corPValThresh) && !is.na(corPValThresh) ){
      featureCorrs <- apply(featureData, 2, "cor.test", responseData)
      featurePVals <- sapply(featureCorrs, FUN=function(x)x$p.value)
      
      sigFeatures <- which(featurePVals < corPValThresh)
      featureData <- featureData[,sigFeatures]
    }
    
    #sbf(featureData, responseData, sbfControl = sbfControl(functions = gamScores))
    #use filter() function
    list(featureData = featureData, responseData = responseData)
}
