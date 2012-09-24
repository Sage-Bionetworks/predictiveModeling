#' convertDataFrameToFeatureMatrix
#'
#' Convert a data frame to a numeric feature matrix that can be used as features in a predictive model 
#' Numeric features are left as numeric. Character or factor features are converted to binary features if
#' they have 2 unique values or if there are more than 2 unique values it is expanded to a binary feature
#' for each possible value.
#'
#' @author Adam Margolin
#' @export

convertDataFrameToFeatureMatrix <- function(dataFrame){
  if(is(dataFrame, "AnnotatedDataFrame")){
    dataFrame <- dataFrame@data
  }
  
  featureMatrix <- c()
  for (colCtr in 1:ncol(dataFrame)){
    featureMatrix <- cbind(featureMatrix, convertDataFrameColumnToFeatures(dataFrame[, colCtr], colnames(dataFrame)[colCtr]))
  }
  colnames(featureMatrix) <- make.names(colnames(featureMatrix))
  return(featureMatrix)
}

convertDataFrameColumnToFeatures <- function(dfColumn, colName){
  features <- matrix(NA, nrow=length(dfColumn), ncol=1)
  colnames(features) <- colName
  if (is.numeric(dfColumn)){
    print(paste("leaving", colName, "as numeric"))
    features[,1] <- dfColumn
  }else if (is.character(dfColumn)){
    factorLevels <- unique(dfColumn)
    if(length(factorLevels) == 2){
      print(paste("converting", colName, "to binary"))
      features[dfColumn == factorLevels[1]] <- 0
      features[dfColumn == factorLevels[2]] <- 1
    }else{
      print(paste("converting", colName, "to factors"))
      features <- matrix(0, nrow=length(dfColumn), ncol=length(factorLevels))
      curColNames <- c()
      for (factorCtr in 1:length(factorLevels)){
        curFactorLevel <- factorLevels[factorCtr]
        features[dfColumn==curFactorLevel, factorCtr] <- 1
        curColNames[factorCtr] <- paste(colName, curFactorLevel, sep="_")
      }
      colnames(features) <- curColNames
    }
  }else if (is.factor(dfColumn)){
    print("factors not yet supported")
  }else {
    print("error converting data.frame to features. Unknown column type")
  }
  
  return(features)
}
