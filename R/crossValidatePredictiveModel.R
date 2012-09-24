#' Perform cross validation on a preditive model, this method support both caret models and PredictiveModels
#'
#' @param featureData
#' @param responseData
#' @param method either an instance of PredictiveModel or a string holding the name of one of the machine learning methods that caret supports
#' @param numFolds defaults to 5
#' @param trControl defaults to defaultTrainControl
#' @return a list of PredictiveModelPerformance one per fold
#' @seealso defaultTrainControl
#' @author Adam Margolin
#' @export
crossValidatePredictiveModel <- 
		function(featureData, responseData, model, numFolds = 5, trControl = defaultTrainControl(), ...){
  
  #-----------------------------------------------------------------------
  # Split the data into training and test partitions
  # -----------------------------------------------------------------------
  set.seed(2)
  foldIndices <- createFolds(featureData[,1], k = numFolds, list = TRUE)
  
  message("Training partitions")

  modelResults <- foreach(fold = foldIndices) %dopar% {
    foldModel <- model$copy()
    foldModel$customTrain(featureData[-fold,], responseData[-fold], trControl = trControl, ...)
    return(foldModel)
  }
  
  foldTestPredictions <- foreach(i=1:numFolds) %do% modelResults[[i]]$customPredict(featureData[foldIndices[[i]],])
  testPredictions <- do.call("c", foldTestPredictions)
  
  foldTrainPredictions <- foreach(i=1:numFolds) %do% modelResults[[i]]$customPredict(featureData[-foldIndices[[i]],])
  trainPredictions <- do.call("c", foldTrainPredictions)
  
  foldTestObservations <- foreach(i=1:numFolds) %do% responseData[foldIndices[[i]]]
  testObservations <- do.call("c", foldTestObservations)
  
  foldTrainObservations <- foreach(i=1:numFolds) %do% responseData[-foldIndices[[i]]]
  trainObservations <- do.call("c", foldTrainObservations)
  
  res <- list(trainPredictions = trainPredictions, trainObservations = trainObservations,
              testPredictions = testPredictions, testObservations = testObservations)
  
  return(res)
}
