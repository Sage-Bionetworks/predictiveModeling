#' Perform cross validation on a preditive model, this model supports Cox model and predictiveModeling
#'
#' @param featureData
#' @param responseData
#' @param model either an instance of PredictiveModel or a string holding the name of one of the machine learning methods that caret supports
#' @param numFolds defaults to 5
#' @param trControl defaults to defaultTrainControl
#' @return a list of PredictiveModelPerformance one per fold
#' @seealso defaultTrainControl
#' @author Adam Margolin
#' @export
crossValidatePredictiveSurvivalModel <- function(model, exprData, copyData, clinicalFeaturesData, clinicalSurvData, numFolds = 5, ...){
    sampleNamesList <- list(colnames(exprs(exprData)), colnames(exprs(copyData), rownames(clinicalFeaturesData), rownames(clinicalSurvData)))
    commonSampleNames <- Reduce("intersect", sampleNamesList)
    
    exprData <- exprData[,commonSampleNames]
    copyData <- copyData[,commonSampleNames]
    clinicalFeaturesData <- clinicalFeaturesData[commonSampleNames,]
    clinicalSurvData <- clinicalSurvData[commonSampleNames,]
    
    #-----------------------------------------------------------------------
    # Split the data into training and test partitions
    # -----------------------------------------------------------------------
    set.seed(3)
    foldIndices <- createFolds(1:length(commonSampleNames), k=numFolds, list=TRUE)
    
    message("Training partitions")
    
    modelResults <- foreach(fold = foldIndices) %do% {
      foldModel <- model$copy()
      foldModel$customTrain(exprData[,-fold], copyData[,-fold], clinicalFeaturesData[-fold,], clinicalSurvData[-fold,])
      
      trainPerformance <- SurvivalModelPerformance$new(as.numeric(foldModel$customPredict(exprData[,-fold], copyData[,-fold], clinicalFeaturesData[-fold,])),
                                                       clinicalSurvData[-fold,])
      testPerformance <- SurvivalModelPerformance$new(as.numeric(foldModel$customPredict(exprData[,fold], copyData[,fold], clinicalFeaturesData[fold,])),
                                                      clinicalSurvData[fold,])
      return(list(trainPerformance=trainPerformance, testPerformance=testPerformance) )
    }
    
#     foldTestPredictions <- foreach(i=1:numFolds) %do% modelResults[[i]]$customPredict(exprData[,foldIndices[[i]]], 
#                                                                                       copyData[,foldIndices[[i]]], clinicalFeaturesData[foldIndices[[i]],])
#     
#     foldTrainPredictions <- foreach(i=1:numFolds) %do% modelResults[[i]]$customPredict(exprData[,-foldIndices[[i]]], 
#                                                                                        copyData[,-foldIndices[[i]]], clinicalFeaturesData[-foldIndices[[i]],])
#     
#     foldTestObservations <- foreach(i=1:numFolds) %do% clinicalSurvData[foldIndices[[i]],]
#     
#     foldTrainObservations <- foreach(i=1:numFolds) %do% clinicalSurvData[-foldIndices[[i]],]
#     
#     foldResults <- list(trainPredictions = foldTrainPredictions, trainObservations = foldTrainObservations,
#                 testPredictions = foldTestPredictions, testObservations = foldTestObservations)
#     
#     modelPerformance <- PredictiveModelPerformance$new(foldResults)
#     return(modelPerformance)
    trainPerformanceCV <- SurvivalModelPerformanceCV$new(lapply(modelResults, function(x){x$trainPerformance}))
    testPerformanceCV <- SurvivalModelPerformanceCV$new(lapply(modelResults, function(x){x$testPerformance}))
    
    return(list(trainPerformanceCV = trainPerformanceCV, testPerformanceCV = testPerformanceCV))
  }
