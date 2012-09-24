#' Perform one iteration of model training on a portion of the data
#'
#' @param testIndices
#' @param featureData
#' @param responseData
#' @param method either an instance of PredictiveModel or a string holding the name of one of the machine learning methods that caret supports
#' @param trControl defaults to defaultTrainControl
#' @return an instance of PredictiveModelResults
#' @author Nicole Deflaux
#' @export
trainPartition <-
		function(testIndices, featureData, responseData, model, trControl = defaultTrainControl(), ...) {
	message("Getting Training Indices")
	trainIndices <- setdiff(seq(along=responseData), testIndices)
	
  message("Selecting Features")
	trainFeatures <- featureData[trainIndices,]
	testFeatures <- featureData[testIndices,]
	trainResponse <- responseData[trainIndices]
	testResponse <- responseData[testIndices]
	
  # Need to generate the model if the argument is instaed a type of machine learning that caret supports
	if (is.character(model)){
    model <- CaretModel$new(modelType=model)
	}
  
	model$customTrain(trainFeatures, trainResponse, filterData=FALSE, trControl=trControl, ...)
  
  prediction_train <- model$customPredict(trainFeatures)
  prediction_test <- model$customPredict(testFeatures)
	
	predictedValues <- vector(mode='numeric', length=length(responseData))
	predictedValues[trainIndices] <- prediction_train
	predictedValues[testIndices] <- prediction_test
	
	message("creating PredictiveModelResults object")
	PredictiveModelResults$new(predictiveModel=model, 
												 features=featureData, 
												 trainIndices=trainIndices,
												 testIndices=testIndices,
												 predictedValues=predictedValues, 
												 observedValues=responseData)	
}

