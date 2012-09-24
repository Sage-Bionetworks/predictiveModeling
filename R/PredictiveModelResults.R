setRefClass(Class = 'PredictiveModel')

#' PredictiveModelResults
#'
#' A class to hold results from PredictiveModels
#' @author Adam Margolin
#' @export

PredictiveModelResults <- setRefClass(Class = 'PredictiveModelResults',
            
            fields = list(predictiveModel='PredictiveModel',
		          features='matrix',
		          trainIndices='numeric',
		          testIndices='numeric',
		          predictedValues='numeric',
		          observedValues='numeric'),
            
            methods = list(
              initialize = function(predictiveModel, features, trainIndices, testIndices, predictedValues, observedValues, ...){
                .self$predictiveModel <- predictiveModel
                .self$features <- features
                .self$trainIndices <- trainIndices
                .self$testIndices <- testIndices
                .self$predictedValues <- predictedValues
                .self$observedValues <- observedValues
                
                return(.self)
              }
              
            )
	)
