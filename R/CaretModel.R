setRefClass(Class = "PredictiveModel")

#' CaretModel
#'
#' A class in the PredictiveModel class hierarchy that wraps models returned from caret.
#' This class allows all methods implemented in the caret machine learning package to plug into pipelines
#' using the PredictiveModeling API by wrapping all caret methods to implement customTrain() and customPredict()
#' methods.
#'
#' @author Adam Margolin
#' @export

CaretModel <- setRefClass(Class = "CaretModel",
                          contains="PredictiveModel",
                          fields=c("model", "modelType"),
                          methods = list(
                            initialize = function(modelType, ...){
                              .self$modelType = modelType
                              
                              return(.self)
                            },
                            
                            rawCaretModel = function(){
                              return(.self$model)
                            },
                            
                            copy = function() {
                              
                              result <- CaretModel$new(.self$modelType)
                              result$model <- .self$model
                              
                              return(result)
                            },
                            
                            customTrain = function(featureData, responseData, trControl = trainControl(), # trControl = defaultTrainControl(),
                                             tuneGrid = NULL, ...){
                              .self$model <- caret::train(featureData, 
                                responseData, 
                                 method = .self$modelType,
                                 preProcess = NULL, # preProcess = c("center", "scale"),
                                 tuneLength = 4,
                                 trControl = trControl,
                                  tuneGrid = tuneGrid,
                                  ...
                                )
                            },
                            
                            customPredict = function(featureData){
                              predictedResponse <- predict(.self$model, featureData)
                              return(predictedResponse)
                            }
                          )
                          )

