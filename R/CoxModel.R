setRefClass(Class = "PredictiveModel")

#' CoxModel
#'
#' Simple wrapper for training a cox model via cv.glmnet and implementing the PredictiveModel API
#'
#' @author Adam Margolin
#' @export

CoxModel <- setRefClass(Class = "CoxModel",
                           contains="PredictiveModel",
                           fields="model",
                           methods = list(
                             initialize = function(...){
                               return(.self)
                             },
                             
                             rawCaretModel = function(){
                               return(.self$model)
                             },
                             
                             customTrain = function(featureData, responseData, trControl = defaultTrainControl(),
                                              filterData = TRUE, tuneGrid = NULL){
                                .self$model <- cv.glmnet(featureData,responseData,family = "cox",alpha =1, nfolds=3)
                             },
                             
                             customPredict = function(featureData){
                               predictedResponse <- predict(.self$model, featureData, s="lambda.min")
                               return(predictedResponse)
                             }
                             )
                           )
