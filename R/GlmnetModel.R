setRefClass(Class = "LinearModel")

#' GlmnetModel
#' 
#' Wrapper for elastic net model implemented by glmnet, allowing cv.glmnet to implement the PredictiveModel API
#'
#' @seealso caret
#' @author Adam Margolin
#' @export

GlmnetModel <- setRefClass(Class = "GlmnetModel",
                           contains="LinearModel",
                           fields=c("model","family"),
                           
                           methods = list(
                             initialize = function(family="gaussian", ...){
                               .self$family=family
                               return(.self)
                             },
                             
                             customTrain = function(featureData, responseData,  ...){
                                .self$model <- cv.glmnet(featureData,responseData, family=.self$family, ...)
                             },
                             
                             customPredict = function(featureData){
                               predictedResponse <- predict(.self$model, featureData, s="lambda.min")
                               return(predictedResponse)
                             },
                             
                           
                             getCoefficients = function(){
                               return(coef(.self$model,s = "lambda.min"))                             
                             }
                             )
                           )
