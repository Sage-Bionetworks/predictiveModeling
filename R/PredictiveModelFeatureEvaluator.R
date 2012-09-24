#' PredictiveModelFeatureEvaluator
#'
#' Not implemented yet. A class to evaluate the significance of a feature list returned from a predictive model
#'
#' @author Adam Margolin
#' @export

PredictiveModelFeatureEvaluator <- setRefClass(Class = "PredictiveModelFeatureEvaluator",
                          fields=c("referenceFeatures", "significantFeatures"),
                          methods = list(
                            initialize = function(referenceFeatures, significantFeatures, ...){
                              .self$referenceFeatures = referenceFeatures
                              .self$significantFeatures = significantFeatures
                              
                              return(.self)
                            },
                            
                            gsea = function(){
                              
                            }
                            )
                            )