#' PredictiveModel
#'
#' Base class for predictive models. Defines customTrain() and customPredict() methods.
#'
#' @author Adam Margolin
#' @export
PredictiveModel <- setRefClass(Class="PredictiveModel",
                               methods = list(
                                  customTrain = function(featureData, responseData){ 
                                  },

                                  customPredict = function(featureData){
                                  }
                               )
                              
                               )
