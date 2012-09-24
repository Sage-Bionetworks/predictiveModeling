setRefClass(Class = "PredictiveModel")

#' LinearModel
#'
#' Base class for linear models, adding a getCoefficients method to PredictiveModel
#'
#' @author Adam Margolin
#' @export
LinearModel <- setRefClass(Class="LinearModel",
                           contains="PredictiveModel",
                               methods = list(
                                  getCoefficients <- function(){
                                }
                              )
                           )