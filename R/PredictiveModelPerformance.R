#' PredictiveModelPerformance
#'
#' A class implementing various performance evaluation methods upon PredictiveModelResults
#'
#' @seealso PredictiveModelResults
#' @author Adam Margolin
#' @export
PredictiveModelPerformance <- setRefClass(Class = 'PredictiveModelPerformance',
                                          
	  fields = list(foldResults='list',
                  trainPredictions="numeric",
                  trainObservations="numeric",
                  testPredictions="numeric",
                  testObservations="numeric"
                  ),
                                          
    methods = list(
        initialize = function(foldResults, ...){
            .self$foldResults <- foldResults
            
            .self$trainPredictions <- do.call("c", foldResults$trainPredictions)
            .self$trainObservations <- do.call("c", foldResults$trainObservations)
            .self$testPredictions <- do.call("c", foldResults$testPredictions)
            .self$testObservations <- do.call("c", foldResults$testObservations)
          
            return(.self)
        },
        
        getR2 = function(){
#             return(cor(.self$testPredAndObs$pred, .self$testPredAndObs$obs) ^ 2)
        },
        
        getTestError = function(){
#           return((.self$testPredAndObs$pred - .self$testPredAndObs$obs) ^ 2)
        },
        
        plotPredAndObs = function(){
          .plotHelper(
                  trainPred=.self$trainPredAndObs$pred,
                  trainObs=.self$trainPredAndObs$obs,
                  testPred=.self$testPredAndObs$pred,
                  testObs=.self$testPredAndObs$obs)          
        }
    )
	)


.plotHelper <- function(trainPred, trainObs, testPred, testObs) {
  par(mfrow=c(2,1))
  plot(
       trainPred,
       trainObs,
       main='predicted versus observed for training data',
       xlab='predicted',
       ylab='observed')
  plot(
       testPred,
       testObs,
       main='predicted versus observed for test data',
       xlab='predicted',
       ylab='observed')
}




