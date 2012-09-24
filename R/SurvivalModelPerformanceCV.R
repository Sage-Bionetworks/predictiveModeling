#' SurvivalModelPerformanceCV
#'
#' A class containing a list of SurvivalModelPerformance objects corresponding to the results of each cross validation fold
#'
#' @author Adam Margolin
#' @export

SurvivalModelPerformanceCV <- setRefClass(Class = 'SurvivalModelPerformanceCV',
                                        fields = list(foldResults="list"
                                                      ),
                                        
                                        methods = list(
                                          initialize = function(foldResults, ...){
                                            .self$foldResults <- foldResults
                                          },
                                          
                                          getFoldCIndices = function(){
                                            cIndices <- sapply(.self$foldResults, function(x){x$getExactConcordanceIndex()}) 
                                            return(cIndices)
                                          }
                                          )                                   
                                        )
