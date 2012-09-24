setRefClass(Class = "Surv")

#' SurvivalModelPerformance
#'
#' A class implementing various performance evaluation methods for the results of a predictive survival model
#'
#' @author Adam Margolin
#' @export

SurvivalModelPerformance <- setRefClass(Class = 'SurvivalModelPerformance',
  fields = c("predictions",
             "observationTime",
             "observationStatus"),
  
  methods = list(
    initialize = function(pred, obs){
      .self$predictions <- pred
      .self$observationTime <- obs[,1]
      .self$observationStatus <- obs[,2]
    },
    
    getConcordanceIndex = function(){
      cIndex <- concordance.index(x=.self$predictions, surv.time=.self$observationTime, 
                        surv.event=.self$observationStatus, na.rm=TRUE, alpha= .05)
    },
    
    getExactConcordanceIndex = function(){
      cIndex <- exactConcordanceIndexV(.self$predictions, Surv(.self$observationTime, .self$observationStatus))
    }
    
  )                                   
)
