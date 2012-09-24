#' MostCorrelatedFeatures
#'
#' Simple example class of a predictive model user the features most correlated with response as the regression coefficients
#' @author Adam Margolin
#' @export

MostCorrelatedFeatures <- setRefClass(Class = "MostCorrelatedFeatures",
                          contains="PredictiveModel",
                          fields=list(coefficients="numeric"),
                          methods = list(
                            initialize = function(...){
                              return(.self)
                            },
                            
                            train = function(featureData, responseData, numFeatures = 200, filterData = TRUE, ...){
                              if(filterData == TRUE){
                                message("filtering data...")
                                processedData <- filterPredictiveModelData(featureData, responseData)
                                featureData <- processedData$featureData
                                responseData <- processedData$responseData
                              }
                              
                              featureCorrs <- apply(featureData, 2, "cor", responseData)
                              topCorrs <- featureCorrs[order(abs(featureCorrs), decreasing=TRUE, na.last=TRUE)[1:numFeatures]]
                              
                              fitCoefs <- lm(responseData ~ featureData[,names(topCorrs)])$coefficients
                              names(fitCoefs)[2:length(fitCoefs)] <- names(topCorrs)
                              fitCoefs <- fitCoefs[which(!is.na(fitCoefs))]
                              .self$coefficients <- fitCoefs
                            },
                            
                            predict = function(featureData){
                              featureNames <- names(.self$coefficients[2:length(.self$coefficients)])
                              predictedResponse <- featureData[,featureNames] %*% .self$coefficients[featureNames]
                              return(predictedResponse)
                            }
                          )
                          )
