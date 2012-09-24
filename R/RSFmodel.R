
setRefClass(Class = "PredictiveModel")

#' RSFmodel
#'
#' Constructs a predictive survival model using a random forest algorithm.
#'
#' @author Gaurav Pandey
#' @export

RSFmodel <- setRefClass(Class = "RSFmodel",
                        contains = "PredictiveModel",
                        fields=c("model","numFeatures","numTrees"),
                         
                         methods = list(
                           initialize = function(nFeatures=100,nTrees=10){
                             #                                    .self$family=family
                             .self$numFeatures = nFeatures;
                             .self$numTrees = nTrees;
                             return(.self)
                           },
                           
                           customTrain = function(exprData, copyData, clinicalFeaturesData, clinicalSurvData,  ...){
                             featureData <-t(createAggregateFeatureDataSet(list(expr = exprData, copy = copyData)))
                             allfeature_sd <- apply(featureData, 2, sd, na.rm=TRUE)
                             sorted_sd <- sort(allfeature_sd, decreasing=TRUE)
                             sorting_index <- order(allfeature_sd, na.last=TRUE, decreasing=TRUE)
                             data_for_model <- data.frame(featureData[,sorting_index[1:numFeatures]],time=clinicalSurvData[,"time"],status=clinicalSurvData[,"status"])                           
                             .self$model <- rsf(Surv(time,status)~., data=data_for_model, ntree=numTrees, big.data=TRUE)
                             
                           },
                           
                           customPredict = function(exprData, copyData, clinicalFeaturesData){
                                                                featureData <-t(createAggregateFeatureDataSet(list(expr = exprData, copy = copyData)))
                             data_for_test <- data.frame(featureData, pData(clinicalFeaturesData))
                             data_for_test$lymphnodes = factor(data_for_test$lymphnodes)
                             data_for_test$histology = factor(data_for_test$histology)
                             data_for_test$HER2 = factor(data_for_test$HER2)
                             data_for_test$ER = factor(data_for_test$ER)
                             data_for_test$PR = factor(data_for_test$PR)
                             data_for_test$ERPR = factor(data_for_test$ERPR)
                             
                             result <- predict(.self$model, data_for_test)
                             predictedResponse <- result$mortality
                             return(predictedResponse)
                           }
                           )
                         )
