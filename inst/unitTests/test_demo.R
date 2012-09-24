unitTestScienceOnlineDemo <- function() {
  ### need to load RUnit. How do we load by default?  
  
  ## load the data
  data(demoData)
  
  checkTrue(exists('copyData_ccle'))
  checkTrue(all(rownames(pData(copyData_ccle)) == colnames(exprs(copyData_ccle))))
  checkTrue(exists('oncomapData_ccle'))
  checkTrue(all(rownames(pData(oncomapData_ccle)) == colnames(exprs(oncomapData_ccle))))
  checkTrue(exists('drugData_sanger'))
  
  ## merge our features
  featureData <- createAggregateFeatureDataSet(list(copy = copyData_ccle, mut = oncomapData_ccle))
  featureData <- t(featureData)
  checkEquals(ncol(featureData), nrow(exprs(copyData_ccle)) + nrow(exprs(oncomapData_ccle)))
  
  ## create list with feature and response data with columns in the same order
  dataSets_feature_response <- createFeatureAndResponseDataList(featureData, drugData_sanger)
  checkEquals(rownames(dataSets_feature_response$featureData), rownames(dataSets_feature_response$responseData))  
  
  #### see if we can use caret preProcess function -- for me preProcess(featureData, method = c("center", "scale")) did not work
  ## scale and transpose feature matrix
  featureData_scaled <- scale(dataSets_feature_response$featureData)
  responseData_scaled_matrix <- scale(dataSets_feature_response$responseData[,"PLX4720", drop=FALSE])
  responseData_scaled <- as.numeric(responseData_scaled_matrix)
  names(responseData_scaled) <- rownames(responseData_scaled_matrix)
  
  featureAndResponseData_filtered <- filterPredictiveModelData(featureData_scaled, responseData_scaled,
                                                               filterResponseNas = TRUE, filterFeatureNasBy = "columns", featureVarianceThreshold=0, corPValThresh = .1)
  
  checkEquals(rownames(featureAndResponseData_filtered$featureData), names(featureAndResponseData_filtered$responseData))
  
  ## fit a model
  predictiveModel_eNet <- CaretModel$new(modelType = "glmnet")
  predictiveModel_eNet$customTrain(featureAndResponseData_filtered$featureData, featureAndResponseData_filtered$responseData, tuneGrid=createENetTuneGrid(alphas=1))
  
  ## call plot via CaretModel
  # eNetPredictiveModel$plot() # This member function appears to have been removed... 
  
  model_eNet <- predictiveModel_eNet$rawCaretModel() 
  ## call plot on the raw object returned by caret
  plot(model_eNet)
  
  print(model_eNet$finalModel$tuneValue)
  
  coefs_eNet <- model_eNet$finalModel$beta[, ncol(model_eNet$finalModel$beta)]
 
#### This returns an error when run on Windows 'Error in plot.new() : figure margins too large'
#### It will be rewritten per COMPBIO-3 so there is no point in fixing it for Windows
  plotPredictiveModelHeatmap(coefs_eNet, t(featureAndResponseData_filtered$featureData), featureAndResponseData_filtered$responseData, outputFile="unitTest_plot.jpg")
  
  ## perform machine learing with elastic net
  eNet_cv <- crossValidatePredictiveModel(featureAndResponseData_filtered$featureData, featureAndResponseData_filtered$responseData, 
                                          model=CaretModel$new(modelType="glmnet"), tuneGrid=createENetTuneGrid(alphas=1), numFolds = 3)
  
  plot(eNet_cv$trainPredictions, eNet_cv$trainObservations)
  plot(eNet_cv$testPredictions, eNet_cv$testObservations)
  
  model_glmnet <- GlmnetModel$new()
  model_glmnet$customTrain(featureAndResponseData_filtered$featureData, featureAndResponseData_filtered$responseData, alpha=1)
  
  ## perform machine learning with a custom method (R5, though!)
  MyLassoModel <- setRefClass(Class = "MyLassoModel",
                                  contains = "PredictiveModel", 
                                  fields = c("model"),
                                  methods = list(
                                    customTrain = function(featureData, responseData, ...) {
                                      .self$model <- cv.glmnet(featureData,responseData,family = "gaussian",alpha =1, nfolds=5)
                                    },
                                    
                                    customPredict = function(featureData, ...){
                                      predictedResponse <- predict(.self$model, featureData, s="lambda.min")
                                      return(predictedResponse)
                                    }
                                    )
                                  )
  
  myLassoModel <- MyLassoModel$new()
  myLassoModel$customTrain(featureAndResponseData_filtered$featureData, featureAndResponseData_filtered$responseData)
  plot(myLassoModel$model)
  
  myLassoModel_cv <- crossValidatePredictiveModel(featureAndResponseData_filtered$featureData, featureAndResponseData_filtered$responseData, 
                                                  model=MyLassoModel$new(), numFolds = 3)
  
  plot(myLassoModel_cv$trainPredictions, myLassoModel_cv$trainObservations)
  plot(myLassoModel_cv$testPredictions, myLassoModel_cv$testObservations)
  
  
}

