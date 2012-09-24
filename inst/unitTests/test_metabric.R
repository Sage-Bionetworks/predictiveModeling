#unitTestMetabric <- function() {
#  ## TODO: remove the following two lines. the correct way to do this is pkgname::methodname
#  library(survival)
#  library(survcomp)
#  ## tisk-tisk. bad news above.
#  
#  
#  ## load the data
#  data(demoData)
#  
#  checkTrue(exists('exprData_metabric'))
#  #checkTrue(all(rownames(pData(exprData_metabric)) == colnames(exprs(exprData_metabric))))
#  checkTrue(exists('copyData_metabric'))
#  #checkTrue(all(rownames(pData(copyData_metabric)) == colnames(exprs(copyData_metabric))))
#  checkTrue(exists('clinicalData_metabric'))
#  
#  survObj <- Surv(clinicalData_metabric[,"survYears"], clinicalData_metabric[,"survDeath"])
#  
#  #### prepare feature data for predictive modeling by transposing the matrix to have samples on the rows and features on the columns and scaling the columns
#  featureData <-t(createAggregateFeatureDataSet(list(expr = exprData_metabric, copy = copyData_metabric)))
#
#  dataSets_expr_clinical <- filterPredictiveModelData(featureData, survObj, filterFeatureNasBy = "columns")
#  
#  #### Let's make a Cox model now.
#  predictiveModel_cox <- GlmnetModel$new(family="cox")
#  
#  ### as an example that runs quickly, train the model with the first 200 features
#  predictiveModel_cox$customTrain(dataSets_expr_clinical$featureData, dataSets_expr_clinical$responseData)
#  ### get predicted survival values using customPredict
#  trainPredictions <- predictiveModel_cox$customPredict(dataSets_expr_clinical$featureData)
#  ### to evaluate performance calculate concordance.index of the prediction versus observations
#  cIndex_train <- concordance.index(x=trainPredictions, surv.time=dataSets_expr_clinical$responseData[,"time"], surv.event=dataSets_expr_clinical$responseData[,"status"], na.rm=TRUE, alpha= .05)
#  cIndex_train$c.index # returns the cindex1
#  cIndex_train$lower # lower CI bound of cindex1
#  cIndex_train$upper # upper CI bound of cindex1
#  
#  GlmnetModelCV <- GlmnetModel$new(family="cox")
#  
#  cvResults_cox <- crossValidatePredictiveCoxModel(dataSets_expr_clinical$featureData, dataSets_expr_clinical$responseData, GlmnetModelCV, numFolds=5)
#  
#  # by default, 5 fold cross validation
#  cindexTrain<-c()
#  cindexTest<-c()
#  for (i in 1:3){
#    cTrain<-concordance.index(x=cvResults_cox$trainPredictions[[i]], surv.time=cvResults_cox$trainObservations[[i]][,"time"], surv.event=cvResults_cox$trainObservations[[i]][,"status"], na.rm=TRUE, alpha= .05)
#    cindexTrain <- c(cindexTrain,cTrain$c.index)
#    cTest<-concordance.index(x=cvResults_cox$testPredictions[[i]], surv.time=cvResults_cox$testObservations[[i]][,"time"], surv.event=cvResults_cox$testObservations[[i]][,"status"], na.rm=TRUE, alpha= .05)
#    cindexTest <- c(cindexTest,cTest$c.index)
#  }
#  
#  # graphical comparison
#  boxplot(cbind(cindexTrain,cindexTest))
#}
#
