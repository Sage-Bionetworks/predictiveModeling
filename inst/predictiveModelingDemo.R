### R code from vignette source 'predictiveModelingDemo.Rnw'

###################################################
### code chunk number 1: loadlib
###################################################
library(predictiveModeling)

# Change the values of these variables 
myName <- Sys.getenv("USER")
myWorkingDirectory <- "."

setwd(myWorkingDirectory)


###################################################
### code chunk number 2: fakeLogin (eval = FALSE)
###################################################
library(synapseClient)
synapseLogin()

project <- Project(list(
  name=paste("Machine Learning Results - ", myName)
  ))
project <- createEntity(project)

onWeb(project)

analysis <- Analysis(list(
    name="Elastic net versus custom methods",
    description="Several Machine Learning methods run upon CCLE Data with Sanger Drug Response",
    parentId=propertyValue(project, "id")
    ))
analysis <- createEntity(analysis)

dataset <- Dataset(list(
  name="Analysis Plots",
  parentId=propertyValue(project, "id")
  ))
dataset <- createEntity(dataset)


###################################################
### code chunk number 3: fakeLoadData (eval = FALSE)
###################################################
## 
#### Load Data from Synapse ####
idExpressionLayer <- "48344"
expressionLayer <- loadEntity(idExpressionLayer)
exprSet <- expressionLayer$objects$exprSet

idCopyLayer <- "48339"
copyLayer <- loadEntity(idCopyLayer)
copySet <- copyLayer$objects$copySet

idOncomapLayer <- "48341"
oncomapLayer <- loadEntity(idOncomapLayer)
oncomapSet <- oncomapLayer$objects$oncomapSet

idSangerLayer <- "48337"
sangerLayer <- loadEntity(idSangerLayer)
sangerADF <- sangerLayer$objects$sangerADF


###################################################
### code chunk number 4: realLoadData
###################################################
# data(demoData)


###################################################
### code chunk number 5: exploreSangerData
###################################################
print(colnames(pData(sangerADF)))


###################################################
### code chunk number 6: aggregateFeatures
###################################################
ds_features_cn_mut_ccle <- createAggregateFeatureDataSet(list(copy = copySet, mut = oncomapSet))
checkEquals(nrow(ds_features_cn_mut_ccle), nrow(exprs(copySet)) + nrow(exprs(oncomapSet)))


###################################################
### code chunk number 7: intersectFeaturesAndResponse
###################################################
dataSets_ccleFeaturesCnMut_sangerChems <- createFeatureAndResponseDataList(ds_features_cn_mut_ccle, sangerADF)

ls(dataSets_ccleFeaturesCnMut_sangerChems)
checkEquals(colnames(dataSets_ccleFeaturesCnMut_sangerChems$featureData), colnames(dataSets_ccleFeaturesCnMut_sangerChems$responseData))


###################################################
### code chunk number 8: normalizeData
###################################################
featureData_scaled <- t(scale(t(dataSets_ccleFeaturesCnMut_sangerChems$featureData)))
responseData_scaled <- t(scale(t(dataSets_ccleFeaturesCnMut_sangerChems$responseData["PLX4720",,drop=FALSE])))

print(dim(featureData_scaled))
print(dim(responseData_scaled))


###################################################
### code chunk number 9: brafMutBoxplot
###################################################
boxplot(as.numeric(responseData_scaled) ~ featureData_scaled["BRAF_mut",], names = c("BRAF WT", "BRAF Mut"), ylab="PLX4720 IC50", col="blue")


###################################################
### code chunk number 10: trainElasticNetModel
###################################################
predictiveModel_eNet <- CaretModel$new(modelType = "glmnet")
predictiveModel_eNet$train(t(featureData_scaled), t(responseData_scaled), tuneGrid=createENetTuneGrid(alphas=1))


###################################################
### code chunk number 11: evaluateElasticNetTuningParameter
###################################################
caretModel_eNet <- predictiveModel_eNet$rawCaretModel()
print(plot(caretModel_eNet))
print(caretModel_eNet$finalModel$tuneValue)


###################################################
### code chunk number 12: plotElasticNetHeatmap
###################################################
coefs_eNet <- caretModel_eNet$finalModel$beta[, ncol(caretModel_eNet$finalModel$beta)]
outputFileElasticNet <- 'PLX4720_ElasticNetModel.jpg'
plotPredictiveModelHeatmap(coefs_eNet, featureData_scaled, responseData_scaled, outputFile=outputFileElasticNet)


###################################################
### code chunk number 13: storeElasticNet (eval = FALSE)
###################################################
elasticNetLayer <- Layer(list(
                            name="ElasticNet Results for PLX4720",
                            type="M", 
                            parentId=propertyValue(dataset, "id")))
elasticNetLayer <- addFile(elasticNetLayer, outputFileElasticNet)
storeEntity(elasticNetLayer)
## 
## step1 <- stopStep()
## onWeb(step1)
## 
## propertyValue(step1, 'name') <- "Single run using elastic net"
## propertyValue(step1, 'description') <- "I found that ... looked very interesting due to ..."
## step1 <- updateEntity(step1)
## 
## step2 <- startStep(analysis)
## propertyValue(step2, 'name') <- "Cross validation using elastic net"
## propertyValue(step2, 'input') <- propertyValue(step1, "input")
## step2 <- updateEntity(step2)
## 
## onWeb(analysis)
## 
## 


###################################################
### code chunk number 14: crossValidateElasticNet
###################################################
cvResults_eNet <- crossValidatePredictiveModel(t(featureData_scaled), t(responseData_scaled), model=CaretModel$new(modelType="glmnet"), tuneGrid=createENetTuneGrid(alphas=1), numFolds=3)


###################################################
### code chunk number 15: plotElasticNetPredAndObs
###################################################
cvResults_eNet$plotPredAndObs()


###################################################
### code chunk number 16: crossValidateElasticNet
###################################################
cvResults_eNet_lowLambda <- crossValidatePredictiveModel(t(featureData_scaled), t(responseData_scaled), model=CaretModel$new(modelType="glmnet"), tuneGrid=createENetTuneGrid(alphas=1, lambdas=c(2e-10, 1e-10)), numFolds=3)

cvResults_eNet_lowLambda$plotPredAndObs()


###################################################
### code chunk number 17: boxplot_eNet_vs_eNetLowLambda
###################################################
boxplot(cvResults_eNet$getTestError(), cvResults_eNet_lowLambda$getTestError(), log="y", names = c("ENet", "ENet Low Lambda"), ylab="Test Errors", col="blue")
t.test(cvResults_eNet$getTestError(), cvResults_eNet_lowLambda$getTestError())


###################################################
### code chunk number 18: fitMostCorrFeaturesModel
###################################################
predictiveModel_mostCorr <- MostCorrelatedFeatures$new()
predictiveModel_mostCorr$train(t(featureData_scaled), t(responseData_scaled))

outputFileMostCorr <- 'PLX4720_MostCorrModel.jpg'
coefs_mostCorr <- predictiveModel_mostCorr$coefficients[2:length(predictiveModel_mostCorr$coefficients)]
plotPredictiveModelHeatmap(coefs_mostCorr, featureData_scaled, responseData_scaled, outputFile=outputFileMostCorr)


###################################################
### code chunk number 19: plotMostCorrFeaturesModelPredAndObs
###################################################
cvResults_mostCorr <- crossValidatePredictiveModel(t(featureData_scaled), t(responseData_scaled), model=MostCorrelatedFeatures$new(), numFolds=3)
cvResults_mostCorr$plotPredAndObs()


###################################################
### code chunk number 20: boxplot_eNet_vs_eNetLowLambda_vs_mostCorrFeatures
###################################################
cvResults_eNet$getR2()
cvResults_eNet_lowLambda$getR2()
cvResults_mostCorr$getR2()

boxplot(cvResults_eNet$getTestError(), cvResults_eNet_lowLambda$getTestError(), cvResults_mostCorr$getTestError(), log="y", names = c("ENet", "ENet Low Lambda", "Multiple Regression"), ylab="Test Error", col="blue")


###################################################
### code chunk number 21: sessionInfo
###################################################
toLatex(sessionInfo())


