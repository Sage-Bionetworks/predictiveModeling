# This demo uses an unreleased version of caret
# http://dl.dropbox.com/u/10053401/caret_5.06.002.tar.gz

library(predictiveModeling)
data(demoData)


###################################################
### Use data wrangling methods from predictiveModeling 
### to get our data in shape for caret
###################################################
ds_chem_ic50s_sanger <- log10(ds_chem_ic50s_sanger)
print(rownames(ds_chem_ic50s_sanger))

ds_features_cn_mut_ccle <- createAggregateFeatureDataSet(list(copy = ds_copy_ccle, 
                                                              mut = ds_oncomap_ccle))
checkEquals(nrow(ds_features_cn_mut_ccle), nrow(ds_copy_ccle) + nrow(ds_oncomap_ccle))


dataSets_ccleFeaturesCnMut_sangerChems <- createFeatureAndResponseDataList(ds_features_cn_mut_ccle, 
                                                                           ds_chem_ic50s_sanger)
ls(dataSets_ccleFeaturesCnMut_sangerChems)
checkEquals(colnames(dataSets_ccleFeaturesCnMut_sangerChems$featureData), 
            colnames(dataSets_ccleFeaturesCnMut_sangerChems$responseData))


featureData_scaled <- t(scale(t(dataSets_ccleFeaturesCnMut_sangerChems$featureData)))
responseData_scaled <- 
  t(scale(t(dataSets_ccleFeaturesCnMut_sangerChems$responseData["PLX4720",,drop=FALSE])))

print(dim(featureData_scaled))
print(dim(responseData_scaled))


boxplot(as.numeric(responseData_scaled) ~ featureData_scaled["BRAF_mut",], 
        names = c("BRAF WT", "BRAF Mut"), 
        ylab="PLX4720 IC50", col="blue")


processedData <- filterPredictiveModelData(t(featureData_scaled),
                                           t(responseData_scaled))
featureData <- processedData$featureData
responseData <- processedData$responseData

###################################################
### Data is wrangled, now let's fit some models using 
### caret
###################################################

## See the caret Train vignette for more information about these functions and their parameters

trControl <- caret::trainControl(method = "cv", 
                                 number=3, 
                                 returnResamp="all", 
                                 verboseIter=TRUE)

model_eNet <- caret::train(featureData, 
                           responseData,
                           method="glmnet",
                           preProcess = NULL,
                           tuneLength = 4,
                           trControl = trControl,
                           tuneGrid=createENetTuneGrid(alphas=1)
                           )

print(plot(model_eNet))
print(model_eNet)
print(model_eNet$finalModel$tuneValue)

coefs_eNet <- model_eNet$finalModel$beta[, ncol(model_eNet$finalModel$beta)]
outputFileElasticNet <- 'PLX4720_ElasticNetModel.jpg'
plotPredictiveModelHeatmap(coefs_eNet, 
                           featureData_scaled, 
                           responseData_scaled, 
                           outputFile=outputFileElasticNet)


###################################################
### Let's do cross validation on several models, leaving 
### out a subset of the data for test purposes
###################################################

set.seed(2)

inTrain <- createDataPartition(responseData, p = .8, list = FALSE, times = 1)
trainFeatureData <- featureData[inTrain,]
testFeatureData <- featureData[-inTrain,]
trainResponseData <- responseData[inTrain]
testResponseData <- responseData[-inTrain]

# For each model, perform three separate 10-fold cross-validations as the resampling scheme
cvTrainControl <- caret::trainControl(method = "repeatedcv", 
                                      number = 10,
                                      repeats = 3, 
                                      returnResamp="all", 
                                      verboseIter=TRUE)

# model 1: elastic net
eNet_cv <- caret::train(trainFeatureData, 
                        trainResponseData,
                        method="glmnet",
                        preProcess = NULL,
                        tuneLength = 4,
                        trControl = cvTrainControl,
                        tuneGrid=createENetTuneGrid(alphas=1)
                        )

# model 2: low lambda elastic net
eNet_lowLambda_cv <- caret::train(trainFeatureData, 
                                  trainResponseData,
                                  method="glmnet",
                                  preProcess = NULL,
                                  tuneLength = 4,
                                  trControl = cvTrainControl,
                                  tuneGrid=createENetTuneGrid(alphas=1,
                                    lambdas=c(2e-10, 1e-10))
                                  )

# model 3: a custom model implementing a linear model
# TODO this model doesn't make much sense, the tuning grid is not used, the sort method is irrelevant

linearModelTrain <- function(data, parameter, levels, last, ...) {
            message("in linear model train")
            linearModel <- lm(.outcome ~ ., data)
            list(fit=linearModel)
}


linearModelPredict <- function(object, newdata) {
    message("in linear model predict")
    predict(object$fit, newdata) # this calls predict.lm
  
#   testCoefs <- object$fit$coefficients
#   testCoefs <- testCoefs[-1]
#   testCoefs <- testCoefs[!is.na(testCoefs)]
#   testCoefs <- as.matrix(testCoefs)
#   testFeatures <- newdata[, row.names(testCoefs)]
#   prediction <- testFeatures %*% testCoefs
#   prediction <- prediction + object$fit$coefficients[1]
}

linearModelSort <- function(x) {
    message("in linear model sort")
    x[order(x$alpha, x$lambda),]
}

mytrain <- function(...) linearModelTrain(...)
mypredict <- function(...) linearModelPredict(...)
mysort <- function(...) linearModelSort(...)

## If you want to step through your custom methods, uncomment these lines
# debug(linearModelTrain)
# debug(linearModelPredict)
# debug(linearModelSort)

customCvTrainControl <- caret::trainControl(method = "repeatedcv",
                                            number = 2, #10, this takes a long time to run so let's try and make it shorter
                                            repeats = 2, #3,
                                            returnResamp="all", 
                                            verboseIter=TRUE,
                                            custom = list(
                                              parameters = createENetTuneGrid(lambdas=c(0.001, 1), alphas=c(1,1)),
                                              model = mytrain,
                                              prediction = mypredict,
                                              probability = NULL,
                                              sort = mysort))

myLinear_cv <- caret::train(trainFeatureData, 
                            trainResponseData,
                            method="custom",
                            trControl = customCvTrainControl
                            )


###################################################
### Now let's compare their performance
###################################################

# Model 1
eNet_cv_predVals <- extractPrediction(list(eNet_cv), 
                                      testX = testFeatureData, 
                                      testY = testResponseData)

plotObsVsPred(eNet_cv_predVals)

testPred <- subset(eNet_cv_predVals, dataType == "Test")
head(testPred)
ddply(testPred, .(model), defaultSummary)


# Model 2
eNet_lowLambda_cv_predVals <- extractPrediction(list(eNet_lowLambda_cv), 
                                                testX = testFeatureData, 
                                                testY = testResponseData)

plotObsVsPred(eNet_lowLambda_cv_predVals)

testPred <- subset(eNet_lowLambda_cv_predVals, dataType == "Test")
head(testPred)
ddply(testPred, .(model), defaultSummary)


# Models 3
myLinear_cv_predVals <- extractPrediction(list(myLinear_cv), 
                                          testX = testFeatureData, 
                                          testY = testResponseData)

plotObsVsPred(myLinear_cv_predVals)

testPred <- subset(myLinear_cv_predVals, dataType == "Test")
head(testPred)
ddply(testPred, .(model), defaultSummary)
