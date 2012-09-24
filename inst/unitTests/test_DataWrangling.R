
unitTestDataFramesAndBiocObjects <- function() {
    
    ## load the data
    data(demoData)
    
    checkTrue(exists('copyData_ccle'))
    checkTrue(all(rownames(pData(copyData_ccle)) == colnames(exprs(copyData_ccle))))
    checkTrue(exists('oncomapData_ccle'))
    checkTrue(all(rownames(pData(oncomapData_ccle)) == colnames(exprs(oncomapData_ccle))))
    checkTrue(exists('drugData_sanger'))
    
    #### First pass dataframes to the methods #####
    
    ## merge our features
    ds_features_cn_mut_ccle <- createAggregateFeatureDataSet(list(copy = exprs(copyData_ccle), 
                                                                  mut = exprs(oncomapData_ccle)))
    checkEquals(nrow(ds_features_cn_mut_ccle), nrow(exprs(copyData_ccle)) + nrow(exprs(oncomapData_ccle)))
    
    
    ## find the intersection of our feature cell lines and response cell lines
    dataSets_ccleFeaturesCnMut_sangerChems <- createFeatureAndResponseDataList(t(ds_features_cn_mut_ccle), 
                                                                               pData(drugData_sanger))
    
    ## Necessary for legacy support (as this unit test)
    dataSets_ccleFeaturesCnMut_sangerChems$featureData <- t(dataSets_ccleFeaturesCnMut_sangerChems$featureData)
    dataSets_ccleFeaturesCnMut_sangerChems$responseData <- t(dataSets_ccleFeaturesCnMut_sangerChems$responseData)
    
    checkEquals(colnames(dataSets_ccleFeaturesCnMut_sangerChems$featureData), 
                colnames(dataSets_ccleFeaturesCnMut_sangerChems$responseData))  
    
    ## sanity check that it is the size we expect
    checkEquals(sum(23224, 33), nrow(dataSets_ccleFeaturesCnMut_sangerChems$featureData))
    checkEquals(324, ncol(dataSets_ccleFeaturesCnMut_sangerChems$featureData))
    checkEquals(23, nrow(dataSets_ccleFeaturesCnMut_sangerChems$responseData))
    checkEquals(324, ncol(dataSets_ccleFeaturesCnMut_sangerChems$responseData))

    #### Next pass ExpressionSets and Annotated dataframes to the methods #####
    
    ## merge our features
    featuresFromBioc <- createAggregateFeatureDataSet(list(copy = copyData_ccle, 
                                                            mut = oncomapData_ccle))
    checkEquals(ds_features_cn_mut_ccle, featuresFromBioc)
    
    ## find the intersection of our feature cell lines and response cell lines
    featuresAndResponseFromBioc <- createFeatureAndResponseDataList(t(featuresFromBioc), 
                                                                    pData(drugData_sanger))
    
    ## Necessary for legacy support (as this unit test)
    featuresAndResponseFromBioc$featureData <- t(featuresAndResponseFromBioc$featureData)
    featuresAndResponseFromBioc$responseData <- t(featuresAndResponseFromBioc$responseData)
    
    checkEquals(dataSets_ccleFeaturesCnMut_sangerChems, featuresAndResponseFromBioc)     
}

