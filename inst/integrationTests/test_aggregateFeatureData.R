integrationTest_aggregateFeatureData <- function () {
    library("Biobase")
    
    ## Load Data from Synapse
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
    
    checkTrue(exists('exprSet'))
    checkTrue(all(rownames(pData(exprSet)) == colnames(exprs(exprSet))))
    checkTrue(exists('copySet'))
    checkTrue(all(rownames(pData(copySet)) == colnames(exprs(copySet))))
    checkTrue(exists('oncomapSet'))
    checkTrue(all(rownames(pData(oncomapSet)) == colnames(exprs(oncomapSet))))
    checkTrue(exists('sangerADF'))
    
    checkEquals("X143B_BONE", colnames(exprs(copySet))[1])
    checkEquals(883, length(colnames(exprs(copySet))))
    
    checkEquals("X143B_BONE", colnames(exprs(exprSet))[1])
    checkEquals(807, length(colnames(exprs(exprSet))))
    checkEquals(807, length(intersect(colnames(exprs(copySet)), colnames(exprs(exprSet))))) # all are found in the larger set
    
    checkEquals("X143B_BONE", colnames(exprs(oncomapSet))[1])
    checkEquals(786, length(colnames(exprs(oncomapSet))))
    checkEquals(786, length(intersect(colnames(exprs(copySet)), colnames(exprs(oncomapSet))))) # all are found in the larger set
    
    checkEquals(760, length(Reduce('intersect', list(colnames(exprs(copySet)), colnames(exprs(exprSet)), colnames(exprs(oncomapSet))))))
    checkEquals(883, length(Reduce('union', list(colnames(exprs(copySet)), colnames(exprs(exprSet)), colnames(exprs(oncomapSet))))))
    
    checkEquals(23, ncol(pData(sangerADF)))
    checkEquals(524, nrow(pData(sangerADF)))
    checkEqualsNumeric(log10(76833.806), pData(sangerADF)['HUO3N1_BONE', 'Go_6976'])
    checkEqualsNumeric(log10(336.972), pData(sangerADF)['OVCAR8_OVARY', 'MG-132'])
    checkEqualsNumeric(log10(1.29E+00), pData(sangerADF)['ISTMEL1_SKIN', 'Docetaxel'])
}