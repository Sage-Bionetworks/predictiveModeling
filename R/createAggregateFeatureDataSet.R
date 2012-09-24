#' createAggregateFeatureDataSet
#'
#' Combines multiple datasets into a feature matrix.
#' Takes a list of matrices or ExpressionSets and rbinds them using their intersection of column names. The key value
#' for each dataset given as a argument is pre-pended to the name of the features of that dataset.
#'
#' @author Adam Margolin
#' @export

createAggregateFeatureDataSet <- 
		function(dataSetList)
{

    ## If its a list of ExpressionSets, convert them to data frames
    if(is(dataSetList[[1]], "ExpressionSet")) {
      dataSetList <- lapply(dataSetList, exprs)
    }
  
	## Retrieve the column names from each feature set and determine
	## the names in common
	colNamesList <- lapply(dataSetList, colnames)
	commonColNames <- Reduce('intersect', colNamesList)
	
	## For each datset, append the name of the dataset to each feature 
	for (curDsName in names(dataSetList)){
		rownames(dataSetList[[curDsName]]) <- sapply(rownames(dataSetList[[curDsName]]),
				function(rowName) { paste(rowName, curDsName, sep="_")}, USE.NAMES = FALSE)
	}
	
	## For each feature set, make a matrix with only the column
	## names in common and then rbind the matrices together
	ds_common_featList <- lapply(dataSetList, function(x){x[,commonColNames]})
	ds_feat <- do.call("rbind", ds_common_featList)
	
	ds_feat
}
