#' Create a default trControl object for use with caret
#'
#' @return trainControl
#' @seealso caret::trainControl
#' @author Adam Margolin
#' @export
defaultTrainControl <- 
		function()
{
	caret::trainControl(method = "cv", number=3, returnResamp="all", verboseIter=TRUE)
}
