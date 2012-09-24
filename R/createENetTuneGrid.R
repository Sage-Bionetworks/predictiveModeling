#' createENetTuneGrid
#' 
#' Create a tuning grid appropriate for elastic net models 
#'
#' @param lambdamin
#' @param lambdamax
#' @param lambdas a specific vector of lambdas to use instead of the range specified by the lambdamin and lambdamax parameters
#' @param alphamin
#' @param alphamax
#' @param alphas a specific vector of alphas to use instead of the range specified by the alphamin and alphamax parameters
#' @param len number of rows desired in the resulting tuning grid
#' @param verbose whether to print verbose information about the tuning grid
#' @return the tuning grid
#'
#' @author Adam Margolin
#' @export

createENetTuneGrid <- 
		function(lambdamin=-5, lambdamax=0, lambdas=NULL, alphas=NULL, alphamin=NULL, len=5, verbose=TRUE) 
{
	if(is.null(alphas) & is.null(alphamin))
		## The default for alphas
		alphas <- c( 10^(-5:-1),seq(.2,1, by=0.1))
	if(is.null(alphas) & !is.null(alphamin))
		## If we don't want to test all the low alpha values
		alphas <- round(seq(alphamin, 1, length=len), 2)
	## otherwise, use the provided list of alphas
	len <- length(alphas)
  if (is.null(lambdas)){
	  lambdas <- exp(seq(lambdamin, lambdamax, length = 25 * len))
  }
	message(paste("Testing", length(alphas), "values of alpha from", round(min(alphas),5), "to", round(max(alphas),5)))
	message(paste("Testing", length(lambdas), "values of lambda from", round(min(lambdas),3), "to", round(max(lambdas), 3)))
	enetGrid <- expand.grid(.alpha = alphas,
			.lambda = lambdas)
	enetGrid
}
