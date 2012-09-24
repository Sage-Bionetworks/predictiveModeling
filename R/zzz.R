#' Package-local cache if we ever want to stash anything in a cache specific to this package
.cache <- new.env(parent=emptyenv())

#' Package-local cache getter
#'
#' @param key the key of the value to be returned
#' @return the value for that key or null
.getCache <-
		function(key)
{
	.cache[[key]]
}

#' Package-local cache setter
#'
#' @param key the key of the value to be cached
#' @param value the value to be cached
.setCache <-
		function(key, value)
{
	.cache[[key]] <- value
}

#' Package-local cache deleter
#'
#' @param key the key of the key/value pair to be deleted from the cache
.deleteCache <-
		function(keys)
{
	indx <- which(keys %in% ls(.cache))
	if(length(indx) > 0)
		rm(list=keys[indx], envir=.cache)
}

#' Package defaults
.onLoad <-
		function(libname, pkgname)
{
	.setCache("debug", FALSE)
}
