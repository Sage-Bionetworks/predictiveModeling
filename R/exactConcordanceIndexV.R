#' Compute the exact value of concordance index (faster version)
#'
#' @param predictions a vector of predictions
#' @param observations a matrix containing time and status columns
#' @author Erhan Bilal
#' @export

exactConcordanceIndexV <- function(predictions, observations) {
  pos1<-which(is.na(predictions)==0)

  pos2<-which(is.na(observations[,1])==0)

  pos3<-which(is.na(observations[,2])==0)
  pos<-intersect(pos1,intersect(pos2,pos3))
  predictions <-predictions[pos]
  observations<-observations[pos,]
  
  xy <- expand.grid(i=1:(length(observations)/2), j=1:(length(observations)/2))
  
  iComparable <- ((xy$i < xy$j) & ((observations[xy$i,2] == TRUE & observations[xy$j, 2] == TRUE) | 
    (observations[xy$i,2] == TRUE & (observations[xy$j,"time"] > observations[xy$i,"time"])) | 
    (observations[xy$j,2] == TRUE & (observations[xy$i,"time"] > observations[xy$j,"time"]))))
  
  add2 <- (iComparable & (observations[xy$i,"time"] < observations[xy$j,"time"] & predictions[xy$i] > predictions[xy$j] | 
    observations[xy$i,"time"] > observations[xy$j,"time"] & predictions[xy$i] < predictions[xy$j] | 
    observations[xy$i,"time"] == observations[xy$j,"time"] & predictions[xy$i] == predictions[xy$j]))

  add1 <- (iComparable & (observations[xy$i,"time"] != observations[xy$j,"time"] & predictions[xy$i] == predictions[xy$j]))
  
  return((2*sum(add2) + sum(add1)) / (2*sum(iComparable)))
  
}  
