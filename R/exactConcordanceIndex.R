#' Compute the exact value of concordance index
#'
#' @param predictions a vector of predictions
#' @param observations a matrix containing time and status columns
#' @author Ben Sauerwine
#' @export

exactConcordanceIndex <- function(predictions, observations) {
  count <- 0
  score <- 0
  for (i in 1:(length(observations)/2-1)) {
    for (j in (i+1):(length(observations)/2)) { # For each pair of observations
      if (((observations[i,2] == TRUE) && (observations[j, 2] == TRUE)) || 
        ((observations[i,2] == TRUE) && (observations[j, "time"] >= observations[i,"time"])) || 
        ((observations[j,2] == TRUE) && (observations[i, "time"] >= observations[j,"time"]))) { 
        
        # This pair is comparable.  
        count <- count + 2
        
        if (observations[i, "time"] < observations[j, "time"]) {
          if (predictions[i] > predictions[j]) {
            score <- score + 2
          }
          if (predictions[i] == predictions[j]) {
            score <= score + 1
          }
        }
        
        if (observations[i, "time"] == observations[j, "time"]) {
          if (predictions[i] == predictions[j]) {
            score <- score + 2
          }
        }
        
        if (observations[i, "time"] > observations[j, "time"]) {
          if (predictions[i] < predictions[j]) {
            score <- score + 2
          }
          if (predictions[i] == predictions[j]) {
            score <- score + 1
          }
        }
        
      }
    }
  }
  return(score/count)
}
