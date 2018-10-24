# Distance Matrix to Vector Mapping function
#
#


mapp <-function(distanceMatrix, a, b) {
  if(missing(distanceMatrix)) {
    stop("need a distance matrix")
  } else if(missing(a) & missing(b)) {
    n <- nrow(distanceMatrix)
    indexes <-
      cbind(unlist(lapply(x[-15], function(i) {
        x[-(1:i)]
      })), unlist(lapply(x[-15], function(i) {
        rep.int(i, times = (15 - i))
      })))
    return(obj <- as.vector(distanceMatrix[indexes]))
  } else {
    place <- function(a,b){
      (n-1) * b + a
    }
    requested <-
    return()
  }

}