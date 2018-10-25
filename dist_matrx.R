# Distance Matrix Creation Function
#
# Description:
# Required Parameters: df: dataframe
# Optional Parameters: drected: Change to TRUE if graph is directed,
#                      verbose: Change to true for printing returned results to console

distMatrix <- function(df,
                       #directed = FALSE,
                       verbose = FALSE) {
   #distMetric <- setDist()
   
   # Create a matrix, then calculate distances
   results <- matrix(apply(cbind(rep(1:NROW(df), each = NROW(df)),
                                 rep(1:NROW(df), times = NROW(df))),
                           1,
                           function(x) {
                              distMetric(df[x[1],], df[x[2],]) }),
                     nrow = nrow(df),
                     ncol = nrow(df))
   
   # Diagonal of a distance matrix should be 0's
   results[cbind(1:nrow(results), 1:nrow(results))] <- 0
   
   # Name the columns and rows of the distance matrix
   rownames(results) <- colnames(results) <- rownames(df)
   
   # Print and return results
   if (verbose) {
      print(results)
   }
   
   results
   
} # End distMatrix function
