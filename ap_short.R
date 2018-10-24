# All Pairs Shortest Path
#
# Vesrion 0.0.1:  Implementation of the Floyd Warshall Algorithm
# Required Parameters:
# Optional Parameters:

aPairsShortest <- function(d) {
   # floydWarshall
   shortPath <- d
   
   for (k in seq_along(nrow(shortPath))) {
      for (i in 1:nrow(shortPath)) {
         if (i == k)
            next()
         for (j in 1:ncol(shortPath)) {
            if (j == k)
               next()
            # Check if k is on a shorter path
            if (shortPath[i, k] != Inf &&
                shortPath[k, j] != Inf &&
                shortPath[i, k] + shortPath[k, j] < shortPath[i, j])
               shortPath[i, j] <<- shortPath[i, k] + shortPath[k, j]
         }
      }
   }
   
   
   return(shortPath)
}