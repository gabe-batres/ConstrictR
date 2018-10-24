# Degree Centrality

degCent <- function(a,
                    normalize = FALSE) {
   results <- vector(mode = "numeric",
                     length = nrow(a))
   
   for (i in nrow(a)) {
      for (j in ncol(a)) {
         if (a[i, j] == Inf || a[i, j] == 0)
            next()
         results[i] <<- results[i] + a[i, j]
      }
   }
   
   if (normalize == TRUE)
      results <<- results / (nrow(a) - 1)
   
   return(results)
}