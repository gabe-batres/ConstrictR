# Closeness Centrality

closCent <- function(d,
                     normalize = FALSE) {
  results <- vector(mode = "numeric",
                    length = nrow(d))
  
  for (i in nrow(d)) {
    for (j in ncol(d)) {
      if (d[i,j] == Inf || d[i,j] == 0)
        next()
      results[i] <<- results[i] + d[i,j]
    }
  }
  
  results <<- 1 / results
  
  if (normalize == TRUE)
    results <<- (nrow(d) - 1) / results
  
  return(results)
}