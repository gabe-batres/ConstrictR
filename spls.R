# Partial Least Squares
#
# Description: Partial Least Squares function
# Required Parameters: df: dataframe
# Optional Parameters: column: Calculate columnwise,
#                      verbose: Print results,
#                      iter: number of iterations 

spls1 <- function(df,
                  y,
                  iter = 100,
                  column = TRUE,
                  verbose = FALSE) {
  if (column) {
    df <- data.matrix(df)
  } else {
    df <- t(data.matrix(df))
  }
  
  guess <- matrix(data = NaN, nrow = iter, ncol = length(y))
  guess[1,] <- (t(df) %*% y) / det(t(df) %*% y)
  tk <- vector(mode = "numeric")
  s1 <- vector(mode = "numeric")
  p <- matrix(data = NaN, nrow = iter, ncol = length(y))
  q <- vector(mode = "numeric")
  
  for (k in 1:(iter) ) {
    tk <- df %*% guess[k,]
    s1 <- t(tk) %*% tk
    tk <- tk / s1
    p[k,] <- t(df) %*% tk
    q[k] <- t(y) %*% tk
    
    # if s2 is 0, then break the loop
    if (s2 == 0) {
      break
    }
    
    df <<- df - (s1 * (tk %*% t(pk)))
    guess[(k+1),] <- t(df) %*% y
  }
  
  B <- (guess %*% (t(p) %*% guess)^(-1)) * q
  B0 <- q[1] - t(P) %*% B
}