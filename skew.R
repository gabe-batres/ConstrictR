# Kurtosis 
#
# Description: Implements sample Skew. Implementation from macroption.com.
#              We are assuming that each column has different statistics for 
#              each data point defined by row. We will generate a vector of 
#              of skew measures for each column.
# Required Parameters: df:dataframes
# Optional Parameters: 

skew <- function(df,column=TRUE,verbose=FALSE) {
  # Assume df is a dataframe, want a matirx.
  if (column) {
    df <- data.matrix(df)
  } else {
    df <- t(data.matrix(df))
  }
  
  n <- length(df[, 1])
  xavg <- colMeans(df)
  
  k <- (n * (n - 1) ^ (1 / 2)) / (n - 2) *
    sum((apply(df, MARGIN = 1,
               function(x) {
                 x - xavg
               })) ^ 3) /
    (sum((apply(df, MARGIN = 1,
                function(x) {
                  x - xavg
                })) ^ 2)) ^ (3 / 2)
  
  return(k)
}

