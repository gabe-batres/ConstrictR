# Kurtosis 
#
# Description: Implements sample Kurtosis. Implementation from macroption.com.
#              We are assuming that each column has different statistics for 
#              each data point defined by row. We will generate a vector of 
#              of kurtosis measures for each column.
# Required Parameters: df:dataframes
# Optional Parameters: 

kurtosis <- function(df,column=TRUE,verbose=FALSE) {
  # Assume df is a dataframe, want a matirx.
  df <- data.matrix(df)
  if (column=TRUE){
    n <- length(df[,1])
    xavg <- colMeans(df)
    
    k <- (n(n + 1)(n - 1)) / ((n - 2)(n - 3)) *
      sum((apply(df, MARGIN = 1, 
                 function(x) {x - xavg})) ^ 4) /
      (sum((apply(df, MARGIN = 1,
                  function(x) {x - xavg})) ^ 2))^2
    
    return(k)
  } else {
    # Variables to kurtosis equation
    n <-length(df[1,])
    xavg <- rowMeans(df)
    # Assume each column gives a different statistics
    k <- (n(n + 1)(n - 1)) / ((n - 2)(n - 3)) *
      sum((apply(df, MARGIN = 2, 
                 function(x) {x - xavg})) ^ 4) /
      (sum((apply(df, MARGIN = 2,
                  function(x) {x - xavg})) ^ 2))^2
    
    return(k)
  }
}

