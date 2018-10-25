# Basic Dataframe Descirptive Statistics
#
# Description: Calculate the basic descriptive statistics of your dataframe.
# Statistics: min, max, range, median, mean, variance, standard deviation, and standard error
# Required Parameters: df: dataframe
# Optional Parameters: column: Change to false if your table is organized row-wise,
#                      verbose: Change to true for printing returned results to console

# =====================================================================
graphBase <- function() {
   # Scatter Plot of Data Points
   plot( df,
         type = "p",
         col = "gray50",
         pch = "." )
   
   # Upper and Lower Bounds for framing 
   abline(h = min(df[,2]), col = "gray80")
   abline(h = max(df[,2]), col = "gray80")
   abline(v = min(df[,1]), col = "gray80")
   abline(v = max(df[,1]), col = "gray80")
}
# =====================================================================


# Read in dataframe and convert to matrix and, if neccessary, transpose
readData <- function(df,
                     column = TRUE,
                     verbose = FALSE) {
   if (column) {
      r <- data.matrix(df)
   } else {
      r <- t(data.matrix(df))
   }
   
   if (verbose) {
      print(r)
   }
   
   r
}

# Linear/Non-Linear Least Squares Function ----------------------------
# Object 'a' is a vector or matrix of independent variables.
# Object 'b' is a vector of dependent variable values.
leastSq <- function(a,
                    b,
                    verbose = FALSE) {
   # Create a matrix A
   A <- cbind(1,a)
   # Calculate
   AtA <- t(A) %*% A
   Atb <- t(A) %*% b
   
   # Solve the linear equation
   sol <- as.vector(solve(AtA,Atb))
   
   if (verbose) {
      graphBase()
      
      lines(
         x = c(min(df[,1]), max(df[,1])),
         y = c(sol[1] + (sol[2] * min(df[,1])),
               sol[1] + (sol[2] * max(df[,1]))),
         type = "l",
         col = "chartreuse3"
      )
   }
   
   # Reurn the solution
   sol
}


# Class Relative Frequency Function -----------------------------------
classRelativeFrequency <- function(vect,
                                   verbose = FALSE) {
   # Get vector of unique values in vector
   unqVar <- unique(vect)
   # Hold number of unique values
   n <- length(unqVar)
   
   # Create a vector of relative frequencies
   crf <- sapply(1:n, function(i) {
      length(vect[vect == unqVar[i]]) / n})
   
   # Name respective variables in returned results vector
   names(crf) <- unqVar
   
   if (verbose) {
      print(crf)
   }
   
   # Return results
   crf
}

# Mean Wrapper Function -----------------------------------------------
meanWrapperFunction <- function(df,
                                column = TRUE,
                                verbose = FALSE) { 
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   results <- colMeans(df)
   
   if (verbose) {
      print(results)
      
      graphBase()
      
      points(
         x = results[1],
         y = results[2],
         pch = 8,
         col = "darkorchid" ) 
      
   }
   
   results
}


# Range Function ------------------------------------------------------
rangeFunction <- function(df,
                          column = TRUE,
                          verbose = FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   minimum <- apply(df, 2, min)
   maximum <- apply(df, 2, max)
   
   results <- maximum - minimum
   
   names(results) <- colnames(df)
   
   if (verbose) {
      print(results)
   }
   
   results
}


# Min - Max Wrapper ---------------------------------------------------
minMaxWrapper <- function(df,
                          type,
                          column = TRUE,
                          verbose = FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   if (type == "Max") {
      results <- apply(df, 2, max)
   } else if (type == "Min") {
      results <- apply(df, 2, min)
   } else {
      stop("No Valid Type Given!")
   }
   
   names(results) <- colnames(df)
   
   if (verbose) {
      print(results)
   }
   
   results
}


# Sample Variance -----------------------------------------------------
sampleVariance <- function(df,
                           column=TRUE,
                           verbose=FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   yBar <- meanWrapperFunction(df)
   n <- NROW(df)
   
   results <- sapply(1:NCOL(df),
                     function(i) { sum((df[,i] - yBar[i])^2) / (n - 1) })
   
   names(results) <- colnames(df)
   
   if (verbose) {
      print(results)
   }
   
   results
}


# Standard Deviation --------------------------------------------------
stdDeviation <- function(df,
                         column=TRUE,
                         verbose=FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   results <- sqrt(sampleVariance(df))
   
   names(results) <- colnames(df)
   
   if (verbose) {
      print(results)
   }
   
   results
}


# Kurtosis ------------------------------------------------------------
#
# Description: Implements sample Kurtosis. Implementation from macroption.com.
#              We are assuming that each column has different statistics for 
#              each data point defined by row. We will generate a vector of 
#              of kurtosis measures for each column.
# Required Parameters: df:dataframes
# Optional Parameters: 

kurtosis <- function(df,
                     column=TRUE,
                     verbose=FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   # Variables to kurtosis equation
   n <- NROW(df)
   # Assume each column gives a different statistics
   xavg <- colMeans(df)
   
   ns <- (n * (n + 1) * (n - 1)) / ((n - 2) * (n - 3))
   
   numerator <- sapply(1:NCOL(df),
                       function(i) { sum((df[,i] - xavg[i])^4) })
   denominator <- sapply(1:NCOL(df),
                         function(i) { (sum((df[,i] - xavg[i]) ^ 2))^(2) })
   
   kurtResults <- ns * sapply(1:NCOL(df),
                              function(i) {numerator[i] / denominator[i]})
   
   names(kurtResults) <- colnames(df)
   
   if (verbose) {
      print(kurtResults)
   }
   
   kurtResults
}


# Skew ----------------------------------------------------------------
# Description: Implements sample Skew. Implementation from macroption.com.
#              We are assuming that each column has different statistics for 
#              each data point defined by row. We will generate a vector of 
#              of skew measures for each column.

skew <- function(df,
                 column=TRUE,
                 verbose=FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   # Hold number of datapoints
   n <- NROW(df)
   # Average of each measurement variable
   xavg <- colMeans(df)
   
   ns <- (n * sqrt(n - 1)) / (sqrt(n - 2))
   
   numerator <- sapply(1:NCOL(df),
                       function(i) { sum((df[,i] - xavg[i])^3) })
   denominator <- sapply(1:NCOL(df),
                         function(i) { (sum((df[,i] - xavg[i]) ^ 2))^(2/3) })
   
   skewResults <- ns * sapply(1:NCOL(df),
                              function(i) {numerator[i] / denominator[i]})
   
   names(skewResults) <- colnames(df)
   
   if (verbose) {
      print(skewResults)
   }
   
   skewResults
}


# Descriptive Statistics Function -------------------------------------
descStats <- function(df,
                      column = TRUE,
                      verbose = FALSE) {
   if (is.data.frame(df)) {
      df <- readData(df, column)
   }
   if (!column) {
      df <- t(df)
   }
   
   s <- new.env()
   
   s$min <- minMaxWrapper(df, type = "Min")
   s$max <- minMaxWrapper(df, type = "Max")
   s$range <- rangeFunction(df)
   #s$median <-
   s$mean <- meanWrapperFunction(df)
   s$var <- sampleVariance(df)
   s$sd <- stdDeviation(df)
   #s$se <-
   s$kurt <- kurtosis(df)
   s$skew <- skew(df)
   
   # Print and return results
   if (verbose == TRUE) {
      print("Objects in returned environment:", quote = FALSE)
      print(objects(s))
   }
   
   s
} # End desc_stats function