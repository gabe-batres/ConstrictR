# Dimension Reduction
#
# Description: Dimension Reduction  program for dataframe objects. Here we coerce the data to a matrix for easier mathematic manipulation.  And we allow selection of dimension algorithm.
# Statistics:
# Required Parameters: df: dataframe
#                      type: type of clustering / algorithm to use
# Optional Parameters: k: number of clusters,
#                      distance: distance metric,
#                      linkage: linkage criteria,
#                      repeat: repeat kmeans clustering to gain more accurate results,
#                      column: column major,
#                      verbose: print results


# Partial Least Squares
source(spls)

dimReduct <-
  function(df,
           type,
           column   = TRUE,
           verbose  = FALSE,
           ...) {
    dimReduc <-new.env()
    
    # Coerce data.frame to matrix
    if (column) {
      dimReduc$data <- data.matrix(df)
    } else {
      # If data is row major, transpose
      dimReduc$data <- t(data.matrix(df))
    }
    
    # List of clustering types
    clusterings <- list(hierarchical = hierarchical(),
                        k = kMean(),
                        print("No type given"))
    
    # Select and run clustering on data
    switch(type, clusterings)
    
    return()
  } # End function