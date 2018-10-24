# Clustering Function
#
# Description: Clustering program for dataframe objects.
#              Here we coerce the data to a matrix for easier mathematic manipulation.
#              And we allow selection of clustering algorithm.
# Statistics:
# Required Parameters: df: dataframe
#                      type: type of clustering / algorithm to use
# Optional Parameters: k: number of clusters,
#                      distance: distance metric,
#                      linkage: linkage criteria,
#                      repeat: repeat kmeans clustering to gain more accurate results,
#                      column: column major,
#                      verbose: print results


# Load agglomerative hierarcical function file
#source("hierarchical.R")
# Load kmeans function file
#source("kMean.R")

setDist <- function(distanceMetric = "euclidean") {
   if (distanceMetric == "euclidean")
      function(a, b) {
         sqrt(sum((a - b)^2))
      }
   if (distanceMetric == "euclidSquare")
      function(a, b) {
         sum((a - b)^2)
      }
}

clust <-
   function(df,
            type,
            k        = Inf,
            iter     = 1,
            linkage  = "avg",
            column   = TRUE,
            verbose  = FALSE,
            ...) {
      # Coerce data.frame to matrix
      if (column) {
         data <- data.matrix(df)
      } else {
         # If data is row major, transpose
         data <- t(data.matrix(df))
      }
      
      
      # List of clustering types
      clusterings <- list(hierarchical = hierarchical(),
                          k = kMean(),
                          print("No type given"))
      
      # Select and run clustering on data
      switch(type, clusterings)
      
   } # End function