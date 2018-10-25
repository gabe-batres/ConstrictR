# K-Means clustering
#
# Description: K-Means Clustering program for matrix objects.
#              !!! - We exist inside the parent frame so the data object \
#                    and other variables are previously decalared
# Statistics:
# Existing Parameters: df: dataframe
#                      type: type of clustering / algorithm to use
#                      k: number of clusters,
#                      distance: distance metric,
#                      linkage: linkage criteria,
#                      repeat: repeat kmeans clustering to gain more accurate results,
#                      column: column major,
#                      verbose: print results

# Get neccessary sub-functions for running k-Means Clustering
#source("kMSeeding.R")
#source("kMClusterFunctions.R")

# Create a graph function =============================================
kMGraph <- function() {
   # Scatter Plot of Data Points
   plot(df,
        type = "p",
        col = "gray80",
        pch = ".")
   
   # Upper and Lower Bounds
   abline(h = min(df[, 2]), col = "gray90")
   abline(h = max(df[, 2]), col = "gray90")
   abline(v = min(df[, 1]), col = "gray90")
   abline(v = max(df[, 1]), col = "gray90")
   
   # Center Point
   points(x = ((max(df[, 1]) + min(df[, 1])) / 2),
          y = ((max(df[, 2]) + min(df[, 2])) / 2),
          pch = 4)
   
   # Mean Point
   points(
      x = mean(df[, 1]),
      y = mean(df[, 2]),
      col = "red",
      pch = 3
   )
   
   ### Least Squares Line, call function
   A <- cbind(1,unlist(df[,1]))
   AtA <- t(A) %*% A
   Atb <- t(A) %*% df[,2]
   
   lsLine <- as.vector(solve(AtA, Atb))
   lines(
      x = c(min(df[, 1]),
            max(df[, 1])),
      y = c(lsLine[1] + (lsLine[2] * min(df[, 1])),
            lsLine[1] + (lsLine[2] * max(df[, 1]))),
      type = "l",
      col = "purple"
   )
} # ===================================================================


# Functions -----------------------------------------------------------
# Set distance matrix
distMetric <- setDist(distance = "euclidSquare")

compareClusts <- function(k, clustrs) {
   assigned <- simpleAssign(clustrs)
   
   
   # Update the distances to the centroids
   kMdist <- matrix(apply(cbind(rep.int(1:NROW(df), k),
                                rep(1:k, each = NROW(df))),
                          1, 
                          function(x) {
                             distMetric(df[x[1],], clustrs[x[2], ])
                          }),
                    nrow = NROW(df),
                    ncol = k)
   
   # Get the sum of the variabilities
   thisDiss <- sum(sapply(1:k, 
                          function(v) { 
                             sum(kMdist[which(assigned == v), v]) }))
   
   thisDiss
}
#----------------------------------------------------------------------
# Functional call of a single iteration of k-Means clustering
kMPass <- function(k, verbose = FALSE, ...) {
   
   # Get new k seeds
   seeds <-
      kMSeed(k,
             first = "Random",
             rest = "PlusPlus",
             verbose = verbose)
   
   # Run clustering
   kMClustering(seeds,
                choice = "lloyds",
                verbose = verbose,
                times = 25)
}





# Clustering function
kMean <- function(k,
                  nTimes,
                  cached = TRUE,
                  verbose = FALSE,
                  ...) {
   
   # For holding best results through multiple clustering calls
   bestResults <- matrix(data = NaN,
                         nrow = k,
                         ncol = NROW(df))
   
   bestDiss <- numeric(length = 1)
   
   while (nTimes >= 1) {
      # Run k-Means Clustering
      currentResults <- kMPass(k = k,
                               verbose = verbose)
      
      if (all(is.nan(bestResults))) {
         # Save the best results for later
         bestResults <- currentResults
         # Get the dissimilarity; the sum of the intercluster variability
         # First update all the distances to the closest centroids
         
         # Finally hold the value
         bestDiss <- compareClusts(k, currentResults)
      } else if (nTimes >= 1) {
         if (verbose) {
            print("Comparing Clusters.")
         }
         
         thisDiss <- compareClusts(k, currentResults)
         
         if (thisDiss < bestDiss) {
            if (verbose) {
               print("Better Clustering Found!")
            }
            
            bestResults <- currentResults
            bestDiss <- thisDiss
         }
      }
      
      nTimes <- nTimes - 1
   }
   
   # Return K-Means cetroids
   bestResults
} # End K-Means
