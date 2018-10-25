# Agglomerative Heirarchical Clustering
#
# Description: Agglomerative Heirarchical Clustering program for matrix objects.
#              !!! - We exist inside the parent frame so the data object and other variables are previously decalared
# Statistics:
# Existing Parameters: data: matrix coerced from dataframe
#                      type: type of clustering / algorithm to use
#                      k: number of clusters,
#                      distance: distance metric,
#                      linkage: linkage criteria,
#                      repeat: repeat kmeans clustering to gain more accurate results,
#                      column: column major,
#                      verbose: print results


hierarchical <- function(distanceMetric = "euclidean",
                         linkage = "average") {
   # ---------------------------------------------- Set distance matric
   if (distanceMetric == "euclidean") {
      distMetric <- function(a, b) {
         sqrt(sum((a - b) ^ 2))
      }
   } else if (distanceMetric == "euclidSquare") {
      distMetric <- function(a, b) {
         sum((a - b) ^ 2)
      }
   } else {
      print("No Valid Distance Metric Selected!")
      stop()
   }
   
   
   # --------------------------------------------- Set linkage criteria
   if (linkage == "average") {
      linkCrit <- function(clustA, clustB) {
         if (is.vector(clustA)) {
            avgA <- clustA
         } else {
            avgA <- colMeans(clustA)
         }
         
         if (is.vector(clustB)) {
            avgB <- clustB
         } else {
            avgB <- colMeans(clustB)
         }
         # Average the points in given clusters, then calculate distance
         distMetric(avgA, avgB)
      }
   } else if (linkage == "single") {
      linkCrit <- function(clustA, clustB) {
         # Find the closest 2 datapoints
         min(apply(cbind(rep(1:length(clustA), each = length(clustB)),
                         rep(1:length(clustB), times = length(clustA))),
                   1,
                   function(x) { distMetric(clustA[x[1]], clustB[x[2]]) }))
      }
   } else if (linkage == "complete") {
      linkCrit <- function(clustA, clustB) {
      # Find the farthest 2 datapoints
      max(apply(cbind(rep(1:length(clustA), each = length(clustB)),
                      rep(1:length(clustB), times = length(clustA))),
                1,
                function(x) { distMetric(clustA[x[1]], clustB[x[2]]) }))
      }
   } else {
      print("No Valid Linkage Selected!")
      stop()
   }
   
   
   # ------------------------------------------------ Variable Creation
   # Create an empty distance matrix
   distMatrix <- matrix(
      data = NaN,
      nrow = NROW(df),
      ncol = NROW(df)
   )
   rownames(distMatrix) <- row.names(df)
   colnames(distMatrix) <- row.names(df)
   
   # Create a matrix to return values
   returnMatrix <- matrix(
      NaN,
      nrow = nrow(distMatrix),
      ncol = (nrow(distMatrix) - 1)
   )
   rownames(returnMatrix) <- rownames(distMatrix)
   colnames(returnMatrix) <- sapply(1:ncol(returnMatrix), 
                                   function(x) { paste("Iteration ", x) })
   
   # --------------------------------------------------- Initialization
   # Enter initial values into new data structures:
   # First index in return matrix is just the distance matrix labels
   returnMatrix[, 1] <- rownames(distMatrix)
   
   
   # Find and enter distances into distance matrix
   for (x in 1:nrow(distMatrix)) {
      for (y in 1:ncol(distMatrix)) {
         # Distances are symmetric
         if (x > y) {
            distMatrix[x, y] = distMatrix[y, x]
         } else if (x == y) {
            # Keep distance from self at NaN
            next()
         } else if (x < y) {
            # Get distance between points by calling linkage function
            distMatrix[x, y] <-
               linkCrit(df[x,], df[y,])
         }
      }
   }
   
   # Reset iterator tracker
   iter <- 1
   
   
   # Begin looping over distance matrix until we have 1 cluster
   while (length(unique(as.vector(returnMatrix[, iter]))) != 1) {
      # Find minimum distance and hold index values
      # This call returns only 1st location found when treating a matrix like a vector(?)
      minDistInd <- 
         as.vector(arrayInd(which.min(distMatrix), dim(distMatrix)))
      
      # Get new cluster from previous cluster names from minimum distance indices
      newCluster <-
         which(returnMatrix[, ncol(returnMatrix)] == rownames(distMatrix[minDistInd, ]),
               arr.ind = TRUE)
      
      newName <- paste("cluster #", iter)
      
      # Duplicate the last column of the return matrix
      if (iter > 1) {
         returnMatrix[, iter - 1] <- returnMatrix[, iter]
      }
      # Replace cluster names for rows matching desired points
      for (name in rownames(newCluster)) {
         returnMatrix[name, paste("Iteration", iter, sep = " ")] <-
            paste("clust", iter, sep = "-")
      }
      
      # Delete previous rows and columns in distance matrix
      distMatrix <- distMatrix[-minDistInd,-minDistInd]
      # Add row and column to distance matrix to hold new distance values
      distMatrix <- cbind(distMatrix, NaN)
      distMatrix <- rbind(distMatrix, NaN)
      # Name the new row and column
      colnames(distMatrix)[ncol(distMatrix)] <- newName
      rownames(distMatrix)[nrow(distMatrix)] <- newName
      
      # Updated new row and column with distances to other clusters
      for (cn in 1:colnames(distMatrix)) {
         if (cn == newName) {
            break
         }
         # Find which points are in a cluster
         otherClust <-
            which(returnMatrix[, ncol(returnMatrix)] == cn,
                  arr.ind = TRUE,
                  useNames = TRUE)
         # Run linkage between the new cluster and current cluster in loop
         distMatrix[cn, nrow(distMatrix)] <-
            linkCrit(rownames(newCluster), rownames(otherClust))
         # Since distances are symmetric
         distMatrix[nrow(distMatrix), cn] <-
            distMatrix[cn, nrow(distMatrix)]
      }
      
      
      # Increment iterator
      iter <- iter + 1
      
   } # End Loop
   
   # Return list of iterations of clustering
   return(list)
   
} # End hierarchical clustering function