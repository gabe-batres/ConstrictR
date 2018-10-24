# K-Means Seeding
#
# Description: Functions for selecting seeds for clustering by K-Means.
# Statistics:
# Existing Parameters: data: matrix coerced from dataframe
#                      type: type of clustering / algorithm to use
#                      k: number of clusters,
#                      distance: distance metric,
#                      linkage: linkage criteria,
#                      repeat: repeat kmeans clustering to gain more accurate results,
#                      column: column major,
#                      verbose: print results


# Verboose and Graphing functions =====================================
kMSeedGraph <- function(initial) {
   # Add Seed Points to created graph
   points(
      x = initial[, 1],
      y = initial[, 2],
      col = "green",
      pch = 8
   )
} # ===================================================================



# Other functions =====================================================
filterPoints <- function(filterAmount) {
   sample(1:NROW(df), as.integer(filterAmount * NROW(df)), replace = FALSE)
}

closePoints <- function(k, distVect, how) {
   if (how == "kClosest") {
      # Remove the k closest data points
      for (i in 1:k) {
         distVect[distVect == min(distVect[distVect > 0])] <- 0
      }
   } else if (how == "Mean") {
      distVect[distVect < (mean(distVect) / k)] <- 0
   }
   
   distVect
}

distanceMaker <- function(point) {
   apply(df, 1, function(x) {distMetric(x, point) })
}

probabilityMaker <- function(distVect) {
   # hold the distances for future operations..
   probDist <- distVect
   # Calculate probablity based on distance to clusters
   probabilityVector <- (probDist ^ 2) / sum(probDist ^ 2)
   probabilityVector
}
#======================================================================



kMSeed <- function(k, 
                   first = "Random", 
                   rest = "PlusPlus",
                   filter = FALSE,
                   filterBy = 0.9,
                   weights = NULL, 
                   d = NaN,
                   verbose = FALSE) {
   # Create empty matrix to hold initial centroid values
   seeds <- matrix(data = 0, nrow = k, ncol = NCOL(df))
   
   if (verbose) {
      # Create a graph
      kMGraph()
   }
   #-------------------------------------------------------------------
   # Create a vector of data points to choose from
   if (filter) {
      indexFilter <- filterPoints(filterAmount = filterBy)
      
      if (verbose) {
         # Add to previously created graph
         points(df[indexFilter,],
                col = "gray30",
                pch = ".")
      }
   } else {
      indexFilter <- 1:NROW(df)
   }
   
   # Assign initial centroid
   if (first == "Random") {
      # Pick a random point as the initial seed
      selectOne <- sample(indexFilter,
                          1,
                          prob = weights)
      seeds[1, ] <- unlist(df[selectOne,])
      
      # Remove chosen point to ensure sampling without replacement
      indexFilter <- indexFilter[-selectOne]
   } else if (first == "Mean") {
      # Choose the first seed as the avearge of all the datapoints
      seeds[1,] <- apply(df, 2, function(x) {mean(x)})
   }
   
   ## Add Close Points Filter
   
   # Seed subsequent centroids
   if (rest == "PlusPlus") {
      # Get distances to newly created cluster
      distances <- distanceMaker(seeds[1,])
      
      # Get weights to choose points by
      probVector <- probabilityMaker(distances[indexFilter])
      
      m <- 2
      
      # If there are more centroids to be selected
      while (m <= k) {
         # Select datapoint at a probability proportional to distance
         #   from nearest centroid
         selected <- sample(indexFilter, 1, prob = probVector)
         # Save the new centroid
         seeds[m,] <- unlist(df[selected,])
         
         indexFilter <- indexFilter[indexFilter != selected]
         
         # Get new minimum distance,
         #   either from the new centroid or from previous centroids
         distances <- distanceMaker(seeds[m, ])
         
         # Remove the selected point from the probability distance vector
         # keep only the minimum distances for probability calculations
         
         # Update the probability vector for available points
         probVector <- probabilityMaker(distances[indexFilter])
         
         m <- m + 1
      }
   } else if (rest == "Random") {
      indexes <- sample(indexFilter, (k - 1), replace = FALSE, prob = weights)
      seeds[-1,] <- unlist(df[indexes, ])
   } else if (rest == "Triangle") {
      ###farfunction1
      # Create a distance vector
      distances <- distanceMaker(seeds[1,])
      names(distances) <- row.names(df)
      
      distances <- closePoints(k, distances, "kClosest")
      
      # Create a vector to use the triangle inequality
      triangleIneq <- distances
      
      for (i in 2:k) {
         # Choose the next seed based on maximum triangulated distance
         seeds[i, ] <- unlist(df[triangleIneq == max(triangleIneq),])
         # Set distance to zero to eliminate it in future calculations
         triangleIneq[triangleIneq == max(triangleIneq)] <- 0
         
         # Calculate distances from new centroid
         distances <- distanceMaker(seeds[i,])
         names(distances) <- row.names(df)
         
         distances <- closePoints(k, distances, "kClosest")
         
         triangleIneq[distances == 0] <- 0
         
         triangleIneq[triangleIneq != 0] <- 
            triangleIneq[triangleIneq != 0] + distances[triangleIneq != 0]
      }
   } else if (rest == "Other") {
      ###farfunction2
      # Create a distance vector
      distances <- distanceMaker(seeds[1,])
      names(distances) <- row.names(df)
      # Remove close points
      distances <- closePoints(k, distances, "Mean")
      # Shrink
      distances <- distances / min(distances[distances > 0])
      
      # Create a vector to use the triangle inequality
      triangleIneq <- distances
      
      for (i in 2:k) {
         # Choose the next seed based on maximum triangulated distance
         seeds[i, ] <- unlist(df[triangleIneq == max(triangleIneq),])
         # Set distance to zero to eliminate it in future calculations
         triangleIneq[triangleIneq == max(triangleIneq)] <- 0
         
         # Calculate distances from new centroid
         distances <- distanceMaker(seeds[i,])
         names(distances) <- row.names(df)
         
         distances <- closePoints(k, distances, how = "Mean")
         distances <- distances / min(distances[distances > 0])
         
         triangleIneq[distances == 0] <- 0
         
         triangleIneq[triangleIneq != 0] <- 
            triangleIneq[triangleIneq != 0] + distances[triangleIneq != 0]
         
      }
   } #-----------------------------------------------------------------
   
   
   
   # Other Seedings ---------------------------------------------------
   if (first == "Least" && rest == "None") {
      # Seed along least squares line
      linEq <- leastSquares()
      
      seeds[1, ] <- c(min(df[,1]),
                      (linEq[1] + linEq[2] * min(df[,1])))
      seeds[k, ] <- c(max(df[,1]),
                      (linEq[1] + linEq[2] * max(df[,1])))
      
      unit <- sqrt(distMetric(seeds[1,], seeds[k, ])) / (k - 1)
      for (i in 2:(k - 1)) {
         seeds[i,1] <- min(df[,1]) + ((i - 1) * unit)
         seeds[i,2] <- linEq[1] + (linEq[2] * seeds[i,1])
      } 
   } else if (first == "Dimension" && rest == "None") {
      unit <- (max(df[,d]) - min(df[,d])) / (k - 1)
      seeds[1,d] <- min(df[,d])
      for (i in 2:k) {
         seeds[i,d] <- seeds[(i - 1),d] + unit
      }
   } else if (first == "MeanMinMaxFar" && rest == "None") {
      seeds[1,] <- apply(df, 2, function(x) {mean(x)})
      seeds[2,] <- c(min(df[,1]), min(df[,2]))
      seeds[3,] <- c(max(df[,1]), min(df[,2]))
      seeds[4,] <- c(min(df[,1]), max(df[,2]))
      seeds[5,] <- c(max(df[,1]), max(df[,2]))
      ###farfunction1
      # Create a distance vector
      distances <- distanceMaker(seeds[1,])
      names(distances) <- row.names(df)
      
      distances <- closePoints(k, distances, "kClosest")
      
      # Create a vector to use the triangle inequality
      triangleIneq <- distances
      
      for (s in 2:5) {
         # Calculate distances from new centroid
         distances <- distanceMaker(seeds[s,])
         names(distances) <- row.names(df)
         
         distances <- closePoints(k, distances, "kClosest")
         
         triangleIneq[distances == 0] <- 0
         
         triangleIneq[triangleIneq != 0] <- 
            triangleIneq[triangleIneq != 0] + distances[triangleIneq != 0]
      }
      
      for (i in 6:k) {
         # Choose the next seed based on maximum triangulated distance
         seeds[i, ] <- unlist(df[triangleIneq == max(triangleIneq),])
         # Set distance to zero to eliminate it in future calculations
         triangleIneq[triangleIneq == max(triangleIneq)] <- 0
         
         # Calculate distances from new centroid
         distances <- distanceMaker(seeds[i,])
         names(distances) <- row.names(df)
         
         distances <- closePoints(k, distances, "kClosest")
         
         triangleIneq[distances == 0] <- 0
         
         triangleIneq[triangleIneq != 0] <- 
            triangleIneq[triangleIneq != 0] + distances[triangleIneq != 0]
      }
   } #-----------------------------------------------------------------
   
   
   if (verbose) {
      print(seeds)
      
      kMSeedGraph(seeds)
   }
   
   seeds
} # ===================================================================
