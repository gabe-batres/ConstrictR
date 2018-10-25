# k-Means Clustering Algorithms
# 
# Description: 
# Statistics:
# Parameters: data: matrix coerced from dataframe
#                   type: type of clustering / algorithm to use
#                   k: number of clusters,
#                   distance: distance metric,
#                   repeat: repeat kmeans clustering to gain more \
#                             accurate results,
#                   column: column major,
#                   verbose: print results

# Verboose and Graphing functions =====================================
kMCentsGraph <- function(currentCentroids, useColor) {
   # Add Seed Points to created graph
   points(
      x = currentCentroids[, 1],
      y = currentCentroids[, 2],
      col = useColor,
      pch = 20
   )
}


kMClusterVerbose <- function(pointsList) {
   centColors <- heat.colors((length(pointsList) - 1), alpha = 0.5)
   
   #assignedColors <- rainbow(k, alpha = 0.1)
   #for (d in 1:k) {
   #   points(
   #      x = df[assigned == d, ][, 1],
   #      y = df[assigned == d, ][, 2],
   #      col = assignedColors[d],
   #      pch = 1
   #   )
   #}
   
   for (c in seq_along(pointsList)) {
      kMCentsGraph(unlist(pointsList[[c]]), centColors[c])
   }
   
   kMCentsGraph(pointsList[[length(pointsList)]], "blue")
} # ===================================================================





# Some Functions ======================================================

distVectM <- function(filter, point) {
   apply(df[filter,], 1, function(row) { distMetric(row, point) })
}


moveCs <- function(k, centroids) {
   k <- k
   # Create a variable to hold calculations
   newCs <- centroids
   
   function(assigned) {
      # Move the centroids AND track each points movement
      for (j in 1:k) {
         # IFF a point lands in an empty space, leave it until the next calculations
         if (length(assigned[assigned == j]) == 0) {
            print("Bad Centroid!")
            next
         }
         # Calculate new centroids
         newCs[j,] <<- colMeans(df[(assigned == j),])
      }
      newCs
   }
}

simpleAssign <- function(c) {
   ## Set variables
   k <- nrow(c)
   a <- rep(1, times = NROW(df))
   d <- distVectM(NROW(df), c[1, ])
   
   # Find closest centroid to each point
   for (j in 2:k) {
      # Calculate distances for current centroid
      t <- distVectM(NROW(df), c[j,])
      
      # Change assignment to current centroid if it is closer than previous
      a[t < d] <- j
      
      # Update closest centroid distance
      d[t < d] <-
         t[t < d]
   }
   a
}
# ===================================================================





# Main ================================================================
# K-Means single iteration; function will repeat recursively
kMClustering <- function(initialSeeds,
                         choice,
                         verbose = FALSE,
                         times = 20,
                         ...) {
   # Initialize 
   centroids <- initialSeeds
   
   # Hold number of centroids
   k <- nrow(centroids)
   
   # Create a
   move <- moveCs(k, centroids)
   
   # Assignment vector
   a <- rep(1, times = NROW(df))
   
   
   iV <- 1:NROW(df)
   
   # Verbose
   if (verbose) {
      graphList <- list()
   }
   
   # Select clustering algorithm=======================================
   if (choice == "lloyds") { # ---------------------- Lloyd's Algorithm
      
      # Set variables for Lloyd's Algorithm
      closeCent <- rep(0, times = length(iV))
      testDist <- rep(0, times = length(iV))
      filtr <- rep(TRUE, times = length(iV))
      newCents <- centroids
      
      
      recursivePass <- function(t) {
         if (verbose) {
            print(paste("Recursive call #", t))
         }
         
         
         closeCent <<- distVectM(iV, centroids[1,])
         
         # Find closest centroid to each point
         for (j in 2:k) {
            filtr <<- (a != j)
            
            # Calculate distances for current centroid
            testDist <<- distVectM(iV, centroids[j,])
            
            # Change assignment to current centroid if it is closer than previous
            a[filtr & (testDist < closeCent)] <- j
            
            # Update closest centroid distance
            closeCent[filtr & (testDist < closeCent)] <<-
               testDist[filtr & (testDist < closeCent)]
         }
         
         newCents <<- move(a)
         
         # If no centroids changed, break recursion and return results
         if (all(centroids == newCents) | t == times) {
            return()
         }
         
         # Hold new centroid locations
         centroids <<- newCents
         
         if (verbose) {
               graphList[[t]] <<- newCents
         }
         
         # Recursively call Lloyd's Algorithm to cluster
         recursivePass(t + 1)
      }
   } else if (choice == "elkans") { # --------------- Elkan's Algorithm
      #trackMoves <- TRUE
      
      # Create objects to hold bounds
      uprBnd <- rep(Inf, times = NROW(df))
      lwrBnd <- matrix(data = 0,
                   nrow = NROW(df),
                   ncol = k)
      
      cD <- matrix(data = 0,
                   nrow = k,
                   ncol = k)
      
      # Original paper runs assignment once
      a <- simpleAssign(centroids)
      
      
      r <- rep(TRUE, times = NROW(df))
      these <- rep(TRUE, times = NROW(df))
      fV <- rep(TRUE, times = NROW(df))
      
      s <- 1:k
      
      
      newCentroids <- centroids
      
      recursivePass <- function(t) {
         if (TRUE) {
            print(paste("Recursive call #", t))
         }
         
         
         # Compute centroid-to-centroid distances
         for (y in 1:k) {
            cD[, y] <<- apply(centroids, 1,
                              function(r) {
                                 distMetric(r, centroids[y, ])
                              })
         }
         # Set half distances between closest centroids
         s <- apply(cD, 1,  function(r) min(r[r != 0]) ) / 2
         # ------------------------------------------------------------
         
         these <<- (uprBnd > s[a])
         
         
         for (j in 1:k) {
            # Logical filter vector
            # Identify all points where the upper bound is:
            # 1. Greater than the half distance between the closest centroid
            # 2. Not already assigned
            # 3. Greater than the lower bound for the ith datapoint in the vector
            # 4. Greater than half the the distance to the currently assigned centroid
            fV <<- (these &
                      (a != j) &
                      (uprBnd > lwrBnd[, j]) &
                      (uprBnd > (cD[cbind(a, j)] / 2)))
            
            
            if (any(fV & r)) {
               # Recalculate outdated upper bounds
               uprBnd[fV] <<- sapply(iV[fV], function(x) { distMetric(df[x, ], centroids[a[x], ]) })
               #r[fV] <- FALSE
            # Recheck and update filter
            #fV <<- (fV & (uprBnd > lwrBnd[, j] & uprBnd > (cD[cbind(a, j)] / 2)))
            }
            
            ## Calculate lower bounds
            lwrBnd[, j] <<- apply(df[, ], 1,
                                function(r) {
                                   distMetric(r, centroids[j, ])
                                })
            
            # Change centroid assignments
            a[(lwrBnd[, j] < uprBnd)] <<- j
            
            
            if (j == 4) {
               print(any(fV))
            }
         }
         
         
         # Move the centroids AND track each points movement ----------
         newCentroids <<- move(a)
         
         # If no centroids changed, break recursion and return results
         if (all(centroids == newCentroids) | t == times) {
            return()
         }
         
         # Track movement of centroids, reuse variable
         s <- sapply(1:k, function(j) {
                                      distMetric(newCentroids[j, ],
                                                 centroids[j, ])
                                   })
         
         # Update bounds ----------------------------------------------
         for (x in 1:k) {
            
            uprBnd[a == x] <<- uprBnd[a == x] + s[x]
            
            
            #lwrBnd[, x] <<- lwrBnd[, x] - s[x]
         }
         # Floor lower bound at zero
         #lwrBnd[lwrBnd < 0] <<- 0
         # ------------------------------------------------------------
         
         
         # Hold new centroid locations
         centroids <<- newCentroids
         
         if (verbose) {
            graphList[[t]] <<- newCentroids
         }
         
         # Recursively call Lloyd's Algorithm to cluster
         recursivePass(t + 1)
      }
      # ------------------------------------------- Hamerly's Algorithm
   } else if (choice == "hamerlys") {
      # Setup bounds
      a <- rep(1,times = NROW(df))
      uprBnd <- rep(Inf, times = NROW(df))
      lwrBnd <- rep(0, times = NROW(df))
      
      cD <- matrix(data = 0,
                   nrow = k,
                   ncol = k)
      
      # Logical filter vector
      z <- vector(mode = "logical", length = NROW(df))
      # Index Vector
      iV <- 1:NROW(df)
      
      updateBounds <- function(p) {
         # Upper bound
         uprBnd <<- uprBnd + p[a]
         
         # Lower bound
         lwrBnd <<- lwrBnd - max(p)
      }
      
      
      recursivePass <- function(t) {
         # Compute centroid-to-centroid distances
         for (j in 1:k) {
            cD[, j] <- apply(centroids, 1,
                             function(r) { distMetric(r, centroids[j,]) })
         }
         # Set half distances between closest centroids
         s <- apply(cD, 1,
                    function(r) {min(r[r > 0])}) / 2
         # ---------------------------------------------------------------
         
         # Create a filter
         z <- ((uprBnd > s[a]) & (uprBnd > lwrBnd))
         # Recalculate upper bound of some points
         uprBnd <<- unlist(lapply(iV,
                              function(i) { distMetric(df[i, ],
                                                       centroids[a[i],]) }))
         # Update z filter
         z <- ((uprBnd > s[a]) & (uprBnd > lwrBnd))
         # ---------------------------------------------------------------
         
         for (j in 1:k) {
            # Find the two closest centroids
            tT <- apply(df[z,], 1, function(r) { distMetric(r, centroids[j,]) })
            
            # Change centroid assignments
            a[z][tT < uprBnd] <<- j
            # Hold Previous upper bound as new lower bound
            #lwrBnd[tT < uprBnd] <<- uprBnd[tT < uprBnd]
            # Update new upper bound as distance to assigned centroid
            uprBnd[z][tT < uprBnd[z]] <<- tT[tT < uprBnd[z]]
         }
         
         
         # Move the centroids AND track each points movement -------------
         newCents <- move(a)
         
         s <- unlist(lapply(1:k, function(j) {distMetric(newCents[j,],
                                                           centroids[j,])}))
         # -------------------------------------------------------------
         
         # Update bounds -------------------------------------------------
         #updateBounds(movement)
         #----------------------------------------------------------------
         
         
         # Hold centroids for next call
         centroids <<- newCents
         
         if (verbose) {
            graphList[[t]] <<- newCentroids
         }
         
         # Recursively call Lloyd's Algorithm to cluster
         recursivePass(t + 1)
      }
   } else if (choice == "old") {
      # Setup bounds
      a <- rep(1,times = NROW(df))
      uprBnd <- rep(Inf, times = NROW(df))
      lwrBnd <- rep(0, times = NROW(df))
      
      cD <- matrix(data = 0,
                   nrow = k,
                   ncol = k)
      
      # Logical filter vector
      z <- vector(mode = "logical", length = NROW(df))
      # Index Vector
      iV <- 1:NROW(df)
      
      
      # Helper functions ----------------------------------------------
      # Calculate distances to centroids...
      dist2All <- function(v) {
         # cantAvoid is a vector of indexes for the datapoints we need to calculate
         distances <- matrix(apply(cbind(rep.int(v, times = k),
                                         rep(1:k, each = length(v))),
                                   1,
                                   function(x) { distMetric(df[x[1], ],centroids[x[2],]) }),
                             nrow = length(v),
                             ncol = k)
         
         
         # Find the minimum distance from a data point to a cluster
         a[v] <<- apply(distances, 1, which.min)
         
         # Hold the distance values, since it is the minimum
         uprBnd[v] <<- distances[cbind(1:length(v), a[v])]
         
         # Find the minimum distance from a data point to the second closest cluster
         lwrBnd[v] <<- vapply(1:length(v),
                              function(i) { min(distances[i, -a[v[i]]]) },
                              double(1))
      }
      
      # Helper function to change cluster count.
      #cChanged <- function(change)
      
      
      
      # Move the centers and calculate distance moved...
      moveNupdate <- function() {
         newCentroids <<- move(a)
         
         # Calculate drift
         s <<- unlist(lapply(1:k, function(j) { distMetric(newCentroids[j,],
                                                         centroids[j,])}))
         
         # Update the bounds ------------------------------------------
         # Get index location of the largest mover.
         bigMover <- which.max(s)
         
         # Increase upper bound by the 
         uprBnd <<- uprBnd + s[a]
         
         
         # Decrease the lower bound by the distance of the bigest mover
         lwrBnd[a != bigMover] <<- a[a != bigMover] - s[bigMover]
         # If the closest centroid was the bigest mover,
         # then decrease lower bound by second biggest mover
         lwrBnd[a == bigMover] <<-
            a[a == bigMover] - max(s[-bigMover])
         
      } # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
      
      
      recursivePass <- function(t) {
         if (verbose) {
            print(paste("recursivePass", t))
         }
         
         # Get centroid-to-centroid distances
         for (j in 1:k) {
            cD[, j] <<- apply(centroids, 1,
                             function(r) { distMetric(r, centroids[j,]) })
         }
         # Set half distances between closest centroids
         s <<- apply(cD, 1,
                    function(r) {min(r[r > 0])}) / 2
         
         # ---------------------------------------------------------------
         
         z <<- pmax(s[a], lwrBnd)
         fV <- which(z < uprBnd)
         
         # For data points that fail the upper bound filter, update the upper bound
         if (any(z < uprBnd)) {
            uprBnd[fV] <- sapply(fV,
                                 function(i) { distMetric(df[i, ], centroids[a[i],]) })
         }
         # If all the datapoints are in the best clustering positions
         if (all(z > uprBnd))
            print(paste("best bounds, no filter at ", t))
         
         # Hold index of data points that still fail the upper bound test
         fV <- which(z < uprBnd)
         # And hold old assignment values
         old <- a
         
         # Run cluster distance calculations on those data points
         dist2All(fV)
         
         # If no points changed their respective cluster assignment
         if (all(old == a)) {
            print(paste("No change at call ", t))
            return()
         }
         
         # Otherwise we need to change the clustering information
         #cChanged(old) -test
         moveNupdate()
         
         # ------------------------------------------------------------
         
         # Hold centroids for next call
         centroids <<- newCents
         
         if (verbose) {
            graphList[[t]] <<- newCentroids
         }
         
         # If reached the maximum number times
         if (t > times) {
            if (verbose) {
               print("Maximum number of times reached")
            }
            return()
         }
         
         # Remove the old information
         #rm()
         
         # Recursively call clustering
         recursivePass(t + 1)
      }
   }
   # ==================================================================
   
   
   # While not converged, recursively call clustering =================
   recursivePass(1)
   
   if (verbose) {
      kMClusterVerbose(graphList)
   }
   
   centroids
}