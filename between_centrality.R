# Betweenness Centrality

betwCent <- function(dist,
                    normalize = FALSE) {
   results <- vector(mode = "numeric",
                     length = nrow(dist))
   
   # Character matrix to track path node names
   tracker <- matrix(data = NA,
                     nrow = nrow(dist),
                     ncol = nrow(dist))
   
   # floydWarshall
   for (k in 1:nrow(dist)) {
      # For each data point find shortest path to all points
      for (i in 1:nrow(dist)) {
         if (i == k)
            next()
         for (j in 1:ncol(dist)) {
            if (j == k)
               next()
            # Check if k is on a shorter path
            if (dist[i, k] + dist[k, j] < dist[i, j]) {
               # Update
               dist[i, j] <- dist[i, k] + dist[k, j]
               # Hold Names
               if (is.na(tracker[i, k]) & is.na(tracker[k, j])) {
                  tracker[i, j] <- paste0(i, k, j)
               } else if (is.na(tracker[i, k])) {
                  tracker[i, j] <- paste0(i, tracker[k, j])
               } else if (is.na(tracker[k, j])) {
                  tracker[i, j] <- paste0(tracker[i, k], j)
               } else {
                  tracker[i, j] <- paste0(tracker[i, k],
                                          substr(tracker[k, j], 2, nchar(tracker[k, j])))
               }
            }
         }
      }
   }
   
   
   for (x in 1:(nrow(tracker) - 1)) {
      for (y in (x + 1):nrow(tracker)) {
         if (is.na(tracker[x, y]))
            next()
         btw <- as.numeric(substring(tracker[x, y],
                                     2:(nchar(tracker[x, y]) - 1),
                                     2:(nchar(tracker[x, y]) - 1)))
         results[btw] <- results[btw] + 1
      }
   }
   
   if (normalize == TRUE)
      results <<- results / (nrow(a) - 1)
   
   results
}