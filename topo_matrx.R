# Topological Overlap Matrix Creation Function
#
# Description:
# Required Parameters: df: dataframe
# Optional Parameters: drected: Change to TRUE if graph is directed,
#                      verbose: Change to true for printing returned results to console

topOMatrix <- function(df,
                       directed = FALSE,
                       verbose = FALSE) {
   # Start with an adjacency matrix
   adj <- adjMatrix(df)
   
   # Calculate row and column sums
   k <- rowSums(adj)
   l <- colSums(adj)
   
   # Create a matrix where Aij=ki*lj where u is all nodes in the matrix
   m <- k %*% t(l)
   
   # Create the Topological Overlap Matrix (TOM)
   t <- matrix(apply(cbind(rep(1:NROW(df), each = NROW(df)),
                           rep(1:NROW(df), times = NROW(df))),
                     1,
                     function(x) {
                        (m[x] + adj[x]) / ((min(k[x[1]], k[x[2]]) + 1) - adj[x])
                     }),
               nrow = NROW(df),
               ncol = NROW(df))
   
   # Set diagonal values to 1
   t[cbind(1:NROW(df), 1:NROW(df))] = 1
   
   # Name the columns and rows of the topological matrix
   rownames(t) <- colnames(t) <- rownames(df)
   
   # Print and return results
   if (verbose) {
      print(t)
   }
   
   return(t)
   
} # End topOMatrix function
