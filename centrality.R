# Centrality Function
#
# Description: "Parent Frame" for centrality functions
# Statistics:
# Required Parameters: df: dataframe
# Optional Parameters: type: type of centrality measure
#                      ...

centrality <- function(df,
                       type = "all",
                       verbose = FALSE,
                       ...) {
   c <- new.env()
   # Create an Adjacency Matrix of the dataframe
   c$adjMatrx <- adjMatrix(df)
   # Create a Distance Matrix of the dataframe
   c$distMatrx <- distMatrix(df)
   # Create a Topological Overly Matrix of the dataframe
   c$topOMatrx <- topOMatrix(df)
   
   # Create a Shortest Path Matrix of the dataframe, expected that dist matrix exists in environment
   c$sPMatrx <- aPairsShortest(c$distMatrx)
   
   # Degree Centrality
   c$degree <- degCent(c$adjMatrx)
   # Closeness Centrality
   c$closeness <- closCent(c$distMatrx)
   # Betweenness Centrality
   c$between <- betwCent(c$distMatrx)
   # Eigenvector Centrality
   ###c$eigen <- eigCent()
   # Harmonic Centrality
   # Katz Centrality
   
   # Print and return results
   if (verbose == TRUE) {
      print("Objects in returned centrality environment:", quote = FALSE)
      print(objects(c))
   }
   
   # Return the centrality environment
   c
} # End graph centrality