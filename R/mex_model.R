#' @title mex_model
#' @description
#' \code{mex_model} Takes a dataset and models missingness using a variety of
#'  methods, such as hierarchical clustering, CART, and BRT.
#' @details
#' In this section we perform some missingness exploration using the following
#' order of operations:
#' 1. Perform hierarchical clustering on the binary dataset, which has had its
#'  euclidean distance performed
#' 2. cut the hclust object into 4 pieces
#' 3. Add these 4 categories as a column in a dataframe
#' 4. return this new dataset, "mex.clust"
#' @param data       Dataset you are using.
#' @format Gives a dataset called mex.clust
#' @section Warning: This section is still undergoing testing.
#' @export

mex_model <- function (data) {
  ## run the clustering on a dataset made of the 
  hclust.fit <- hclust(dist(is.na.data.frame(data)))
    
  ## cut the hclust into 4 pieces.
  c.id <- as.factor(cutree(hclust.fit, 4))
  
  ## add the 4 pieces into a dataframe
  sim.clust <- data.frame(data, 
                          c.id)
  
  list(mex.clust = sim.clust)
}

## Future development include:
## - adding in:
  ## rpart modelling
  ## BRT modelling
  ## Plotting:
    ## e.g., type -plot("name of fitted mex object")- and then scroll through
    ## different hclust, CART, and BRT plots in the R viewer.




