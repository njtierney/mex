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
  ##
  ### The rpart section
  ##
  
  ## load the rpart library
  library(rpart)
  
  mex.cart <- rpart(c.id ~ C1 + C2 + C3 + F1 + F2,
                      data = sim.clust,
                      na.action = na.rpart, 
                      method = "class")
  
  #load the rpart plotting library
  library(rpart.plot)
  
  ## plot the rpart tree - currently doesn't allow us to plot it ourselves
  mex.cart.plot <- prp(cart.small, 
                        extra = 1, 
                        type = 4, 
                        prefix = "Prop. Miss = ")
  
  ##
  ### What is included in the output of the function?
  ##

  # list of the commands returned from the function
  list(mex.clust = sim.clust,
       mex.cart = mex.cart,
       mex.cart.plot = mex.cart.plot)
}

## Future development include:
## - adding in:
  ## rpart modelling (cannot get the plots to work)
  ## BRT modelling ()
  ## Plotting:
    ## e.g., type -plot("name of fitted mex object")- and then scroll through
    ## different hclust, CART, and BRT plots in the R viewer.
  ## Adding testing to the code, so that I can work on making my code better.




