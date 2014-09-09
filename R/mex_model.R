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
#' @import magrittr
#' @import rpart
#' @import rpart.plot
#' @export

mex_model <- function (data, ...) {
  
  # run the clustering on a dataset get the data, then make it an na.matrix,
  # then calculate euclidean distance then perform hierarchical clustering
  hclust.fit <- data %>%
                     is.na.data.frame %>%
                     dist %>%
                     hclust
  
  # use hclust.fit, cut it into 4 pieces, then make it a factor
  c.id  <- hclust.fit %>%
                      cutree(4) %>%
                      as.factor
  
  ## add the 4 pieces into a dataframe
  sim.clust <- data.frame(data, 
                          c.id)

  # ======
  # rpart
  # ======
  
  # run the rpart model, using all variables (specified by . ) 
  mex.cart <- rpart(c.id ~ . ,
                      data = sim.clust,
                      na.action = na.rpart, 
                      method = "class")
  
  ## plot the rpart tree - currently doesn't allow us to plot it ourselves
  mex.cart.plot <- prp(mex.cart, 
                        extra = 1, 
                        type = 4, 
                        prefix = "Miss Clust = ")

  
  # ================
  # function output
  # ================
  
  # list commands returned from the function
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

## What sort of tests do I need to run on this package?
  ## does it return 3 things?
  ## is 