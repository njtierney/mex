#' Model-assisted missing data exploration
#' 
# ============================================================================
# Generics
# ============================================================================
#' Model-assisted missing data exploration
#'
#' Fit models to explore the structure of missing values in a dataset.
#'
#' This is the main function in the \code{mex} package.  It runs all of the
#' routines for exploring missing data and returns a \code{mex} object which
#' contains all of the results.
#' 
#' @import magrittr
#' @import rpart
#' @import rpart.plot
#' @import dplyr
#' 
#' @param data A dataset that is of class `data.frame`
#'
#' @export
mex <- function(...)
    UseMethod("mex")


# ============================================================================
# Constructors
# ============================================================================

#' @export
mex.data.frame <- function(data, 
                           ...)
{

  # ========================
  # hierarchical clustering
  # ========================
  
    # perform clustering on a dataset 
    
    # get the data, then make it an na.matrix, then calculate euclidean distance
    # then perform hierarchical clustering
    
    hclust.fit <- data %>%
      is.na.data.frame %>%
      dist %>%
      hclust
    
    # use hclust.fit, cut it into 4 pieces, then make it a factor
    c.id  <- hclust.fit %>%
      cutree(4) %>%
      as.factor
    
    ## add the 4 pieces into a dataframe
    hclust.mex <- data.frame(data, 
                            c.id)
    
  # ======
  # rpart
  # ======
  
    # run the rpart model, using all variables (specified by . ) 
    rpart.mex <- rpart(c.id ~ . ,
                      data = hclust.mex,
                      na.action = na.rpart, # surrogate approach 
                      method = "class") # a classification tree
    
    ## plot the rpart tree
    rpart.plot <- prp(rpart.mex, 
                      extra = 1, 
                      type = 4, 
                      prefix = "Miss Clust = ")
    
    ## Error: currently doesn't allow us to plot it ourselves
  
  # =======
  # output
  # =======
  
  # output the new dataframe, the rpart object, and the rpart plot.
  structure(list(
    hclust.mex = hclust.mex,
    rpart.mex = rpart.mex,
    rpart.plot = rpart.plot
    ), 
    class = "mex")
  
} # end mex.data.frame approach

#' @export
mex.default <- function(x, ...)
{
    # Do stuff here...

    # Create a `mex` object and return.
    res <- x
    class(res) <- c("mex", class(res))
    return(res)
}

# ============================================================================
# Methods
# ============================================================================

#' @export
print.mex <- function(mexfit)
{
    cat("mex: length =", length(mexfit), "\n\n")
    NextMethod(mexfit)
}

#' @export
plot.mex <- function(mexfit)
    NextMethod(mexfit)
