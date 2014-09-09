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
#' @export
mex <- function(...)
    UseMethod("mex")


# ============================================================================
# Constructors
# ============================================================================

#' @export
mex.data.frame <- function(data, ...)
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
                      na.action = na.rpart, 
                      method = "class")
    
    ## plot the rpart tree - currently doesn't allow us to plot it ourselves
    rpart.plot <- prp(rpart.mex, 
                      extra = 1, 
                      type = 4, 
                      prefix = "Miss Clust = ")
  
  # =======
  # output
  # =======
  
  structure(list(
    hclust.mex = hclust.mex,
    rpart.mex = rpart.mex,
    rpart.plot = rpart.plot
    ), class = "mex")
  
#     # Create a `mex` object and return.
#     res <- data
#     # make res gain an additional class, mex
#     class(res) <- c("mex", class(res))
#   
#     return(res)
}

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
