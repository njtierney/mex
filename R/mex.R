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
mex.data.frame <- function(d, ...)
{
    # Do stuff here...

    # Create a `mex` object and return.
    res <- d
    class(res) <- c("mex", class(res))
    return(res)
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
