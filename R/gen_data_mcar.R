#' Create MCAR missingness in a dataframe.
#' 
#' \code{gen_data_mcar} takes a dataframe and adds in missingness, 
#'                      returning the original, and newdata
#'
#' @param data      = data you want to inflict the missingness on
#' @param miss.perc = percent of missing data you want (approximately)
#' @param seed      = set the random seed for reproducibility.
#' @param col.num   = range of column numbers to set to random missingness.
#'                    This could be improved.
#'                    
#' @return this function should return a dataframe
#' 
#' @examples
#' dat.mcar <- gen_data_mcar(data = iris, 
#'                           miss.perc =  0.5,
#'                           seed = 1234,
#'                           col.num = c(1:3))
#'                    


gen_data_mcar <- function(data, 
                          miss.perc,
                          seed,
                          col.num){
  
  ## create a working copy of the data
    newdata <- data
    
  ## set the % missing data
    newdata$prob <- miss.perc
  
  ## set the seed
  set.seed(seed)
    
    # for each row in the dataset, go through each j-th specified column
    for (i in (1:nrow(newdata))){
      
      for (j in (col.num)){
        
        ## make a random uniform for the data, 
        newdata$runif <- runif(nrow(newdata),  ## for all rows
                               min = 0, 
                               max = 1) ## set a probability between 0 and 1.
        
        ## IF runif is equal to or smaller than prob, 
        if (newdata$runif[i] <= newdata$prob[i]) { ## open if-else
          
          ## THEN make that element missing
          newdata[i, j] <- NA
          
        } ## close if-else
        
      } ## close j loop
      
    } ## close i loop
  
## drop those auxiliary variables, runif, and prob.
## so select everything but those
mcar.dat <- select(newdata, 
                   C1,
                   C2,
                   C3,
                   F1,
                   F2,
                   R1,
                   R2)
            
## write in a column that calculates the % data missing in each row.

  ## this walks through each row, 
    for (m in (1:nrow(mcar.dat))){
      
      ## finds how many observations are missing 
      ## divides by the # of observations in that row
      ## makes a row equal to the % missing data.
      
      mcar.dat$miss_perc[m] <- 
          ## finds how many observations are missing, 
          ## divides by the # of observations in that 
          ## row makes a row equal to the % missing data.
        sum(is.na(mcar.dat[m, ])) / ncol(mcar.dat)
      
    }

list(mcar.dat = mcar.dat,
     orig.dat = newdata)

} ## close the function
