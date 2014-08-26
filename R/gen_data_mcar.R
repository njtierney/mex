## this is the function that creates the MCAR data.

###########
#' @param data      = data you want to inflict the missingness on
#' @param miss.perc = percent of missing data you want (approximately)
#' @param seed =   set the random seed so that the results can be replicated.
#' @param col.num = range of column numbers to set to random. Could be improved.
gen_data_mcar <- function(data, 
                       miss.perc,
                       seed,
                       col.num){
  
  ## create a working copy of the data
    newdata <- data
    
  ## set the % missing data
    newdata$prob <- miss.perc
  
  set.seed(seed)
    for (i in (1:nrow(newdata))){
      
      for (j in (col.num)){
        
        ## make a random uniform for sim1, 
        newdata$runif <- runif(nrow(newdata),  ## for all rows
                               min = 0, 
                               max = 1) ## set a probability between 0 and 1.
        
        ## IF runif is equal to or smaller than prob, make it missing (NA)
        
        if (newdata$runif[i] <= newdata$prob[i]) { ## open if-else
          
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
        ## sum(is.na(data[row,]))
      
      ## divides by the # of observations in that row
        ## (ncol(data))
      
      ## makes a row equal to the % missing data.    
        ## (miss_perc)
      
      mcar.dat$miss_perc[m] <- sum(is.na(mcar.dat[m, ])) / ncol(mcar.dat)
      
    }

list(mcar.dat = mcar.dat,
     orig.dat = newdata)

} ## close the function
