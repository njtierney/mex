#' @title sim_miss_data
#'
#' @description
#' \code{sim_miss_data} generates a dataframe with a specified missingness
#' pattern.
#'
#' @details
#' This function gives the user a great deal of control over creating different
#' patterns of missingness, however it was created with a specific purpose in 
#' mind, and so it might actually need to be broken up into a couple of 
#' different functions, as it is quite a large function.
#' 
#' @param data      dataset you want to inflict the missingness on
#' @param R         Number of datasets you want.
#' @param miss.perc percent of missing data you want (approximately)
#' @param var       which variable will cause the missingness?
#' @param varlist   which variables will be used in the dataset?
#' @param dv_miss_prob    set the probability that the dv will go missing
#' @param miss.bound.on   set the missing boundary on, creating a MNAR/MAR 
#'                        scenario.
#'                        = 1 when you want the missingness to only occur when 
#'                        miss.bound.valary is a certain value
#'                        = 0 when you don't want the missingness to occur 
#'                        according to a miss.bound.value
#' @param miss.bound.val  at what value do you want things to go missing?
#'                        e.g., from value 60 - make the missingness occur
#' @param max_miss        what is the probability that things will go missing 
#'                        if miss.bound is on?
#' @param directory       where you want the file to be saved, e.g.,
#'                        "~/Dropbox/ALL THE THINGS/PhD/MD_Paper_Prep/knitr/
#'        2014_28_04_miss_data_sim/simulated_data/mcar"
#' @param seed      set the random seed so that the results can be replicated.
#' 
#' #examples
#' 
#' @return this function currently saves as an Rdataset.
#'  
gen.mar.dv.R <- function(data, 
                         R,
                         miss_prob, 
                         var, 
                         varlist,
                         dv_miss_prob,
                         miss.bound.on,
                         miss.bound.val,
                         max_miss,
                         directory,
                         seed){
  
  set.seed(seed)
  
## find what number the string value of 'var' corresponds to
  var <- which(names(sim1) == var)
  
## start the 'R' repeat loop for making the dataset
  for (r in 1:R){ 
    
  ## make a copy of the data
    newdata <- data
    
  ## make the probability 0 if var <= miss_prob,
  ## for each row for var, make the proability of missingness relative to
  ## the maximum value of var, multiplied by miss_prob.
  ## this means that the higher the value of var, the more likely it is to be 
  ## missing.
  
    for (i in (1:nrow(newdata))){
      
      newdata$prob[i] <- ( newdata[i,var] / max(newdata[var]) ) * miss_prob
      
      ## make a random uniform for sim1
      newdata$runif <- runif(nrow(newdata), 
                             min = 0, 
                             max = 1)      
    } ## close i-loop
    
    ## if the missing boundary setting is "on".
    
    if(miss.bound.on == 1){
      
      ## for all rows of data
      
      for (i in (1:nrow(newdata))){
        
        ## if, for each row, the value of the specified variable, var,
        ## is greater than or = the boundary value
        ## make the probability of missingness = the max_miss.
        
        if(newdata[i,var] >= miss.bound.val){
          
          newdata$prob[i] <- max_miss
          
        }  ## close the if-loop
        
      } ## close the nrow-if-loop
      
    } ## close the miss.bound-if-loop
    
    ## this drops out the dependent variable from the missingness
    
    ## make a copy of the varlist
    var.a <- varlist  
    
    ## make a new list that contains the varlist without the variable 
    ## causing the missing dependance.
    
    new.c <- var.a[var.a != var.a[var-1]]
    
    ## then, for every row
    for(i in (1:nrow(newdata))){
      
      ## if the random uniform is <= the probability of missingness.
      
      if(newdata$runif[i] <= newdata$prob[i]){
        
        ## make that entire row (of variables, new.c) missing.
        
        newdata[i,new.c] <- NA 
        
      } # close if-loop    
      
    } # close i-loop
    
    ## induce missingness for the variable 'causing' the missingness
    
    newdata$dep_pr <- dv_miss_prob
    
    ## generate value for % of missingness 
    
    ## for -dep_pr- (dv_miss_prob)
    
    for (i in (1:nrow(newdata))){
      
      if(newdata$runif[i] <= newdata$dep_pr[i]){
        
        newdata[i,var] <- NA                 } # close second if-else
      
    } # close i loop
    
    ## make a missing_percentage variable.
    
    for(i in (1:nrow(newdata))){
      
      newdata$miss_perc[i] <- sum(is.na(newdata[i, ])) / ncol(newdata)
      
    } ## end of miss_perc loop
    
    #set the working directory to where I want the datasets saved.
    setwd(directory)
    
    #- where namevar will change according to the variable inputted
    namevar <- names(newdata[var])
    
    save(newdata,
         file = paste("mar.data",
                      namevar,
                      "missing",
                      dv_miss_prob,
                      "threshold",
                      miss.bound.on,
                      "p.miss",
                      miss_prob,
                      "seed",
                      seed,
                      ".RData", 
                      sep="-")) 

  } # close the 'R' loop
  
}## close function loop
