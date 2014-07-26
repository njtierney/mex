#' @title mcar_test
#'
#' @description
#' \code{mcar_test} evaluates the difference in expected mean or count according to a 
#' specified missing/non missing variable
#'
#' @details
#' mcar_test is based off of the test for whether data is completely missing at random,
#' as specified in Little (1988) 
#' This code allows you to specify what the factors are, and it then performs
#' 
#' Special thanks to Dr. Nicole White for her help writing the initial code in early 2013
#' a t-test on the data, after removing the factors and using them for a chi-2 test.
#' we then give the results table for a t-test and a chi2 test.
#' 
#' @param data         Dataset you are using.
#' @param y            The variable for which you want to split the dataset into two parts
#'                     dependent upon the missingness
#' @param factor.list  Those variables that are factors. 


## notes:
#the dataset must contain only numerical values - no strings.

MCAR.test <- function(data, 
                      y, 
                      factor.list){

  
  ## Part I
  ## Make a corresponding binary (miss/notmiss) dataframe for data, 
  ## as a reference point for missingness.
  
      data.X <- as.data.frame(is.na(data))
  
  ## IMPORTANT NOTE - in this dataset, 
     ## a value of 0 = data is PRESENT in the real dataset (as it is NOT NA)
     ## a value of 1 = data is ABSENT in the real dataset (as it IS NA)
  
## let yvar become the column number that is the same name as y 
    yvar <- which(names(data) == y)
  
## turn those variables listed as factors in factor.list, into factors.
  
  data[ ,factor.list] <- data.frame(apply(data[factor.list], 
                                         2, 
                                         as.factor))

## pull data in the list of variables in the factor list into a new dataset to be
  ## used in the chi-squared contingency test.
  
  data.chisq.test <- data.b[ ,factor.list]

## Pull the rest of the data, NOT in the factor.list...
  
data.t.test <- data[ , !(colnames(data) %in% factor.list)]

## Now split this function into two parts
  ## the t-test part
  ## the chi-squared part.

############################
##### The Chi-Squared Part
############################

## The General Idea is:
  ## Make a loop passing through each variable
      ## make a contingency table according to the +/- of 'yvar'.
        ## store this table in a list (cannot do just yet)
        ## can we then plot this information to assist in subsequent inferences?
      ## conduct a chi-sq test on the table and pull relevant info out and 
      ## put it vectors to assemble into a table later move on to the next table

## code for making a contingency table inspired from:
  ## r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

## store how many columns there are in this dataset.
  chi.ncol <- ncol(data.chisq.test)

## attach the names of dataset to variable 'names'
  names.chi <- names(data.chisq.test)

## The 'container' for p-values to be placed.##
  chi.p.value <- rep(0,chi.ncol)

## container' for 'variable', which is used in the dataframe at the end of the script
  chi.variable <- rep(0,chi.ncol)

## a container to store the results from the chisq.test
chi.results.list <- vector("list", chi.ncol)

chi.ctable.list <- vector("list", chi.ncol)

## This loop goes through the variable, t, which loops through 1 - #col
for (t in 1:chi.ncol)
  { ## start of loop.

  chi.ctable.list[[t]] <- table(data.chisq.test[ ,t], 
                                data.X[ ,yvar]) 
  
  
  chisq.tmp <- table(data.chisq.test[ ,t], 
                     data.X[ ,yvar]) 

  chi.results.list[[t]] <- chisq.test(chisq.tmp)
  
  chi.variable[t] <- names.chi[t]
  
  chi.p.value[t] <- chi.results.list[[t]]$p.value
  
} # close loop


# round the p-values to 6 places
chisq.p.value <- round(chi.p.value, 6)

## make a dataframe with the rows of the variable name, and the columns = output
chi.output <- data.frame(chi.variable,
                          chi.p.value)

##########################
#### The T-Test Part
##########################
  
  #make a variable that stores the numbers of columns in the data set.  
  
  C <- ncol(data.t.test)
  
  #attach the names of dataset to variable 'names'
  names <- names(data.t.test)
  
  ##The 'container' for p-values to be placed.##
  pvalue <- rep(0,C)
  
  ##The 'container' for the population overall to be placed##
  n <- rep(0,C)
  
  ## This is the population for the X_VARIABLE when the Y-VARIABLE is present.
  n_x_var_y_pres <- rep(0,C)
  
  ## This is the population for the X_VARIABLE when the Y-VARIABLE is absent.
  n_x_var_y_abs<-rep(0,C)
  
  ## container' for 'variable', which is used in the dataframe at the end of the script
  variable<-rep(0,C)
  
  ## These means are the 'containers' for the mean values of the x_variables to be put.
  mean_x_var_y_pres<-rep(0,C)
  mean_x_var_y_abs<-rep(0,C)
  
  ## A loop, going through the variable t, which loops through 1 - C.
  
  for (t in 1:C){
    ## start of loop.
    
    ## all observations in a variable where -y- is present (data may be missing)
    ##IMPORTANT
      ## when the cell == 0, it means the data is NOT NA, meaning that it is PRESENT
    
    x_var_y_present <- data.t.test[data.X[ ,yvar] == 0, t]
    
    ## takes upon the mean value of the x_variable when y is present.
    mean_x_var_y_pres[t] <- mean(x_var_y_present, 
                                 na.rm=TRUE)
    
    ## all observations in a variable where -y- is absent (data may be missing)
    ## IMPORTANT
      ## when the cell == 1, it means the data IS NA, meaning that it is ABSENT  
    
    x_var_y_absent <- data.t.test[data.X[ ,yvar] == 1, t]
    
    ## takes upon the mean value of the x_variable when y is absent.
    mean_x_var_y_abs[t] <- mean(x_var_y_absent,
                                na.rm=TRUE)
    
    ## removing absent oversations, 
    ##how many observations of this variable are present when BMI is absent/present
    
    ## If this is >10, continue with the operation below  
    ## if this is <10, then they don't get included in part of the operation, and 
    ## will be used in the next operation.
    
    ## this next section performs a t-test on the (non/)missing data,
    ## with na.action=na.omit so as to force the function to 
    ## continue so that it doesn't get hung up when observations are missing
    
    if(length(na.omit(x_var_y_absent)) > 10 & length(na.omit(x_var_y_present)) > 10)
    {
      ## start of the if statement
      
      ## perform an unequal variances t-test comparing the mean values of
      ## x_var_present and x_var_absent.    
      
      ## Compares the mean values of each variable, x,
       ## according to whether y is present or absent.
      tmp <- t.test(x_var_y_present,
                    x_var_y_absent,
                    var.equal=FALSE,
                    na.action=na.omit)
      
      ## stores the pvalue of 't'-th variable 
        pvalue[t] <- tmp$p.value
      
      ## mp takes upon the mean value of the x_variable when y is present.
      # mean_x_var_y_pres[t] -> mp
      
      ##this used to be "mean_y-var_pres[t]", the reason being that it was read as:
      ## "The mean value of x when y is present."
      
      ## this is the number of observations for:
        ## cases within a particular variable when y is present,
        ## which are not missing.
      n_x_var_y_pres[t] <- length(which(is.na(x_var_y_present) == 0))
      
      ## ma takes upon the mean value of the x_variable when y is absent.
      #mean_x_var_y_abs[t] -> ma 
      
      ## this is the number of observations for:
        ## cases within a particular variable when y is ABSENT,
        ## which are not missing.
      n_x_var_y_abs[t] <- length(which(is.na(x_var_y_absent) == 0))
      
      ## this is the total number of observations that are not missing for that variable
      n[t] <- length(which(is.na(x_var_y_present) == 0)) +
              length(which(is.na(x_var_y_absent) == 0))
      
    }   #closing the if statement
    
    ## else, if there aren't enough observations, we omit them from the t-test, 
        ## giving their p-value 99
    
    else {pvalue[t] <- 99 
          
          ## and then record the rest of the details about that variable.
          
          ## mp takes upon the mean value of the x_variable when y is present.
          mp <- mean_x_var_y_pres[t] 
          
          ##this used to be "mean_y-var_pres[t]", the reason being that it was read as:
          ## "The mean value of x when y is present."
          
          ## this is the number of observations for:
          ## cases within a particular variable when y is present,
          ## which are not missing.
          n_x_var_y_pres[t] <- length(which(is.na(x_var_y_present) == 0))
          
          ## mp takes upon the mean value of the x_variable when y is present.
          ma <- mean_x_var_y_abs[t] 
          
          ## this is the number of observations for:
          ## cases within a particular variable when y is ABSENT,
          ## which are not missing.
          n_x_var_y_abs[t] <- length(which(is.na(x_var_y_absent) == 0))
          
          ## total number of observations that are not missing for that variable  
          n[t] <- length(which(is.na(x_var_y_present) == 0)) +
                  length(which(is.na(x_var_y_absent) == 0)) 
    } ## closing the else statement
    
    ## variablename of t takes on the value of the names of the variable t
    variable[t] <- names[t]
  
  } ## closing the t- for-loop.
  
  ## round the values so that it isn't super long
  pvalue <- round(pvalue, 4)

  mean_x_var_y_pres <- round(mean_x_var_y_pres, 4)

  mean_x_var_y_abs <- round(mean_x_var_y_abs, 4)
  
  ## 'finaloutput' becomes a dataframe with the rows of the variable name, 
    ## and the columns = output
  
  finaloutput <- data.frame(variable,
                            pvalue,
                            mean_x_var_y_pres,
                            mean_x_var_y_abs,
                            n_x_var_y_pres,
                            n_x_var_y_abs,
                            n)
  
  ## now store finaloutput in a list to be called later.
  list(mcar.t.test.table = finaloutput[order(pvalue),], 
       mcar.chi2.table = chi.output[order(chisq.p.value),],
       mcar.chi2.results = chi.results.list,
       mcar.chi2.ctab = chi.ctable.list)

} ## end of function

#closes the function.

############################
### NOTE ON THE ERRORS
## I think that the errors are being produced when there is a case where there is no 
## X-i for when Y is present or absent.
