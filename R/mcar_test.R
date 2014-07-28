#' @title mcar_test
#'
#' @description
#' \code{mcar_test} evaluates differences in expected mean or count according
#'  to a specified missing/non missing variable
#'
#' @details
#' Prior to identifying structure in the data, it is useful to ask whether 
#' there is sufficient missingness to warrant such an investigation - and to 
#' try and determine whether the data is missing completely at random (MCAR).
#' This can be done by splitting the data into two groups according to the 
#' presence or absence of a selected dependent variable, and to apply a t-test
#' if the independent variables are continuous or a chi-square test if they are
#' discrete, in order to determine equality of the means or the category 
#' probabilities, respectively. A Bonferroni adjustment (or similar) method can
#' be used to allow for multiple tests. This mcar_test is based off of the test
#' for whether data is completely missing at random, from Little(1988)
#' 
#' @param data         Dataset you are using.
#' @param y            The variable that you want to split the dataset into two
#'                     parts dependent upon the missingness.
#' @param factor.list  Those variables that are factors. 
#' 
#' @format Gives 4 dataframes: 
#'         mcar.t.test.table - the results from the t-test  
#'         mcar.chi2.table
#'         mcar.chi2.results - the results from the chi2 test
#'         mcar.chi2.ctab  - the contingency table
#' 
#' @section Thankyous: Special thanks to Dr. Nicole White for her help writing
#'                     the initial code in early 2013
#' 
#' @section Warning: the data must contain only numerical values - No strings!
#' 
#' @section Warning: Check whether Little used this test first.

MCAR.test <- function(data, 
                      y, 
                      factor.list){

  ## Make a corresponding binary (miss/notmiss) dataframe for data, 
  ## as a reference point for missingness.
  
      data.X <- as.data.frame(is.na(data))
  
  ## IMPORTANT NOTE - in this dataset, 
     ## a value of 0 = data is PRESENT in the real dataset (as it is NOT NA)
     ## a value of 1 = data is ABSENT in the real dataset (as it IS NA)
  
## make yvar name into a column number

    yvar <- which(names(data) == y)
  
## make sure factors in factor.list are indeed factors.
  
  data[ ,factor.list] <- data.frame(apply(data[factor.list], 
                                         2, 
                                         as.factor))

## create a dataset for the chisq2 test, containing factor.list variables
  
  data.chisq.test <- data[ , factor.list]

## create a dataset for the t-test, not containing factor.list variables
  
  data.t.test <- data[ , !(colnames(data) %in% factor.list)]

## This function has two parts
  ## t-test part
  ## chi-squared part.

############################
##### The Chi-Squared Part
############################

## General Idea:
  ## Make a loop passing through each variable
      ## Make a contingency table according to +/- of 'yvar'. 
      ## Store this table
      ## Conduct a chi2 test on the table,
          ## pull relevant info out, 
          ## put into vectors to assemble into a table later
          ## move on to the next table
  ## Future work? 
    ## plot this information to assist in subsequent inferences.

## code for making a contingency table inspired from:
## r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

## how many columns are there in this dataset?

  chi.ncol <- ncol(data.chisq.test)

## store the names of dataset to variable 'names'

  names.chi <- names(data.chisq.test)

## create 'container' for p-values to be placed.

  chi.p.value <- rep(0,chi.ncol)

## create container for variables used in the dataframe at end of the script.

  chi.variable <- rep(0,chi.ncol)

## create container to store the results from the chisq.test

  chi.results.list <- vector("list", chi.ncol)

  chi.ctable.list <- vector("list", chi.ncol)

## This loop goes through the variable, t, which loops through 1 - #col
  for (t in 1:chi.ncol){ 
      ## start of loop.
      
    ## create a contingency table for the given variable, t, which counts
    ## the times that the yvar is present or absent
    chi.ctable.list[[t]] <- table(data.chisq.test[ , t], 
                                    data.X[ , yvar]) 
  
    chisq.tmp <- table(data.chisq.test[ , t], 
                         data.X[ , yvar]) 
  
    ## conduct the chi2 test and store it in the t-th component of the results 
    chi.results.list[[t]] <- chisq.test(chisq.tmp)
    
    ## find the name of the t-th variable
    chi.variable[t] <- names.chi[t]
    
    ## extract the p-value from the results.
    chi.p.value[t] <- chi.results.list[[t]]$p.value
    
    } 
    ## close loop

# round the p-values to 6 places

  chisq.p.value <- round(chi.p.value, 6)

## make a dataframe with rows of the variable name, and columns = output
  
  chi.output <- data.frame(variable = chi.variable,
                           p.value = chi.p.value)

##########################
#### The T-Test Part
##########################
  
  ## store the numbers of columns in the data set.  
  
    C <- ncol(data.t.test)
  
  ## attach the names of dataset to variable 'names'
  
    names <- names(data.t.test)
  
  ## create 'container' for p-values to be placed.
  
    pvalue <- rep(0, C)
  
  ## The 'container' for the population overall to be placed##
  
    n <- rep(0, C)
  
  ## Population for the X_VARIABLE when the Y-VARIABLE is present.
  
    n_x_var_y_pres <- rep(0, C)
  
  ## Population for the X_VARIABLE when the Y-VARIABLE is absent.
  
    n_x_var_y_abs<-rep(0, C)
  
  ## Create container for 'variable', used in dataframe at the end of script.
  
    variable<-rep(0, C)
  
  ## These means are the 'containers' for the mean values of the x_variables

  mean_x_var_y_pres<-rep(0, C)
  mean_x_var_y_abs<-rep(0, C)
  
  ## A loop, going through the variable t, which loops through 1 - C.
  
  for (t in 1:C){
    ## start of loop.
    
    ## all obs in a variable where -y- is present (data may be missing)
    ##IMPORTANT
      ## when the cell == 0, it means the data is NOT NA, 
      ## meaning that it is PRESENT
    
    x_var_y_present <- data.t.test[data.X[ ,yvar] == 0, t]
    
    ## the mean value of the x_variable when y is present.
    mean_x_var_y_pres[t] <- mean(x_var_y_present, 
                                 na.rm=TRUE)
    
    ## all observations in a variable where -y- is absent (data may be missing)
    ## IMPORTANT
      ## when the cell == 1, it means the data IS NA, meaning that it is ABSENT  
    
    x_var_y_absent <- data.t.test[data.X[ ,yvar] == 1, t]
    
    ## The mean value of the x_variable when y is absent.
    mean_x_var_y_abs[t] <- mean(x_var_y_absent,
                                na.rm=TRUE)
    
    ## removing absent oversations, 
    ## how many obs of this variable are present when yvar is absent/present
    ## If >10, continue with the operation below  
    ## if <10, they don't get included in part of the operation, and will be
      ## used in the next operation.
    
    ## Next section performs a t-test on the (non/)missing data,
      ## na.action=na.omit 
        ## forces the function to continue 
      ## so it doesn't get hung up when observations are missing
    
if (length(na.omit(x_var_y_absent)) > 10 & 
      length(na.omit(x_var_y_present)) > 10){
      ## start of the if statement
      
      ## perform unequal variances t-test comparing the mean values of
      ## x_var_present and x_var_absent.    
      
      ## Compares mean values of each variable, x, according pres/abs of y
      tmp <- t.test(x_var_y_present,
                    x_var_y_absent,
                    var.equal=FALSE,
                    na.action=na.omit)
      
      ## stores pvalue of t-th variable 
        pvalue[t] <- tmp$p.value
      
      ## mp takes upon the mean value of the x_variable when y is present.
          # mean_x_var_y_pres[t] -> mp
      ## this was "mean_y-var_pres[t]", the reason being that it was read as:
      ## "The mean value of x when y is present."
      
      ## Number of observations for:
        ## cases within a particular variable when y is PRESENT, 
        ## which are not missing.
      
        n_x_var_y_pres[t] <- length(which(is.na(x_var_y_present) == 0))
      
      ## ma takes the mean value of the x_variable when y is absent.
      ## mean_x_var_y_abs[t] -> ma 
      
      ## Number of observations for:
        ## cases within a particular variable when y is ABSENT,
        ## which are not missing.
      
        n_x_var_y_abs[t] <- length(which(is.na(x_var_y_absent) == 0))
      
      ## Total number of observations NOT missing for that variable
      n[t] <- length(which(is.na(x_var_y_present) == 0)) +
              length(which(is.na(x_var_y_absent) == 0))
      
    }   
    ## closing the if statement
    
    ## else,
      ##if there aren't enough observations, we omit them from the t-test, 
        ## giving their p-value 99
    
    else {pvalue[t] <- 99 
          
          ## then record the rest of the details about that variable.
          
          ## mp takes upon the mean value of the x_variable when y is present.
          mp <- mean_x_var_y_pres[t] 
          
          ## this was "mean_y-var_pres[t]", as it was read as:
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
          
          ## total number of obs that are not missing for that variable  
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

############################
### NOTE ON THE ERRORS
## I think that the errors are being produced when there is a case where there
## is no X-i for when Y is present or absent.
