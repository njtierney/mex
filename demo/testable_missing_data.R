## this is the R code for creating the simulation dataset.
  
## make an id that goes from 1 thru to 1000, in increments of 1.
  ID <- seq(1,1000,1)

# Now let's generate the variables.  There will be 3 continuous variables, and two factors.

#generate 3 missing continuous variables, C1..C3

C1 <- rnorm(1000, mean = 50, sd = 10)

#similar to FEV1% data
C2 <- rnorm(1000, mean = 90, sd = 10)

#similar to BMI data
C3 <- rnorm(1000, mean = 30, sd = 3)

## generate two factors.

## factor 1 - values between 1 and 7

##repeat 1...7, 1000 times)
F1 <- rep(1:7, length.out = 1000)

## make F1 a randomly ordered version of F1
F1 <- sample(F1)

## factor 2 - values between 1 and 10
F2 <- rep(1:10, 100)

## make F2 a randomly ordered version of F2
F2 <- sample(F2)


## Collate variables: ID, C1...C3, F1, F2 into a dataframe, using data.frame

sim1 <- data.frame(ID, C1, C2, C3, F1, F2)

# sim1$F1 <- as.factor(sim1$F1)
# 
#   sim1$F2 <- as.factor(sim1$F2)  
# 
# sim1$ID <- as.factor(sim1$ID)
# 
  str(sim1)
  
## double check the data
head(sim1)

## check the dimensions
dim(sim1)

class(sim1)

str(sim1)
  
## Another section....
  
## I think I found a good dataset to tinker with.
  
## I got it from [this link]
  ## (http://missingdata.lshtm.ac.uk/index.php?option=com_content&view=article&
  ## id=230:data-for-practicals&catid=68:james-and-mikes-book-category&Itemid=140)
  
  ## It seems to have a suitable amount of missingness, and somewhat random, at that.
  
  NCDS.melb <- read.csv(
    "~/Dropbox/ALL THE THINGS/PhD/code/R/Missing Data Package/NCDS-melb.csv")
  
  missmap(NCDS.melb)
  
  dim(NCDS.melb)
  
  require(mi)
  missing.pattern.plot(NCDS.melb, 
                       gray.scale = TRUE,
                       clustered = TRUE)
  
  require(rpart)
  
  ##create a variable giving the % missingness.
  
  ## or maybe I could try the analyses on the

  ##  These plots take forever to render.
  
  ## I wonder if it would be possible to make them render in a faster way?
  
  
  
