##
### fit the model using the BRT
##
## I'm interested to see the differences in fitting the model using gbm.step
## and regular old gbm


data(sim.dat)

library(gbm)
mex.brt = gbm(c.id ~ C1 + C2 + C3 + F1 + F2, 
              data = sim.dat, 
              distribution = "multinomial", 
              n.trees = 5000, 
              shrinkage = 0.01, 
              bag.fraction = 0.5,
              interaction.depth = 2)

summary(mex.brt)

names(mex.brt)

## THIS is concerning.  When I run the results with the original gbm.step
## function, It tells me that C1 was the most important

mex.brt.2 <- gbm(miss_perc ~ C1 + C2 + C3 + F1 + F2, 
                 data = sim.dat, 
                 distribution = "gaussian", 
                 n.trees = 5000, 
                 shrinkage = 0.01, 
                 bag.fraction = 0.5,
                 interaction.depth = 2)

summary(mex.brt)

## It looks like we cannot use the code from elith et al., as it does not
## have a specification for multinomial distributions.
## 

## It ALSO looks like I get VERY different results when using the gbm.step()
## compared to the gbm() function.
## I'm glad that I've found this error, but this may have created more work 
## for me.
## It is now time to delve into 'the source', to find out just what is so 
## different in gbm() and gbm.step().

## source code from elith et al.
source("~/Dropbox/ALL THE THINGS/PhD/Guides/Elith_BRT_Guide_2_of_2/brt.functions.R")

head(sim.dat)

brt.mod <- gbm.step(data= sim.dat, 
                    gbm.x = c(2:6), 
                    gbm.y = 10,
                    family = "gaussian",
                    tree.complexity = 2,
                    learning.rate = 0.01,
                    bag.fraction = 0.5)

summary(brt.mod)


