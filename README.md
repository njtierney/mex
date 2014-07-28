# mex: the **M**issingness **EX**plorer.

Everything is still very much **in a draft phase**, and is in a state of **flux** - the things you see written here are not written in stone (more like a map drawn in the dirt with pointy sticks).

## Why should someone use mex?
If you have missing data, then you need to explore reasons for missing data. With mex, we aim to provide a model-assisted approach to elucidating missingness structure.  With mex, we also want to provide nice visualisations of the model output, and in the future incorporate the use of `shiny` and `ggvis` to enhance exploration. mex helps you do that by pulling together a variety of techniques, some new, some old, to make it easy and simple for a researcher to explore missing data. 

## Main functions.
The main functions in mex are:

- explore
- model
- confirm

### Explore
In the explore step, you want to know how much missing data there is, and if there is a possibility of _bias_.  This exploration step utilises:
- **visual plots** to show how much data there is missing, 
- **t-test and $\chi^2$** to explore whether the mean of expected count is affected by missingness

#### MCAR.test (aka, explore?)
Before searching for structured missingness in the data, it is useful to ask whether the missingness is prevalent enough for us to need an investigation, and determine whether the data may be missing completely at random (MCAR)

We can do this by:

- splitting the data into two groups according to the presence or absence of a selected dependent variable, and
- applying a t-test if the independent variables are continuous or
- a chi-square test if they are discrete, 
This allows us to determine the equality of means or category probabilities, respectively. A Bonferroni adjustment or similar method can be used to allow for multiple tests.

I have written an R function `MCAR.test` which allows the user perform this test.  The function outputs a table giving the results of the t-test and the chi2 test.  This function still currently not working for the dataset `sim.dat.csv`, due to bugs.

##### Datasets to use
A simulated missing dataset was created to explore the missing data methods.  This dataset `sim.dat.csv` is in the Github repo. This dataset contained five variables, two categorical and three continuous, with 1,000 observations in each. The two categorical factors, F1 and F2, ranged uniformly across categories nominally labelled 1-7, and 1-10, respectively.  The three continuous variables, named C1, C2 and C3, were normally distributed with means and standard deviations of 50 and 10, 90 and 10, and 30 and 3, respectively.

I have written a function `sim_miss_data.R` to create missingness in data. Currently, the `sim_miss_data.R` function saves the data with the induced missingness into a directory.  One of the changes that I haven't gotten around to is changing the code so I can save it to an object or .csv, rather than to a directory, as it currently is.

**R Datasets with missingness** Only a few base R datasets seem to have missingness `airquality` and `attenu`, so we are currently just using the `sim.dat` dataset for examples.

### Model
In the model step, you model possible mechanisms for missingness, using the `rpart`, `gbm`, and (unsure of which precise clustering method to use) `hclust`.

#### CART model.
To assist us in detecting missingness structure, we use CART models to predict the proportion of missing data in a row. For the CART model, I used the `rpart` and `rpart.plot` packages.

##### The process for running the CART model using rpart.
- Predict `miss_perc`, the proportion of missing data in a row,  using the appropriate independent variables.
- plot the data appropriately.
- my common mistakes:
    + make sure there aren't strings (or relabel as factors)
    + make sure those variables that are supposed to be factors, are factors.


#### The BRT
For the BRT model, I use the `gbm` package, and the source code from elith et al. (2008)  [found in supplementary file 2](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2008.01390.x/suppinfo). 

```
I'm not sure if I can just upload this code to GitHub and then reference it - or if I should provide the link, so that they, the authors, can get the altmetrics from it?
```

Specifically, I used the functions:
- `gbm.step`:

> fits a gbm model to one or more response variables, using cross-validation to estimate the optimal number of trees.

- `gbm.plot`:

> Plots the partial dependence of the response on one or more predictors.

There are a few other components that can be used from the elith et al (2008) supplementary materials - `gbm.fixed`, `gbm.holdout`, `gbm.simplify`, `gbm.plot.fits`, `gbm.interactions`, `gbm.perspec`, `gbm.predict.grids`. 

A tricky part of fitting the BRT is understanding the right levels for each of the parameters:

- `tree complexity`
- `learning rate`
- `bag fraction`

And the `family` of distributions.

I followed the examples out of the Elith et al guide (2008), which was fiddly.  It would be nice to make this model fitting process easier, perhaps this could be done using `ggvis`, to help control the parameters and get an update in real time.  It is also the case that the gbm.step function from Elith et al(2008) is slowing down everything.  I wonder if it would be possible to look at what they have done and re-write it to be faster.  It's annoying when each model takes 5 minutes+ to run, and you want to test 20 combinations of parameters.

If we are to do any graphing, I would really like it if we made all of the output plots in ggplot.That is, if we are going to produce automated plots, we should use ggplot.  I'd also really like to fix the gbm.plot() code the Elith et al. have written.

### Confirm
In the confirm step, you can use the models created to predict the missingness structure of your own dataset, allowing for a degree of comparison or 'model fit'.

One feature could be to plot the model fit of the proportion of missing data, and the predicted amount from the models.

## How does `mex` compare to other existing solutions?
Current solutions such `MissingDataGUI`, `VIM`, `missmap (in Amelia)`, `missing.pattern.plot (in mi)` usually focus on the user needing to visually search and find the trends. Whilst humans are very good at finding patterns, having a model behind the output has more potential for really helping researchers explore their missing data problems in a precise way.  So whilst it is possible for people to use the methods provided in `mex`, it isn't necessarily easy, and straightforward

### `MissingDataGUI`

### `VIM`

### `missmap` (from the `AMELIA II` package)
gives a visual depiction of the missingness in the dataset

### `missing.pattern.plot` 
Another missingness map tool that exists from the `im` package - this allows you to specify a "clustered" option, which groups data with similar missingness patterns together.

## Future Work
Damjan has made a great point, that the CART and BRT models may neglect useful information from the data's correlation structure.  An approach is to use hierarchical clustering on a binary (present/absent) dataset, and then apply the CART or BRT to predict membership in a particular cluster, using the values of the dataset.

So the code might look something like this.

First run the hierarhical clustering, using `hclust()` on a binary TRUE FALSE dataset.
- predict the clusters identified in hclust:
    + using rpart
    + using brt (currently not working!)

## Future Goals.
Bundle these functions together into a package, with an intuitive grammar for the functions, making it easy for users to:

- explore their missing data
- model their missing data 
- confirm possible missingness patterns 
- create plots and diagnostics. 

These plots and diagnostics could utilie ggplot, ggvis, and shiny, so users can interactively explore their missing data.
