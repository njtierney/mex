mex
===

This is the repository for the upcoming package mex, the **M**issingness **EX**plorer.

Everything is still very much **in a draft phase**, and is in a state of **flux** - the things you see written here are not written in stone (more like a map drawn in the dirt with pointy sticks).

### Why should someone use mex?
If data is missing in your data, then you need to explore reasons for missing data. mex helps you do that by pulling together a variety of techniques, some new, some old, to make it easy and simple for a researcher to explore missing data. With mex, we aim to provide a model-assisted approach to elucidating missingness structure.  With mex, we also want to provide nice visualisations of the model output, and in the future incorporate the use of `shiny` and `ggvis` to enhance exploration.

### What are the main functions.
The main functions in mex are:

- explore
- model
- confirm

#### Explore
In the explore step, you want to know how much missing data there is, and if there is a possibility of _bias_.  This exploration step utilises:
- **visual plots** to show how much data there is missing, 
- **t-test and $\chi^2$** to explore whether the mean of expected count is affected by missingness

#### Model
In the model step, you model the expected mechanisms for missingness, using the `rpart`, `gbm`, and (unsure of which clustering method to use) `hclust`.

#### Confirm
In the confirm step, you can use the models created to predict the missingness structure of your own dataset, allowing for a degree of comparison or 'model fit'.

### How does it compare to other existing solutions?
Current solutions `(such as...)` usually focus on the user needing to visually search and find the trends. Whilst humans are very good at finding patterns, Having a model behind the output has more potential for really helping researchers explore their missing data problems in a precise way.  So whilst it is possible for people to use the methods provided in `mex`, it isn't necessarily easy, and straightforward.













