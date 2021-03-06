---
title       : Beer Rating to ABV Correlation, adjustable with a 3rd variable, number of Raters
subtitle    : Beer Suds
author      : Raymond Miecznik
job         : data analyst
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone}
knit        : slidify::knit2slides
logo        : beer.png
---


## Slide 1 - OverView of Application

The purpose of the application is allow the user to dynamically change the data range input
on the (Y axis) and a 3rd variable (raters) to see how it effects the potential correlation 
of ABV to Rating, while varying the 3rd variable (raters).

The actual Beer detail data has been removed and stripped, like Beer Name, style, Brewery, etc,
as that is not needed for the plotting functions.

The plot displays some basic linear regression details that you would expect, along with a confidence band.

--- .class #id 

## Slide 2 - Data Overview and Summary

12064 observation in the dataset.

* ABV - Alcohol by Volume ( X axis )
* Rating - from 1 to 5 / with 1 being the worst and 5 the best ( Y axis )
* Raters - how many ratings have been submitted by users using the Beer Rating Apps ( 3rd variable )


```r
load(file="dataset.RData")
summary(dataset)
```

```
##       ABV             rating          raters       
##  Min.   : 0.050   Min.   :1.300   Min.   :    0.0  
##  1st Qu.: 5.200   1st Qu.:3.590   1st Qu.:  102.0  
##  Median : 6.400   Median :3.820   Median :  175.0  
##  Mean   : 6.882   Mean   :3.772   Mean   :  439.5  
##  3rd Qu.: 8.000   3rd Qu.:4.030   3rd Qu.:  414.0  
##  Max.   :57.500   Max.   :4.870   Max.   :14364.0  
##  NA's   :172      NA's   :7
```

--- .class #id 

## Slide 3 - App Use Details

* Raters range slider - feed the minimum and maximum values to filter on the dataset using the 3rd variable raters.
* Rating range slider - allows you to also adjust which Rating range you want to look at.

The ABV or ( X axis ) is not adjustable and always display for all values in the dataset.

The app allows you to see all the data if you like by expanding the min/max range values.


--- .class #id 

## Slide 4 - Closing

While the app shows beer data, in practicality, you can point any dataset that you want to explore.

This apps is very useful for data exploration in not having to update the R code and subset different
data ranges to see an update in the plot results.

Thank you!






