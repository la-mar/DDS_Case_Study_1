---
title: "MSDS 6306 Beers and Breweries Case Study"
author: "Peter Flaming"
date: "6/19/2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
## Introduction
This R markdown document gives the code and output for Case Study 1: Questions 1-7 completed for the Master of Science in Data Science graduate course Doing Data Science MSDS 6306 provided by Southern Methodist University during the Summer 2018 semester. The "beers.csv" and "breweries.csv" data used in this Case Study 1 was sourced from 558 US breweries and their listed 2410 US craft beers for sale. 

## Project Directory
These data reside in the GitHub cloud directory: https://github.com/la-mar/DDS_Case_Study_1.git. 

## Libraries Used
library(tidyverse)
library(dplyr)
library(ggplot2)

## Reading Data into R
beers <- read.csv("../DDS_Case_Study_1/data/Beers.csv", header = TRUE)
breweries <- read.csv("../DDS_Case_Study_1/data/Breweries.csv", header = TRUE)

### Question 1. How many breweries are present in each state?
#### Print the count of breweries by state from "breweries" data.frame.

```r
summary(breweries$State)
```

```
## Error in summary(breweries$State): object 'breweries' not found
```

### Question 2. Merge beer data with breweries data. Print the first 6 observations and the last six observations to check the merged file.
#### Merge "beers" data.frame with "breweries" data.frame by the common "Brewery_ID" variable into a new data.frame "beers_breweries".

```r
beers_breweries <- merge(beers, breweries, by.x = "Brewery_id", by.y = "Brew_ID")
```

```
## Error in merge(beers, breweries, by.x = "Brewery_id", by.y = "Brew_ID"): object 'beers' not found
```

#### Rename the variables of new "beers_breweries" data.frame.

```r
colnames(beers_breweries) <- c("Brewery_ID", "Beer_Name", "Beer_ID", "ABV", "IBU", "Stye", "Ounces", "Brewery", "City", "State")
```

```
## Error in colnames(beers_breweries) <- c("Brewery_ID", "Beer_Name", "Beer_ID", : object 'beers_breweries' not found
```

#### Print first 6 observations of "beers_breweries" data.frame.

```r
head(beers_breweries)
```

```
## Error in head(beers_breweries): object 'beers_breweries' not found
```

#### Print last 6 observations of "beers_breweries" data.frame.

```r
tail(beers_breweries)
```

```
## Error in tail(beers_breweries): object 'beers_breweries' not found
```

### Question 3. Report the number of NA's in each column.
#### Print the count of NA's by column from "beers_breweries" data.frame.

```r
beers_breweries %>% group_by(.) %>% summarise_all(funs(sum(is.na(.))))
```

```
## Error in beers_breweries %>% group_by(.) %>% summarise_all(funs(sum(is.na(.)))): could not find function "%>%"
```

### Qustion 4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
#### Compute the median ABV by State for "beers_breweries" data.frame.

```r
median_abv_by_state <- beers_breweries %>% group_by(State) %>% summarise(median_abv = median(ABV, na.rm = TRUE))
```

```
## Error in beers_breweries %>% group_by(State) %>% summarise(median_abv = median(ABV, : could not find function "%>%"
```

#### Compute the median IBU by State for "beers_breweries" data.frame.

```r
median_ibu_by_state <- beers_breweries %>% group_by(State) %>% summarise(median_ibu = median(IBU, na.rm = TRUE))
```

```
## Error in beers_breweries %>% group_by(State) %>% summarise(median_ibu = median(IBU, : could not find function "%>%"
```

#### Plot of Median ABV for "beers_breweries" data.frame.

```r
ggplot(data = median_abv_by_state) + geom_bar(mapping = aes(x = State, y = median_abv), stat = "identity")
```

```
## Error in ggplot(data = median_abv_by_state): could not find function "ggplot"
```

#### Plot of Median IBU for "beers_breweries" data.frame.

```r
ggplot(data = median_ibu_by_state) + geom_bar(mapping = aes(x = State, y = median_ibu), stat = "identity")
```

```
## Error in ggplot(data = median_ibu_by_state): could not find function "ggplot"
```

### Question 5. Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
#### Print the state with the maximum ABV for "beers_breweries" data.frame.

```r
max_abv <- beers_breweries %>% group_by(State) %>% arrange(desc(ABV))
```

```
## Error in beers_breweries %>% group_by(State) %>% arrange(desc(ABV)): could not find function "%>%"
```

#### Print the state with the max IBU beer for "beers_breweries" data.frame.

```r
max_ibu <- beers_breweries %>% group_by(State) %>% arrange(desc(IBU))
```

```
## Error in beers_breweries %>% group_by(State) %>% arrange(desc(IBU)): could not find function "%>%"
```

### Question 6. Summary statistics for the ABV variable.
#### Compute the summary statistics of the ABV variable for the "beers_breweries" data.frame.

```r
summary(beers_breweries$ABV)
```

```
## Error in summary(beers_breweries$ABV): object 'beers_breweries' not found
```

### Question 7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.
#### Scatter Plot of ABV vs IBU variables for the "beers_breweries" data.frame.

```r
ggplot(data = beers_breweries, mapping = aes(x = ABV, y = IBU)) + geom_point(mapping = aes(x = ABV, y = IBU)) + geom_smooth(mapping = aes(x = ABV, y = IBU))
```

```
## Error in ggplot(data = beers_breweries, mapping = aes(x = ABV, y = IBU)): could not find function "ggplot"
```
