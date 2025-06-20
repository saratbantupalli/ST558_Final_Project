# ST558_Final_Project

## Purpose of the App
This app connects to US Treasury API, collect's interest rate data on US Treasury securities and National public debt. The app uses this data to model/predict National public debt based on the interest rate. The idea here is based on the fact that with increasing National debt, soon the biggest payments made by the Federal Government will be interest paid to various Treasury securities. This app tries to model this relationship. This app is a continuation of work I did for a [previous project](https://github.com/saratbantupalli/ST558_Project-2). 

## List of R Packages needed to run the App
```{r}
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(corrplot)
library(caret)
```

## Code to install all packages
```{r}
install.packages(c("tidyverse", "shiny","httr", "jsonlite", "corrplot", "caret", "DT"))

```

 install. packages(c("dplyr", "stringr"))

## Shiny Github code to run the app from R studio
```{r}
shiny::runGitHub("saratbantupalli/ST558_Final_Project")
```

