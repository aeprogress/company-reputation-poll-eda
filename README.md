# Compan Reputation Poll EDA
View the report [here](https://aeprogress.github.io/company-reputation-poll-eda/company_reputation_poll_report.html)

## Dataset Information
# 2022 Axios-Harris Poll

The data comes from [Axios and Harris Poll](https://www.axios.com/2022/05/24/2022-axios-harris-poll-100-rankings). 
The Axios Harris Poll 100 is based on a survey of 33,096 Americans in a nationally representative sample. The two-step process starts fresh each year by surveying the public’s top-of-mind awareness of companies that either excel or falter in society. These 100 “most visible companies” are then rated by a second group of Americans across the seven key dimensions of reputation to determine the ranking. If a company is not on the list, it did not reach a critical level of visibility to be measured.

### Get the data here

```{r}
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-05-31')
tuesdata <- tidytuesdayR::tt_load(2022, week = 22)

poll <- tuesdata$poll
```


### Data Dictionary

# `poll.csv`

|variable  |class     |description |
|:---------|:---------|:-----------|
|company   |character | Company Name |
|industry  |character | Industry group |
|2022_rank |integer   | 2022 Rank (1 is better than 100) |
|2022_rq   |double    | 2022 RQ score. An RQ score is calculated by:  [ (Sum of ratings of each of the 9 attributes)/(the total number of attributes answered x 7) ]  x 100. Score ranges: 80 & above: Excellent; 75-79: Very Good ; 70-74: Good ; 65-69: Fair ; 55-64: Poor ; 50-54: Very Poor ; Below 50: Critical |
|change    |integer   | Change in rank from 2021         |
|year      |integer   | Year for that rank/RQ |
|rank      |integer   | Rank corresponding to the year|
|rq        |double    | RQ score corresponding to the year |

# `reputation.csv`
- All ranks in this for current year only

|variable  |class     |description |
|:---------|:---------|:-----------|
|company   |character | Company Name |
|industry  |character | Industry group |
|name      |character | Name of reputation category (P&S = Product and Service) |
|score     |double    | Score for reputation category |
|rank      |integer   | Rank for reputation category |


## Description

Using a simple R script, the dataset is explored to present the most interesting and useful insights along with the questions they answer.
### Goals
- Presenting findings gathered from analysis in a report format.
- Discovering packages and functions in R for data manipulation and EDA.
- Applying hypnosis tests.

## Content 

```bash
./
│   README.md
│   company_reputation_poll_report.html    
│   company-reputation-poll-eda.Rproj
│   
└───data/
│   │   poll.csv
│   │   reputation.csv
│   
└───presentation/
│   │   presentation.pptx
│   
└───src/
    │   company_reputation_poll_EDA.R
    │   company_reputation_poll_report.Rmd
```

## Used Libraries
- tidyr
- ggpubr
- tidyverse
- ggplot2
- dplyr
- janitor
- here
- reactable
