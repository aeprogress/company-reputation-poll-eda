# This code is for obtaining the data from `tidytuesdayR`

# First comment out the following line to install the needed packages:
# install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# using `tibble`
# get the desired dataset
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2022-05-31')
tuesdata <- as_tibble(tuesdata)

poll <- tuesdata$poll
reputation <- tuesdata$reputation

glimpse(poll)
glimpse(reputation)
