# Load packages.
library(tidyverse)
library(skimr)
library(janitor)
library(GGally)
library(ggbump)
library(arules)

# Load dataset.
tuesdata <- tidytuesdayR::tt_load('2022-05-31')
tuesdata <- as_tibble(tuesdata)

# Individual dataframes.
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# Appened 2022 results.
poll22 <- poll %>%
  select(company, industry, change,`2022_rank`,`2022_rq`) %>%
  mutate(year = 2022, rank=`2022_rank`, rq = `2022_rq`) %>% 
  select(-c(`2022_rank`, `2022_rq`))

poll <- subset(poll, select = -c(`2022_rank`,`2022_rq`) )
poll <- rbind(poll, poll22)

# Remove duplicates and NAs.
poll %>%
  distinct() %>% 
  na.omit() -> poll

reputation %>%
  distinct() %>% 
  na.omit() -> reputation

# Sample both datasets.
glimpse(poll)
glimpse(reputation)

# View a summary of both datasets.
skim(poll)
skim(reputation)

# Count the different industries.
poll %>% 
  group_by(industry) %>% 
  count(industry)

# Categorize ranks.
poll$rank_cat <- arules::discretize(poll$rq, breaks = 7, 
                                    labels = c("Excellent", "Excellent", 
                                               "Good", "Fair", "Poor", 
                                               "Very Poor", "Critical"))

# Plot distribution of companies' rank categorize. 
ggplot(poll, aes(rank_cat)) +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  geom_bar() +
  labs(title = "Distribution of Companies' Rank Categorize") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot industries rankings distributions grouped by year.
ggplot(poll, aes(industry, rank, color = industry)) + 
  geom_point(position = posn_j, shape = 16, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ year, ncol = 3) +
  labs(title = "Industryies ranking Distribtions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot industries rankings form 2017-2022 grouped by industry.
ggplot(poll, aes(year, rank, color = industry)) + 
  geom_point(position = posn_j, shape = 16, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ industry, ncol = 3) +
  labs(title = "Industryies ranking from 2017-2022") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot the mean and SD of industries' rankings.
ggplot(poll, aes(industry, rank)) +
  geom_jitter(width = 0.2, alpha = 0.5, shape = 16) + 
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1)) +
  labs(title = "Industryies' ranking Mean and Standard Deviation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot rank density of each industry.
ggplot(poll, aes(x = rank, fill = industry)) +
  geom_density(color = NA) + 
  facet_wrap(. ~ industry, ncol = 3) +
  labs(title = "Industryies' ranking Density") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot rank density of each industry grouped by year.
ggplot(poll, aes(x = rank, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) + 
  facet_wrap(. ~ year, ncol = 2) +
  labs(title = "Industryies' ranking Density") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot a violin graph of industries' rankings.
ggplot(poll, aes(industry, rank)) +
  geom_violin(scale = "count") + 
  # geom_jitter(width = 0.2, alpha = 0.5, shape = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),) +
  labs(title = "Destipution of Industryies ranking") +
  theme(plot.title = element_text(hjust = 0.5))