# Load packages.
library(tidyverse)
library(skimr)
library(janitor)
library(GGally)
library(ggbump)
library(arules)


#################################################
#                 Data Prep.                    #
#################################################

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

# Initialize a seed for the jitter randomization.
posn_j <- position_jitter(seed = 136)

#################################################
#                   Poll EDA                    #
#################################################

# Sample poll datasets.
glimpse(poll)

# View a summary of poll datasets.
skim(poll)

# Count the different industries.
poll %>% 
  group_by(industry) %>% 
  count(industry)

# Categorize RQ scores.
poll$`rq_category` <- arules::discretize(poll$rq, breaks = 7, 
                                    labels = c("Excellent", "Greate", 
                                               "Good", "Fair", "Poor", 
                                               "Bad", "Critical"))

# Plot distribution of companies' RQ Score categorize. 
ggplot(poll, aes(`rq_category`)) +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  geom_bar() +
  labs(title = "Distribution of Companies' RQ Score Categorize", x = "RQ Score Category", y = "Companies Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Categorize ranks.
poll$`rank_category` <- arules::discretize(poll$rank, breaks = 7, 
                                    labels = c("Excellent", "Greate", 
                                               "Good", "Fair", "Poor", 
                                               "Bad", "Critical"))

# Plot distribution of companies' rank categorize. 
ggplot(poll, aes(`rank_category`)) +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  geom_bar() +
  labs(title = "Distribution of Companies' Rank Categorize", x = "Rnak Category", y = "Companies Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of RQ score for each industry, grouped by rank category.
ggplot(poll, aes(rq, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) + 
  facet_wrap(. ~ `rq_category`, ncol = 2) +
  labs(title = "Industryies' RQ Score Density", x = "RQ Score") +
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


#################################################
#              Reputation EDA                   #
#################################################

# Sample reputation datasets.
glimpse(reputation)

# View a summary of reputation datasets.
skim(reputation)

# Categorize ranks.
reputation$`score_ctegory` <- arules::discretize(reputation$score, breaks = 7, 
                                    labels = rev(c("Excellent", "Great", 
                                               "Good", "Fair", "Poor",
                                               "Bad", "Critical")))

# Plot distribution of companies' score categorize. 
ggplot(reputation, aes(`score_ctegory`)) +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  geom_bar() +
  labs(title = "Distribution of Companies' score Categorize", x = "Score Category", y = "Companies Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Categorize scores.
reputation$`rank_ctegory` <- arules::discretize(reputation$rank, breaks = 7, 
                                                 labels = rev(c("Excellent", "Great", 
                                                            "Good", "Fair", "Poor",
                                                            "Bad", "Critical")))

# Plot distribution of companies' rank category. 
ggplot(reputation, aes(`rank_ctegory`)) +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  geom_bar() +
  labs(title = "Distribution of Companies' Rank Category", x = "Rnak Category", y = "Companies Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of score for each industry, grouped by rank score_category.
ggplot(reputation, aes(score, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) + 
  facet_wrap(. ~ `score_ctegory`, ncol = 2) +
  labs(title = "Industryies' Score Density", x = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of rank for each industry, grouped by rank rank_category.
ggplot(reputation, aes(rank, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) + 
  facet_wrap(. ~ `rank_ctegory`, ncol = 2) +
  labs(title = "Industryies' Score Density", x = "Rank") +
  theme(plot.title = element_text(hjust = 0.5))
  
# Find companies' total score.
reputation %>% 
  group_by(company) %>% 
  summarise(total_score = sum(score)) -> companyTotalScore

# Categorize total scores.
companyTotalScore$`category` <- arules::discretize(companyTotalScore$`total_score`, breaks = 7, 
                                                labels = rev(c("Excellent", "Great", 
                                                               "Good", "Fair", "Poor",
                                                               "Bad", "Critical")))
# Find top companies.
companyTotalScore %>% 
  filter(category == "Excellent") -> topCompanies

reputation %>% 
  filter(company %in% topCompanies$company) -> topScoreCompanies

# Plot top companies score distribution.
ggplot(topScoreCompanies, aes(company, score, color = company)) + 
  geom_point(position = posn_j, shape = 16, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Top Comanies Score Distribution") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot the density of top companies scores.
ggplot(topScoreCompanies, aes(score, fill = company)) +
  geom_density(color = NA, alpha = 0.4) +
  labs(title = "Top Companies Scores Desity") +
  theme(plot.title = element_text(hjust = 0.5))


