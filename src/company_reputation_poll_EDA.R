#################################################
#               By Abdulrahman                  #
#################################################

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
tuesdata <- tidytuesdayR::tt_load("2022-05-31")

# Individual dataframes.
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# Append 2022 results.
poll22 <- poll %>%
  select(company, industry, change, `2022_rank`, `2022_rq`) %>%
  mutate(year = 2022, rank = `2022_rank`, rq = `2022_rq`) %>%
  select(-c(`2022_rank`, `2022_rq`))

poll <- subset(poll, select = -c(`2022_rank`, `2022_rq`))
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

# According to the dataset describtion
# Score ranges:
# 80 & above: Excellent
#  75-79: Very Good
#  70-74: Good
#  65-69: Fair
#  55-64: Poor
#  50-54: Very Poor
#  Below 50: Critical

poll$`rq_category` <- cut(poll$rq,
  breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
  labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
  right = FALSE
)


# Plot distribution of companies' RQ Score categorize.
ggplot(poll, aes(`rq_category`)) +
  geom_bar() +
  geom_text(
    position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5
  ) +
  labs(
    title = "Distribution of Companies' RQ Score Categorize",
    x = "RQ Score Category",
    y = "Companies Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# # Categorize ranks.
# poll$`rank_category` <- cut(poll$rank,
#   breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
#   labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
#   right = FALSE)

# # poll$`rank_category` <- arules::discretize(poll$rank,
# #   breaks = 7,
# #   labels = c(
# #     "Excellent", "Greate",
# #     "Good", "Fair", "Poor",
# #     "Bad", "Critical"
# #   )
# # )

# # Plot distribution of companies' rank categorize.
# ggplot(poll, aes(`rank_category`)) +
#   geom_text(position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5) +
#   geom_bar() +
#   labs(title = "Distribution of Companies' Rank Categorize", x = "Rnak Category", y = "Companies Count") +
#   theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of RQ score for each industry, grouped by rank category.
# ggplot(poll, aes(rq, fill = industry)) +
#   geom_density(color = NA, alpha = 0.5) +
#   # facet_wrap(. ~ `rq_category`, ncol = 2) +
#   labs(title = "Industryies' RQ Score Density", x = "RQ Score") +
#   theme(plot.title = element_text(hjust = 0.5))

# Plot industries rankings distributions grouped by year.
# ggplot(poll, aes(industry, rq, color = industry)) +
#   geom_point(position = posn_j, shape = 16, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   facet_wrap(. ~ year, ncol = 1) +
#   labs(title = "Industryies ranking Distribtions") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   theme(plot.title = element_text(hjust = 0.5))

# Plot average industry RQ score for each industry from 2017-2022.
poll %>%
  group_by(industry, year) %>%
  summarise(.groups = "keep", avg_rq = mean(rq)) %>%
  ggplot(aes(year, `avg_rq`, color = industry)) +
  geom_bump() +
  geom_point(size = 1) +
  labs(title = "Average Industry RQ Score from 2017-2022", y = "Average RQ Score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(. ~ industry, ncol = 3) +
  ggsave("plot4.png", dpi = 200)

# Plot average industry rank for each industry from 2017-2022.
poll %>%
  group_by(industry, year) %>%
  summarise(.groups = "keep", avg_rank = mean(rank)) %>%
  ggplot(aes(year, `avg_rank`, color = industry)) +
  geom_bump() +
  geom_point(size = 1) +
  labs(title = "Average Industry Rank from 2017-2022", y = "Average Rank") +
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
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1)
  ) +
  labs(title = "Industryies' ranking Mean and Standard Deviation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), ) +
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
reputation$`score_ctegory` <- cut(reputation$score,
  breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
  labels = c("Critical", "Bad", "Poor", "Fair", "Good", "Great", "Excellent"),
  right = FALSE
)



# boxplot of `score` by `industry`
# a plot that shows the interquartiles of score vs. industry
# it explains that some industry have a high median value.
# for the `other` catogery tends to have latge voltile in its socore values.
ggplot(reputation, aes(industry, score)) +
  geom_jitter(alpha = 0.3, shape = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "black") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(title = "Distribution of Companies' score Categorize", x = "Score Category", y = "Companies Count") +
  theme(plot.title = element_text(hjust = 0.5))


# Plot distribution of companies' score categorize.
ggplot(reputation, aes(`score_ctegory`)) +
  geom_text(position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_bar() +
  labs(title = "Distribution of Companies' score Categorize", x = "Score Category", y = "Companies Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Categorize scores.
reputation$`rank_ctegory` <- arules::discretize(reputation$rank,
  breaks = 7,
  labels = rev(c(
    "Excellent", "Great",
    "Good", "Fair", "Poor",
    "Bad", "Critical"
  ))
)

# Plot distribution of companies' rank category.
ggplot(reputation, aes(`rank_ctegory`)) +
  geom_text(position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_bar() +
  labs(title = "Distribution of Companies' Rank Category", x = "Rnak Category", y = "Companies Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of score for each industry, grouped by rank score_category.
ggplot(reputation, aes(score, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) +
  # facet_wrap(. ~ `score_ctegory`, ncol = 2) +
  labs(title = "Industryies' Score Density", x = "Score") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot distribution of rank for each industry, grouped by rank rank_category.
ggplot(reputation, aes(rank, fill = industry)) +
  geom_density(color = NA, alpha = 0.5) +
  # facet_wrap(. ~ `rank_ctegory`, ncol = 3) +
  labs(title = "Industryies' Rank Density", x = "Rank") +
  theme(plot.title = element_text(hjust = 0.5))

# Find companies' total score.
reputation %>%
  group_by(company) %>%
  summarise(total_score = sum(score)) -> companyTotalScore

# Categorize total scores.
companyTotalScore$`category` <- arules::discretize(companyTotalScore$`total_score`,
  breaks = 7,
  labels = rev(c(
    "Excellent", "Great",
    "Good", "Fair", "Poor",
    "Bad", "Critical"
  ))
)
# Find top companies.
companyTotalScore %>%
  filter(category == "Excellent") -> topCompanies

reputation %>%
  filter(company %in% topCompanies$company) -> topScoreCompanies

# Plot top companies score distribution.
ggplot(topScoreCompanies, aes(company, score, color = company)) +
  geom_point(position = posn_j, shape = 16, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Top Companies Score Distribution") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot the density of top companies scores.
ggplot(topScoreCompanies, aes(score, fill = company)) +
  geom_density(color = NA, alpha = 0.4) +
  labs(title = "Top Companies Scores Density") +
  theme(plot.title = element_text(hjust = 0.5))

#################################################
#                  By Lina                      #
#################################################

library(tidyverse)
library(skimr)
library(ggbump)
library(tidyr)
library(ggplot2)
library(cowplot)
library(wesanderson)
library(hrbrthemes)
library(dplyr)
library(viridis)
library(forcats)
library(ggplot2)

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Get the Data

poll <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv")
reputation <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv")

summary(poll)

# extracting the results of 2022
df22 <- poll %>%
  select(company, industry, change, `2022_rank`, `2022_rq`) %>%
  mutate(year = 2022, rank = `2022_rank`, rq = `2022_rq`)

# removing old cols
df22 <- subset(df22, select = -c(`2022_rank`, `2022_rq`))
poll <- subset(poll, select = -c(`2022_rank`, `2022_rq`))

# merging results of 2022 with the other years to reduce cols
finalpoll <- rbind(poll, df22)


# filtering the industries

changeInrank <- finalpoll %>% drop_na(rank)
changeInrank <- changeInrank %>% drop_na(change)

Groceries <- changeInrank %>%
  filter(industry == "Groceries")

Ecommerce <- changeInrank %>%
  filter(industry == "Ecommerce")

Other <- changeInrank %>%
  filter(industry == "Other")

Logistics <- changeInrank %>%
  filter(industry == "Logistics")

Automotive <- changeInrank %>%
  filter(industry == "Automotive")

Financial_Services <- changeInrank %>%
  filter(industry == "Financial Services")

Industrial <- changeInrank %>%
  filter(industry == "Industrial")

Food_Beverage <- changeInrank %>%
  filter(industry == "Food & Beverage")

Consumer_Goods <- changeInrank %>%
  filter(industry == "Consumer Goods")

Pharma <- changeInrank %>%
  filter(industry == "Pharma")

Energy <- changeInrank %>%
  filter(industry == "Energy")

Tech <- changeInrank %>%
  filter(industry == "Tech")


# Changes in ranks over the years for each industry
#***** Findings: most tech companies has low rank in 2020 while it raises in 2021
# TODO: extract some intresting scocials or comapanies
Tech %>%
  filter(company == c("Facebook", "Twitter")) %>%
  ggplot(aes(year, rq, color = company)) +
  geom_path(alpha = 0.7) +
  geom_point(size = 3) +
  labs(title = "The RQ scale for some tech companies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: pharams companies has a low during 2020 during covid. However, in 2021 pfizer has a sharpe drop in rank
ggplot(Pharma, aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Pharma Companies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: Automotive companies in general after the year of 2019 has dropped in RQ scale
ggplot(Automotive, aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Automotive Comapnies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: groceries companies has rocked up after 2020
ggplot(Groceries, aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Groceries Comapnies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))



# Remove duplicates from 2022 records
Clean_df22 <- df22 %>% distinct(company, .keep_all = TRUE)
company_rank <- Clean_df22 %>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))


#***** Findings: The top 3 companies all belongs to the retail industry
# Plot top 10 of 2022 using Lollipop Chart along with industry
company_rank %>% ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "white", size = 3) +
  geom_segment(aes(
    x = company,
    xend = company,
    y = 0,
    yend = rank
  )) +
  labs(
    title = "The Highest Ranked Companies",
    subtitle = "Rank"
  ) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# best companies of each industry in 2022
bestcompany <- df22 %>%
  mutate(company = fct_reorder(company, desc(rank))) %>%
  group_by(industry) %>%
  slice(which.min(rank))

#***** Findings:best tech company is Samsung,  for eCommerce Amazon, for cars Toyota, for media we have Spotify
bestcompany %>% ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "black", size = 3) +
  geom_segment(aes(
    x = company,
    xend = company,
    y = 0,
    yend = rank
  )) +
  labs(
    title = "The Highest Ranked Companies For Each Industry",
    subtitle = "Rank"
  ) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


# worst companies of each industry in 2022
worstcompany <- df22 %>%
  mutate(company = fct_reorder(company, rank)) %>%
  group_by(industry) %>%
  slice(which.max(rank))

#***** Findings:worst tech company is Twitter,  for eCommerce Wish, for cars General motors, for media we have Fox corporation
worstcompany %>% ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "black", size = 3) +
  geom_segment(aes(
    x = company,
    xend = company,
    y = 0,
    yend = rank
  )) +
  labs(
    title = "The Lowest Ranked Companies For Each Industry",
    subtitle = "Rank"
  ) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# An RQ score is calculated by:
# 80 & above: Excellent;
# 75-79: Very Good ;
# 70-74: Good ;
# 65-69: Fair ;
# 55-64: Poor ;
# 50-54: Very Poor ;
# Below 50: Critical

# changeInrank$rq[changeInrank$rq >= 80] <- "Excellent"
# changeInrank$rq[changeInrank$rq < 80 & changeInrank$rq >= 75] <- "Very Good"
# changeInrank$rq[changeInrank$rq < 75 & changeInrank$rq >= 70] <- "Good"
# changeInrank$rq[changeInrank$rq < 70 & changeInrank$rq >= 65] <- "Fair"
# changeInrank$rq[changeInrank$rq < 65 & changeInrank$rq >= 55] <- "Poor"
# changeInrank$rq[changeInrank$rq < 55 & changeInrank$rq >= 50] <- "Very Poor"
# changeInrank$rq[changeInrank$rq < 50] <- "Critical"

#***** Findings: most companies has a very good score, non got critical points, while 5 companies got very poor results
changeInrank %>%
  distinct() %>%
  group_by(company) 



#***** Findings: The mode of the Rank for all companies in the yea 2022 is `Very Good`.
# meaning that most of companies in 2022 are rated as `Very Good`.
changeInrank$rq_cat <- cut(changeInrank$rq,
  breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
  labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
  right = FALSE
)

changeInrank %>%
  filter(year == 2022) %>%
  distinct() %>%
  ggplot(aes(rq_cat)) +
    geom_bar(fill = "steelblue") +
    geom_text(
      position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5
    )

# TODO: think about this one
poll %>% 
filter(industry == c("Financial Services", "Ecommerce"))  %>% 
  ggplot(aes(year, rq)) +
    geom_point(shape = 16, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(. ~ industry, ncol = 1) +
    labs(title = "Industryies ranking from 2017-2022") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) +
    ggsave("plot_lm.png", dpi = 200)

############################ Reputation ##############################
vision <- reputation[reputation$name == "VISION", ]
trust <- reputation[reputation$name == "TRUST", ]
citizenship <- reputation[reputation$name == "CITIZENSHIP", ]
culture <- reputation[reputation$name == "CULTURE", ]
ethics <- reputation[reputation$name == "ETHICS", ]
growth <- reputation[reputation$name == "GROWTH", ]
ps <- reputation[reputation$name == "P&S", ]

reputation$cats <- "Excellent"
reputation$cats[reputation$score < 80 & reputation$score > 75] <- "Very Good"
reputation$cats[reputation$score <= 75 & reputation$score > 70] <- "Good"
reputation$cats[reputation$score <= 70 & reputation$score > 65] <- "Fair"
reputation$cats[reputation$score <= 65 & reputation$score > 55] <- "Poor"
reputation$cats[reputation$score <= 55 & reputation$score > 50] <- "Very Poor"
reputation$cats[reputation$score <= 50] <- "Critical"

# TODO later
changeInrank$rq_cat <- cut(changeInrank$rq,
  breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
  labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
  right = FALSE
)

# stacked bar chart for score of tech companies in every name ** this cab be done for other industries
#***** Findings: johnson&johnson has lower scores in every aspect compared to Pfizer
phar <- reputation[reputation$industry == "Pharma", ]
ggplot(phar, aes(x = company, y = score, fill = name, label = score)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "The Pharma Companies Detailed Scores for 2022") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: Honda has excellent score in most aspects except for citizenship the has is Toyota a better citizenship
auto <- reputation[reputation$industry == "Automotive", ]
ggplot(auto, aes(x = company, y = score, fill = name, label = cats)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "The Automotive Companies Detailed Scores for 2022") +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))



#***** Findings: retails and tech have more companies with high score for culture
# rank of companies in term of names, plot top 10 in term of ethics
topculture <- culture %>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))

topculture %>% ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "white", size = 3) +
  geom_segment(aes(
    x = company,
    xend = company,
    y = 0,
    yend = rank
  )) +
  labs(
    title = "The Highest Ranked Companies",
    subtitle = "Rank"
  ) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


# Hypothesis
### Fitting Linear Model
lm.fit <- lm(poll$rank ~ poll$rq, data = poll)

summary(lm.fit)
#***** Findings: the p value of rq is lower than 0.05. Thus, we can reject the null hypothesis
#< 2.2e-16 as the p value would indicate a significant result,
# meaning that the actual p value is even smaller than 2.2e-16
# (a typical threshold is 0.05, anything smaller counts as statistically significant)
# *****Findings:Multiple R-squared of  0.8497 means the it can explain 84% of variation in rank

### to get the confidence interval lower and upper bound
confint(lm.fit)

### abline is to draw the estimated line
plot(poll$rank ~ poll$rq)
abline(lm.fit, lwd = 3, col = "red")
