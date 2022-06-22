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

#################################################
#                 Data Prep.                    #
#################################################

# Load dataset.
poll <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv")
reputation <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv")

# Append 2022 results.
poll22 <- poll %>%
  select(company, industry, change, `2022_rank`, `2022_rq`) %>%
  mutate(year = 2022, rank = `2022_rank`, rq = `2022_rq`) %>%
  select(-c(`2022_rank`, `2022_rq`))

poll <- subset(poll, select = -c(`2022_rank`, `2022_rq`))
poll <- rbind(poll, poll22)
poll <- poll %>% drop_na(rank)
poll <- poll %>% drop_na(change)

#################################################
#                 Poll EDA                      #
#################################################

# View poll summary.
summary(poll)


# Changes in ranks over the years for each industry
#***** Findings: most tech companies has low rank in 2020 while it raises in 2021
# TODO: extract some intresting scocials or comapanies
poll %>%
  filter(industry == "Tech", company == c("Facebook", "Twitter")) %>%
  ggplot(aes(year, rq, color = company)) +
  geom_path(alpha = 0.7) +
  geom_point(size = 3) +
  labs(title = "The RQ scale for some tech companies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: pharams companies has a low during 2020 during covid. However, in 2021 pfizer has a sharpe drop in rank
poll %>%
  filter(industry == "Pharma") %>%
  ggplot(aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Pharma Companies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: Automotive companies in general after the year of 2019 has dropped in RQ scale
poll %>%
filter(industry == "Automotive") %>%
  ggplot(aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Automotive Comapnies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: groceries companies has rocked up after 2020
poll %>%
filter(industry == "Groceries") %>%
ggplot(aes(year, rq, color = company)) +
  geom_path() +
  geom_point(size = 3) +
  labs(title = "The RQ Score for Groceries Comapnies", x = "Years") +
  theme(plot.title = element_text(hjust = 0.5))


# Remove duplicates from 2022 records
company_rank <- poll %>%
  filter(year == 2022, rank < 11) %>%
  distinct(company, .keep_all = TRUE) %>% 
  mutate(company = fct_reorder(company, desc(rank)))


#***** Findings: The top 3 companies all belongs to the retail industry
# Plot top 10 of 2022 using Lollipop Chart along with industry
  ggplot(company_rank, aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "white", size = 3) +
  geom_segment(aes(x = company, xend = company, y = 0, yend = rank)) +
  labs(title = "The Highest Ranked Companies", subtitle = "Rank") +
  theme_light() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

# best companies of each industry in 2022
bestcompany <- poll %>%
  filter(year == 2022) %>% 
  distinct(company, .keep_all = TRUE) %>% 
  mutate(company = fct_reorder(company, desc(rank))) %>%
  group_by(industry) %>%
  slice(which.min(rank))

#***** Findings:best tech company is Samsung,  for eCommerce Amazon, for cars Toyota, for media we have Spotify
ggplot(bestcompany, aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "black", size = 3) +
  geom_segment(aes(x = company, xend = company, y = 0, yend = rank)) +
  labs(title = "The Highest Ranked Companies For Each Industry", subtitle = "Rank") +
  theme_light() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

# worst companies of each industry in 2022
worstcompany <- poll %>%
  filter(year == 2022) %>% 
  distinct(company, .keep_all = TRUE) %>% 
  mutate(company = fct_reorder(company, rank)) %>%
  group_by(industry) %>%
  slice(which.max(rank))

#***** Findings:worst tech company is Twitter,  for eCommerce Wish, for cars General motors, for media we have Fox corporation
worstcompany %>% ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "black", size = 3) +
  geom_segment(aes(x = company, xend = company, y = 0, yend = rank)) +
  labs(title = "The Lowest Ranked Companies For Each Industry", subtitle = "Rank") +
  theme_light() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

# An RQ score is calculated by:
# 80 & above: Excellent;
# 75-79: Very Good ;
# 70-74: Good ;
# 65-69: Fair ;
# 55-64: Poor ;
# 50-54: Very Poor ;
# Below 50: Critical

#***** Findings: most companies has a very good score, non got critical points, while 5 companies got very poor results
poll %>%
  distinct() %>%
  group_by(company) 


#***** Findings: The mode of the Rank for all companies in the yea 2022 is `Very Good`.
# meaning that most of companies in 2022 are rated as `Very Good`.
poll$rq_cat <- cut(poll$rq,
                           breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
                           labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
                           right = FALSE)

poll %>%
  filter(year == 2022) %>%
  distinct() %>%
  ggplot(aes(rq_cat)) +
  geom_bar(fill = "steelblue") +
  geom_text(position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5)


#################################################
#                 Reputation EDA                #
#################################################


reputation$cats <- cut(reputation$score,
                           breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
                           labels = c("Critical", "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"),
                           right = FALSE)

# stacked bar chart for score of tech companies in every name ** this cab be done for other industries
#***** Findings: johnson&johnson has lower scores in every aspect compared to Pfizer
reputation %>%
  filter(industry == "Pharma") %>% 
  ggplot(aes(x = company, y = score, fill = name, label = score)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "The Pharma Companies Detailed Scores for 2022") +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: Honda has excellent score in most aspects except for citizenship the has is Toyota a better citizenship
reputation %>%
filter(industry == "Automotive") %>%
ggplot(aes(x = company, y = score, fill = name, label = cats)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "The Automotive Companies Detailed Scores for 2022") +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))


#***** Findings: retails and tech have more companies with high score for culture
# rank of companies in term of names, plot top 10 in term of ethics
reputation %>%
  filter(name == "CULTURE") %>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank))) %>% 
  ggplot(aes(x = company, y = rank, color = industry)) +
  geom_point(size = 3) +
  geom_text(aes(label = rank), color = "white", size = 3) +
  geom_segment(aes(x = company, xend = company, y = 0, yend = rank)) +
  labs(title = "The Highest Ranked Companies", subtitle = "Rank") +
  theme_light() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())


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
