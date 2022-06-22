library(tidytuesdayR)
library(tidyverse)


#Getting the dataset#
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

#Cleaning the dataset#
poll %>%
  distinct() %>%
  na.omit() -> poll

reputation %>%
  distinct() %>%
  na.omit() -> reputation

posn_j <- position_jitter(seed = 136)


poll$`2022_rq_cat` <- cut(poll$`2022_rq`,
                          breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
                          labels = c("Critical", "Bad", "Poor", "Fair", "Good", "Great", "Excellent"),
                          right = FALSE)

ggplot(poll, aes(`2022_rq_cat`)) +
  geom_text(
    position = "stack", stat = "count", aes(label = ..count..), vjust = -0.5
  ) +
  geom_bar() +
  labs(
    title = "Distribution of Companies' RQ Score Categorize",
    x = "RQ Score Category",
    y = "Companies Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

reputation$`score_ctegory` <- cut(reputation$score,
                                  breaks = c(0, 50, 55, 65, 70, 75, 80, Inf),
                                  labels = c("Critical", "Bad", "Poor", "Fair", "Good", "Great", "Excellent"),
                                  right = FALSE)


ggplot(reputation, aes(industry, score)) +
  geom_point(alpha = 0.26) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), col = "red")

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
  labs(title = "Top Comanies Score Distribution") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Plot the density of top companies scores.
ggplot(topScoreCompanies, aes(score, fill = company)) +
  geom_density(color = NA, alpha = 0.4) +
  labs(title = "Top Companies Scores Desity") +
  theme(plot.title = element_text(hjust = 0.5))

reputation%>%
  gather(-industry, key = "var", value = "value") %>% 
  ggplot(aes(x = value, fill=factor(industry))) +
  geom_density(alpha=0.3) +
  facet_wrap(~ var, scales = "free")+
  guides(fill = guide_legend(title = "Socre category"))+ xlab("score category")
