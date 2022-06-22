
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

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Get the Data

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

summary(poll)

#extracting the results of 2022
df22<- poll %>%
  select(company, industry, change,`2022_rank`,`2022_rq`) %>%
  mutate(year =2022,rank=`2022_rank`,rq= `2022_rq`)

#removing old cols
df22 <- subset(df22, select = -c(`2022_rank`,`2022_rq`) )
poll <- subset(poll, select = -c(`2022_rank`,`2022_rq`) )

#merging results of 2022 with the other years to reduce cols
finalpoll<- rbind(poll, df22)


#filtering the industries

changeInrank <-finalpoll %>% drop_na(rank)
changeInrank <-changeInrank %>% drop_na(change)

Groceries <- changeInrank %>%
  filter(industry == 'Groceries')

Ecommerce <- changeInrank %>%
  filter(industry == 'Ecommerce')

Other <- changeInrank %>%
  filter(industry == 'Other')

Logistics <- changeInrank %>%
  filter(industry == 'Logistics')

Automotive <- changeInrank %>%
  filter(industry == 'Automotive')

Financial_Services<- changeInrank %>%
  filter(industry == 'Financial Services')

Industrial <- changeInrank %>%
  filter(industry == 'Industrial')

Food_Beverage <- changeInrank %>%
  filter(industry == 'Food & Beverage')

Consumer_Goods <- changeInrank %>%
  filter(industry == 'Consumer Goods')

Pharma <- changeInrank %>%
  filter(industry == 'Pharma')

Energy <- changeInrank %>%
  filter(industry == 'Energy')

Tech <- changeInrank %>%
  filter(industry == 'Tech')


#Changes in ranks over the years for each industry
#*****Findings: most tech companies has low rank in 2020 while it raises in 2021
ggplot(Tech, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

#*****Findings: pharams companies has a high during 2020 during covid. However, in 2021 pfizer has a sharpe drop in rank
ggplot(Pharma, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

#*****Findings: Automotive companies in general after the year of 2019 has drop in rank
ggplot(Automotive, aes(year, rank, color = company)) +
   geom_bump()+
  geom_point(size = 3)

#*****Findings: groceries companies has rocked up after 2020
ggplot(Groceries, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)


# Remove duplicates from 2022 records
Clean_df22<- df22 %>% distinct(company, .keep_all = TRUE)
company_rank<-Clean_df22%>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))


library(ggplot2)
#*****Findings: the top 3 companies all belongs to the retail industry
# Plot top 10 of 2022 using Lollipop Chart along with industry
company_rank%>%ggplot( aes(x=company, y=rank,color=industry)) + 
  geom_point(size=3) + 
  geom_text(aes(label = rank), color ="white", size = 3)+
  geom_segment(aes(x=company, 
                   xend=company, 
                   y=0, 
                   yend=rank)) + 
  labs(title="The Highest Ranked Companies", 
       subtitle="Rank") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#best companies of each industry in 2022
bestcompany<-df22 %>%
  mutate(company = fct_reorder(company, desc(rank)))%>%
  group_by(industry) %>%
  slice(which.min(rank))

#*****Findings:best tech company is Samsung,  for eCommerce Amazon, for cars Toyota, for media we have Spotify
bestcompany%>%ggplot( aes(x=company, y=rank,color=industry)) + 
  geom_point(size=3) + 
  geom_text(aes(label = rank), color ="black", size = 3)+
  geom_segment(aes(x=company, 
                   xend=company, 
                   y=0, 
                   yend=rank)) + 
  labs(title="The Highest Ranked Companies For Each Industry", 
       subtitle="Rank") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#worst companies of each industry in 2022
worstcompany<-df22 %>%
  mutate(company = fct_reorder(company, rank))%>%
  group_by(industry) %>%
  slice(which.max(rank))

#*****Findings:worst tech company is Twitter,  for eCommerce Wish, for cars General motors, for media we have Fox corporation
worstcompany%>%ggplot( aes(x=company, y=rank,color=industry)) + 
  geom_point(size=3) + 
  geom_text(aes(label = rank), color ="black", size = 3)+
  geom_segment(aes(x=company, 
                   xend=company, 
                   y=0, 
                   yend=rank)) + 
  labs(title="The Lowest Ranked Companies For Each Industry", 
       subtitle="Rank") +
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

changeInrank$rq[changeInrank$rq >= 80] <- "Excellent" 
changeInrank$rq[changeInrank$rq < 80 & changeInrank$rq > 75] <- "Very Good" 
changeInrank$rq[changeInrank$rq <=75 & changeInrank$rq > 70] <- "Good" 
changeInrank$rq[changeInrank$rq <= 70 & changeInrank$rq > 65] <- "Fair" 
changeInrank$rq[changeInrank$rq <= 65 & changeInrank$rq > 55] <- "Poor" 
changeInrank$rq[changeInrank$rq <= 55 & changeInrank$rq > 50] <- "Very Poor" 
changeInrank$rq[changeInrank$rq <= 50] <- "Critical" 

#*****Findings: most companies has a very good score, non got critical points, while 5 companies got very poor results
ggplot(changeInrank, aes(factor(rq))) +
  geom_bar(fill="steelblue")

############################Reputation##############################
vision <- reputation[reputation$name == 'VISION',]
trust <-  reputation[reputation$name == 'TRUST',]
citizenship <-  reputation[reputation$name == 'CITIZENSHIP',]
culture <-  reputation[reputation$name == 'CULTURE',]
ethics <-  reputation[reputation$name == 'ETHICS',]
growth <-  reputation[reputation$name == 'GROWTH',]
ps <-  reputation[reputation$name == 'P&S',]

reputation$cats = "Excellent"
reputation$cats[reputation$score < 80 & reputation$score > 75] <- "Very Good" 
reputation$cats[reputation$score <=75 & reputation$score > 70] <- "Good" 
reputation$cats[reputation$score <= 70 & reputation$score > 65] <- "Fair" 
reputation$cats[reputation$score <= 65 & reputation$score > 55] <- "Poor" 
reputation$cats[reputation$score <= 55 & reputation$score > 50] <- "Very Poor" 
reputation$cats[reputation$score <= 50] <- "Critical"


#stacked bar chart for score of tech companies in every name ** this cab be done for other industries
#*****Findings: johnson&johnson has lower scores in every aspect compared to Pfizer 
phar <- reputation[reputation$industry == 'Pharma',]
ggplot(phar, aes(x = company, y = score, fill = name, label = score)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

#*****Findings: Honda has excellent score in most aspect expect for citizenship the has is Toyota a better citizenship
auto <- reputation[reputation$industry == 'Automotive',]
ggplot(auto, aes(x = company, y = score, fill = name, label = cats)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


#*****Findings: retails and tech have more companies with high score for culture
# rank of companies in term of names, plot top 10 in term of ethics
topculture<-culture%>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))

topculture%>%ggplot( aes(x=company, y=rank,color=industry)) + 
  geom_point(size=3) + 
  geom_text(aes(label = rank), color ="white", size = 3)+
  geom_segment(aes(x=company, 
                   xend=company, 
                   y=0, 
                   yend=rank)) + 
  labs(title="The Highest Ranked Companies", 
       subtitle="Rank") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#Hypothesis
###Fitting Linear Model
lm.fit <- lm(poll$rank~ poll$rq , data = poll)

summary(lm.fit)
#*****Findings: the p value of rq is lower than 0.05. Thus, we can reject the null hypothesis
#< 2.2e-16 as the p value would indicate a significant result, 
#meaning that the actual p value is even smaller than 2.2e-16 
#(a typical threshold is 0.05, anything smaller counts as statistically significant)
# *****Findings:Multiple R-squared of  0.8497 means the it can explain 84% of variation in rank

###to get the confidence interval lower and upper bound
confint(lm.fit)

###abline is to draw the estimated line 
plot(poll$rq ~ poll$rank)
abline(lm.fit, lwd = 3, col = "red")




