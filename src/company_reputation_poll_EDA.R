
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
ggplot(Tech, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)


ggplot(Energy, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)


ggplot(Pharma, aes(year, rank, color = company)) +
  geom_bump()

ggplot(Automotive, aes(year, rank, color = company)) +
   geom_bump()+
  geom_point(size = 3)

ggplot(Groceries, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

ggplot(Groceries, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

ggplot(Financial_Services, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

ggplot(Food_Beverage, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

#change in rq
ggplot(Food_Beverage, aes(year, rq, color = company)) +
  geom_bump()+
  geom_point(size = 3)


#2ed way to plot

ggplot(Consumer_Goods, aes(year, rank, color = company)) +
  geom_point(size = 7) +
  geom_text(data = Consumer_Goods %>% filter(year == min(year)),
            aes(x = year - .1, label = company), size = 5, hjust = 1) +
  geom_text(data = Consumer_Goods %>% filter(year == max(year)),
            aes(x = year + .1, label = company), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest1"))


# Remove duplicates from 2022 records
Clean_df22<- df22 %>% distinct(company, .keep_all = TRUE)
company_rank<-Clean_df22%>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))
  
#plot top 10 of 2022
ggplot(company_rank, aes(x=company, y=rank,fill=industry)) + 
  geom_bar(stat = "identity", alpha=.6, width=.4) +
  coord_flip()


library(ggplot2)

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

######### change during covid in rank for tech companies

Change_cov19<-poll%>%
  filter(year < 2022 &year>2018)%>% drop_na(rank)


tech <- Change_cov19 %>%
  filter(industry == 'Tech')

ggplot(tech, aes(year, rank, color = company)) +
  geom_bump()+
  geom_point(size = 3)

#top/worst companies of each industry


library(dplyr)
bestcompany<-df22 %>%
  mutate(company = fct_reorder(company, desc(rank)))%>%
  group_by(industry) %>%
  slice(which.min(rank))
  


bestcompany%>%ggplot( aes(x=company, y=rank,color=industry)) + 
  geom_point(size=3) + 
  geom_text(aes(label = rank), color ="black", size = 3)+
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

ggplot(changeInrank, aes(factor(rq))) +
  geom_bar() 

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


tech <- reputation[reputation$industry == 'Tech',]
ggplot(tech, aes(x = company, y = score, fill = name, label = score)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

## same as above but istead od score we show the category of the score
ggplot(tech, aes(x = company, y = score, fill = name, label = cats)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


##

topethics<-ethics%>%
  filter(rank < 11) %>%
  mutate(company = fct_reorder(company, desc(rank)))

# rank of companies in term of names, plot top 10 in term of ethics
topethics%>%ggplot( aes(x=company, y=rank,color=industry)) + 
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



#for each company calculate sum of the score of all the names to show the best companies overall

#Hypothesis




