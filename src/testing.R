setwd("D:/OneDrives/OneDrive/University/2025 Autumn/DSAA811/Assignment/DSAA811-Preliminary-Report/data")

library(tidyverse)
library(sjmisc)
athletes <- read.csv('athlete_events_data_dictionary.csv', header = TRUE)
events <- read.csv('athlete_events.csv', header = TRUE)
countryDefdd<- read.csv('country_definitions_data_dictionary.csv',header = TRUE)
countryDef <- read.csv('country_definitions.csv',header = TRUE)
head(athletes,15)

events %>% filter(Sport == "Gymnastics") %>%
  ggplot() +
  geom_point(aes(x = Age, y = Weight)) +
  labs(title = 'Olympic gymnasts ages and weights') +
  theme_bw()

events  %>% filter(Year == 2016) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2016') +
  theme_minimal(12)


events %>% filter ((Year == 2012) | (Year == 2016)) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2016') +
  theme_minimal(12) +
  facet_wrap(events$Year)

options(max.print=1000000)

events %>% filter (Year == 2000) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2000') +
  theme_bw()

summary(events)

events$Sex <- factor(events$Sex,
                     levels = c("M","F"),
                     labels = c("M","F"))


events %>% filter (Year >= 2000) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2000') + theme_bw() +
  facet_wrap(vars(events$Year >= 2000))




#Within your exploratory analysis include:
#1. A visualisation of an amount or frequency over two qualitative variables.

events  %>% filter(Year == 2016) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2016') +
  theme_minimal(12)

Summer <- events %>% filter (Season == "Summer") %>% filter (Year > 2000)
Summer %>% 
  ggplot(aes(width=60, height=100)) +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2000') + theme_bw() +
  facet_wrap(vars(Summer$Year))


summerCounts <- Summer %>%  
  group_by (Year) %>%
  count(Sport)

summerCounts <- Summer %>% 
  group_by (Year,Sport) %>%
  count(Sport) %>% 
  arrange(Year, desc(n)) %>%
  pivot_wider(values_from = n, names_from = Sport)

summerCounts <- rotate_df(summerCounts)
print.data.frame (summerCounts)
#2. A visualisation comparing two (or more) quantitative associations.

events %>% filter(Sport == "Gymnastics") %>%
  ggplot() +
  geom_point(aes(x = Age, y = Weight)) +
  labs(title = 'Olympic gymnasts ages and weights') +
  theme_bw()

#3. A visualisation demonstrating a trend or relationship in the data.

lmdata <- Summer %>%  
  group_by (Year) %>%
  count(Sport) %>% 
  arrange(Year, desc(n)) %>%
  pivot_wider(values_from = n, names_from = Sport)

ggplot(lmdata) +
  geom_line(aes(x = lmdata[,2,], y = Year))
  theme_linedraw()

lmdata[,2:37,]

#4. A visualisation describing the shape of the distribution of the data.
#5. A visualisation demonstrating the difference in means 
#     (with confidence intervals) across different groups of people

