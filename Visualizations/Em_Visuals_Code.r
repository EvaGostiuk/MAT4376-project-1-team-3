##wrk.dirt <- getwd() + "DataCleaning/polls_us_election_2016_clean.csv"
wrk.dirt <- file.path(getwd(),"DataCleaning","polls_us_election_2016_v4.csv")
clean.data <- read.csv(wrk.dirt)

#library(readr)
#clean.data <- read_csv("polls_us_election_2016_v4.csv")
str(clean.data)

###### Univariate Observations ######

#trump
hist(clean.data$rawpoll_trump) #normal
hist(clean.data$adjpoll_trump) #normal
#clinton
hist(clean.data$rawpoll_clinton) #normal - skewed
hist(clean.data$adjpoll_clinton) #normal - skewed
#johnson
hist(clean.data$rawpoll_johnson) #chi square? 
hist(clean.data$adjpoll_johnson) #normal - skewed
#mcmullin - N/A
hist(clean.data$rawpoll_mcmullin) #N/A
hist(clean.data$adjpoll_mcmullin) #N/A

hist(clean.data$grade)
#hist(clean.data$state)
hist(clean.data$poll_wt)
hist(clean.data$duration) # mostly >10 months(?)
hist(clean.data$samplesize) #not very nice


clean.data$pollster
length(unique(clean.data$pollster)) 
unique(clean.data$pollster) #196 unique pollsters

library(dplyr)
#frequency of pollsters
as.data.frame(table(clean.data$pollster)) %>%
  arrange(desc(Freq))

#frequency of states
as.data.frame(table(clean.data$state)) %>%
  arrange(desc(Freq))

clean.data$poll_wt

#pollsters weight - sum
pollster_weights <- aggregate(clean.data$poll_wt ~ clean.data$pollster, data = clean.data, sum)
pollster_weights
knitr::kable(pollster_weights, caption = "Pollster Weights Table")

##sorted weights
poll_weight <-clean.data$poll_wt
library(dplyr)
pollster_weights <- aggregate(poll_weight ~ clean.data$pollster, data = clean.data, sum)
pollster_weights <- pollster_weights[order(pollster_weights$poll_weight, decreasing = FALSE), ]
knitr::kable(pollster_weights, caption = "Pollster Weights Sorted Table")

#pollsters duration - average
pollster_duration <- aggregate(clean.data$duration ~ clean.data$pollster, data = clean.data, mean)
pollster_duration
knitr::kable(pollster_weights, caption = "Pollster Duration Table")

##sorted duration
duration <- clean.data$duration
pollster_duration <- aggregate(duration ~ clean.data$pollster, data = clean.data, mean)
pollster_duration <- pollster_duration[order(pollster_duration$duration, decreasing = FALSE), ]
knitr::kable(pollster_duration, caption = "Pollster Duration Sorted Table")

#pollsters grade- sorted average
grade <- clean.data$grade
pollster_grade <- aggregate(grade ~ clean.data$pollster, data = clean.data, mean)
pollster_grade <- pollster_grade[order(pollster_grade$grade, decreasing = FALSE), ]
knitr::kable(pollster_grade, caption = "Pollster Grade Sorted Table")

#plus-minus score on grade
plus_minus <- clean.data$`Simple Plus-Minus`
reg.plus_min <- lm(plus_minus ~ grade)
plot(reg.plus_min)
summary(reg.plus_min)

#average plus minus for pollsters
pollster_plus.minus <- aggregate(plus_minus ~ clean.data$pollster, data = clean.data, mean)
pollster_plus.minus <- pollster_plus.minus[order(pollster_plus.minus$plus_minus, decreasing = FALSE), ]
knitr::kable(pollster_plus.minus, caption = "Pollster Plus Minus Sorted Table")


#average house effect for pollsters
house_effect <- clean.data$`House Effect`
pollster_houseef <- aggregate(house_effect ~ clean.data$pollster, data = clean.data, mean)
pollster_houseef <- pollster_houseef[order(sort(pollster_houseef$`clean.data$pollster`)), ]
knitr::kable(pollster_houseef, caption = "Pollster House Effect Sorted Table")

#average mean_rev for pollsters - Alphabetical
mean_rev <- clean.data$`Mean-Reverted Bias...32`
pollster_mean_rev <- aggregate(mean_rev ~ clean.data$pollster, data = clean.data, mean)
pollster_mean_rev <- pollster_mean_rev[order(sort(pollster_mean_rev$`clean.data$pollster`)), ]
knitr::kable(pollster_mean_rev, caption = "Pollster Mean Rev Effect Sorted Table")

#average mean rev ADVANCED for pollsters - Alphabetical
mean_revadv <- clean.data$`Mean-Reverted Advanced Plus Minus`
pollster_mean_revadv <- aggregate(mean_revadv ~ clean.data$pollster, data = clean.data, mean)
pollster_mean_revadv <- pollster_mean_revadv[order(sort(pollster_mean_revadv$`clean.data$pollster`)), ]
knitr::kable(pollster_mean_revadv, caption = "Pollster Mean Rev ADVANCED Effect Sorted Table")

#average expected error (simple) for pollsters - Alphabetical
exp_error <- clean.data$`Simple Expected Error`
pollster_experr <- aggregate(exp_error ~ clean.data$pollster, data = clean.data, mean)
pollster_experr <- pollster_experr[order(sort(pollster_experr$`clean.data$pollster`)), ]
knitr::kable(pollster_experr, caption = "Pollster Mean Rev ADVANCED Effect Sorted Table")

# NCPP / ACPP vs grade
#ncpp <- clean.data$`NCPP / AAPOR / Roper`
#plot(x=grade, y=ncpp)




####### Bivariate Observations ########

##rawpoll vs adjpoll each candidate

#trump#
lm_trump = lm(clean.data$rawpoll_trump ~ clean.data$adjpoll_trump)
summary(lm_trump)
plot(lm_trump)

#clinton#
lm_clinton = lm(clean.data$rawpoll_clinton ~ clean.data$adjpoll_clinton)
summary(lm_clinton)
plot(lm_clinton)

#johnson#
lm_johnson = lm(clean.data$rawpoll_johnson ~ clean.data$adjpoll_johnson)
summary(lm_johnson)
plot(lm_johnson)

#mcmullin#
lm_mcmullin = lm(clean.data$rawpoll_mcmullin ~ clean.data$adjpoll_mcmullin)
summary(lm_mcmullin)
plot(lm_mcmullin)

#poll_wt
poll_grade <- lm(clean.data$poll_wt ~ clean.data$grade ) #poll_Wt -> Y , grade -> x
summary(poll_grade)
#plot(poll_grade)




## ratios for rawpoll vs adjpoll each candidate comparison
ratio.trump <- clean.data$rawpoll_trump/clean.data$adjpoll_trump
ratio.clinton <- clean.data$rawpoll_clinton/clean.data$adjpoll_clinton
ratio.johnson <- clean.data$rawpoll_johnson/clean.data$adjpoll_johnson
ratio.mcmullin <- clean.data$rawpoll_mcmullin/clean.data$adjpoll_mcmullin

#lm_ratios

Ratios <- data.frame(ratio.trump, ratio.clinton, ratio.johnson, ratio.mcmullin)
plot(Ratios)

library(ggplot2)
library(tidyr)

dfc <- rbind(ratio.trump, ratio.clinton, ratio.johnson, ratio.mcmullin)




#dates of poll, grade, rawpoll - scatterplot?

my_df <- data.frame(x = clean.data$startdate, y = clean.data$rawpoll_clinton, group = clean.data$grade)


#linear regression on raw_poll vs. adj_poll on grade
data1.raw_poll <- lm(clean.data$rawpoll_trump ~ clean.data$grade)
data1.raw_poll
summary(data1.raw_poll)
plot(data1.raw_poll)

 ## mean of the raw_poll
#sum_raw_poll <- clean.data$rawpoll_clinton + clean.data$rawpoll_johnson + clean.data$rawpoll_trump + clean.data$adjpoll_mcmullin
#sum_raw_poll
#data2.raw_poll <- lm(mean_raw_poll ~ clean.data$grade)
#summary(data2.raw_poll)
#plot(data2.raw_poll)


# mean_rev_bias vs bias
library("ggplot2")
mean_rev <- clean.data$`Mean-Reverted Bias...32`
bias <-clean.data$Bias
grade <- clean.data$grade
ggplot(clean.data,aes(x=mean_rev,y=grade,group=bias))+
  geom_point(aes(color=bias))

ggplot(clean.data,aes(x=bias,y=grade,group=mean_rev))+
  geom_point(aes(color=mean_rev))

ggplot(clean.data,aes(x=mean_rev,y=bias,group=(grade)))+
  geom_point(aes(color=(grade)))

#duration vs mean reverted bias with grade
mean_rev <- clean.data$`Mean-Reverted Bias...32`
grade <- clean.data$grade
duration <- clean.data$duration
ggplot(clean.data,aes(x=mean_rev,y=duration,group=grade))+
  geom_point(aes(color=grade))

#house effect vs mean rev bias with grade ->>> positive correlation
mean_rev <- clean.data$`Mean-Reverted Bias...32`
grade <- clean.data$grade
house_effect <- clean.data$`House Effect`
ggplot(clean.data,aes(x=mean_rev,y=house_effect,group=grade))+
  geom_point(aes(color=grade))
## linear regression
data3 <- lm(grade ~ mean_rev + house_effect)
summary(data3)
plot(data3)

#house effect, plus_minus and grade -->>> no correlation
house_effect <- clean.data$`House Effect`
plus_minus <- clean.data$`Simple Plus-Minus`
grade <- clean.data$grade
ggplot(clean.data,aes(x=house_effect,y=plus_minus,group=grade))+
  geom_point(aes(color=grade))

#expected error, house effect and grade ---> none / slightlighty negative?
exp_error <- clean.data$`Simple Expected Error`
house_effect <- clean.data$`House Effect`
grade <- clean.data$grade
ggplot(clean.data,aes(x=house_effect,y=exp_error,group=grade))+
  geom_point(aes(color=grade))

#expected error, mean rev (reg) and grade ---> no correlation
exp_error <- clean.data$`Simple Expected Error`
grade <- clean.data$grade
mean_rev <- clean.data$`Mean-Reverted Bias...32`
ggplot(clean.data,aes(x=mean_rev,y=exp_error,group=grade))+
  geom_point(aes(color=grade))







