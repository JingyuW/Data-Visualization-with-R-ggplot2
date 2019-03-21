library(tidyverse)
library(gridExtra)

# 1. Question: the histogram of data for total invasion threat.
inv.threat<-read_csv("/Users/summe/Downloads/Stat-7350-class/Assignment 1/Data/table_1.csv")
tot.cost<-read_csv("/Users/summe/Downloads/Stat-7350-class/Assignment 1/Data/table_2.csv")

#hist(tot.cost$invasion_cost, xlim = c(0,2e+10), breaks = 300)

#ggplot(tot.cost,aes(invasion_cost)) + geom_histogram()
ggplot(inv.threat,aes(invasion_threat)) +
  geom_histogram(binwidth = 0.01)

ggplot(tot.cost,aes(invasion_cost)) + geom_histogram()
ggplot(tot.cost,aes(log(invasion_cost))) + geom_histogram()
ggplot(tot.cost,aes(log(invasion_cost),fill = country)) + geom_histogram()

# tot.cost %>%
#   filter(invasion_cost<2e+10)%>%
#   ggplot(tot.cost,aes(invasion_cost)) + geom_histogram()


# Q2: the histogram of 140 species of invasion threat
spec<-read_csv("/Users/summe/Downloads/Stat-7350-class/Assignment 1/Data/table_6.csv")
ggplot(spec,aes(max_impact_percent)) + geom_histogram(binwidth = 2)

# Q3: the histogram of GDP proportion for 124 countries
GDP<-read_csv("/Users/summe/Downloads/Stat-7350-class/Assignment 1/Data/table_3.csv")
ggplot(GDP,aes(gdp_proportion, fill = country)) + geom_histogram(binwidth = 0.005)

# the relationship between invasion threat and cost
inv.cost<-merge(inv.threat[,2:3],tot.cost[,1:2])
ggplot(data = inv.cost, mapping = aes(x = invasion_threat, 
                                      y = log(invasion_cost))) +geom_point()
inv.cost %>% 
  filter(invasion_cost<2e+09)%>%
  ggplot(aes(x = invasion_threat,y = log(invasion_cost)))+
  geom_point()+geom_smooth()

ggplot(inv.cost.new ,aes(x = invasion_threat,y = log(inv_cost_millions)))+
  geom_point()+geom_smooth(method = "loess", span = 0.4)
ggplot(inv.cost.new ,aes(x = invasion_threat,y = log(inv_cost_millions)))+
  geom_point()+geom_smooth(method = "lm")

# To make easier calculation, we change the unit of cost to millions of US dollar
# inv.cost.new<-merge(inv.threat[,2:3],tot.cost[,c(1,4)])
# ggplot(data = inv.cost.new, mapping = aes(x = invasion_threat, 
#                                           y = log(inv_cost_millions))) +geom_point()
# inv.cost.new %>% 
#   filter(inv_cost_millions<1000)%>%
#   ggplot(aes(x = invasion_threat,y = log(inv_cost_millions)))+
#   geom_point()+geom_smooth()

# TO see the outlier of invasion threat and cost
ggplot(data = inv.cost, mapping = aes(x = invasion_threat, 
                                      y = invasion_cost)) +geom_boxplot()
inv.cost %>% 
  filter(invasion_cost<1e+09)%>%
  ggplot(aes(x = invasion_threat,y = invasion_cost))+
  geom_boxplot()



 #relationship between cost and gdp
cost.gdp<-merge(tot.cost[,1:2],GDP[,3:4])
ggplot(data = cost.gdp, mapping = aes(x = log(invasion_cost),
                                      y = gdp_proportion)) +geom_point()
ggplot(data = cost.gdp, mapping = aes(x = log(invasion_cost),
                                      y = gdp_proportion)) +geom_boxplot()

cost.gdp %>%
  filter(invasion_cost<2e+10)%>%
  ggplot(aes(x = log(invasion_cost),y = gdp_proportion))+
  geom_point()

cost.gdp.new<-merge(tot.cost[,c(1,4)],GDP[,3:4])

cost.gdp.new %>%
  filter(inv_cost_millions<1000)%>%
  ggplot(aes(x = log(inv_cost_millions),y = gdp_mean))+
  geom_point()


# invas.gdp<-merge(inv.threat[,2:3],GDP[,3:4])
# ggplot(data = invas.gdp, mapping = aes(x = invasion_threat, 
#                                       y = gdp_proportion)) +geom_point()

ggplot(inv.cost ,aes(x = invasion_threat,y = log(invasion_cost)))+
  geom_point(size = 2, 
             aes(color = country))+ 
  geom_smooth(method = "lm")

