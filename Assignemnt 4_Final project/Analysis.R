library(ggplot2)
#library(GGally)
library(tidyverse)
library(here)
library(corrplot)
library(reshape2)
library(car)
library(Hmisc)

red_wine<-read_csv(here("Assignemnt 4_Final project","Data","red wine.csv"),
                   col_types = cols())
white_wine<-read_csv(here("Assignemnt 4_Final project","Data","white wine.csv"),
                     col_types = cols())


# Group 1 is red wine and group 2 is white wine. 
comb_wines<-rbind(red_wine,white_wine)
group<-c(rep("red",times=1599),rep("white",times=4898))
wines_new<-cbind(comb_wines,group)

## Reshape datasest
# red wine
red_wine_m<- melt(red_wine,id.vars='quality',
                  measure.vars=c("fixed acidity","volatile acidity",
                                 "citric acid","residual sugar","chlorides",
                                 "free sulfur dioxide","total sulfur dioxide",
                                 "density", "pH", "sulphates","alcohol"))


# white wine
white_wine_m<- melt(white_wine,id.vars='quality',
                    measure.vars=c("fixed acidity","volatile acidity",
                                   "citric acid","residual sugar","chlorides",
                                   "free sulfur dioxide","total sulfur dioxide",
                                   "density", "pH", "sulphates","alcohol"))


ggplot(wines_new, aes(quality, fill = group))+
  geom_histogram(binwidth = 1,position = "dodge")+
  ggtitle("Numbers for both wine in quality level", 
          subtitle = "Red wine and white wine")


t.r<-red_wine_m%>%
  filter(variable%in%c("fixed acidity","volatile acidity",
                       "citric acid","residual sugar","chlorides",
                       "free sulfur dioxide","total sulfur dioxide",
                       "density", "pH", "sulphates","alcohol"))%>%
  group_by(quality,variable)%>%
  summarise(Mean=mean(value))


ggplot(t.r, mapping = aes(x=quality, y=Mean))+
  geom_point(colour = "#0033FF")+
  geom_line(colour = "#0033FF")+
  facet_wrap(~variable, scales = "free_y")+
  ggtitle("Mean for different quality in each variable", 
          subtitle = "Red Wine")


t.w<-white_wine_m%>%
  filter(variable%in%c("fixed acidity","volatile acidity",
                       "citric acid","residual sugar","chlorides",
                       "free sulfur dioxide","total sulfur dioxide",
                       "density", "pH", "sulphates","alcohol"))%>%
  group_by(quality,variable)%>%
  summarise(Mean=mean(value))

  ggplot(t.w, mapping = aes(x=quality, y=Mean))+
  geom_point(colour = "#0033FF")+
  geom_line(colour = "#0033FF")+
  facet_wrap(~variable, scales = "free_y")+
    ggtitle("Mean for different quality in each variable", 
            subtitle = "white Wine")

  
  
  varib_cor_R<-cor(red_wine[,1:11])
  corrplot(varib_cor_R,method="color",type="upper", 
           addCoef.col = "black", tl.col="black", 
           tl.srt=45,insig = "blank", diag=FALSE,
           number.font = 6,number.cex = 0.75)
  
  varib_cor_W<-cor(white_wine[,1:11])
  corrplot(varib_cor_W,method="color",type="upper", 
           addCoef.col = "black", tl.col="black", 
           tl.srt=45,insig = "blank", diag=FALSE,
           number.font = 6,number.cex = 0.75)
  