
sc <- read_csv("data_output/surveys_complete.csv")

filter(sc, genus=="Dipodomys",
       hindfoot_length<=35 )

sc %>% 
  filter(sex=="M",year>=1980)%>%
  select(month, day, species, hindfoot_length)%>%
  arrange(month, day)

sc%>%
  group_by(hindfoot_length<6)%>%
  summarise(mean_length = mean(hindfoot_length),
            n=n())

