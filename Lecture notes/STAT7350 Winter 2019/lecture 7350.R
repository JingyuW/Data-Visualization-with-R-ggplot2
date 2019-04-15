library(tidyverse)
library(gridExtra)

download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")

surveys <- read_csv("data/portal_data_joined.csv")
surveys

surveys_complete <- surveys %>%
  filter(!is.na(weight), 
         !is.na(hindfoot_length), 
         !is.na(sex)) 

nrow(surveys)-nrow(surveys_complete)

## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>%
  filter(n >= 50)

hist(species_counts$n, breaks = 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

nrow(surveys_complete)
ncol(surveys_complete)

write_csv(surveys_complete, path = "data_output/surveys_complete.csv")

#To build the ggplot
# ggplot(data = surveys_complete,
#        mapping = aes(x = weight, y = hindfoot_length))+
#   geom_point()

p <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))
p + geom_point()

p + geom_point(alpha= (1/3), size=2, aes(color = genus))

# log 10 transformation weight
p <- ggplot(data = surveys_complete,
            mapping = aes(x = log10(weight), y = hindfoot_length))
p + geom_point(alpha= (1/3), size=2, aes(color = genus))

#Add fitted curves or lines
p + geom_point(alpha= (1/3), size=2, aes(color = genus)) +
  geom_smooth()

# Another way to expolit a factor
p + geom_point(alpha= (1/3), size=2, aes(color = genus)) +
  facet_wrap(~ genus)

library(dplyr)
Dp <- "Dipodomys"
surveys_complete %>%
  filter(genus == Dp) %>%
  ggplot(aes(x = weight, y = hindfoot_length)) +
  labs(title = Dp) +
  geom_point(aes(colour=species))

ggplot(surveys_complete %>% filter(genus == Dp),
       aes(x = weight, y = hindfoot_length)) +
  labs(title = Dp) +
  geom_point(aes(colour=species))

# boxplot
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot()
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato")+ geom_boxplot(alpha = 0)

# geom_voolin
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin()
 ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
   geom_jitter(alpha = 0.3, color = "tomato")+
   geom_violin(alpha = 0)

# Plot the time series data
yearly_counts <- surveys_complete %>%
  count(year, species_id)
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line() # Did not work

ggplot(data = yearly_counts, mapping = aes(x = year, y = n, group = species_id)) +
  geom_line()
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = species_id)) +
  geom_line()

# split facet plot
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ species_id)

# split facet plot by sex using color
yearly_sex_counts <- surveys_complete %>%
  count(year, species_id, sex)
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id)

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

# split by weight
# yearly_counts_weight <- surveys_complete %>%
#   count(year, species_id, weight)
# ggplot(data = yearly_counts_weight, mapping = aes(x = year, y = n)) +
#   geom_line() +
#   facet_wrap(~ species_id)

# One column, facet by rows
yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))
ggplot(data = yearly_sex_weight,
       mapping = aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(species_id ~ .)

# One row, facet by column
ggplot(data = yearly_sex_weight,
       mapping = aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(. ~ sex)



ggplot(surveys_complete, aes(weight)) +
  geom_histogram()

ggplot(surveys_complete, aes(weight, fill = sex)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ species_id, scale = "free")

ggplot(data = surveys_complete, mapping = aes(x = plot_type)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  facet_wrap(~ species_id)

#chengelle

p <- surveys_complete %>%
  filter(genus == "Neotoma") %>%
  ggplot(aes(x = year, y = weight )) +
  geom_point()
p

p_loess <- p +
  stat_smooth(method = "loess", span = 0.4)
p_loess
p_lm <- p +
  stat_smooth(method = "lm")
grid.arrange(p_loess, p_lm, ncol = 2)


