library(nflscrapR)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# pull and combine all data

all <- read.csv(url("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

thirteen <- scrape_season_play_by_play(2019, weeks = 13)
fourteen <- scrape_season_play_by_play(2019, weeks = 14)

all <- rbind(all, thirteen)
all <- rbind(all, fourteen)

# add score ranges

all1 <- all %>% 
  mutate(scorerange = ifelse(score_differential <= -14, -14, 
                             ifelse(score_differential > -14 & score_differential < 14, score_differential,
                                    ifelse(score_differential >= 14, 14, F))))

#add possession changes

all2 <- all1 %>% 
  filter(!is.na(posteam)) %>% 
  mutate(change = ifelse(posteam != lag(posteam), 1, 0))

#fix the first entry

all2$change[is.na(all2$change)] <- 1

#fix the first entry

all2$scorerange[is.na(all2$scorerange)] <- 0

#view

all2 %>% 
  filter(change == 1) %>% 
  select(posteam, scorerange, change)

#count number of drives

totaldrives <- all2 %>% 
  filter(change == 1) %>% 
  group_by(posteam) %>% 
  summarize(totaldrives = n())

#count number of times up 14

biglead <- all2 %>% 
  filter(change == 1, scorerange == 14) %>% 
  group_by(posteam) %>% 
  summarize(biglead = n())

#count number of times down 14

smalllead <- all2 %>% 
  filter(change == 1, scorerange == -14) %>% 
  group_by(posteam) %>% 
  summarize(smalllead = n())

#arrange for all scores

scorepos <- all2 %>% 
  filter(change == 1) %>% 
  group_by(posteam, scorerange) %>% 
  summarize(count = n())

#combine everything together

all3 <- left_join(scorepos, totaldrives)
all3 <- left_join(all3, biglead)
all3 <- left_join(all3, smalllead)

view(all3)

#fix any entries if missing, I don't think this ended up being necessary

all3$biglead[is.na(all3$biglead)] <- 0

#add percentages

all3 <- all3 %>% 
  mutate(percent = count/totaldrives) %>% 
  mutate(bigpercent = biglead/totaldrives) %>% 
  mutate(smallpercent = smalllead/totaldrives) %>% 
  mutate(diference = bigpercent - smallpercent)

#chart it

all3 %>% 
  filter(posteam != "") %>% 
  ggplot(aes(x = scorerange, y = percent, fill = scorerange)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(high = "#00B368", low = "#B31C00") +
  scale_x_continuous(labels = function(x) sprintf("%+d", x), #to add + sign to positive labels
                     breaks = c(-14, -10, -7, -3, 0, 3, 7, 10, 14),
                     limits = c(-15, 15)) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = c(0, .1, .2, .3), 
                     limits = c(0, .31)) +
  facet_wrap(~reorder(posteam, -diference), 
             scales = "free", 
             strip.position = "bottom", 
             ncol = 4, 
             nrow = 8) +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7),
        strip.text = element_text(face = "bold", vjust = 3),
        strip.placement = "outside",
        plot.title = element_text(size = 15)) +
  labs(title = "Percentage of Possessions Up/Down __ Points",
       subtitle = "2019 Through Week 14
Arranged by % of +14 Possessions minus % of -14 Possessions",
       caption = "Data from @nflscrapR | Viz by @LionsRstats")
