eighteen <- read.csv(url("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
seventeen <- read.csv(url("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv"))
sixteen <- read.csv(url("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2016.csv"))
nineteen <- read.csv(url("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

all <- rbind(sixteen, seventeen, eighteen, nineteen)

stafford <- nineteen %>% 
  mutate(spike = ifelse(str_detect(desc, "spiked"), 1, 0)) %>%
  filter(spike == 0, !is.na(receiver_player_name), play_type == "pass", !is.na(air_yards), passer_player_name == "M.Stafford")

driskel <- nineteen %>% 
  mutate(spike = ifelse(str_detect(desc, "spiked"), 1, 0)) %>%
  filter(spike == 0, !is.na(receiver_player_name), play_type == "pass", !is.na(air_yards), passer_player_name == "J.Driskel")

blough <- nineteen %>% 
  mutate(spike = ifelse(str_detect(desc, "spiked"), 1, 0)) %>%
  filter(spike == 0, !is.na(receiver_player_name), play_type == "pass", !is.na(air_yards), passer_player_name == "D.Blough")

all %>% 
  mutate(spike = ifelse(str_detect(desc, "spiked"), 1, 0)) %>%
  filter(spike == 0, !is.na(receiver_player_name), play_type == "pass", !is.na(air_yards)) %>% 
  ggplot(aes(x = air_yards, y = complete_pass)) +
  geom_smooth(fill = '#CCCCCC',
              alpha = .2,
              color = 'BLACK',
              size = 1,
              span = .5,
              se = FALSE,
  ) +
  geom_smooth(data = stafford,
              fill = '#006db0',
              alpha = .2,
              color = '#006db0',
              size = 1,
              span = .5,
              se = FALSE
  ) +
  geom_smooth(data = blough,
              fill = '#006db0',
              alpha = .9,
              color = 'grey',
              size = 1,
              span = .5,
              se = FALSE
  ) +
  geom_smooth(data = driskel,
              fill = '#006db0',
              alpha = .9,
              color = 'gold',
              size = 1,
              span = .5,
              se = FALSE
  ) +
  scale_x_continuous(
    limit = c(-5, 50),
    breaks = c(seq(
      from = -5,
      to = 50,
      by = 5
    )),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    limit = c(0, 1),
    breaks = c(seq(
      from = 0,
      to = 1,
      by = .1
    )),
    minor_breaks = NULL,
    labels = scales::percent
  ) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Completion Percentage") +
  xlab("Target Depth") +
  labs(title = "Lions QBs CPOE",
       subtitle = "Through week 16",
       caption = "Data from @nflscrapR | viz by @LionsRstats") -> fullchart


ggsave("lionsCPOE.png", fullchart, height = 9, width = 16)