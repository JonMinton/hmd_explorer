rm(list = ls())


pacman::p_load(tidyverse, plotly)

# Aim is to produce some figures showing many HMD countries with the aim of identifying which countries are UK countries.


dta_e0 <- read_csv("hmd_explorer/data/hmd_e0.csv")
dta  <- read_csv("hmd_explorer/data/hmd_data.csv")

data_nrs <- tribble(
  ~code, ~year, ~gender, ~e0,
  "GBR_SCO", 2017, "female", 81.15,
  "GBR_SCO", 2017, "male", 77.14
)

dta_e0 <- bind_rows(
  dta_e0,
  data_nrs
)


dta_e0 %>% 
#  filter(code == "GBR_SCO") %>% 
  filter(code %in% c(
    "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
  )) %>% 
  filter(gender != "total") %>% 
  filter(year>=1959) %>% 
  group_by(gender, code) %>% 
  arrange(year) %>% 
  mutate(change_e0 = e0 - lag(e0)) %>% 
  ggplot(aes(x = year, y = change_e0, group = code)) + 
  geom_line() + geom_point() +
  facet_grid(. ~ gender) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + 
  labs(
    title = "Annual changes in life expectancy over time in seven countries from 1960",
    x = "Year", 
    y = "Change in life expectancy from previous year in years"
  ) + 
  geom_point(
    aes(x = year, y = change_e0),
    size = 2, shape = 15, colour = "red",
    data = dta_e0 %>% 
      filter(code %in% c(
         "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
      )) %>% 
      filter(gender != "total") %>% 
      group_by(gender, code) %>% 
      arrange(year) %>% 
      mutate(change_e0 = e0 - lag(e0)) %>% 
      filter(year == 2015)
  )


# Anonymised trends

dta_e0 %>% 
  #  filter(code == "GBR_SCO") %>% 
  filter(code %in% c(
    "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
  )) %>% 
  filter(gender != "total") %>% 
  filter(year>=1959) %>% 
  group_by(gender, code) %>% 
  arrange(year) %>% 
  mutate(change_e0 = e0 - lag(e0)) %>% 
  ggplot(aes(x = year, y = change_e0, group = code)) + 
  geom_line() + geom_point() +
  facet_grid(code ~ gender,
             labeller = labeller(
               code = c(
                 DNK = "A",
                 ESP = "B",
                 FIN = "C",
                 GBR_SCO = "D",
                 GBRCENW = "E",
                 FRACNP = "F",
                 USA = "G"
               )
             )
             ) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + 
  labs(
    title = "Annual changes in life expectancy over time in seven countries from 1960",
    subtitle = "Countries: Spain, Finland, Scotland, England/Wales, France, USA, Denmark",
    x = "Year", 
    y = "Change in life expectancy from previous year in years"
  ) + 
  geom_point(
    aes(x = year, y = change_e0),
    size = 2, shape = 15, colour = "red",
    data = dta_e0 %>% 
      filter(code %in% c(
         "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
      )) %>% 
      filter(gender != "total") %>% 
      group_by(gender, code) %>% 
      arrange(year) %>% 
      mutate(change_e0 = e0 - lag(e0)) %>% 
      filter(year == 2015)
  )





# deanonymised trends

dta_e0 %>% 
  #  filter(code == "GBR_SCO") %>% 
  filter(code %in% c(
    "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
  )) %>% 
  filter(gender != "total") %>% 
  filter(year>=1959) %>% 
  group_by(gender, code) %>% 
  arrange(year) %>% 
  mutate(change_e0 = e0 - lag(e0)) %>% 
  ggplot(aes(x = year, y = change_e0, group = code)) + 
  geom_line() + geom_point() +
  facet_grid(code ~ gender
  ) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + 
  labs(
    title = "Annual changes in life expectancy over time in seven countries from 1960",
    subtitle = "Countries: Spain, Finland, Scotland, England/Wales, France, USA, Denmark",
    x = "Year", 
    y = "Change in life expectancy from previous year in years"
  ) + 
  geom_point(
    aes(x = year, y = change_e0),
    size = 2, shape = 15, colour = "red",
    data = dta_e0 %>% 
      filter(code %in% c(
        "DNK", "ESP", "FIN", "GBR_SCO", "GBRCENW", "FRACNP", "USA"
      )) %>% 
      filter(gender != "total") %>% 
      group_by(gender, code) %>% 
      arrange(year) %>% 
      mutate(change_e0 = e0 - lag(e0)) %>% 
      filter(year == 2015)
  )




# deanonymised trends

dta_e0 %>% 
  filter(gender != "total") %>% 
  filter(year>=1959) %>% 
  group_by(gender, code) %>% 
  arrange(year) %>% 
  mutate(change_e0 = e0 - lag(e0)) %>% 
  ggplot(aes(x = year, y = change_e0, group = code)) + 
  geom_line(alpha = 0.2) + geom_point(alpha = 0.2) +
  facet_grid(. ~ gender
  ) + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + 
  labs(
    title = "Annual changes in life expectancy over time in many countries from 1960",
    x = "Year", 
    y = "Change in life expectancy from previous year in years"
  ) +
  geom_line(
    aes(x = year, y = change_e0, group = code, colour = code),
    size = 1.4,
    data = dta_e0 %>% 
      filter(gender != "total") %>% 
      filter(year>=1959) %>% 
      filter(code %in% c("USA", "GBRCENW", "GBR_SCO")) %>% 
      group_by(gender, code) %>% 
      arrange(year) %>% 
      mutate(change_e0 = e0 - lag(e0)) 
  ) +
  geom_point(
    aes(x = year, y = change_e0, group = code, colour = code, shape = code),
    size = 1.4,
    data = dta_e0 %>% 
      filter(gender != "total") %>% 
      filter(year>=1959) %>% 
      filter(code %in% c("USA", "GBRCENW", "GBR_SCO")) %>% 
      group_by(gender, code) %>% 
      arrange(year) %>% 
      mutate(change_e0 = e0 - lag(e0)) 
  ) 


# Life course

dta %>% 
  filter(Year == 2016) %>% 
  filter(code == "GBR_SCO") %>% 
  filter(gender != "Total") %>% 
  filter(Age <= 90) %>% 
  mutate(mr = num_deaths / exposure) %>% 
  ggplot(aes(x = Age, y = mr, group = gender, color = gender)) +
  geom_line() + 
  labs(x = "Age in years", y = "Mortality rate", title = "Scotland, 2016")

dta %>% 
  filter(Year == 2016) %>% 
  filter(code == "GBR_SCO") %>% 
  filter(gender != "Total") %>% 
  filter(Age <= 90) %>% 
  mutate(mr = num_deaths / exposure) %>%
  mutate(lmr = log(mr + 0.0001, 10)) %>% 
  ggplot(aes(x = Age, y = lmr, group = gender, color = gender)) +
  geom_line() + 
  labs(x = "Age in years", y = "Log mortality rate", title = "Scotland, 2016")

