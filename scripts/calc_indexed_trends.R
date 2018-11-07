# Quick script to get indexed trends by age group for Scotland and other countries 


rm(list = ls())

pacman::p_load(
  tidyverse
)


dta <- read_csv("hmd_explorer/data/hmd_data.csv")

names(dta) <- tolower(names(dta))

correction <- 50


dta %>% 
  filter(code %in% c("GBR_SCO", "GBRTENW", "FRATNP", "USA")) %>% 
  mutate(code = fct_recode(code, 
    Scotland = 'GBR_SCO', `England & Wales` = 'GBRTENW',
    France = 'FRATNP', `United States` = "USA"
    ),
    code = fct_relevel(code,
      'Scotland', 'England & Wales', 'France', 'United States'                       
    )
  ) %>% 
  filter(year >= 1980) %>% 
  mutate(
    age_group = cut(
      age, 
      breaks = c(0, 14, 34, 54, 74, 89, 109),
      include.lowest = T
    )
  ) %>% 
  filter(gender != "Total") %>% 
  filter(!is.na(age_group)) %>% 
  group_by(code, year, gender, age_group) %>% 
    summarise(num_deaths = sum(num_deaths), exposure = sum(exposure)) %>% 
  ungroup() %>% 
  mutate(mr = (num_deaths + correction) / (exposure + correction)) %>% 
  group_by(code, gender, age_group) %>% 
  mutate(mr_i = 100 * mr / mr[year == 1981]) %>% 
  ungroup() -> dta_indexed

dta_indexed %>% 
  filter(gender == 'Female') %>% 
  ggplot(aes(x = year, y = mr_i, group = age_group, linetype = age_group, size = age_group, colour = age_group)) + 
  geom_line() + 
  facet_wrap(~code) + 
  theme_minimal() + 
  geom_hline(yintercept = 100) +
  scale_size_manual(values = c(1, 1, 1, 1.3, 1.3, 1.3)) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid", "longdash", "twodash")) + 
  labs(
    title = "Index of mortality rates, England/Wales and Scotland. Females",
    subtitle = "Base year: 1981", 
    caption = "Source: Human Mortality Database",
    x = "Year", 
    y = "Index value (100 = 1981 rates)",
    colour = "Age group", 
    linetype = "Age group",
    size = "Age group"
  ) + 
  ylim(c(30, 130))

ggsave("figures/age_trends_females.png", height = 25, width = 25, units = 'cm', dpi = 300)

dta_indexed %>% 
  filter(gender == 'Male') %>% 
  ggplot(aes(x = year, y = mr_i, group = age_group, linetype = age_group, size = age_group, colour = age_group)) + 
  geom_line() + 
  facet_wrap(~code) + 
  theme_minimal() + 
  geom_hline(yintercept = 100) +
  scale_size_manual(values = c(1, 1, 1, 1.3, 1.3, 1.3)) + 
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "solid", "longdash", "twodash")) + 
  labs(
    title = "Index of mortality rates, England/Wales and Scotland. Males",
    subtitle = "Base year: 1981", 
    caption = "Source: Human Mortality Database",
    x = "Year", 
    y = "Index value (100 = 1981 rates)",
    colour = "Age group", 
    linetype = "Age group",
    size = "Age group"
  ) + 
  ylim(c(30, 130))

ggsave("figures/age_trends_males.png", height = 25, width = 25, units = 'cm', dpi = 300)




