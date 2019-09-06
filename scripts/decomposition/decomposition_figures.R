# Script to produce figures showing decomposition components

rm(list = ls())

pacman::p_load(tidyverse)


readxl::excel_sheets("data/decomp_data.xlsx")

dta <- readxl::read_excel("data/decomp_data.xlsx", sheet = "data_tidied")
dta <- dta[,1:30]

period_lookup <- tribble(
  ~period, ~period_label,
  1,       "2000-02 to 2012-14",
  2,       "2012-14 to 2015-17"
)

dta <- dta %>% 
  gather(`Infectious diseases`:`Drug-related`, key = "cause", value = "contribution") %>% 
  left_join(period_lookup, by = c("Period" = "period")) %>% 
  select(period = period_label, age_group = `age group`, gender, cause, contribution, total = Total)

dta <- dta %>% 
  mutate(age_group = ifelse(age_group == "999", NA_character_, age_group)) %>% 
  mutate(age_group = factor(age_group, 
                            ordered = TRUE,
                            levels = c(
                              "0", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29",
                              "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                              "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90+"
                            )
                            
                            )) %>% 
  mutate(period = factor(period, ordered = TRUE))

# Figures: decomposition heatmap

dta %>% 
  filter(!is.na(age_group)) %>% 
  ggplot(
    aes(
      x = age_group, y = cause, fill = contribution
      )
    ) + 
  facet_grid(period ~ gender) + 
  geom_tile() + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Decomposition by age and cause of death, by gender and period"
    )

ggsave("figures/decomposition/decomposition_heatmap_01.png", height = 20, width = 20, units = "cm", dpi = 300)


# greyscale variant


dta %>% 
  filter(!is.na(age_group)) %>% 
  mutate(direction = ifelse(contribution > 0, "+", "-")) %>% 
  ggplot(
    aes(
      x = age_group, y = cause, fill = contribution
    )
  ) + 
  facet_grid(period ~ gender) + 
  geom_tile(aes(fill = contribution)) + 
  geom_text(aes(label = direction)) + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Decomposition by age and cause of death, by gender and period"
  )

ggsave("figures/decomposition/decomposition_heatmap_01_greyok.png", height = 20, width = 20, units = "cm", dpi = 300)


# Another variant: border if negative

dta %>% 
  filter(!is.na(age_group)) %>% 
  mutate(direction = ifelse(contribution > 0, F, T)) %>% 
  ggplot(
    aes(
      x = age_group, y = cause, fill = contribution
    )
  ) + 
  facet_grid(period ~ gender) + 
  geom_tile(aes(fill = contribution, color = direction), width = 0.95, height = 0.95) + 
  geom_text(aes(label = round(contribution, 1)), size = 1.35) +
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Decomposition by age and cause of death, by gender and period"
  ) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) 

ggsave("figures/decomposition/decomposition_heatmap_01_greyok_boxed.png", height = 20, width = 20, units = "cm", dpi = 300)

dta %>% 
  filter(!is.na(age_group)) %>%
  select(-total) %>% 
  group_by(age_group, gender, cause) %>% 
  summarise(change_contribution =  contribution[period == "2012-14 to 2015-17"] - contribution[period == "2000-02 to 2012-14"]) %>% 
  mutate(direction = ifelse(change_contribution > 0, F, T)) %>% 
  ggplot(aes(x = age_group, y = cause, fill = change_contribution)) + geom_tile() + 
  geom_tile(aes(fill = change_contribution, color = direction), width = 0.95, height = 0.95) + 
  geom_text(aes(label = round(change_contribution, 1)), size = 1.5) +
  facet_wrap(~gender) +
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Change in Decomposition by age and\ncause of death between periods, by gender",
    fill = "Change"
  ) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) 

ggsave("figures/decomposition/decomposition_heatmap_02_greyok_boxed.png", height = 12, width = 20, units = "cm", dpi = 300)






# now change in contribution over time 

dta %>% 
  filter(!is.na(age_group)) %>%
  select(-total) %>% 
  group_by(age_group, gender, cause) %>% 
  summarise(change_contribution =  contribution[period == "2012-14 to 2015-17"] - contribution[period == "2000-02 to 2012-14"]) %>% 
  ggplot(aes(x = age_group, y = cause, fill = change_contribution)) + geom_tile() + 
  facet_wrap(~gender) +
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Change in Decomposition by age and\ncause of death between periods, by gender",
    fill = "Change"
  )

ggsave("figures/decomposition/decomposition_heatmap_02.png", height = 12, width = 20, units = "cm", dpi = 300)



dta %>% 
  filter(!is.na(age_group)) %>%
  select(-total) %>% 
  group_by(age_group, gender, cause) %>% 
  summarise(change_contribution =  contribution[period == "2012-14 to 2015-17"] - contribution[period == "2000-02 to 2012-14"]) %>% 
  mutate(direction = ifelse(change_contribution > 0, "+", "-")) %>% 
  ggplot(aes(x = age_group, y = cause, fill = change_contribution)) + geom_tile() + 
  geom_text(aes(label = round(change_contribution, 1))) +
  facet_wrap(~gender) +
  geom_tile(aes(fill = change_contribution, color = direction), width = 0.95, height = 0.95) + 
  scale_fill_gradient2() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(
    x = "Age group", y = "Cause of death", title = "Change in Decomposition by age and\ncause of death between periods, by gender",
    fill = "Change"
  ) +
  scale_color_manual(values = c("white", "black"), guide = FALSE) 

ggsave("decomposition_heatmap_02_freyok_boxed.png", height = 12, width = 20, units = "cm", dpi = 300)



## Decomposition by age group alone

dta %>% 
  filter(!is.na(age_group)) %>%
  select(-total) %>% 
  group_by(age_group, gender, period) %>% 
  summarise(contribution =  sum(contribution)) %>% 
  ggplot(aes(x = age_group, y = contribution, colour = period, group = period, shape = period)) + 
  facet_wrap(~gender, ncol = 1) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept =  0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_color_discrete() +
  labs(
    x = "Age group", y = "Contribution in weeks/year", title = "Decomposition by age group and period",
    fill = "Period"
  )

ggsave("figures/decomposition/decomp_age.png", height = 20, width = 20, units = "cm", dpi = 300)
  

# Decomposition by cause alone
dta %>% 
  filter(!is.na(age_group)) %>%
  select(-total) %>% 
  group_by(cause, gender, period) %>% 
  summarise(contribution =  sum(contribution)) %>% 
  ggplot(aes(x = cause, y = contribution, fill = period, group = period)) + 
  facet_wrap(~gender, ncol = 1) + 
  geom_col(position = "dodge") +
  geom_hline(yintercept =  0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete() +
  labs(
    x = "Cause of death", y = "Contribution in weeks/year", title = "Decomposition of cause of death and period",
    fill = "Period"
  )

ggsave("figures/decomposition/decomp_cause.png", height = 20, width = 20, units = "cm", dpi = 300)



# Now to replicate the kind of output below

# 
# by_factor_l2 <- dta %>% 
#   left_join(lookup) %>% 
#   filter(level == 2)  %>% 
#   filter(measure_name == "Deaths") %>% 
#   select(-measure_name, -measure_id) %>% 
#   filter(location_name != "Global") %>% 
#   filter(age_name == "Age-standardized") %>% 
#   filter(metric_name == "Rate") %>% 
#   filter(cause_name == "All causes") %>% 
#   select(year, sex = sex_name, sdi = location_name, rei = rei_name, rate = val) %>%   mutate(sdi = stringr::str_replace(sdi, " SDI", "")) %>% 
#   mutate(sdi = factor(sdi, levels = c("Low", "Low-middle", "Middle", "High-middle", "High"), ordered = T)) 


dta %>% 
  filter(!is.na(age_group)) %>% 
  group_by(gender, period, age_group) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(cumulative_contribution = cumsum(contribution)) %>% 
  ggplot(aes(x = age_group, y = cumulative_contribution, colour = period, group= period)) + 
  facet_wrap(~gender) + 
  geom_line() + 
  scale_colour_discrete() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete() +
  labs(
    x = "Age group", y = "Cumulative Contribution in weeks/year", title = "Cumulative contribution by age group, and period",
    fill = "Period"
  ) + 
  geom_hline(yintercept =  0)

ggsave("figures/decomposition/cumulative_decomp_age.png", height = 20, width = 20, units = "cm", dpi = 300)


dta %>% 
  group_by(gender, period, cause) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(cumulative_contribution = cumsum(contribution)) %>% 
  ggplot(aes(x = cause, y = cumulative_contribution, colour = period, group= period, shape = period)) + 
  
  facet_wrap(~gender) + 
  geom_line() + geom_point() + 
  scale_colour_discrete() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete() +
  labs(
    x = "Cause", y = "Cumulative Contribution in weeks/year", title = "Cumulative contribution by Cause, and period",
    fill = "Period"
  ) + 
  geom_hline(yintercept =  0)


# now to do the same, but with causes arranged by contribution in period 1

tmp_male <- dta %>% filter(period == "2000-02 to 2012-14") %>%
  filter(gender == "males") %>% 
  group_by(cause) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(rnk = rank(contribution)) %>% 
  arrange(rnk) %>% 
  pull(cause)

dta %>% 
  filter(gender == "males") %>% 
  mutate(cause = factor(cause, levels = tmp_male, ordered = TRUE)) %>% 
  group_by(period, cause) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(cumulative_contribution = cumsum(contribution)) %>% 
  ggplot(aes(x = cause, y = cumulative_contribution, colour = period, group= period, shape = period)) + 
  geom_line() + geom_point() + 
  scale_colour_discrete() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete() +
  labs(
    x = "Cause", y = "Cumulative Contribution in weeks/year", title = "Cumulative contribution by Cause, and period. Males",
    subtitle = "Ordered by size/direction of contribution",
    fill = "Period"
  ) + 
  geom_hline(yintercept =  0)
  
ggsave("figures/decomposition/cumulative_cause_ordered_males.png", height = 20, width = 20, units = "cm", dpi = 300)



tmp_female <- dta %>% filter(period == "2000-02 to 2012-14") %>%
  filter(gender == "females") %>% 
  group_by(cause) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(rnk = rank(contribution)) %>% 
  arrange(rnk) %>% 
  pull(cause)

dta %>% 
  filter(gender == "females") %>% 
  mutate(cause = factor(cause, levels = tmp_female, ordered = TRUE)) %>% 
  group_by(period, cause) %>% 
  summarise(contribution = sum(contribution)) %>% 
  mutate(cumulative_contribution = cumsum(contribution)) %>% 
  ggplot(aes(x = cause, y = cumulative_contribution, colour = period, group= period, shape = period)) + 
  geom_line() + geom_point() + 
  scale_colour_discrete() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_discrete() +
  labs(
    x = "Cause", y = "Cumulative Contribution in weeks/year", title = "Cumulative contribution by Cause, and period. Females",
    subtitle = "Ordered by size/direction of contribution",
    fill = "Period"
  ) + 
  geom_hline(yintercept =  0)

ggsave("figures/decomposition/cumulative_cause_ordered_females.png", height = 20, width = 20, units = "cm", dpi = 300)

# 
# by_factor_l2 %>% 
#   spread(sex, rate) %>% 
#   mutate(diff_abs = Male - Female) %>% 
#   group_by(year, sdi) %>%
#   mutate(diff_cumulative = cumsum(diff_abs)) %>% 
#   filter(year %in% c(1990, 2010)) %>% 
#   mutate(start_pos = diff_cumulative - diff_abs) %>% 
#   mutate(is_increasing = diff_cumulative > start_pos) %>% 
#   group_by(year, sdi) %>% 
#   mutate(max_cumulative = diff_cumulative[length(diff_cumulative)]) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = start_pos, xend = diff_cumulative, y = rei, yend = rei, colour = is_increasing)) + 
#   geom_segment( arrow = arrow(length = unit(0.05, "npc"))) + 
#   facet_grid(sdi ~ year) +
#   geom_vline(xintercept = 0) + 
#   geom_vline(aes(xintercept = max_cumulative), linetype = "dashed") + 
#   guides(colour = FALSE) + 
#   labs(x = "Gender difference in rates", y = "Risk factors (level 2)", title = "Contribution of risk factors to gender differences in all-cause mortality rates", subtitle = "Age-standardised rates per 100 000", caption = "Source: GBD")
# ggsave("figures/contribution_of_level2_riskfactors_death_rate.png", height = 30, width = 30, units = "cm", dpi = 300)

