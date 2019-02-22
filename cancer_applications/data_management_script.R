rm(list = ls())

pacman::p_load(
  tidyverse
)


data <- read_csv("cancer_applications/raw_data/opendata_mort9217_scotland.csv")

tmp <- data %>% 
  select(Country, CancerSite, Sex, Year, contains("MortalityRate")) %>% 
  gather(contains("MortalityRate"), key = "age", value = "rate") 
  
names(tmp) <- tolower(names(tmp))

tmp

tmp <- tmp %>% 
  mutate(age = str_remove_all(age, "MortalityRateAge")) 

xtabs(~age + sex, data = tmp)
unique(tmp$age)

data_tidied <- tmp %>% 
  mutate(
    age = factor(
      age, 
      levels = c(
          "Under5",    
          "5To9",
          "10To14",    
          "15To19",    
          "20To24",    
          "25To29",
          "30To34",    
          "35To39",    
          "40To44",    
          "45To49",    
          "50To54",   
        "55To59",    
        "60To64",    
        "65To69",    
        "70To74",    
        "75To79",    
        "80To84",    
        "85To89",    
        "90AndOver"
      ),
      ordered = TRUE
    )
  )


data_tidied %>%
  select(-country) %>% 
#  group_by(cancersite) %>% tally() %>% View()
  filter(cancersite == 'All cancer types') %>% 
  filter(sex == 'All') %>% 
  ggplot(aes(x = year, y = age, fill = rate)) + 
  geom_tile() +
  scale_fill_viridis_c()


data_tidied %>%
  select(-country) %>% 
  #  group_by(cancersite) %>% tally() %>% View()
  filter(cancersite == 'All cancer types') %>% 
  filter(sex != 'All') %>% 
  ggplot(aes(x = year, y = age, fill = rate)) + 
  geom_tile() + 
  facet_wrap(~sex)




