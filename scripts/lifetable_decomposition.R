rm(list = ls())

require(tidyverse)
require(readxl)
require(plotly)

# Data 

## load data from Maria

male_excel_sheets <- excel_sheets(path = "scripts/decomposition/decomposition input males.xlsx")
female_excel_sheets <- excel_sheets(path = "scripts/decomposition/decomposition input females.xlsx")

cause_deaths_tidied <- tibble(sex = "male", year = male_excel_sheets) %>% 
  mutate(data = map(
    male_excel_sheets, 
    ~read_excel("scripts/decomposition/decomposition input males.xlsx",
                sheet = .x
      )
    )
  ) %>% 
  bind_rows(
    tibble(sex = "female", year = female_excel_sheets) %>% 
      mutate(data = map(
        female_excel_sheets,
        ~read_excel("scripts/decomposition/decomposition input females.xlsx",
                    sheet = .x)
        )
      )
  ) %>% 
  unnest() %>% 
  select(-sex1) %>% 
  gather(-c(sex:period), key = "cause", value = "deaths_from_cause")

deaths_total <- cause_deaths_tidied %>% 
  select(sex, period, agegroup, population, deaths) %>% 
  distinct()

deaths_cause <- cause_deaths_tidied %>% 
  select(sex, period, agegroup, cause, deaths_from_cause)

# dta_hmd <- read_csv(file = "hmd_explorer/data/hmd_data.csv")
# dta_scot <- dta %>% 
#   filter(code == "GBR_SCO", gender != "Total") 


# Params

radix <- 100000


# Now to replicate the code below using the gathered/tidied data above
lifetable <- deaths_total %>% 
  mutate(mx = deaths / population) %>% 
  group_by(sex, period) %>% 
  arrange(agegroup) %>% 
  mutate( ### QUESTION: Can ax be expressed directly, and as an additional column? 
    # This would allow the formulae to be generalised further to different age categories 
    qx = case_when(
      agegroup == min(agegroup)                  ~ mx,
      rank(agegroup, ties.method = "first") == 2 ~ (8 * mx) / (2 + 4 * mx),
      agegroup == max(agegroup)                  ~ 1, 
      TRUE                                       ~ 10 * mx / (2 +  5 * mx)
    )
  ) %>% 
  mutate(
    px = 1 - qx
  ) %>% 
  mutate(
    lx = radix
  ) %>% 
  mutate(
    lx = case_when(
      agegroup == min(agegroup)      ~ lx,
      TRUE                           ~ lag(px) * lag(lx)
    )
  ) %>% 
  mutate(
    dx = lx - lead(lx) 
  ) %>% 
  # ggplot(aes(x = agegroup, y = lx, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .)
  # ggplot(aes(x = agegroup, y = mx, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .) + scale_y_log10()
  # ggplot(aes(x = agegroup, y = qx, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .) + scale_y_log10()  
  # ggplot(aes(x = agegroup, y = px, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .) 
  mutate(
    Lx = case_when( # Lx: Average person-years lived between ages x and x + 1
      agegroup == max(agegroup)                           ~ lx / mx, # 
      agegroup == min(agegroup)                           ~ (0.9 * lead(lx) +    0.1 * lx),
      rank(agegroup, ties.method = "first") == 2          ~ 2 *    (lx + lead(lx)),
      TRUE                                                ~ 2.5 *  (lx + lead(lx))
    )
  )  %>% 
  # ggplot(aes(x = agegroup, y = Lx, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .)
  mutate(
    Tx = Lx,
    Tx = case_when(
      agegroup == max(agegroup)    ~ Tx,
      TRUE                         ~ Lx + lead(Tx)
    )
  ) %>% 
  mutate(ex = Tx / lx)  %>% 
#  ggplot(aes(x = agegroup, y = Tx, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .)
  ggplot(aes(x = agegroup, y = ex, group = period, colour = period)) + geom_point() + geom_line() + facet_grid(sex ~ .)
 

  # TO CHECK : The figures above suggest that ex doesn't show conditional life expectancy in years, but in number of age categories
  # IS this correct? What are the age categories?
  
   
#   year1$Lx<-ifelse(year1$agegroup==20,year1$lx/year1$mx,
#                    ifelse(year1$agegroup==1,(0.9*lead(year1$lx,1))+(0.1*year1$lx),
#                           ifelse(year1$agegroup==2,(year1$lx+lead(year1$lx,1))*2,(year1$lx+lead(year1$lx,1))*2.5)))
# year1$Tx<-ifelse(year1$agegroup==20,year1$Lx,NA)
# for (i in 20:1) {
#   year1$Tx<-ifelse(year1$agegroup==20,year1$Tx,year1$Lx+lead(year1$Tx,1))}
#   



# The script below tries to replicate the above using the HMD  - of course it won't allow decomposition but 
# good practice to generalise - will use the HMD methods protocol and consider integrating with other cause-specific datasets 


# 
# 
# decomp_scot <- dta_scot %>% 
#   rename(year = Year, age = Age) %>% 
#   mutate(mx = num_deaths / exposure) %>% # mx: central force of mortality
#   group_by(gender, year) %>% 
#   arrange(age) %>% 
#   mutate(qx = case_when( # qx: probability of dying between this and next age group
#     age == min(age)                          ~  mx,
#     rank(age, ties.method = "first") == 2    ~  (8 * mx) / (2 + 4*mx), # is this appropriate when using finer-grained age groups?
#     age == max(age)                          ~  1,
#     TRUE                                     ~  10 * mx / (2 + (5*mx))
#     )
#   ) %>% 
#   mutate(px = 1 - qx) %>% # px: probability of surviving between this and next age group
#   mutate(
#     lx = 100000           # lx: cumulative survivorship by age x (out of 100000)
#   ) %>% 
#   mutate(
#     lx = case_when(
#       age == min(age)     ~ lx,
#       TRUE                ~ lag(px)*lag(lx)   # probability of surviving last time period, multiplied by number alive last time period
#     )
#   ) %>% 
#   mutate(
#     dx = case_when(
#       age == max(age)     ~ lx, # number of additional deaths last years
#       TRUE                ~ lx - lead(lx)
#     )
#   ) %>% 
#   mutate(
#     Lx = case_when( # Lx: Average person-years lived between ages x and x + 1 
#       
#       age == max(age)                           ~ lx / mx, # 
#       age == min(age)                           ~ (0.9 * lead(lx) +    0.1 * lx),
#       rank(age, ties.method = "first") == 2     ~ 2 *    (lx + lead(lx)),
#       TRUE                                      ~ 2.5 *  (lx + lead(lx))
#     )
#   ) %>% 
#   mutate(
#     Tx = case_when(
#       age == max(age) ~ Lx,
#       TRUE            ~ NA_real_
#     )
#   ) %>% 
#   arrange(desc(age)) %>% 
#   mutate(
#     Tx = case_when(
#       age == max(age)     ~ Tx,
#       TRUE                ~ Lx + lead(Tx)
#     )
#   ) %>% 
#   mutate(ex = Tx / lx)
# 


##calculate life tables for 1st time period
# year1<- arrange(year1,agegroup)
# year1$mx<-year1$deaths/year1$population
# year1$qx<-ifelse(year1$agegroup==1,year1$mx,
#                  ifelse(year1$agegroup==2,(2*4*year1$mx)/(2+(4*year1$mx)),
#                         ifelse(year1$agegroup==20,1,(2*5*year1$mx)/(2+(5*year1$mx)) )))
# year1$px<-1-year1$qx
# year1$lx<-NA
# year1$lx[1]=100000
# for (i in 1:20){
#   year1$lx<-ifelse(year1$agegroup==1,year1$lx, lag(year1$px, 1)*lag(year1$lx,1))}
# year1$dx<-ifelse(year1$agegroup==20,year1$lx,year1$lx-lead(year1$lx))
# year1$Lx<-ifelse(year1$agegroup==20,year1$lx/year1$mx,
#                  ifelse(year1$agegroup==1,(0.9*lead(year1$lx,1))+(0.1*year1$lx),
#                         ifelse(year1$agegroup==2,(year1$lx+lead(year1$lx,1))*2,(year1$lx+lead(year1$lx,1))*2.5)))
# year1$Tx<-ifelse(year1$agegroup==20,year1$Lx,NA)
# for (i in 20:1) {
#   year1$Tx<-ifelse(year1$agegroup==20,year1$Tx,year1$Lx+lead(year1$Tx,1))}
# year1$ex<-year1$Tx/year1$lx



##begin decomposition 1 (between year1 and year2)
decomp1=propyear1
decomp1$all_causes<-ifelse(decomp1$agegroup==20,(year1$lx/100000)*((year2$Tx/year2$lx)-(year1$Tx/year1$lx)),
                           (year1$lx/100000)*((year2$Lx/year2$lx)-(year1$Lx/year1$lx))+(lead(year2$Tx,1)/100000)*((year1$lx/year2$lx)-(lead(year1$lx,1)/lead(year2$lx,1))))
y<-ncol(propyear1)
for (i in c(7:y)) {
  decomp1[,(i)]<-(decomp1$all_causes*(((propyear2[,i]*year2$mx)-(propyear1[,i]*year1$mx))/(year2$mx-year1$mx)))}
decomp1[,c(1,3,4,5,6)]=NULL

y<-ncol(decomp1)
totals<-colSums(decomp1[,c(2:y)])
totals<-append(0,totals)        
decomp1<-rbind(decomp1,totals)
decomp1$agegroup[21]<-"all ages"

##change decomposition to weeks per annum rather than years
y<-ncol(decomp1)
decomp1[2:y]<-(decomp1[2:y]*52)/yrsfirstperiod
decomp1$period<-firstperiod

