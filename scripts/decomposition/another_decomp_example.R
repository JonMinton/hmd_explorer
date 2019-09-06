# Another decomposition example 

rm(list = ls())

require(tidyverse)
require(readxl)


# Functions

order_age <- function(x){
  out<-  factor(x, ordered = TRUE, 
                   levels = c(
                     "<1",     "1-4",   "5-9",   "10-14", "15-19", "20-24",
                     "25-29",  "30-34", "35-39", "40-44", "45-49", "50-54",
                     "55-59",  "60-64", "65-69", "70-74", "75-79", "80-84",
                     "85-89", ">=90"  
                   )
      )
  out
}

convert_Mx_to_Qx <- function(Mx, Age, x_int){
  out <- case_when(
    Age == "<1"     ~ Mx,
    Age == ">=90"   ~ 1,
    TRUE            ~ 2 * x_int * Mx / (2 + x_int * Mx)
  )
  out
}

calc_lx <- function(X, radix = 100000){
  px <- X$px
  lx <- vector(mode = "numeric", length = length(px))
  lx[1] <- radix
  for (i in 2:length(lx)){
    lx[i] <- lx[i-1] * px[i-1]
  }
  out <- X
  out$lx <- lx
  out
}

calc_Lx <- function(X){
  
  # For Lx, the formulae are:
  
  # age 0: 0.9 * l[x] + 0.1 * l[x+1]
  # age >=90 : l[x] / m[x]
  # otherwise: (l[x] + l[x+1] ) * x_int / 2
  
  age <- X$Age
  lx <- X$lx
  max_Mx <- X$Mx[length(X$Mx)]
  x_int <- X$x_int
  
  Lx <- vector(mode = "numeric", length = length(lx))
  
  Lx[1] <- 0.9 * lx[2] + 0.1 * lx[1]
  Lx[length(Lx)] <- lx[length(lx)] / max_Mx
  
  for (i in 2:(length(Lx)-1)){
    Lx[i] <- (lx[i] + lx[i+1]) * x_int[i] / 2
  }
  
  out <- X
  out$Lx <- Lx
  out
}

calc_Tx <- function(Lx){
  N <- length(Lx)
  Tx <- vector(mode = "numeric", length = N)
  
  
  Tx[N] <- Lx[N]
  
  for (i in (N-1):1){
    Tx[i] <- Tx[i+1]+Lx[i]
  }
  Tx
}


# Decomposition section


calc_Cx <- function(X1, X2, radix = 100000){
  # X1: data for group 1 (e.g. period 1)
  # X2: data for group 2 (e.g. period 2)
  stopifnot(dim(X1) == dim(X2))
  N <- dim(X1)[1]
  
  Cx <- vector(mode = "numeric", length = N)
  
  Cx[N] <- (X1$lx[N] / radix) * ((X1$Tx[N] / X1$lx[N]) - (X2$Tx[N] / X2$lx[N]))
  
  for (i in 1:(N-1)){
    Cx[i] <- (X2$lx[i] / radix) * (X1$Lx[i] / X1$lx[i] - X2$Lx[i] / X2$lx[i]) + 
      (X1$Tx[i+1] / radix) * ( X2$lx[i] / X1$lx[i] - X2$lx[i+1]/X1$lx[i+1])
  }
  Cx
}

# Now calc_Cxy

calc_Cxy <- function(lifetable, cause_deaths, Cx, period1 = "2009-2011", period2 = "2015-2017"){
  
  Mx_period1 <- lifetable %>% 
    ungroup() %>% 
    filter(Period == !!period1) %>% 
    select(Age, Mx)
  
  Mx_period2 <- lifetable %>% 
    ungroup() %>% 
    filter(Period == !!period2) %>% 
    select(Age, Mx)
  
  dxy_period1 <- cause_deaths %>% 
    filter(Period == !!period1) %>% 
    select(Age,  cause, num_deaths)
  
  dxy_period2 <- cause_deaths %>% 
    filter(Period == !!period2) %>% 
    select(Age,  cause, num_deaths)
  
  
  propxy_period1 <- dxy_period1 %>% 
    group_by(Age) %>% 
    mutate(propxy = num_deaths / sum(num_deaths)) %>% 
    ungroup()
  
  propxy_period2 <- dxy_period2 %>% 
    group_by(Age) %>% 
    mutate(propxy = num_deaths / sum(num_deaths)) %>% 
    ungroup()
  
  propxy_period1 <- propxy_period1 %>% 
    left_join(Mx_period1, by = "Age") %>% 
    rename(Mx1 = Mx, propxy1 = propxy) %>% 
    select(-num_deaths)
  
  propxy_period2 <- propxy_period2 %>% 
    left_join(Mx_period2, by = "Age") %>% 
    rename(Mx2 = Mx, propxy2 = propxy) %>% 
    select(-num_deaths)
  
  Cxy <- propxy_period1 %>% 
    left_join(propxy_period2, by = c("Age", "cause")) %>% 
    left_join(Cx, by = "Age") %>% 
    mutate(Cxy = Cx * (propxy1 * Mx1 - propxy2 * Mx2) / (Mx1 - Mx2)) %>% 
    select(Age, cause, Cxy)
  
  Cxy
}


# This loads in the data with population and cause-specific death count
flat_data <- read_excel("data/decomposition work book.xlsx", sheet = "flattened")

# The first task is to split the data into numerator (deaths) and denominator (population count)

pop_data <- flat_data %>% 
  select(Country, Gender, Period, Age, Population) %>% 
  mutate(Age = order_age(Age))


death_data_cause <- flat_data %>% 
  select(-Population) %>% 
  gather(`Neoplasms`:`Other causes`, key = "cause", value = "num_deaths") %>% 
  select(Country, Gender, Period, Age, cause, num_deaths) %>% 
  mutate(Age = order_age(Age))



# To get total number of deaths, group by everything other than cause and sum

death_data_total <- death_data_cause %>% 
  group_by(Country, Gender, Period, Age) %>% 
  summarise(total_num_deaths = sum(num_deaths)) 

# The columns in a lifetable are:

# n_D_x : observed deaths (in death_data_total)
# d_P_x : observed population (in pop_data)
# n_M_x : mortality rate (Dx / Px)
# n_q_x : probability of dying in interval [to be discussed]

# The probability of dying in the interval depends on 
# x_int : age interval
# It's 
# Mx : for the smallest age category
# 1  : for the largest age category
# 2 * x_int * Mx / (2 + x_int * Mx)   : for all other age categories

# So let's create a small table which allows x_int to be looked up given Age

age_interval_lookup <- tribble(
  ~Age,    ~x_int,
  "<1",    NA,
  "1-4",   4,
  "5-9",   5,
  "10-14", 5,
  "15-19", 5, 
  "20-24", 5,
  "25-29", 5,
  "30-34", 5,
  "35-39", 5,
  "40-44", 5,
  "45-49", 5,
  "50-54", 5,
  "55-59", 5,
  "60-64", 5,
  "65-69", 5,
  "70-74", 5,
  "75-79", 5,
  "80-84", 5,
  "85-89", 5,
  ">=90", NA  
)


# The required tables can now be joined as follows

incomplete_lifetable <- pop_data %>% 
  left_join(death_data_total) %>% 
  left_join(age_interval_lookup) %>% 
  rename(Dx = total_num_deaths, Px = Population) %>% 
  mutate(Age = order_age(Age)) %>%  # Adding back Age as an ordered factor
  mutate(Mx = Dx / Px) %>% 
  mutate(Qx = pmap_dbl(list(Mx, Age, x_int), convert_Mx_to_Qx)) %>% 
  mutate(px = 1 - Qx) 


complete_lifetable <- incomplete_lifetable %>% 
  group_by(Country, Gender, Period) %>% 
  nest() %>% 
  mutate(data = map(data, calc_lx)) %>% 
  unnest() %>%
  group_by(Country, Gender, Period) %>% 
  mutate(dx = ifelse(is.na(lead(lx)), lx, lx - lead(lx))) %>% 
  nest() %>% 
  mutate(data = map(data, calc_Lx)) %>% 
  unnest() %>% 
  group_by(Country, Gender, Period) %>% 
  mutate(Tx = calc_Tx(Lx)) %>% 
  mutate(ex = Tx / lx) 



# Decomposition

# I'm going to refer to
# Cx : contribution of difference between groups due to difference in age interval x
# Cxy : contribution of difference between groups due to difference in age interval x AND cause y

# To start with I'm going to calculate Cx only, as this is a prerequisite for calculating Cxy


Cx <- complete_lifetable %>% 
  group_by(Country, Gender, Period) %>% 
  nest() %>% 
  spread(Period, data) %>% 
  mutate(Cx = map2(`2009-2011`, `2015-2017`, calc_Cx)) %>% 
  pull(Cx) %>% pluck(1)

# now as a data_frame
Cx <- age_interval_lookup %>% 
  select(Age) %>% 
  mutate(Age = order_age(Age)) %>% 
  mutate(Cx = Cx)

Cxy <- calc_Cxy(lifetable=complete_lifetable, cause_deaths=death_data_cause, Cx=Cx, period1 = "2009-2011", period2 = "2015-2017")



# Cxy - step-by-step decomposition outwith a function

# Mx_period1 <- complete_lifetable %>% 
#   ungroup() %>% 
#   filter(Period == "2009-2011") %>% 
#   select(Age, Mx)
# 
# Mx_period2 <- complete_lifetable %>% 
#   ungroup() %>% 
#   filter(Period == "2015-2017") %>% 
#   select(Age, Mx)
# 
# dxy_period1 <- death_data_cause %>% 
#   filter(Period == "2009-2011") %>% 
#   select(Age,  cause, num_deaths)
# 
# dxy_period2 <- death_data_cause %>% 
#   filter(Period == "2015-2017") %>% 
#   select(Age,  cause, num_deaths)
# 
# 
# propxy_period1 <- dxy_period1 %>% 
#   group_by(Age) %>% 
#   mutate(propxy = num_deaths / sum(num_deaths)) %>% 
#   ungroup()
# 
# propxy_period2 <- dxy_period2 %>% 
#   group_by(Age) %>% 
#   mutate(propxy = num_deaths / sum(num_deaths)) %>% 
#   ungroup()
# 
# propxy_period1 <- propxy_period1 %>% 
#   left_join(Mx_period1, by = "Age") %>% 
#   rename(Mx1 = Mx, propxy1 = propxy) %>% 
#   select(-num_deaths)
# 
# propxy_period2 <- propxy_period2 %>% 
#   left_join(Mx_period2, by = "Age") %>% 
#   rename(Mx2 = Mx, propxy2 = propxy) %>% 
#   select(-num_deaths)
# 
# Cxy <- propxy_period1 %>% 
#   left_join(propxy_period2, by = c("Age", "cause")) %>% 
#   left_join(Cx, by = "Age") %>% 
#   mutate(Cxy = Cx * (propxy1 * Mx1 - propxy2 * Mx2) / (Mx1 - Mx2)) %>% 
#   select(Age, cause, Cxy)



# 
# ggplot(Cxy, aes(x = Age, y = cause, fill = Cxy)) + 
#   geom_tile()
# 
# ggplot(Cxy2, aes(x = Age, y = cause, fill = Cxy)) + 
#   geom_tile()
