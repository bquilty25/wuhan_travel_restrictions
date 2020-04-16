# Scaling factor sensitivity analysis

pacman::p_load(tidyverse,lubridate)

# Read in Baidu overall index mobilty leaving Wuhan 2019 & 2020
baidu <- read_csv("data/scaling_factor_baidu.csv") %>% 
  mutate(Date = dmy(Date))

###############

# Tian (2020) 
tian <- read_csv("data/tian2020.csv", col_names = FALSE) %>% 
  rename(Date = X1, tian_20 = X2) %>% 
  left_join(., baidu, by = c("Date"="Date")) %>% 
  select(-baidu_index19) %>% 
  mutate(trav_no = sum(tian_20)) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) 
  
# Scaling factor = 40927

###############

# Sanche (2020)
sanche <- baidu %>% 
  filter(Date >= as.Date("2020-01-10") & Date <= as.Date("2020-01-23")) %>% 
  mutate(trav_no = 5000000) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) 

# Scaling factor = 46677

###############

# News report (2020)
news <- baidu %>% 
  filter(Date >= as.Date("2020-01-10") & Date <= as.Date("2020-01-20")) %>% 
  mutate(trav_no = 4098600) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum)

# Scaling factor = 55839

###############

# Cao (2020) 
cao <- read_csv("data/cao2020.csv", col_names = FALSE) %>% 
  select(X1) %>% 
  rename(cao_20 = X1) %>% 
  bind_cols(baidu %>% 
              filter(Date >= as.Date("2020-01-16") & Date <= as.Date("2020-01-22"))) %>% 
  select(-baidu_index19) %>% 
  mutate(trav_no = sum(cao_20)) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum)
  
# Scaling factor = 120003

###############

# Zhou (2020)

# Scaling factor 138412.01

###############
