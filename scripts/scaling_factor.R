######################################################################################################################### 
# Code for scaling factor sensitivity analysis
# For study "The Effect of Inter-City Travel Restrictions on Geographical Spread of COVID-19: Evidence from Wuhan, China"
# Written by Charlie Diamond
#########################################################################################################################

### Load packages ----
pacman::p_load(tidyverse,lubridate)

### Read in Baidu overall index mobilty leaving Wuhan 2019 & 2020 ----
baidu <- read_csv("data/scaling_factor_baidu.csv") %>% 
  mutate(Date = dmy(Date))

### Tian et al (2020) DOI: 10.1126/science.abb6105  ----
tian <- read_csv("data/tian2020.csv", col_names = FALSE) %>% 
  rename(Date = X1, tian_20 = X2) %>% 
  left_join(., baidu, by = c("Date"="Date")) %>% 
  select(-baidu_index19) %>% 
  mutate(trav_no = sum(tian_20)) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) %>% 
  pull(scale) %>% 
  first()
  
# Scaling factor = 40927


### Sanche et al (2020) DOI:10.1101/2020.02.07.20021154 ----
sanche <- baidu %>% 
  filter(Date >= as.Date("2020-01-10") & Date <= as.Date("2020-01-23")) %>% 
  mutate(trav_no = 5000000) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) %>% 
  pull(scale) %>% 
  first()

# Scaling factor = 46677


### News report (2020) http://news.cjn.cn/sywh/202001/t3539167.htm ----
news <- baidu %>% 
  filter(Date >= as.Date("2020-01-10") & Date <= as.Date("2020-01-20")) %>% 
  mutate(trav_no = 4098600) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) %>% 
  pull(scale) %>% 
  first()

# Scaling factor = 55839


### Cao et al (2020) DOI: 10.1101/2020.02.07.20021071
cao <- read_csv("data/cao2020.csv", col_names = FALSE) %>% 
  select(X1) %>% 
  rename(cao_20 = X1) %>% 
  bind_cols(baidu %>% 
              filter(Date >= as.Date("2020-01-16") & Date <= as.Date("2020-01-22"))) %>% 
  select(-baidu_index19) %>% 
  mutate(trav_no = sum(cao_20)) %>% 
  mutate(baidu_sum = sum(baidu_index20)) %>% 
  mutate(scale = trav_no/baidu_sum) %>% 
  pull(scale) %>% 
  first()
  
# Scaling factor = 120003


### Zhou (2020) DOI: 10.1101/2020.02.15.20023440 ----

# Scaling factor 138412.01

