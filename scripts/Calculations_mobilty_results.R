######################################################################################################################### 
# Code for Results - Effect of the cordon sanitaire on mobility.
# For study "The Effect of Inter-City Travel Restrictions on Geographical Spread of COVID-19: Evidence from Wuhan, China"
# Written by Charlie Diamond
#########################################################################################################################

### Load packages ----
pacman::p_load(tidyverse,lubridate, tsibble, imputeTS)

### Read in mobility data ----
baidu_comp <- read_csv("data/baidu_scenarios.csv")

### Percent change between S1 and S2 in 23 days before restrictions (mean daily % increase from 2019 - 2020) ----
baidu_comp %>% 
  filter(restrict_day >= -22 & restrict_day <= 0) %>% 
  mutate(day_percent = ((baidu_index20- baidu_index19)/baidu_index19)*100 ) %>% 
  select(day_percent) %>% 
  summarise(mean = mean(day_percent, na.rm = TRUE),
            sd = sd(day_percent, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Total number of travellers leaving Wuhan in 3 days before restrictions, and % increase from 2019 ----
baidu_comp %>% 
  filter(restrict_day >= -2 & restrict_day <= 0) %>% 
  mutate(b_sum20 = sum(baidu_index20), b_sum19 = sum(baidu_index19)) %>% 
  mutate(trav20 = b_sum20*50000) %>% 
  mutate(trav19 = b_sum19*50000) %>% 
  mutate(diff = (b_sum20-b_sum19)*50000) %>% 
  pull(trav20) %>% 
  first()

baidu_comp %>% 
  filter(restrict_day >= -2 & restrict_day <= 0) %>% 
  mutate(day_percent = ((baidu_index20- baidu_index19)/baidu_index19)*100 ) %>% 
  select(day_percent) %>% 
  summarise(mean = mean(day_percent, na.rm = TRUE),
            sd = sd(day_percent, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Percentage change in mean no. daily travellers from 23 days pre restrictions to 23 days post restrictions (S1) ----
pre <- baidu_comp %>% 
  filter(restrict_day >= -22 & restrict_day <= 0) %>% 
  mutate(b_mean20 = mean(baidu_index20)) %>% 
  select(b_mean20) %>% 
  pull() %>% 
  first()

post <- baidu_comp %>% 
  filter(restrict_day >= 1 & restrict_day <= 23) %>% 
  mutate(b_mean20 = mean(baidu_index20)) %>% 
  select(b_mean20) %>% 
  pull() %>% 
  first()

((pre-post)/pre)*100

### Percentage change in mean travel volume from 23 days pre restrictions to 23 days post restrictions (S2) ----
pre2 <- baidu_comp %>% 
  filter(restrict_day >= -22 & restrict_day <= 0) %>% 
  mutate(b_mean19 = mean(baidu_index19)) %>% 
  select(b_mean19) %>% 
  pull() %>% 
  first()

post2 <- baidu_comp %>% 
  filter(restrict_day >= 1 & restrict_day <= 23) %>% 
  mutate(b_mean19 = mean(baidu_index19)) %>% 
  select(b_mean19) %>% 
  pull() %>% 
  first()

((pre2-post2)/pre2)*100

### Mean daily travellers pre restrictions (23 days) S1 ----
baidu_comp %>% 
  filter(restrict_day >= -22 & restrict_day <= 0) %>% 
  mutate(trav20 = baidu_index20*50000) %>% 
  select(trav20) %>% 
  summarise(mean = mean(trav20, na.rm = TRUE),
            sd = sd(trav20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Mean daily travellers post restrictions (23 days) S1 ----
baidu_comp %>% 
  filter(restrict_day >= 1 & restrict_day <= 23) %>% 
  mutate(trav20 = baidu_index20*50000) %>% 
  select(trav20) %>% 
  summarise(mean = mean(trav20, na.rm = TRUE),
            sd = sd(trav20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Mean daily travellers pre restrictions (23 days) S2 ----
baidu_comp %>% 
  filter(restrict_day >= -22 & restrict_day <= 0) %>% 
  mutate(trav19 = baidu_index19*50000) %>% 
  select(trav19) %>% 
  summarise(mean = mean(trav19, na.rm = TRUE),
            sd = sd(trav19, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Mean daily travellers post restrictions (23 days) S2 ----
baidu_comp %>% 
  filter(restrict_day >= 1 & restrict_day <= 23) %>% 
  mutate(trav19 = baidu_index19*50000) %>% 
  select(trav19) %>% 
  summarise(mean = mean(trav19, na.rm = TRUE),
            sd = sd(trav19, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Number of travellers leaving in the 5 days post restrictions ---
baidu_comp %>% 
  filter(restrict_day >= 1 & restrict_day <= 5) %>% 
  mutate(b_sum20 = sum(baidu_index20)) %>% 
  mutate(trav20 = b_sum20*50000) %>% 
  pull(trav20) %>% 
  first()

### Daily excess travellers due to Chunyun (compare S2 and S4) ---
baidu_comp %>%
  filter(restrict_day >= -13 & restrict_day <= 26) %>%
  mutate(change = (baidu_index19 - baidu_index19non)*50000) %>%
  select(change) %>% 
  summarise(mean = mean(change, na.rm = TRUE),
            sd = sd(change, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)


##################################################
# Daily average travellers from Wuhan to 4 cities
##################################################

### Read in Baidu overall index mobilty leaving Wuhan 2019 & 2020 ----
baidu <- read_csv("data/scaling_baidu_mobility_index.csv") %>% 
  mutate(Date = dmy(Date))

### Read in prf connectivity matrix export (sum of rows = total index) ----
connect_all <- readRDS("data/china_prf_connectivity_0101_0301.rds")

### Dates where matrix is available ----
dates <- connect_all$date

# Function to estimate total travller number from Wuhan to each prefecture ----
trav_from_wuhan <- function(prf_code, scale){
  
  # Observed flows from Wuhan for 2020 
  index_flows <- list()
  
  for (i in 1:length(dates)) {index_flows[[i]]<- 
    
    connect_all$connect[[i]] %>% 
    as_tibble() %>% 
    filter(from==420100) %>% # Wuhan
    t() %>% 
    as.data.frame() %>% 
    mutate(code= rownames(.)) %>% 
    as.tibble() %>% 
    mutate(index_flow20 = as.numeric(as.character(V1))) %>% 
    select(-V1) %>% 
    filter(code != "from") %>%
    mutate(index_20 = baidu %>% 
             filter(Date == as.Date(dates[i])) %>% 
             select(baidu_index20) %>% 
             pull()) %>% 
    mutate(index_19 = baidu %>% 
             filter(Date == as.Date(dates[i])) %>% 
             select(baidu_index19) %>% 
             pull()) %>% 
    mutate(flow_percent_20 = index_flow20/index_20) %>%
    mutate(index_flow19 = NA, flow_percent_19 = NA) %>%
    mutate(date = dates[i])
  
  }
  
  # Bind rows of flow data 
  index_flows_data <- bind_rows(index_flows)

  # Join baidu total with observed pairwise flows
  new_baidu <- left_join(baidu_comp, index_flows_data %>% 
                           filter(code == prf_code),
                         by= c("Date"="date")) %>% 
    mutate(index_flow19non = NA, flow_percent_19non = NA, flow_percent_20non = NA, index_flow20non = NA) %>% 
    select(code, date= Date, baidu_index19, baidu_index20, baidu_index19non, baidu_index20non, flow_percent_19, flow_percent_20, flow_percent_19non,flow_percent_20non, index_flow19, index_flow20, index_flow19non, index_flow20non)
  
  # Interpolate values for missing values in observed period and calulate pairwise flows
  mid_baidu <- new_baidu %>% 
    filter(date >= as.Date("2020-01-01")) %>% 
    mutate(flow_percent_20 = na_interpolation(flow_percent_20)) %>% 
    mutate(index_flow20 = baidu_index20*flow_percent_20) %>% 
    mutate(flow_percent_19 =  as.numeric(if_else(date <= as.Date("2020-01-19"), flow_percent_20, 
                                                 sum(index_flow20, na.rm= TRUE)/sum(baidu_index20, na.rm = TRUE)))) %>% 
    mutate(index_flow19 = baidu_index19*flow_percent_19) %>% 
    mutate(flow_percent_19non = sum(index_flow20, na.rm= TRUE)/sum(baidu_index20, na.rm = TRUE)) %>% 
    mutate(index_flow19non = baidu_index19non*flow_percent_19non) %>% 
    mutate(flow_percent_20non = if_else(date <= as.Date("2020-01-23"), flow_percent_19non,flow_percent_20)) %>% 
    mutate(index_flow20non = baidu_index20non*flow_percent_20non)
  
  # Estimates of pairwise flows out of observed range, based on average 2020 flows for whole of observed period
  pre_baidu <- new_baidu %>% 
    filter(date < as.Date(min(mid_baidu$date))) %>% 
    mutate(flow_percent_19 = sum(mid_baidu$index_flow20, na.rm= TRUE)/sum(mid_baidu$baidu_index20, na.rm = TRUE)) %>% 
    mutate(flow_percent_20 = sum(mid_baidu$index_flow20, na.rm= TRUE)/sum(mid_baidu$baidu_index20, na.rm = TRUE)) %>% 
    mutate(flow_percent_19non = sum(mid_baidu$index_flow20, na.rm= TRUE)/sum(mid_baidu$baidu_index20, na.rm = TRUE)) %>% 
    mutate(flow_percent_20non = sum(mid_baidu$index_flow20, na.rm= TRUE)/sum(mid_baidu$baidu_index20, na.rm = TRUE)) %>% 
    mutate(index_flow19 = baidu_index19 * flow_percent_19) %>% 
    mutate(index_flow20 = baidu_index20 * flow_percent_20) %>% 
    mutate(index_flow19non = baidu_index19non * flow_percent_19non) %>% 
    mutate(index_flow20non = baidu_index20non*flow_percent_20non) %>%
    mutate(code = prf_code) %>% 
    mutate(code = as.character(code))
  
  # Combinded observed and estimated pairwise flows. For 2020, after 12th Feb percentage = mean perecntage for 28th jan - 12th feb
  prf_migration <- bind_rows(mid_baidu, pre_baidu) %>% 
    arrange(date) %>% 
    mutate(code = prf_code)
  
  # Number of trav estimates
  no_trav <- prf_migration %>% 
    mutate(trav_19 = as.integer(index_flow19 * scale)) %>% 
    mutate(trav_20 = as.integer(index_flow20 * scale)) %>% 
    mutate(trav_19non = as.integer(index_flow19non * scale)) %>% 
    mutate(trav_20non = as.integer(index_flow20non * scale)) %>% 
    mutate(day =  1:nrow(.)) %>% 
    select(date, day, code , trav_19, trav_20, trav_19non, trav_20non)
  
  return(no_trav)
  
}

top_codes <- read_csv("data/codes_20.csv") %>% 
  filter(CNTY_CODE == 110100 |CNTY_CODE == 500100 |CNTY_CODE == 330100 |CNTY_CODE == 440300)

top_n <- top_codes[1] %>% pull()

top_list <- list()

for (i in 1:length(top_n)) { top_list[[i]] <- 
  
  trav_from_wuhan(top_n[i], 50000) %>% 
  select(code, date, trav_20)
}

### Beijing ----
bind_rows(top_list) %>% 
  filter(code == 110100) %>% 
  select(trav_20) %>% 
  summarise(mean = mean(trav_20, na.rm = TRUE),
            sd = sd(trav_20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Chongqing ----
bind_rows(top_list) %>% 
  filter(code == 500100) %>% 
  select(trav_20) %>% 
  summarise(mean = mean(trav_20, na.rm = TRUE),
            sd = sd(trav_20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Hangzhou ----
bind_rows(top_list) %>% 
  filter(code == 330100) %>% 
  select(trav_20) %>% 
  summarise(mean = mean(trav_20, na.rm = TRUE),
            sd = sd(trav_20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

### Shenzhen ----
bind_rows(top_list) %>% 
  filter(code == 440300) %>% 
  select(trav_20) %>% 
  summarise(mean = mean(trav_20, na.rm = TRUE),
            sd = sd(trav_20, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
