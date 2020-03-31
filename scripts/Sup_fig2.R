# Script to make supplementary figure 2, pairwise travel flux from Wuhan to largest 20 cities (by population)

pacman::p_load(tidyverse,lubridate,sf,RColorBrewer,imputeTS, tsibble, cowplot, ggpubr)

# Read in Baidu overall index mobilty leaving Wuhan 2019 & 2020
baidu <- read_csv("data/scaling_baidu_mobility_index.csv") %>% 
  mutate(Date = dmy(Date))

# Read in prf connectivity matrix export (sum of rows = total index)
connect_all <- readRDS("data/china_prf_connectivity_0101_0301.rds")

# Dates where matrix is available
dates <- connect_all$date

# Read in shapefile
shp <- readRDS("data/china_shp.rds") %>% 
  st_as_sf()

# Just names 
names <- shp %>% 
  as_tibble() %>% 
  filter(lvl == "prf") %>% 
  left_join(.,  shp %>% 
              as_tibble() %>% 
              filter(lvl == "prv") , by= c("prv"="prv")) %>% 
  as_tibble() %>% 
  select(CNTY_CODE = CNTY_CODE.x, PYNAME = PYNAME.x, prv= PYNAME.y)

# Get codes function
getcodes <- function(){
  return(names) %>% 
    view()
}

# Function to look at total travller number from Wuhan 
trav_from_wuhan <- function(prf_code, scale){
  
  
  # Observed flows from Wuhan for 2020 
  index_flows <- list()
  
  for (i in 1:length(dates)) {index_flows[[i]]<- 
    
    connect_all$connect[[i]] %>% 
    as_tibble() %>% 
    filter(from==420100) %>% 
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
  
  dayweek19 <- baidu %>% 
    filter(Date >= as.Date("2020-01-06") & Date <= as.Date("2020-01-10") | Date >= as.Date("2020-02-08") ) %>% 
    mutate(day = wday(Date)) %>% 
    group_by(day) %>% 
    summarise(mean19= mean(baidu_index19))
  
  # mean of non-chunyun post-shutdown index 2020 - weekday                  
  dayweek20 <- baidu %>% 
    filter(Date >= as.Date("2020-02-08")) %>% 
    mutate(day = wday(Date)) %>% 
    group_by(day) %>% 
    summarise(mean20= mean(baidu_index20, na.rm = TRUE))
  
  # Full total baidu estimates for 4 all scenarios
  baidu_comp <- baidu %>% 
    tidyr::complete(Date= seq.Date(min(as.Date("2019-11-23")), max(baidu$Date), by= "day")) %>% 
    mutate(day = wday(Date)) %>% 
    left_join(., dayweek19, by = c("day"="day")) %>% 
    left_join(., dayweek20, by = c("day"="day")) %>% 
    mutate(baidu_index19 = if_else(Date >= as.Date("2019-11-23") & Date <= as.Date("2019-12-31"),mean19, baidu_index19)) %>% 
    mutate(baidu_index20 = if_else(Date >= as.Date("2019-11-23") & Date <= as.Date("2019-12-31"), mean19, baidu_index20)) %>% 
    mutate(baidu_index20 = if_else(Date >= as.Date("2020-03-03"), mean20, baidu_index20)) %>% 
    mutate(baidu_index19non = if_else(Date >= as.Date("2020-01-06") & Date <= as.Date("2020-01-10") | Date >= as.Date("2020-02-08") ,baidu_index19, mean19)) %>% 
    mutate(baidu_index20non = if_else(Date > as.Date("2020-01-23"), baidu_index20, baidu_index19non))
  
  # Join baidu total with observed flows
  new_baidu <- left_join(baidu_comp, index_flows_data %>% 
                           filter(code == prf_code),
                         by= c("Date"="date")) %>% 
    mutate(index_flow19non = NA, flow_percent_19non = NA, flow_percent_20non = NA, index_flow20non = NA) %>% 
    select(code, date= Date, baidu_index19, baidu_index20, baidu_index19non, baidu_index20non, flow_percent_19, flow_percent_20, flow_percent_19non,flow_percent_20non, index_flow19, index_flow20, index_flow19non, index_flow20non)
  
  # Interpolate values for missing values in observed period and calulate flows
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
  
  # Estimates of flows out of observed range, based on average 2020 flows for whole of observed period
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
  
  # Combinded observed and estimated - for 2020 after 12th Feb percentage = mean perecntage for 28th jan - 12th feb
  prf_migration <- bind_rows(mid_baidu, pre_baidu) %>% 
    arrange(date) %>% 
    mutate(code = prf_code)
  
  # Number of trav estimates (Zhou)
  no_trav <- prf_migration %>% 
    mutate(trav_19 = as.integer(index_flow19 * scale)) %>% 
    mutate(trav_20 = as.integer(index_flow20 * scale)) %>% 
    mutate(trav_19non = as.integer(index_flow19non * scale)) %>% 
    mutate(trav_20non = as.integer(index_flow20non * scale)) %>% 
    mutate(day =  1:nrow(.)) %>% 
    select(date, day, code , trav_19, trav_20, trav_19non, trav_20non)
  
  return(prf_migration)
  
}


#Top 20 population plot
top_codes <- read_csv("data/codes_20.csv")

top_n <- top_codes[1] %>% pull()

top_list <- list()

for (i in 1:length(top_n)) { top_list[[i]] <- 
  
  trav_from_wuhan(top_n[i], 50000) %>% 
  select(code, date, index_flow19, index_flow20,index_flow19non, index_flow20non) %>% 
  mutate(restrict_day = rep(c(-61:0, 1:52), times= 1)) %>% 
  complete(restrict_day = seq(min(-61), max(52), by= 0.1)) %>% 
  mutate(code = na_interpolation(code),
         index_flow19 = na_interpolation(index_flow19),
         index_flow20 = na_interpolation(index_flow20),
         index_flow19non = na_interpolation(index_flow19non),
         index_flow20non = na_interpolation(index_flow20non))

}

#S1
S1 <- bind_rows(top_list) %>% 
  left_join(., top_codes, by=c("code"= "CNTY_CODE")) %>% 
  mutate(name = factor(name, levels = rev(c("Shanghai", "Beijing", "Chongqing", "Tianjin", "Guangzhou", "Shenzhen", "Chengdu", "Nanjing",
                                            "Xi'an", "Hangzhou","Dongguan", "Foshan", "Shenyang","Harbin", "Qingdao", "Dalian","Jinan", "Zhengzhou", 
                                            "Changsha", "Kunming")))) %>% ggplot(aes(x=restrict_day, y= name)) +
  geom_tile(aes(x=restrict_day, y= name, fill= index_flow20)) +
  coord_cartesian(xlim = c(-28,28)) +
  scale_fill_viridis_c(name="Baidu outflow index   ",option="inferno", begin = 0.05, end = 0.9,breaks=c(0,0.04,0.08,0.12),limits= c(0,0.1352029),
                       guide = guide_coloursteps(even.steps = F, show.limits = T,barwidth=unit(7,"cm"))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.5, yend = 20.5, colour = "Travel restrictions and physical distancing enacted"), linetype = "dashed") + 
  geom_segment(aes(x = 2, xend = 2, y = 0.5, yend = 20.5, color = "Lunar New Year"), linetype = "dotted")+
  scale_color_manual(name="", values=c("grey70","red"))+
  ggtitle("Scenario 1: Chunyun & travel restrictions (Observed 2020)") +
  ylab("")+
  xlab("Days Since Travel Restrictions") +
  labs(color = "Year") +
  guides(fill= guide_colorbar(order = 1)) +
  theme(axis.text.y = element_text(hjust = 0, size = 8),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank(),
        title = element_text(size = 9)
  )

#S2
S2 <- bind_rows(top_list) %>% 
  left_join(., top_codes, by=c("code"= "CNTY_CODE")) %>% 
  mutate(name = factor(name, levels = rev(c("Shanghai", "Beijing", "Chongqing", "Tianjin", "Guangzhou", "Shenzhen", "Chengdu", "Nanjing",
                                            "Xi'an", "Hangzhou","Dongguan", "Foshan", "Shenyang","Harbin", "Qingdao", "Dalian","Jinan", "Zhengzhou", 
                                            "Changsha", "Kunming")))) %>% ggplot(aes(x=restrict_day, y= name)) +
  geom_tile(aes(x=restrict_day, y= name, fill= index_flow19)) +
  coord_cartesian(xlim = c(-28,28)) +
  scale_fill_viridis_c(name="Baidu outflow index   ",option="inferno", begin = 0.05, end = 0.9,breaks=c(0,0.04,0.08,0.12),limits= c(0,0.1352029),
                       guide = guide_coloursteps(even.steps = F, show.limits = T,barwidth=unit(7,"cm"))) +
  geom_segment(aes(x = 2, xend = 2, y = 0.5, yend = 20.5, color = "Lunar New Year"), linetype = "dotted")+
  scale_color_manual(name="", values=c("grey70","red"))+
  ggtitle("Scenario 2: Chunyun & no travel restrictions (Observed 2019)") +
  ylab("")+
  xlab("Days Since Travel Restrictions") +
  labs(color = "Year") +
  guides(fill= guide_colorbar(order = 1)) +
  theme(axis.text.y = element_text(hjust = 0, size = 8),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank(),
        title = element_text(size = 9)
  )

#S3
S3 <- bind_rows(top_list) %>% 
  left_join(., top_codes, by=c("code"= "CNTY_CODE")) %>% 
  mutate(name = factor(name, levels = rev(c("Shanghai", "Beijing", "Chongqing", "Tianjin", "Guangzhou", "Shenzhen", "Chengdu", "Nanjing",
                                            "Xi'an", "Hangzhou","Dongguan", "Foshan", "Shenyang","Harbin", "Qingdao", "Dalian","Jinan", "Zhengzhou", 
                                            "Changsha", "Kunming")))) %>% ggplot(aes(x=restrict_day, y= name)) +
  geom_tile(aes(x=restrict_day, y= name, fill= index_flow20non)) +
  coord_cartesian(xlim = c(-28,28)) +
  scale_fill_viridis_c(name="Baidu outflow index   ",option="inferno", begin = 0.05, end = 0.9,breaks=c(0,0.04,0.08,0.12),limits= c(0,0.1352029),
                       guide = guide_coloursteps(even.steps = F, show.limits = T,barwidth=unit(7,"cm"))) +
  geom_segment(aes(x = 0, xend = 0, y = 0.5, yend = 20.5, colour = "Travel restrictions and physical distancing enacted"), linetype = "dashed") + 
  geom_segment(aes(x = 2, xend = 2, y = 0.5, yend = 20.5, color = "Lunar New Year"), linetype = "dotted")+
  scale_color_manual(name="", values=c("grey70","red"))+
  ggtitle("Scenario 3: No Chunyun & travel restrictions (Hypothetical)") +
  ylab("")+
  xlab("Days Since Travel Restrictions") +
  labs(color = "Year") +
  guides(fill= guide_colorbar(order = 1)) +
  theme(axis.text.y = element_text(hjust = 0, size = 8),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank(),
        title = element_text(size = 9)
  )


#S4
S4 <- bind_rows(top_list) %>% 
  left_join(., top_codes, by=c("code"= "CNTY_CODE")) %>% 
  mutate(name = factor(name, levels = rev(c("Shanghai", "Beijing", "Chongqing", "Tianjin", "Guangzhou", "Shenzhen", "Chengdu", "Nanjing",
                                            "Xi'an", "Hangzhou","Dongguan", "Foshan", "Shenyang","Harbin", "Qingdao", "Dalian","Jinan", "Zhengzhou", 
                                            "Changsha", "Kunming")))) %>% ggplot(aes(x=restrict_day, y= name)) +
  geom_tile(aes(x=restrict_day, y= name, fill= index_flow19non)) +
  coord_cartesian(xlim = c(-28,28)) +
  scale_fill_viridis_c(name="Baidu outflow index   ",option="inferno", begin = 0.05, end = 0.9,breaks=c(0,0.04,0.08,0.12),limits= c(0,0.1352029),
                       guide = guide_coloursteps(even.steps = F, show.limits = T,barwidth=unit(7,"cm"))) +
  geom_segment(aes(x = 2, xend = 2, y = 0.5, yend = 20.5, color = "Lunar New Year"), linetype = "dotted")+
  scale_color_manual(name="", values=c("grey70","red"))+
  ggtitle("Scenario 4: No Chunyun & no travel restrictions (Hypothetical)") +
  ylab("")+
  xlab("Days Since Travel Restrictions") +
  labs(color = "Year") +
  guides(fill= guide_colorbar(order = 1)) +
  theme(axis.text.y = element_text(hjust = 0, size = 8),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank(),
        title = element_text(size = 9)
  )


ggarrange(S1,S3,S2,S4, common.legend = TRUE, legend = "bottom") #+
  #ggsave("output/Sup_2.png", dpi= 320, width = 297, height = 190, units = "mm")




