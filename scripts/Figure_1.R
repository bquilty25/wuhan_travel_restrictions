# Script to make figure 1, mobility flux leaving Wuhan by scenario

# Estimate total baidu index leaving Wuhan for out of range 

pacman::p_load(tidyverse,lubridate,sf,RColorBrewer,imputeTS, tsibble, cowplot)

# Read in Baidu overall index mobilty leaving Wuhan 2019 & 2020
baidu <- read_csv("data/scaling_baidu_mobility_index.csv") %>% 
  mutate(Date = dmy(Date))

# mean of non-chunyun post-shutdown index 2020 - weekday
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
  mutate(baidu_index20non = if_else(Date > as.Date("2020-01-23"), baidu_index20, baidu_index19non)) %>% 
  mutate(restrict_day = c(-61:0, 1:52))


baidu_comp %>%
  pivot_longer(cols = c("baidu_index20", "baidu_index19", "baidu_index19non", "baidu_index20non"), names_to = "year", values_to = "index") %>%
  mutate(year = factor(year, levels = rev(c("baidu_index20","baidu_index19", "baidu_index20non", "baidu_index19non")))) %>%
  ggplot(aes(x=restrict_day, y= year, fill= index))+
  geom_tile(aes(x=restrict_day, y= year, fill= index)) +

  coord_cartesian(xlim = c(-20.5,20.5), ylim = c(1,4.8))+
  scale_fill_viridis_c(name="Baidu outflow index   ",option="inferno", begin = 0.05, end = 0.9,breaks=c(0,2,4,6,8,10,12),
                       guide = guide_coloursteps(even.steps = F, show.limits = T,barwidth=unit(7,"cm"))) +
  scale_y_discrete(labels = rev(c("Scenario 1:\nChunyun &\ncordon sanitaire\n(Observed 2020)",
                                  "Scenario 2:\nChunyun &\nno cordon sanitaire\n(Observed 2019)",
                                  "Scenario 3:\nNo Chunyun &\ncordon sanitaire\n(Hypothetical)",
                                  "Scenario 4:\nNo Chunyun &\nno cordon sanitaire\n(Hypothetical)")), position = "left")+
  ylab("") +
  xlab("Days Since Travel Restrictions") +
  labs(color = "Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0.5, yend = 4.5, color = "Travel restrictions and physical distancing enacted", linetype = "dashed"), size = 1.5)+
  geom_segment(aes(x = 2.5, xend = 2.5, y = 0.5, yend = 4.5, color = "Lunar New Year",linetype = "dotted"), size = 1.2)+
  scale_color_manual(name="", labels= c("Cordon sanitaire and other NPIs enacted", "Lunar New Year"), values=c("grey70", "grey70"))+
  scale_linetype_manual(name="", labels= c("Cordon sanitaire and other NPIs enacted", "Lunar New Year"), values=c("dashed", "dotted"))+
  geom_rect(aes(xmin = -30.8, xmax = 30.8, ymin = 2.4975, ymax= 2.5025), color= "white", fill= "white")+
  geom_text(aes(x=restrict_day, y=4.9, label = Date), angle = 90, size = 3, color = "grey50")+
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank()
  ) +
  ggsave("output/Fig_1_v2.pdf",
         width = 297,
         height = 160,
         dpi = 320,
         units = "mm")


  