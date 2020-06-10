################################ 
# Code for creating Figure 1 - Total domestic travel outflow from Wuhan under 4 travel pattern scenarios. 
# For study "The Effect of Inter-City Travel Restrictions on Geographical Spread of COVID-19: Evidence from Wuhan, China"
# Written by Charlie Diamond & Billy Quilty
################################

### Required packages
pacman::p_load(tidyverse,lubridate, tsibble)

### Read in Baidu overall index mobilty leaving Wuhan 2020 & 2019 (Scenario 1 & 2)
baidu <- read_csv("data/scaling_baidu_mobility_index.csv") %>% 
  mutate(Date = dmy(Date))

### Mean of non-chunyun index 2019 - weekday
dayweek19 <- baidu %>% 
  filter(Date >= as.Date("2020-01-06") & Date <= as.Date("2020-01-10") | Date >= as.Date("2020-02-08") ) %>% 
  mutate(day = wday(Date)) %>% 
  group_by(day) %>% 
  summarise(mean19= mean(baidu_index19, na.rm = TRUE))

### Mean of non-chunyun post-shutdown index 2020 - weekday                  
dayweek20 <- baidu %>% 
  filter(Date >= as.Date("2020-02-08")) %>% 
  mutate(day = wday(Date)) %>% 
  group_by(day) %>% 
  summarise(mean20= mean(baidu_index20, na.rm = TRUE))

### Estimate total baidu index leaving Wuhan for all 4 travel scenarios, both observed and hypothetical. Out of date ranges also estimated.
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

### Create plot
baidu_comp %>%
  pivot_longer(cols = c("baidu_index20", "baidu_index19", "baidu_index19non", "baidu_index20non"), names_to = "year", values_to = "index") %>%
  mutate(year = factor(year, levels = rev(c("baidu_index20","baidu_index19", "baidu_index20non", "baidu_index19non")))) %>%
  
  ggplot(aes(x=Date, y= year, fill= index)) +
  geom_tile(aes(x=Date, y= year, fill= index)) +
  
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-02-07")),
                  expand=F) +
  colorspace::scale_fill_continuous_sequential(name="Baidu outflow index",palette = "Greens",breaks=seq(2,10,by=2),guide=guide_colorsteps(show.limits=T,barwidth=20,title.position="top",title.hjust=0.5)) +
  
  scale_y_discrete(labels = rev(c("Scenario 1:\nChunyun &\ncordon sanitaire\n(Observed 2020)",
                                  "Scenario 2:\nChunyun &\nno cordon sanitaire\n(Observed 2019)",
                                  "Scenario 3:\nNo Chunyun &\ncordon sanitaire\n(Hypothetical)",
                                  "Scenario 4:\nNo Chunyun &\nno cordon sanitaire\n(Hypothetical)")), position = "left")+ 
  ylab("") +
  xlab("Date") +
  labs(color = "Year")+
  scale_x_date(
    breaks = scales::breaks_width("1 week"),
    labels = scales::label_date_short(),
    limits = c(as.Date("2019-12-01"),as.Date("2020-02-07"))) +
  
  geom_segment(aes(x = as.Date("2020-01-23"), xend = as.Date("2020-01-23"), y = 0.5, yend = 4.5, color = "Travel restrictions and physical distancing enacted", linetype = "dashed"), size = 0.5)+
  geom_segment(aes(x =as.Date("2020-01-25"), xend = as.Date("2020-01-25"), y = 0.5, yend = 4.5, color = "Lunar New Year",linetype = "dotted"), size = 0.5) +
  scale_color_manual(name="", labels= c("Cordon sanitaire and other NPIs enacted", "Lunar New Year"), values=c("black", "black")) + 
  scale_linetype_manual(name="", labels= c("Cordon sanitaire and other NPIs enacted", "Lunar New Year"), values=c("dashed", "dotted")) +

  geom_segment(aes(x = as.Date("2019-12-01"), xend = as.Date("2020-02-07"), y = 1.4975, yend= 1.5025), color= "white") +
  geom_segment(aes(x = as.Date("2019-12-01"), xend = as.Date("2020-02-07"), y = 2.4975, yend= 2.5025), color= "white") +
  geom_segment(aes(x = as.Date("2019-12-01"), xend = as.Date("2020-02-07"), y = 3.4975, yend= 3.5025), color= "white") +
  
  theme(axis.text.y = element_text(hjust = 0, size = 10),
        axis.ticks.y =element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")) 
  
  ggsave("output/Fig_1_v3.png",
         width = 297,
         height = 160,
         dpi = 320,
         units = "mm")
  ggsave("output/Fig_1_v3.pdf",
         width = 297,
         height = 160,
         dpi = 320,
         units = "mm")


  