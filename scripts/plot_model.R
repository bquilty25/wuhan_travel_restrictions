pacman::p_load(tidyverse,
               furrr,
               scales,
               forcats,
               outbreaks,
               projections,
               patchwork,
               tictoc,
               stringr,
               dtplyr,
               tidyfast)

source("scripts/utils.R")

plan(multiprocess)

model_run <- readRDS(file="output/model_run.rds")
gc()
inc_daily <-model_run  %>%
  mutate(inc_sum=future_pmap( .f=summarise_projections,.l=list(inc,cumulative=F),.progress = T)) %>% 
  select(-inc) %>% 
  unnest(inc_sum) %>% 
  group_by(PYNAME,travel_restrictions,chunyun,Reff,k,date) %>% 
  summarise_at(vars(inc_mean,inc_median,inc_0.025,inc_0.25,inc_0.75, inc_0.975),funs(sum)) %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) 

inc_cumulative <- model_run %>% 
  mutate(inc_sum=future_pmap(.f=summarise_projections,.l=list(inc,cumulative=T),.progress = T)) %>% 
  select(-inc) %>% 
  unnest(inc_sum) %>% 
  group_by(PYNAME,travel_restrictions,chunyun,Reff,k,date) %>% 
  summarise_at(vars(inc_mean,inc_median,inc_0.025,inc_0.25,inc_0.75, inc_0.975),funs(sum)) %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55"))

### 10 local cases ----
inc_cumulative %>% 
        group_by(PYNAME,Reff,k,chunyun,travel_restrictions,date) %>% 
        summarise_at(vars(inc_median,inc_0.025,inc_0.975),funs(sum)) %>% 
        filter(Reff=="2.2",travel_restrictions=="Travel restrictions",chunyun=="Chunyun") %>% 
        group_by(PYNAME,k) %>% 
     pivot_longer(cols=c(inc_median,inc_0.025,inc_0.975)) %>% 
     filter(value>=10) %>% 
     group_by(PYNAME,k,name) %>% 
     summarise(min_date=min(date)) %>% 
     mutate(min_date=format(as.Date(min_date,format="%Y-%m-%d"),"%d %b")) %>% 
     pivot_wider(id_cols = c(PYNAME, k), names_from = "name",values_from = "min_date") %>% 
     select(inc_median,inc_0.975,inc_0.025) 

### 100 local cases ----
inc_cumulative %>% 
  group_by(PYNAME,Reff,k,chunyun,travel_restrictions,date) %>% 
  summarise_at(vars(inc_median,inc_0.025,inc_0.975),funs(sum)) %>% 
  filter(Reff=="2.2",travel_restrictions=="Travel restrictions",chunyun=="Chunyun") %>% 
  group_by(PYNAME,k) %>% 
  pivot_longer(cols=c(inc_median,inc_0.025,inc_0.975)) %>% 
  filter(value>=100) %>% 
  group_by(PYNAME,k,name) %>% 
  summarise(min_date=min(date)) %>% 
  mutate(min_date=format(as.Date(min_date,format="%Y-%m-%d"),"%d %b")) %>% 
  pivot_wider(id_cols = c(PYNAME, k), names_from = "name",values_from = "min_date") %>%
  select(inc_median,inc_0.975,inc_0.025) 

### cases on 23rd jan ----
inc_cumulative %>%
  filter(date==as.Date("2020-01-23")) %>% 
  group_by(PYNAME,Reff,k,chunyun,travel_restrictions,date) %>% 
  filter(Reff=="2.2",travel_restrictions=="Travel restrictions",chunyun=="Chunyun") %>% 
  summarise(inc_median = signif(sum(inc_median),3),
            inc_0.025 = signif(sum(inc_0.025),3),
            inc_0.975 = signif(sum(inc_0.975),3)) %>% 
  unite(col = "inc_ci",c(inc_0.025,inc_0.975),sep = " - ") %>% 
  unite(col="inc_23_jan",c(inc_median,inc_ci),sep=" (") %>% View()

### cases on 1st march ----
# inc_cumulative%>%
#   filter(date==as.Date("2020-03-01")) %>% 
#   filter(chunyun=="Chunyun",trav_rest=="Travel restrictions enacted") %>% 
#   select(-c(Reff,travel_restrictions)) %>% 
#   group_by(PYNAME,Reff,k,chunyun,travel_restrictions) %>% 
#   summarise(inc_median = sum(inc_median),
#             inc_0.025 = sum(inc_0.025),
#             inc_0.975 = sum(inc_0.975)) %>% print(n=Inf)
# 
# inc_cumulative%>%
#   mutate(trav_rest=fct_recode(travel_restrictions,"Travel restrictions enacted"="Travel restrictions"),
#          Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
#                                    "Re = 1.1"="1.1",
#                                    "Re = 0.55"="0.55")) %>% 
#   filter(date==as.Date("2020-03-01")) %>% 
#   select(-c(Reff,travel_restrictions)) %>% 
#   group_by(date,PYNAME,Reff_reduction,chunyun,trav_rest) %>% 
#   summarise(inc_median = sum(inc_median),
#             inc_0.025 = sum(inc_0.025),
#             inc_0.975 = sum(inc_0.975)) %>% 
#   ggplot()+
#   geom_pointrange(aes(y=signif(inc_median,3),ymin=inc_0.025,ymax=inc_0.975,colour=fct_rev(Reff_reduction),x=fct_rev(Reff_reduction)))+
#   geom_text(aes(label=formatC(signif(inc_median,3),format="f", big.mark=",", digits=0),y=as.integer(inc_median),x=fct_rev(Reff_reduction)),nudge_x = 0.2,size=2,hjust=0)+
#   facet_nested(stringr::word(PYNAME)~chunyun+fct_rev(trav_rest)) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2))+
#   scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
#   theme_minimal()+
#   labs(y="Median cumulative number of cases by 1st March (95% CI)")+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         legend.box = "vertical",
#         panel.border = element_rect( fill = NA),
#         legend.position = "bottom",
#         axis.ticks = element_line())+
#   scale_color_brewer(name="",palette = "Dark2")+
#    #scale_colour_viridis_d(name = "", option = "viridis",begin=0.1,end = 0.9) +
#    ggsave(
#      filename = "output/cases_1_march_inc2.5_k2.png",
#      width = 210,#210*2,
#      height = 148,#297*4,
#      dpi = 320,
#      units = "mm"
#    )

#graph v2
inc_cumulative%>%
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) %>% 
  filter(date==as.Date("2020-03-01")) %>% 
  select(-c(Reff,travel_restrictions)) %>% 
  group_by(date,PYNAME,Reff_reduction,chunyun,trav_rest) %>% 
  summarise(inc_median = sum(inc_median),
            inc_0.025 = sum(inc_0.025),
            inc_0.975 = sum(inc_0.975)) %>% 
  ggplot()+
  geom_pointrange(aes(y=signif(inc_median,3),ymin=inc_0.025,ymax=inc_0.975,colour=fct_rev(Reff_reduction),x="",shape=trav_rest),position = position_dodge(width=0.75))+
  #geom_text(aes(label=formatC(signif(inc_median,3),format="f", big.mark=",", digits=0),y=as.integer(inc_median),x=fct_rev(Reff_reduction)),nudge_x = 0.2,size=2,hjust=0)+
  facet_nested(~chunyun+stringr::word(PYNAME),nest_line=T) +
  scale_shape_discrete(name="")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme_minimal()+
  labs(y="Median cumulative number of cases by 1st March (95% CI)")+
  theme(axis.title.x=element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
       # axis.text.x=element_blank(),
        legend.box = "vertical",
        #panel.border = element_rect( fill = NA),
        legend.position = "bottom",
        axis.ticks.y = element_line())+
  scale_color_brewer(name="",palette = "Dark2")
  #scale_colour_viridis_d(name = "", option = "viridis",begin=0.1,end = 0.9) +

  ggsave(
    filename = "output/cases_1_march_inc.png",
    width = 210,#210*2,
    height = 148,#297*4,
    dpi = 320,
    units = "mm"
  )
  
  ggsave(
    filename = "output/cases_1_march_inc.pdf",
    width = 210,#210*2,
    height = 148,#297*4,
    dpi = 320,
    units = "mm"
  )


### Daily Incidence ----
inc_daily %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) %>% 
  filter(date==as.Date("2020-03-01")) %>% 
  filter(Reff=="0.55",travel_restrictions=="Travel restrictions",chunyun=="Chunyun") %>% 
  select(-c(Reff,travel_restrictions)) %>% 
  group_by(date,PYNAME,Reff_reduction,chunyun,trav_rest) %>% 
  summarise(inc_median = sum(inc_median),
            inc_0.025 = sum(inc_0.025),
            inc_0.975 = sum(inc_0.975)) %>% print(n=Inf)
  
inc_daily %>% 
  filter(k==0.54) %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.20"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) %>%
  select(-c(Reff,travel_restrictions)) %>% 
  group_by(date,PYNAME,Reff_reduction,chunyun,trav_rest) %>% 
  summarise(inc_median = sum(inc_median),
            inc_0.025 = sum(inc_0.025),
            inc_0.975 = sum(inc_0.975)) %>% 
  #left_join(quartile_cities,by="PYNAME") %>% 
  ggplot() +
  geom_ribbon(aes(
    x = date,
    ymin = inc_0.025,
    ymax = inc_0.975,
    fill = trav_rest
  ),
  alpha = 0.3) +
  geom_line(aes(x = date, y = inc_median, colour = trav_rest),fill=NA) +
  scale_linetype_discrete(name="Importations")+
  scale_fill_brewer(name = "Mobility profile", palette= "Set2",type = "qual") +
  scale_colour_brewer(name = "Mobility profile", palette = "Set2",type="qual") +
  ggnewscale::new_scale_colour()+
  geom_vline(aes(xintercept=as.Date("2020-01-23"),colour="Cordon sanitaire and other NPIs imposed"))+
  scale_color_manual(name="", values=c("red"))+
  facet_nested(stringr::word(PYNAME)~chunyun+fct_rev(Reff_reduction),scales="free_y") +
  scale_y_continuous(trans=pseudo_log_trans(sigma = 1, base = exp(1)),breaks=c(0,1,10,100,1000))+
  #annotation_logticks(sides="l")+
  #scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_date(
    breaks = scales::breaks_width("1 month"),
    labels = scales::label_date_short()
  ) +
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-03-05")),
                  ylim = c(0, 1000)
                  ) +
  labs(
    y = "Median daily incidence (95% CI)",
    x = "Date"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect( fill = NA),
        axis.ticks = element_line())

ggsave(
  filename = "output/inc_four_cities.png",
  width = 210,#210*2,
  height = 148,#297*4,
  dpi = 320,
  units = "mm"
)

ggsave(
  filename = "output/inc_four_cities.pdf",
  width = 210,#210*2,
  height = 148,#297*4,
  dpi = 320,
  units = "mm"
)
