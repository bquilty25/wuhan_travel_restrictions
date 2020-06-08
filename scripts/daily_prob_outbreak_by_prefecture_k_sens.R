pacman::p_load(tidyverse,
               furrr,
               sf,
               scales,
               forcats,
               outbreaks,
               projections,
               patchwork,
               tictoc,
               stringr,
               dtplyr,
               tidyfast,
               data.table,
               profvis,lubridate,htmlTable,
               ggforce)

source("scripts/utils.R")

plan(multiprocess)

top_cities <- read_csv("data/codes_20.csv")

prf_pop <- read.csv("data/prf_pop_2018.csv")

prf_sf <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf")

prf <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>% 
  st_drop_geometry() %>% 
  select(PYNAME,CNTY_CODE)

wuhan_prev <- readRDS("data/kucharski_model_output.rds") %>% filter(date<as.Date("2020-02-07"))

outbreak_constraint <- function(p, R, k) { (1+(R/k)*p)^(-k)-1+p }

outbreak_p <- Vectorize(function(R, k) { 
  uniroot(outbreak_constraint, c(1e-6, 1-1e-6), R=R, k=k)$root
})

outbreak_threshold <- function(c,p){(log(1-c))/(log(1-p))}

daily_inf_imports <- safely(function(cnty,nsim,R,k,O) {
  
  prf <- trav_from_wuhan(cnty, 50000) %>%
    as.data.table() %>% 
    dt_pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                    names_to = "trav") %>% lazy_dt() %>% 
    mutate(trav_rest = dt_case_when(
      trav == "trav_19" ~ "No travel restrictions",
      trav == "trav_20" ~ "Travel restrictions",
      trav == "trav_19non" ~ "No travel restrictions",
      trav == "trav_20non" ~ "Travel restrictions",
      TRUE ~ NA_character_
    )
    ) %>%
    mutate(
      chunyun = dt_case_when(
        trav == "trav_19" ~ "Chunyun",
        trav == "trav_20" ~ "Chunyun",
        trav == "trav_19non" ~ "Non-Chunyun",
        trav == "trav_20non" ~ "Non-Chunyun",
        TRUE ~ NA_character_
      )
    ) %>% 
    as.data.frame() %>% 
    unite("trav",c("trav_rest","chunyun"),remove=F,sep=", ")
  
  travel <- prf %>%  
    #lazy_dt() %>% 
    dplyr::select(date, trav, value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(wuhan_prev, by = "date") %>%
    filter(!is.na(E_and_I)) %>% 
    as.data.frame()
  
  
  inf <- tidyr::crossing(sim = 1:nsim,
                         travel) %>% 
    lazy_dt() %>% 
    mutate(draws = 1,
           n_daily_imports = as.integer(future_pmap_dbl(
             .f = n_infected,
             .l = list(
               cases = E_and_I,
               pop = wuhan_pop,
               lambda = value,
               n = draws)
           )))%>% 
    group_by(trav,sim) %>% 
    arrange(date) %>% 
    mutate(cum_imports=cumsum(n_daily_imports)) %>% 
    mutate(single_case_prob=future_pmap_dbl(.f=outbreak_p,.l=list(R=R,k=k)),
           outbreak = cum_imports>outbreak_threshold(p=single_case_prob,c=O)) %>% 
    ungroup() %>% 
    select(trav,date,sim,cum_imports,outbreak) %>% 
    as.data.frame()
  
  return(inf)
})

gc()
tic()
inf_arrivals_all <- crossing(R = 2.2,
                         k = 0.1,
                         O = 0.95,
                         prf %>% slice(1:100) ) %>% 
  lazy_dt() %>% 
  mutate(inc=future_pmap(
    .l = list(
      cnty = CNTY_CODE,
      nsim = 100,
      R=R,
      k= k,
      O=O
    ),.f=daily_inf_imports,.progress = T)) %>% 
  as.data.frame() 
toc()

saveRDS(inf_arrivals_all,file="output/inf_arrivals_all.rds")
inf_arrivals_all <- read_rds("output/inf_arrivals_all.rds")
gc()
delay_interval_plot_data <- inf_arrivals_all %>% 
  mutate(no_error = map_lgl(.f = ~ is.null(.x$error),.x=inc)) %>% 
  filter(no_error) %>%
  unnest() %>% 
  drop_na() %>%
  unnest() %>% 
  ungroup() %>% 
  lazy_dt() %>% 
  group_by(PYNAME,trav,k,O,date) %>% 
  summarise(daily_outbreak_n=sum(outbreak)) %>% 
  ungroup() %>% 
  group_by(PYNAME,trav,k,O) %>% 
  summarise(date_2.5=min(date[daily_outbreak_n>2.5]),
            date_25=min(date[daily_outbreak_n>25]),
            date_50=min(date[daily_outbreak_n>50]),
            date_75=min(date[daily_outbreak_n>75]),
            date_97.5=min(date[daily_outbreak_n>97.5])) %>% 
  as.data.frame() %>% 
  complete(PYNAME,trav,fill=list(date_2.5=as.Date("2020-02-07"),
                                 date_25=as.Date("2020-02-07"),
                                 date_50=as.Date("2020-02-07"),
                                 date_75=as.Date("2020-02-07"),
                                 date_97.5=as.Date("2020-02-07"))) %>% 
  separate(col = trav, into = c("travel_restrictions","chunyun"),sep=", ",remove=T) %>% 
  #filter(date!=as.Date("2020-06-01")|travel_restrictions!="No travel restrictions") %>% 
  right_join(prf_pop,by="PYNAME") %>% 
  ungroup() %>% 
  distinct(PYNAME,k,O,travel_restrictions,chunyun,.keep_all=T) %>% 
  as.data.frame() 

saveRDS(delay_interval_plot_data,"output/delay_interval_data_all.rds")
delay_interval_plot_data <- readRDS("output/delay_interval_data_all.rds")

# delay_interval_plot_data %>% 
#   mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
#   filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi") %>%
#   select(PYNAME,travel_restrictions,k,O,chunyun,date_50,date_2.5,date_97.5) %>% 
#   mutate_at(vars(date_50:date_97.5),funs(format(.,"%d %b"))) %>% 
#   unite("CI",c(date_2.5:date_97.5),sep=" - ") %>% 
#   mutate(CI=paste0("(",CI,")")) %>% 
#   unite("estimate and 95% CI", c(date_50,CI),sep=" ") %>% 
#   pivot_wider(names_from = c("chunyun","travel_restrictions"),values_from = `estimate and 95% CI`) %>% 
#   htmlTable(rnames=F)

delay_interval_plot_data %>% 
       mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi") %>%
  filter(O==0.95,chunyun=="Chunyun",travel_restrictions=="Cordon sanitaire imposed",k==0.1) %>% View()
       select(PYNAME,travel_restrictions,k,O,chunyun,date_50,date_2.5,date_97.5) %>% 
  ggplot()+
  geom_pointrange(aes(x=as.factor(k),y=date_50,ymin=date_2.5,ymax=date_97.5,colour=travel_restrictions,shape=travel_restrictions),position = position_dodge(width=0.5),alpha=0.95)+
  scale_shape(name="")+
  geom_hline(aes(yintercept=as.Date("2020-01-23"),linetype="Date cordon sanitaire and other NPIs imposed"))+
  scale_linetype_manual(name="", values=c("dashed"))+
 # scale_x_discrete(breaks=c(0.04,0.1,0.2,0.54,2))+
  labs(x="Overdispersion in R (k)", y="Date of highly-probable sustained transmission")+
  facet_nested(chunyun~stringr::word(PYNAME))+
  scale_color_brewer(name="",type="qual",palette = "Set1")+
  scale_y_date(
    breaks = scales::breaks_width("1 week"),
    labels = scales::label_date_short()) +
  coord_cartesian(ylim = c(as.Date("2019-12-01"), as.Date("2020-02-05")),
                  expand=F) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")
  )+
  ggpubr::rotate_x_text(angle = 45)

ggsave(
  filename = "output/outbreak_k.png",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)
ggsave(
  filename = "output/outbreak_k.pdf",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)


plot_data <- delay_interval_plot_data %>% 
  na.omit() %>% 
  mutate(region_id=as.factor(str_sub(CNTY_CODE,1,1))) %>% 
  mutate(region=forcats::fct_recode(region_id,
                                    "Northern China" = "1",
                                    "Northeast China"="2",
                                    "Eastern China"="3",
                                    "Central and Southern China"="4",
                                    "Southwestern China"="5",
                                    "Northwest China"="6")) %>% 
  mutate(plot_number=forcats::fct_recode(region_id,
                                         "A"="1",
                                         "B"="2",
                                         "C"="3",
                                         "D"="4",
                                         "E"="5",
                                         "F"="6"))


p <- plot_data%>% 
    #filter(region_id==i) %>% 
  #filter(chunyun=="Chunyun") %>%
  mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
  nest(-c(region_id,plot_number)) %>% 
  mutate(plot = map2(data,plot_number,~ggplot(data=.x,aes(y=forcats::fct_reorder(PYNAME,as.numeric(pop_tot)),
                                                          yend=forcats::fct_reorder(PYNAME,as.numeric(pop_tot))))+   
  geom_vline(aes(xintercept=as_date("2020-01-23")),colour="red")+
  geom_segment(aes(x=date_25,xend=date_75,colour=travel_restrictions),size=1,alpha=0.3)+
  geom_point(aes(x=date_50,colour=travel_restrictions),size=1,alpha=1)+
  geom_path(aes(x=date_50,group=PYNAME),size=0.5,arrow = arrow(ends = "last",type="closed",length=unit(0.08,"cm")))+
  scale_size_manual(values=c(1,0.75),guide=FALSE)+
  scale_colour_brewer(name="",palette="Set1")+
  scale_x_date( breaks = scales::breaks_width("1 week"), 
                labels = scales::label_date_short())+
  facet_nested(~region+chunyun,scales = "free_y")+
  coord_cartesian(xlim=c(as_date("2019-11-25"),as_date("2020-02-07")))+
  labs(x="Date of highly-probable sustained transmission",y="Prefecture")+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.box = "vertical",
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_line(colour = "grey"),
          text = element_text(size=6),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"))+
    ggtitle(.y)
  ))

  pmap(list(
    filename=paste0("output/all_pref_k/all_pref_k_region_", p$plot_number, ".png"),    
  width = 210,
  height = 148,
  dpi = 400,
  units = "mm",
  plot=p$plot),
  .f=ggsave)
  
  pmap(list(
    filename=paste0("output/all_pref_k/all_pref_k_region_", p$plot_number, ".pdf"),    
    width = 210,
    height = 148,
    dpi = 400,
    units = "mm",
    plot=p$plot),
    .f=ggsave)


