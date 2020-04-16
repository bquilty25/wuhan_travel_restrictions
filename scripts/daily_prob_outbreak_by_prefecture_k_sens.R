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
               profvis,lubridate,htmlTable)

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
  # slice(1:50) %>% 
  st_drop_geometry() %>% 
  select(PYNAME,CNTY_CODE)


inc <- read.csv("data/inferred_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(cases = ifelse(date > as.Date("2020-02-01"), 25362, cases))

inc_extend <- data.frame(date=seq.Date(from=as.Date("2020-02-12"),to=as.Date("2020-03-01"),"days"),
                         cases=25362)

inc <- inc %>% bind_rows(inc_extend)

outbreak_constraint <- function(p, R, k) { (1+(R/k)*p)^(-k)-1+p }

outbreak_p <- Vectorize(function(R, k) { 
  uniroot(outbreak_constraint, c(1e-6, 1-1e-6), R=R, k=k)$root
})

outbreak_threshold <- function(c,p){(log(1-c))/(log(1-p))}

daily_inf_imports <- safely(function(cnty,nsim,R,k) {
  
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
    lazy_dt() %>% 
    select(date, trav, value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(inc, by = "date") %>%
    filter(!is.na(cases)) %>% 
    as.data.frame()
  
  
  inf <- tidyr::crossing(sim = 1:nsim,
                         travel) %>% 
    lazy_dt() %>% 
    mutate(draws = 1,
           n_daily_imports = as.integer(future_pmap_dbl(
             .f = n_infected,
             .l = list(
               cases = cases,
               pop = wuhan_pop,
               lambda = value,
               n = draws)
           )))%>% 
    group_by(trav,sim) %>% 
    arrange(date) %>% 
    mutate(cum_imports=cumsum(n_daily_imports)) %>% 
    mutate(single_case_prob=future_pmap_dbl(.f=outbreak_p,.l=list(R=R,k=k)),
           outbreak = cum_imports>outbreak_threshold(p=single_case_prob,c=0.95)) %>% 
    ungroup() %>% 
    select(trav,date,sim,cum_imports,outbreak) %>% 
    as.data.frame()
  
  return(inf)
})

inf_arrivals <- crossing(R=2.2,k= c(0.16,0.54,2),prf) %>% 
  lazy_dt() %>% 
  mutate(inc=future_pmap(
    .l = list(
      cnty = CNTY_CODE,
      nsim = 100,
      R=R,
      k= k
    ),.f=daily_inf_imports,.progress = T)) %>% 
  as.data.frame() 


delay_interval_plot_data <- inf_arrivals %>% 
  mutate(no_error = map_lgl(.f = ~ is.null(.x$error),.x=inc)) %>% 
  filter(no_error) %>%
  unnest() %>% 
  drop_na() %>%
  unnest() %>% 
  ungroup() %>% 
  lazy_dt() %>% 
  group_by(PYNAME,trav,k,date) %>% 
  summarise(daily_outbreak_n=sum(outbreak)) %>% 
  ungroup() %>% 
  group_by(PYNAME,trav,k) %>% 
  summarise(date_2.5=min(date[daily_outbreak_n>2.5]),
            date_25=min(date[daily_outbreak_n>25]),
            date_50=min(date[daily_outbreak_n>50]),
            date_75=min(date[daily_outbreak_n>75]),
            date_97.5=min(date[daily_outbreak_n>97.5])) %>% 
  as.data.frame() %>% 
  complete(PYNAME,trav,fill=list(date_2.5=as.Date("2020-06-01"),
                                 date_25=as.Date("2020-06-01"),
                                 date_50=as.Date("2020-06-01"),
                                 date_75=as.Date("2020-06-01"),
                                 date_97.5=as.Date("2020-06-01"))) %>% 
  separate(col = trav, into = c("travel_restrictions","chunyun"),sep=", ",remove=T) %>% 
  #filter(date!=as.Date("2020-06-01")|travel_restrictions!="No travel restrictions") %>% 
  right_join(prf_pop,by="PYNAME") %>% 
  ungroup() %>% 
  distinct(PYNAME,k,travel_restrictions,chunyun,.keep_all=T) %>% 
  as.data.frame() 

delay_interval_plot_data %>% 
  mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi") %>%
  select(PYNAME,travel_restrictions,k,chunyun,date_50,date_2.5,date_97.5) %>% 
  mutate_at(vars(date_50:date_97.5),funs(format(.,"%d %b"))) %>% 
  unite("CI",c(date_2.5:date_97.5),sep=" - ") %>% 
  mutate(CI=paste0("(",CI,")")) %>% 
  unite("estimate and 95% CI", c(date_50,CI),sep=" ") %>% 
  pivot_wider(names_from = c("chunyun","travel_restrictions"),values_from = `estimate and 95% CI`) %>% 
  htmlTable(rnames=F)

delay_interval_plot_data %>% 
  filter(chunyun=="Chunyun",k==0.54) %>%
  mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
  #filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi") %>%
  ggplot(aes(y=forcats::fct_reorder(PYNAME,as.numeric(pop_tot)),yend=forcats::fct_reorder(PYNAME,as.numeric(pop_tot))))+   
  geom_vline(aes(xintercept=as_date("2020-01-23")),colour="red")+
  geom_segment(aes(x=date_25,xend=date_75,colour=travel_restrictions),size=1,alpha=0.3)+
  geom_point(aes(x=date_50,colour=travel_restrictions),size=1,alpha=1)+
  geom_path(aes(x=date_50,group=PYNAME),size=0.5,arrow = arrow(ends = "last",type="closed",length=unit(0.08,"cm")))+
  scale_size_manual(values=c(1,0.75),guide=FALSE)+
  scale_colour_brewer(name="",palette="Dark2")+
  scale_x_date( breaks = scales::breaks_width("1 day"), 
                labels = scales::label_date_short())+
  facet_wrap(~chunyun)+
  # scale_y_discrete(guide = guide_axis(n.dodge = ))+
  coord_cartesian(xlim=c(as_date("2019-11-25"),as_date("2020-03-01")))+
  labs(x="Date of sustained transmission",y="Prefecture")+
  theme_light()+
  theme(legend.position = "top",
        text = element_text(size=6)
  )
  ggsave(
    filename = "output/delay_lineplot_chunyun.png",
    height = 210*2,
    width = 150*2,
    dpi = 320,
    units = "mm"
  )
  ggsave(
    filename = "output/delay_lineplot_chunyun.pdf",
    height = 210*2,
    width = 150*2,
    dpi = 320,
    units = "mm"
  )

delay_interval_plot_data %>% 
  filter(chunyun=="Non-Chunyun",k==0.54) %>% 
  mutate(travel_restrictions=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions")) %>% 
  ggplot(aes(y=forcats::fct_reorder(PYNAME,as.numeric(pop_tot)),yend=forcats::fct_reorder(PYNAME,as.numeric(pop_tot))))+   
  geom_vline(aes(xintercept=as_date("2020-01-23")),colour="red")+
  geom_segment(aes(x=date_25,xend=date_75,colour=travel_restrictions),size=1,alpha=0.3)+
  geom_point(aes(x=date_50,colour=travel_restrictions,size=travel_restrictions),alpha=1)+
  geom_path(aes(x=date_50,group=PYNAME),size=0.5,arrow = arrow(ends = "last",type="closed",length=unit(0.08,"cm")))+
  scale_colour_brewer(name="",palette="Dark2")+
  scale_size_manual(values=c(1,0.75),guide=FALSE)+
  scale_x_date( breaks = scales::breaks_width("1 day"), 
                labels = scales::label_date_short())+
  facet_wrap(~chunyun)+
  # scale_y_discrete(guide = guide_axis(n.dodge = ))+
  coord_cartesian(xlim=c(as_date("2019-11-25"),as_date("2020-03-01")))+
  labs(x="Date of sustained transmission",y="Prefecture")+
  theme_light()+
  theme(legend.position = "top",
        text = element_text(size=6))
  ggsave(
    filename = "output/delay_lineplot_non_chunyun.png",
    height = 210*2,
    width = 150*2,
    dpi = 320,
    units = "mm"
  )
  
  ggsave(
    filename = "output/delay_lineplot_non_chunyun.pdf",
    height = 210*2,
    width = 150*2,
    dpi = 320,
    units = "mm"
  )