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
               data.table)

source("scripts/utils.R")

top_cities <- read_csv("data/codes_20.csv")

four_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>% 
  st_drop_geometry() %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi")


wuhan_prev <- readRDS("data/kucharski_model_output.rds") %>% filter(date<as.Date("2020-02-07"))

plot_Reff_scenarios <- safely(function(cnty,Reff,k,alt_si,nsim) {
  
  prf <- trav_from_wuhan(cnty, 50000) %>%
    as.data.table() %>% 
    dt_pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                 names_to = "trav") %>% 
    lazy_dt() %>% 
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
    left_join(wuhan_prev, by = "date") %>%
    filter(!is.na(E_and_I)) %>% 
    mutate(after_restrictions=ifelse(date>as.Date("2020-01-23"),"Seeded after restrictions","Seeded prior to restrictions")) 
  
  inf <- tidyr::crossing(sim = 1:nsim,
                         travel %>% as.data.frame()) %>% 
    lazy_dt() %>% 
    mutate(draws = 1,
           n_daily_imports = as.integer(future_pmap_dbl(
             .f = n_infected,
             .l = list(
               cases = E_and_I,
               pop = wuhan_pop,
               lambda = value,
               n = draws
             )
           ))) %>%
    group_by(sim,
             trav,
             date) %>%
    arrange(sim,trav, date) %>%
    as.data.frame() %>% 
    dt_uncount(n_daily_imports, .remove = FALSE) %>%
    ungroup() %>%
    lazy_dt() %>% 
    mutate(row_id = row_number()) %>%
    dt_nest(sim,
            trav,
            after_restrictions,
            row_id) %>%
    lazy_dt() %>% 
    mutate(bp = future_pmap(
      .f = bp_rt_fun,
      .l = list(
        df = data,
        Reff = Reff,
        alt_si=alt_si,
        k=k
      ),.progress = T
    )) %>% 
    select(-data) %>% 
    lazy_dt() %>% 
    mutate(sum_bp=future_map_dbl(.f=sum,.x=bp)) %>% 
    filter(sum_bp>0) 
  
  inc<- inf %>% 
    lazy_dt() %>% 
    ungroup() %>%
    select(-c(row_id,sum_bp)) %>% 
    #group_by(sim,trav,after_restrictions) %>%
    as.data.table() %>% 
    dt_nest(sim,trav,after_restrictions) %>% 
    lazy_dt() %>% 
    mutate(proj=future_map(.f=flatten,data)) %>% 
    select(-data) %>% 
    mutate(add_proj=future_map(.f=merge_add_projections,proj)) %>% 
    ungroup() %>% 
    select(-c(proj,sim)) %>% 
    dt_nest(trav,after_restrictions) %>% 
    lazy_dt() %>% 
    mutate(proj=future_map(.f=flatten,data)) %>% 
    mutate(merged_proj=future_map(.f=merge_projections,proj))
  

    inc_daily <- inc %>%
    mutate(sum_proj=future_pmap(.f=summarise_proj,
                                .l=list(merged_proj,cumulative=F))) %>%
    select(trav,after_restrictions,sum_proj) %>%
    as.data.table() %>%
    dt_unnest(sum_proj) %>%
    as.data.frame() %>%
    separate(col = trav, into = c("travel_restrictions","chunyun"),sep=", ",remove=T)  %>%
    lazy_dt() %>%
    group_by(travel_restrictions,chunyun,date) %>%
    summarise_at(vars(inc_0.1:inc_0.9),
      #inc_mean,inc_median,inc_0.025,inc_0.25,inc_0.75, inc_0.975
                 funs(sum)) %>%
    lazy_dt() %>%
    mutate(trav_rest=fct_recode(travel_restrictions,
                                "Cordon sanitaire imposed"="Travel restrictions",
                                "No cordon sanitaire"="No travel restrictions"),
           Reff_reduction=fct_recode(as.factor(Reff),
                                     "Re = 2.2"="2.2",
                                     "Re = 1.1"="1.1",
                                     "Re = 0.55"="0.55")) %>%
      as.data.frame()
  # 
  # inc_cumulative <- inc %>%
  #   mutate(sum_proj=future_pmap(.f=summarise_proj,
  #                               .l=list(merged_proj,cumulative=T))) %>% 
  #   select(trav,after_restrictions,sum_proj) %>% 
  #   dt_unnest(sum_proj) %>% 
  #   as.data.frame() %>% 
  #   separate(col = trav, into = c("travel_restrictions","chunyun"),sep=", ",remove=T)  %>% 
  #   lazy_dt() %>% 
  #   group_by(travel_restrictions,chunyun,date) %>% 
  #   summarise_at(vars(inc_mean,inc_median,inc_0.025,inc_0.25,inc_0.75, inc_0.975),
  #                funs(sum)) %>% 
  #   lazy_dt() %>% 
  #   mutate(trav_rest=fct_recode(travel_restrictions,
  #                               "Cordon sanitaire imposed"="Travel restrictions",
  #                               "No cordon sanitaire"="No travel restrictions"),
  #          Reff_reduction=fct_recode(as.factor(Reff),
  #                                    "Re = 2.2"="2.2",
  #                                    "Re = 1.1"="1.1",
  #                                    "Re = 0.55"="0.55")) %>% 
  #   as.data.frame()

  gc()
    
return(list(inc_daily=inc_daily))
})


jobs <- crossing(Reff = c(2.2, 1.1, 0.55),
                 k    = c(0.04,0.1,0.2),
                      alt_si= FALSE,
                      four_cities) %>% 
  nrow()
jobs

plan(list(tweak(multiprocess, workers = min(future::availableCores(), jobs)),
          tweak(multiprocess, workers = max(1, round(future::availableCores() / jobs)))),
     gc = TRUE, earlySignal = TRUE)
#plan(multiprocess)
options('future.globals.maxSize' = 1014*2*1024^2)
gc()
tic()
model_run <- crossing(Reff = c(2.2, 1.1, 0.55), 
                      k    = c(0.04,0.1,0.2),
                      alt_si= FALSE,
                      four_cities)  %>% 
  lazy_dt() %>% 
  mutate(inc=future_pmap(.l = list(
      k=k,
      Reff = Reff,
      cnty = CNTY_CODE,
      alt_si=alt_si,
      nsim = 1
    ),
    .f=plot_Reff_scenarios,
    .progress = T, 
    .options = furrr::future_options(scheduling = 20))
    ) %>% 
  as.tibble()

toc()

saveRDS(model_run,file="output/model_run_deciles.rds")


saveRDS(model_run,file="output/model_run_2020_05_22.rds")


model_run %>% 
  unnest() %>% 
  drop_na() %>% 
  unnest_wider(inc) %>%
  unnest(inc_daily) %>% 
  lazy_dt() %>% 
  group_by(PYNAME,travel_restrictions,chunyun,Reff,k,alt_si,date) %>% 
  summarise_at(vars(inc_0.1:inc_0.9),funs(sum)) %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) %>% 
  as.data.frame() %>% 
  filter(k==0.1,alt_si==FALSE,PYNAME=="Beijing Shi") %>% 
  mutate(trav_rest=fct_rev(trav_rest)) %>% 
  pivot_longer(cols=c(inc_0.1:inc_0.9)) %>% 
  ggplot()+
  # geom_ribbon(aes(x = date, ymin = inc_0.025, ymax = inc_0.975, fill = trav_rest),alpha=0.1) +
  # geom_ribbon(aes(x = date, ymin = inc_0.25, ymax = inc_0.75, fill = trav_rest),alpha=0.2) +
  geom_line(aes(x = date, y = value, group=name, colour = trav_rest),fill=NA,alpha=0.5) +
  geom_line(aes(x = date, y = value, group=name, colour = trav_rest),fill=NA,alpha=0.5) +
  scale_fill_brewer(name = "Mobility profile", 
                       palette= "Set1",
                       type = "qual"
                       ) +
  scale_colour_brewer(name = "Mobility profile",
                       palette= "Set1",
                       type = "qual"
  ) +
  scale_linetype_manual(name = "Mobility profile",values=c("solid","dashed")) +
  ggnewscale::new_scale("linetype") +
  geom_vline(aes(xintercept=as.Date("2020-01-23"),linetype="Date cordon sanitaire and other NPIs imposed"))+
  scale_linetype_manual(name="", values=c("dashed"))+
  facet_nested(alt_si+chunyun~fct_rev(Reff_reduction)+trav_rest,scales="free_y") +
  #annotation_logticks(sides="l")+
  #scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_date(
    breaks = scales::breaks_width("1 week"),
    labels = scales::label_date_short()
  ) +
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-02-01")),
                  ylim = c(0, 2500),
                  expand=F
  ) +
  labs(
    y = "Median daily incidence",
    x = "Date"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")
        )
  
ggsave(
  filename = "output/inc_beijing.png",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)
ggsave(
  filename = "output/inc_beijing.pdf",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)



model_run %>% 
  unnest() %>% 
  drop_na() %>% 
  unnest_wider(inc) %>%
  unnest(inc_daily) %>% 
  lazy_dt() %>% 
  group_by(PYNAME,travel_restrictions,chunyun,Reff,k,alt_si,date) %>% 
  summarise_at(vars(inc_mean,inc_median,inc_0.025,inc_0.25,inc_0.75, inc_0.975),funs(sum)) %>% 
  mutate(trav_rest=fct_recode(travel_restrictions,"Cordon sanitaire imposed"="Travel restrictions","No cordon sanitaire"="No travel restrictions"),
         Reff_reduction=fct_recode(as.factor(Reff),"Re = 2.2"="2.2",
                                   "Re = 1.1"="1.1",
                                   "Re = 0.55"="0.55")) %>% 
  as.data.frame() %>% 
  filter(k==0.1,alt_si==TRUE) %>% 
  mutate(trav_rest=fct_rev(trav_rest)) %>% 
  ggplot()+
  geom_ribbon(aes(x = date, ymin = inc_0.025, ymax = inc_0.975, fill = trav_rest),alpha=0.1) +
  geom_ribbon(aes(x = date, ymin = inc_0.25, ymax = inc_0.75, fill = trav_rest),alpha=0.2) +
  geom_line(aes(x = date, y = inc_median, colour = trav_rest,linetype=trav_rest),fill=NA,size=1) +
  scale_fill_brewer(name = "Mobility profile", 
                    palette= "Set1",
                    type = "qual"
  ) +
  scale_colour_brewer(name = "Mobility profile",
                      palette= "Set1",
                      type = "qual"
  ) +
  scale_linetype_manual(name = "Mobility profile",values=c("solid","dashed")) +
  ggnewscale::new_scale("linetype") +
  geom_vline(aes(xintercept=as.Date("2020-01-23"),linetype="Date cordon sanitaire and other NPIs imposed"))+
  scale_linetype_manual(name="", values=c("dashed"))+
  facet_nested(PYNAME+chunyun~fct_rev(Reff_reduction),scales="free") +
  #annotation_logticks(sides="l")+
  #scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_date(
    breaks = scales::breaks_width("1 week"),
    labels = scales::label_date_short()
  ) +
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-02-01")),
                  ylim = c(0, 2500),
                  expand=F
  ) +
  labs(
    y = "Median daily incidence",
    x = "Date"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")
  )

ggsave(
  filename = "output/inc_four_cities_alt_si.png",
  width = 210,
  height = 297,
  dpi = 400,
  units = "mm"
)
ggsave(
  filename = "output/inc_four_cities_alt_si.pdf",
  width = 210,
  height = 297,
  dpi = 400,
  units = "mm"
)
