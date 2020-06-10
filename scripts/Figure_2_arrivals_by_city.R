################################ 
# Code for estimating the number of daily arrivals by city
# for study "The Effect of Inter-City Travel Restrictions on Geographical Spread of COVID-19: Evidence from Wuhan, China" 
# written by Billy J Quilty
################################

### Load packages ----
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

### Load utility functions ----
source("scripts/utils.R")

### Parallel processing options ----
plan(multiprocess)
options('future.globals.maxSize' = 1014*4*1024^2)

### Load data ----
four_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>% 
  st_drop_geometry() %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi")

wuhan_prev <- readRDS("data/kucharski_model_output.rds") %>% filter(date<as.Date("2020-02-07"))

### Define arrivals function ----
arrivals_fun <- function(cnty,nsim) {
  
  # Estimate volume of travel from Wuhan to prefecture with cnty_code `cnty` and define scenarios
  prf <- trav_from_wuhan(cnty, 50000) %>%
    as.data.table() %>% 
    dt_pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                    names_to = "trav") %>% 
    lazy_dt() %>% 
    mutate(trav_rest = dt_case_when(
      trav == "trav_19" ~ "No cordon sanitaire",
      trav == "trav_20" ~ "Cordon sanitaire imposed",
      trav == "trav_19non" ~ "No cordon sanitaire",
      trav == "trav_20non" ~ "Cordon sanitaire imposed",
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
  
  # Join to daily Wuhan prevalence estimates
  travel <- prf %>% 
    lazy_dt() %>% 
    select(date, trav,chunyun, trav_rest,  value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(wuhan_prev, by = "date") %>%
    filter(!is.na(E_and_I)) 
  
  # Calculate number of daily imports according to travel volume and prevalence in Wuhan
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
    as.data.frame()
  
  return(inf)
}

### Run function for the 4 cities of interest ----
tic()
imports <- four_cities %>% 
  lazy_dt() %>% 
  select(-NAME) %>% 
  mutate(arrivals = future_pmap(.l = list(cnty = CNTY_CODE,
                                          nsim=100), 
                                .f = arrivals_fun))
toc()

### Summarise simulations ----
imp_sum <- imports %>% 
  as.data.table() %>% 
  dt_unnest(arrivals) %>% 
  lazy_dt() %>% 
  group_by(PYNAME,date,trav_rest,chunyun) %>% 
  summarise(n_daily_imports_mean = mean(n_daily_imports),
            n_daily_imports_median = quantile(probs = 0.5, n_daily_imports),
            n_daily_imports_0.025 = quantile(probs = 0.025, n_daily_imports),
            n_daily_imports_0.25 = quantile(probs = 0.25, n_daily_imports),
            n_daily_imports_0.75 = quantile(probs = 0.75, n_daily_imports),
            n_daily_imports_0.975 = quantile(probs = 0.975, n_daily_imports)) %>% 
  ungroup() %>% 
  mutate(after_restrictions=ifelse(date>as.Date("2020-01-23"),"Arrived after restrictions","Arrived prior to restrictions")) %>% 
  as.data.frame() 
  
### Daily number of infected arrivals plot ----
p1 <- imp_sum %>% 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = n_daily_imports_0.025,ymax=n_daily_imports_0.975, fill=trav_rest),alpha=0.25)+
  geom_line(aes(x = date, y = n_daily_imports_median, colour=trav_rest, linetype = trav_rest),size=1) +
  scale_fill_brewer(name = "Mobility profile", 
                    palette= "Set1",
                    type = "qual") +
  scale_colour_brewer(name = "Mobility profile",
                      palette= "Set1",
                      type = "qual") +
  scale_linetype_manual(name = "Mobility profile",values=c("solid","dashed")) +
  ggnewscale::new_scale("linetype") +
  geom_vline(aes(xintercept=as.Date("2020-01-23"),linetype="Date cordon sanitaire and other NPIs imposed"))+
  scale_linetype_manual(name="", values=c("dashed"))+
  facet_nested(chunyun~stringr::word(PYNAME) ) +
  scale_x_date(breaks = scales::breaks_width("1 week"),
    labels = scales::label_date_short()) +
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-02-01")),
                  ylim = c(0, NA),
                  expand=F) +
  labs(y = "Median daily infected arrivals",x = "Date") +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")
  )

### Cumulative number of infected arrivals plot ----
p2 <- imp_sum %>% 
  group_by(PYNAME,trav_rest,chunyun) %>% 
  mutate_at(vars("n_daily_imports_mean":"n_daily_imports_0.975"),cumsum) %>% 
ggplot() +
  geom_ribbon(aes(x = date, ymin = n_daily_imports_0.025,ymax=n_daily_imports_0.975, fill=trav_rest),alpha=0.25)+
  geom_line(aes(x = date, y = n_daily_imports_median, colour=trav_rest, linetype = trav_rest),size=1) +
  scale_fill_brewer(name = "Mobility profile", 
                    palette= "Set1",
                    type = "qual") +
  scale_colour_brewer(name = "Mobility profile",
                      palette= "Set1",
                      type = "qual") +
  scale_linetype_manual(name = "Mobility profile",values=c("solid","dashed")) +
  ggnewscale::new_scale("linetype") +
  geom_vline(aes(xintercept=as.Date("2020-01-23"),linetype="Date cordon sanitaire and other NPIs imposed"))+
  scale_linetype_manual(name="", values=c("dashed"))+
  facet_nested(chunyun~stringr::word(PYNAME) ) +
  scale_x_date(breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  coord_cartesian(xlim = c(as.Date("2019-12-01"), as.Date("2020-02-01")),
                  ylim = c(0, NA),
                  expand=F) +
  labs(y = "Median cumulative infected arrivals",x = "Date") +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm")
  )

### Combine and save plots to file ----
p1 / p2 + 
    plot_annotation(tag_levels = c("A", "B")) +
    plot_layout(guides = "collect", ncol = 1) &
    theme(
      legend.position = 'bottom',
      legend.justification = "centre",
      legend.box = "horizontal"
    )
  #

ggsave(
  filename = "output/inf_arrivals.png",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)

ggsave(
  filename = "output/inf_arrivals.pdf",
  width = 210*1.5,
  height = 148*1.5,
  dpi = 400,
  units = "mm"
)

### Max daily imports, Chunyun ----
imp_sum %>% 
  filter(trav_rest=="Cordon sanitaire imposed",chunyun=="Chunyun") %>% 
  pivot_longer("n_daily_imports_mean":"n_daily_imports_0.975") %>% 
  group_by(PYNAME,trav_rest,chunyun,after_restrictions,name) %>% 
  filter(value==max(value)) %>% mutate(value=as.integer(value)) %>% 
  print(n=Inf)

### Max daily imports, Non-Chunyun ----
imp_sum %>% 
  filter(trav_rest=="Cordon sanitaire imposed",chunyun=="Non-Chunyun") %>% 
  pivot_longer("n_daily_imports_mean":"n_daily_imports_0.975") %>% 
  group_by(PYNAME,trav_rest,chunyun,after_restrictions,name) %>% 
  filter(value==max(value)) %>% mutate(value=as.integer(value)) %>% 
  print(n=Inf)

#### Cumulative cases on date of travel restrictions in baseline scenario 1 ----
imp_sum %>% 
  group_by(PYNAME,trav_rest,chunyun) %>% 
  mutate_at(vars("n_daily_imports_mean":"n_daily_imports_0.975"),cumsum) %>% 
  filter(trav_rest=="Cordon sanitaire imposed",chunyun=="Chunyun",date==as.Date("2020-01-23")) %>% 
  print(n=Inf)
