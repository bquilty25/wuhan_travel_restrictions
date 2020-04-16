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

plan(multiprocess)

top_cities <- read_csv("data/codes_20.csv")

four_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>% 
  st_drop_geometry() %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi")

quartile_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>% 
  st_drop_geometry() %>% 
  filter(CNTY_CODE==411700|
           CNTY_CODE == 610900 |
           CNTY_CODE == 371000 |
           CNTY_CODE == 211300 |
           CNTY_CODE == 542500) %>% 
  mutate(quartile=c("25%","75%","50%","97.5%","2.5%"))

  
inc <- read.csv("data/inferred_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(cases = ifelse(date > as.Date("2020-02-01"), 25362, cases))

plot_Reff_scenarios <- safely(function(cnty,Reff,k,nsim) {
  
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
    mutate(after_restrictions=ifelse(date>as.Date("2020-01-23"),"Seeded after restrictions","Seeded prior to restrictions")) %>% 
    as.data.frame()
  
  
  inf <- tidyr::crossing(sim = 1:nsim,
                         travel) %>% 
    #lazy_dt() %>% 
    mutate(draws = 1,
           n_daily_imports = as.integer(future_pmap_dbl(
             .f = n_infected,
             .l = list(
               cases = cases,
               pop = wuhan_pop,
               lambda = value,
               n = draws
             )
           ))) %>%
    group_by(sim,
             trav,
             date,
             after_restrictions) %>%
    arrange(sim,trav, date) %>%
    lazy_dt() %>% 
    dt_uncount(n_daily_imports, .remove = FALSE) %>%
    ungroup() %>%
    mutate(row_id = row_number()) %>%
    dt_nest(sim,
            trav,
            after_restrictions,
            row_id) %>%
    mutate(bp = future_pmap(
      .f = bp_rt_fun,
      .l = list(
        df = data,
        Reff = Reff,
        k=k
      )
    )) %>%  
    as.data.frame() %>% 
    select(-data)
    
    return(inf)
})

gc()
tic()
model_run <- crossing(Reff = c(2.2, 1.1, 0.55), 
                           k=c(0.54), 
                      quartile_cities) %>%
  mutate(inc=future_pmap(
    .l = list(
      k=k,
      Reff = Reff,
      cnty = CNTY_CODE,
      nsim = 200
    ),.f=plot_Reff_scenarios,.progress = T))%>% 
  unnest(inc) %>% 
  drop_na()
toc()


saveRDS(model_run,file="output/model_run_quartile_cities.rds")

