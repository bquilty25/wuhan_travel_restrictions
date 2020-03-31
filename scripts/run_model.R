pacman::p_load(tidyverse,
               furrr,
               scales,
               forcats,
               outbreaks,
               projections,
               patchwork,
               tictoc,
               stringr)

source("scripts/utils.R")

plan(multiprocess)

top_cities <- read_csv("data/codes_20.csv")

four_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%filter(lvl=="prf") %>% st_drop_geometry() %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi")

  
inc <- read.csv("data/inferred_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(cases = ifelse(date > as.Date("2020-02-01"), 25362, cases))

plot_Reff_scenarios <- safely(function(cnty,Reff,nsim) {
  
  prf <- trav_from_wuhan(cnty, 50000) %>%
    pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                 names_to = "trav") %>% 
    mutate(trav_rest = case_when(
      trav == "trav_19" ~ "No travel restrictions",
      trav == "trav_20" ~ "Travel restrictions",
      trav == "trav_19non" ~ "No travel restrictions",
      trav == "trav_20non" ~ "Travel restrictions",
      TRUE ~ NA_character_
    )
    ) %>%
    mutate(
      chunyun = case_when(
        trav == "trav_19" ~ "Chunyun",
        trav == "trav_20" ~ "Chunyun",
        trav == "trav_19non" ~ "Non-Chunyun",
        trav == "trav_20non" ~ "Non-Chunyun",
        TRUE ~ NA_character_
      )
    ) %>% 
    unite("trav",c("trav_rest","chunyun"),remove=F,sep=", ")
  
  travel <- prf %>%
    select(date, trav, value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(inc, by = "date") %>%
    filter(!is.na(cases)) %>% 
    mutate(after_restrictions=ifelse(date>as.Date("2020-01-23"),"Seeded after restrictions","Seeded prior to restrictions"))
  
  
  inf <- tidyr::crossing(sim = 1:nsim,
                         travel) %>%
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
    uncount(n_daily_imports, .remove = FALSE) %>%
    ungroup() %>%
    mutate(row_id = row_number()) %>%
    group_by(sim,
             trav,
             after_restrictions,
             row_id) %>%
    nest() %>%
    mutate(bp = future_pmap(
      .f = bp_rt_fun,
      .l = list(
        df = data,
        Reff = Reff
      )
    ))
    
    return(inf)
})

gc()
tic()
Reff_inc_strat <- crossing(
  Reff = c(2.2, 1.1, 0.55),
  quantile_cities) %>%
  mutate(inc=future_pmap(
    .l = list(
      Reff = Reff,
      cnty = CNTY_CODE,
      nsim = 500
    ),.f=plot_Reff_scenarios,.progress = T))
toc()

saveRDS(Reff_inc_strat,file="output/incidence_four_cities_2.2_500sims.rds")

