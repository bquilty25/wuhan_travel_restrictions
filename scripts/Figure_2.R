pacman::p_load(tidyverse,
               furrr,
               scales,
               forcats,
               outbreaks,
               projections,
               patchwork,
               tictoc,
               ggthemes)

source("scripts/utils.R")

plan(multiprocess)

top_cities <- read_csv("data/codes_20.csv")

four_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>%
  st_drop_geometry() %>% 
  filter(PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi")

inc <- read.csv("data/inferred_data.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(cases = ifelse(date > as.Date("2020-02-01"), 25362, cases))

arrivals_plot <- function(cnty,name) {
  prf <- trav_from_wuhan(cnty, 50000) %>%
    pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                 names_to = "trav") %>% 
    mutate(trav_rest = case_when(
      trav == "trav_19" ~ "No travel restrictions",
      trav == "trav_20" ~ "Travel restrictions enacted",
      trav == "trav_19non" ~ "No travel restrictions",
      trav == "trav_20non" ~ "Travel restrictions enacted",
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
    select(date, trav_rest, chunyun, value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(inc, by = "date") %>%
    filter(!is.na(cases)) 
  
  
  imp <- tidyr::crossing(sim = 1:1000,
                         travel) %>%
    mutate(draws = 1,
           n_daily_imports = as.integer(future_pmap_dbl(
             .f = n_infected,
             .l = list(
               cases = cases,
               pop = wuhan_pop,
               lambda = value,
               n = draws)))) %>% 
    as.data.frame()
  
   return(imp)
}

imp <- four_cities %>%
  mutate(arrivals = future_pmap(.l = list(cnty = CNTY_CODE,
                                          name = PYNAME), 
                                .f =
                                  arrivals_plot))

imp_sum <- imp %>%
  unnest() %>% 
  group_by(PYNAME,date,trav_rest,chunyun) %>% 
  nest() %>% 
mutate(imports = future_map(.x=data,.f=summarise_imports)) %>% 
  unnest(imports) %>% 
mutate(before=fct_rev(before),
       trav_rest=fct_rev(trav_rest))

infected_arrivals <- imp %>%
  unnest() %>% 
  group_by(PYNAME,date,trav_rest,chunyun) %>% 
  nest() %>% 
  mutate(imports = future_map(.x=data,.f=summarise_imports)) %>% 
  unnest(imports) %>% 
  mutate(week=tsibble::yearweek(date),
         weekday=factor(weekdays(date,T), levels = rev(c("Mon", "Tue", "Wed", "Thu",
                                                         "Fri", "Sat", "Sun"))),
         day=ifelse(weekday=="Mon",day(date),NA)) %>% 
  ggplot(aes(x = week, y = weekday)) +
  scale_fill_viridis_c(name="Estimated median number of daily \ninfected arrivals from Wuhan",option="plasma", na.value = "grey93",begin=0.1,end=0.8,breaks=c(2,4,6,8),guide = guide_coloursteps(show.limits = T,barwidth=unit(5,"cm"))) +
  geom_tile(aes(fill = as.integer(n_daily_imports_median)),size=0.4,colour="white") +
  geom_text(aes(label = day),colour="white",size=1.5)+
  geom_point(data=. %>% filter(date==as.Date("2020-01-23"),trav_rest=="Travel restrictions enacted"),aes(x=week,y=weekday,fill=NA_real_,colour = "23 Jan 2020 - travel restrictions imposed"), size = 1.5,pch=16)+
  scale_colour_manual(values="white",name="")+
  facet_nested(stringr::word(PYNAME)~chunyun+fct_rev(trav_rest)) +
  scale_x_date()+
  coord_fixed(ratio=7)+
  theme_tufte(base_family = NULL)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical")


### index by four cities ----
index_plot <- function(cnty,name) {
  prf <- trav_from_wuhan(cnty, 50000) %>%
    pivot_longer(cols = c(trav_19, trav_20, trav_19non, trav_20non),
                 names_to = "trav") %>% 
    mutate(trav_rest = case_when(
      trav == "trav_19" ~ "No travel restrictions",
      trav == "trav_20" ~ "Travel restrictions enacted",
      trav == "trav_19non" ~ "No travel restrictions",
      trav == "trav_20non" ~ "Travel restrictions enacted",
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
    select(date, trav_rest, chunyun, value, -day) %>%
    mutate(wuhan_pop = 12894578) %>%
    left_join(inc, by = "date") %>%
    filter(!is.na(cases)) 
  return(travel)
}

all_arrivals <- four_cities %>% 
  select(PYNAME,CNTY_CODE) %>% 
  mutate(index=future_pmap(.f=index_plot,.l=list(cnty=CNTY_CODE,name=PYNAME))) %>% 
  unnest() %>% 
  mutate(week=tsibble::yearweek(date),
         weekday=factor(weekdays(date,T), levels = rev(c("Mon", "Tue", "Wed", "Thu",
                                                     "Fri", "Sat", "Sun"))),
         day=ifelse(weekday=="Mon",day(date),NA)) %>% 
  #mutate(value=na_if(value,0)) %>% 
ggplot(aes(x = week, y = weekday)) +
 scale_fill_viridis_c(name="Estimated number of daily \narrivals from Wuhan",option="viridis", breaks=c(2000,4000,6000), guide=guide_coloursteps(show.limits = T,barwidth=unit(5,"cm")),
                     na.value = "grey93") +
  geom_tile(aes(fill = value),size=0.4,colour="white") +
  geom_text(aes(label = day),colour="white",size=1.5)+
  geom_point(data=. %>% filter(date==as.Date("2020-01-23"),trav_rest=="Travel restrictions enacted"),aes(x=week,y=weekday,fill=NA_real_,colour = "23 Jan 2020 - travel restrictions imposed"), size = 1.5,pch=16)+
  scale_colour_manual(values="white",name="")+
  facet_nested(stringr::word(PYNAME)~chunyun+fct_rev(trav_rest)) +
  scale_x_date()+
  coord_fixed(ratio=7)+
  theme_tufte(base_family = NULL)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical")


all_arrivals/infected_arrivals+plot_annotation(tag_levels = c("A","B"))+plot_layout(guides = "collect",ncol=1)& theme(legend.position = 'bottom')

ggsave(
  filename = "output/arrivals_plot_v3.png",
  width = 210,
  height = 297,
  dpi = 320,
  units = "mm"
)