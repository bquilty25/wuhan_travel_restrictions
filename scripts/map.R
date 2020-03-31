pacman::p_load(tidyverse,sf,mapview,ggrepel,stringr,colorspace)
devtools::install_github("yutannihilation/ggsflabel")
pacman::p_load(tidyverse,
               furrr,
               scales,
               forcats,
               outbreaks,
               projections,
               patchwork,
               tictoc)

source("scripts/utils.R")

plan(multiprocess)



all_china <- readRDS("data/china_shp.rds") %>% st_as_sf() %>% filter(lvl=="prf")
five_cities <- readRDS("data/china_shp.rds") %>% 
  st_as_sf() %>%
  filter(lvl=="prf") %>%
  filter(PYNAME=="Wuhan Shi"|PYNAME=="Beijing Shi"|PYNAME=="Chongqing Shi"|PYNAME=="Hangzhou Shi"|PYNAME=="Shenzhen Shi") %>% 
  mutate(epicentre=ifelse(PYNAME=="Wuhan Shi","Epicentre","Cities of interest"))

five_cities_labels <- five_cities %>% st_centroid()

world <- st_read("data/Countries_WGS84/Countries_WGS84.shp") 
world <- st_transform(world,st_crs(all_china))

lims <- st_bbox(all_china)

pal3 <- colorspace::qualitative_hcl(3, palette = "Dark 2")
ggplot()+
  geom_sf(data=world,fill="grey70",colour="white")+
  geom_sf(data=all_china,colour="white",fill=pal3[3],alpha=0.5)+
  geom_sf(data=five_cities,aes(fill=epicentre),colour="white",show.legend=FALSE,size=1)+
  geom_sf(data=five_cities_labels,colour="black",show.legend=FALSE,size=2)+
  ggsflabel::geom_sf_text_repel(data=five_cities,aes(label=stringr::word(PYNAME)), 
                                nudge_x = c(5,10,0,-10,10)/2, nudge_y = c(5,5,5,-5,-5)/2,
                                seed = 10)+
  scale_fill_manual(values=rev(pal3[1:2]))+
  theme_void()+
  coord_sf(xlim=c(lims$xmin,lims$xmax),ylim=c(lims$ymin,lims$ymax))+
ggsave(
  filename = "output/map.png",
  width = 297,
  height = 210,
  dpi = 320,
  units = "mm"
)