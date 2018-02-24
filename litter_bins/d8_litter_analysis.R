library(tidyverse)
library(sf)
library(magrittr)
library(geosphere)
library(leaflet)

litter_bins <- st_read('litter_bins', 'Litter_Bins') %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  mutate(x_val = map_dbl(geometry, function(d) d[1]),
         y_val =  map_dbl(geometry, function(d) d[2]))

litter_bin_survey <- read_csv('bin_repeat_1.csv') %>% 
  rename(lat = y, long = x, type = `What type of litter bin?`, condition = `What is it's condition?`) %>% 
  mutate(label = map2(type, condition, function(t, c){
    htmltools::HTML(str_glue('<strong>Type:</strong> {t}<br><strong>Condition</strong> {c}'))
  }))


suggested_bins <- read_csv('Needed_bins.csv')

bardstown_litter <- litter_bins %>% 
  filter(ROADNAME %in% c('BARDSTOWN RD', 'BAXTER AVE')) %>% 
  mutate(closest_id = pmap_chr(list(x_val, y_val, OBJECTID), function(x, y, object){
    others <- filter(bardstown_litter, OBJECTID != object)
    others <- others %>% 
      mutate(dist = map2_dbl(x_val, y_val, function(other_x, other_y){
        as.numeric(distGeo(c(x,y), c(other_x,other_y)))
        }))
    min_dist <- min(others$dist)
    vals <- others %>% 
      filter(dist == min_dist)
    vals$OBJECTID
  }),
  closest_dist = pmap_dbl(list(x_val, y_val, OBJECTID), function(x, y, object){
    others <- filter(bardstown_litter, OBJECTID != object)
    others <- others %>% 
      mutate(dist = map2_dbl(x_val, y_val, function(other_x, other_y){
        as.numeric(distGeo(c(x,y), c(other_x,other_y)))
      }))
    min_dist <- min(others$dist)
    vals <- others %>% 
      filter(dist == min_dist)
    vals$dist[1]
  }))

liba <- st_read(dsn = 'LIBA_members', 'LIBA_members') %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  mutate(lat = map_dbl(geometry, function(g) g[[2]]),
         long = map_dbl(geometry, function(g) g[[1]])) %>% 
  filter(str_detect(ARC_Street, 'Bardstown') | str_detect(ARC_Street, 'Baxter'),
         lat > 38.2108) %>% 
  mutate(closest_litter = map2_chr(long, lat, function(liba_long, liba_lat){
    dist_litter <- litter_bin_survey %>% 
      select(ObjectID, long, lat) %>% 
      mutate(dist = map2_dbl(long, lat, function(lit_long, lit_lat){
        as.numeric(distm(c(lit_long, lit_lat), c(liba_long, liba_lat)))
      }))
    min_dist <- min(dist_litter$dist)
    vals <- dist_litter %>% filter(dist == min_dist)
    vals$ObjectID[1]
  }),
  closest_litter_dist = map2_dbl(long, lat, function(liba_long, liba_lat){
    dist_litter <- litter_bin_survey %>% 
      select(ObjectID, long, lat)%>% 
      mutate(dist = map2_dbl(long, lat, function(lit_long, lit_lat){
        as.numeric(distm(c(lit_long, lit_lat), c(liba_long, liba_lat)))
      }))
    min_dist <- min(dist_litter$dist)
    vals <- dist_litter %>% filter(dist == min_dist)
    vals$dist[1]
  }),
  suggest_litter_dist = map2_dbl(long, lat, function(liba_long, liba_lat){
    dist_litter <- tibble(OBJECTID = as.character(litter_bin_survey$ObjectID), 
                          long = litter_bin_survey$long,
                          lat = litter_bin_survey$lat) %>% 
      bind_rows(tibble(OBJECTID = as.character(suggested_bins$Needed),
               long = suggested_bins$Longitude,
               lat = suggested_bins$Latitude)) %>% 
      mutate(dist = map2_dbl(long, lat, function(lit_long, lit_lat){
        as.numeric(distm(c(lit_long, lit_lat), c(liba_long, liba_lat)))
      }))
    min_dist <- min(dist_litter$dist)
    vals <- dist_litter %>% filter(dist == min_dist)
    vals$dist[1]
  }),
  label = pmap(list(Match_addr, closest_litter_dist, suggest_litter_dist),
               function(st, closest, suggest){
                 htmltools::HTML(str_glue('<strong>Distance to Closest Bin:</strong> {round(closest,2)}<br>
                                          <strong>Distance to Suggested Closest Bin:</strong> {round(suggest, 2)}'))
               }))


trash_icon <- makeAwesomeIcon(icon = 'trash', library = 'fa', markerColor = 'blue')
suggested_trash <- makeAwesomeIcon(icon = 'trash', library = 'fa', markerColor = 'green')
business_icon <- makeAwesomeIcon(icon = 'dollar', library = 'fa', markerColor = 'red')

d8_litter_map <- leaflet() %>% 
  addTiles() %>%
  addAwesomeMarkers(data = litter_bin_survey, ~long, ~lat, popup = ~label, icon = trash_icon) %>% 
  addAwesomeMarkers(data = liba, ~long, ~lat, label = ~ARC_Street, popup = ~label, icon = business_icon) %>% 
  addAwesomeMarkers(data = suggested_bins, ~Longitude, ~Latitude, icon = suggested_trash)
write_rds(d8_litter_map, 'litter_map.rds')


liba %>%  
  as_tibble() %>% 
  filter(closest_litter_dist != suggest_litter_dist) %>%
  select(closest_litter_dist, suggest_litter_dist) %>% 
  summarize_all(mean)
  
             