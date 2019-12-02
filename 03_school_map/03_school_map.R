
library(geojsonio)
library(tictoc)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(leaflet)
library(stringr)
library(readxl)
library(glue)

school_data <-
  here::here("00_data", "01_school_data", "shool_data.xlsx") %>% 
  read_xlsx()


city_polygon_file_name <-
  read.csv(here::here("02_city_polygon", "city_for_save.csv")) %>% 
  mutate_if(is.factor, as.character)
# glimpse(city_polygon_file_name)

city_polygon <-
  map(
    .x = pull(city_polygon_file_name, file_name),
    .f = ~{
      sf::read_sf(here::here("00_data", "00_city_polygon", paste0(.x, ".shp")))
    }
  )
# length(city_polygon)
city_polygon[[length(city_polygon) + 1]] <-
  sf::read_sf(here::here("00_data", "00_city_polygon", paste0("kyiv", ".shp")))




city_school_palette <-
  colorQuantile(
    palette = "Spectral", ##scico(10, palette = 'roma'),
    probs = seq(0, 1, length.out = 10),
    domain = school_data$count_student,
    reverse = TRUE
  )

map_school <- 
  school_data %>% 
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap, group  = "osm") %>% 
  addProviderTiles(providers$CartoDB.Positron, group  = "light") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group  = "dark")


SHOW_PRECINCTS <- FALSE
for (i in (1:length(city_polygon))) {
  
  if (SHOW_PRECINCTS) {
    pol_city <- city_polygon[[i]]
    stroke <- TRUE
    
  } else {
    pol_city <- st_union(city_polygon[[i]])
    stroke <- FALSE
  }
  map_school <-
    map_school %>%
    addPolygons(
      data = pol_city,
      stroke = stroke, 
      weight = 1,
      fillOpacity = 0.2,
      group = city_polygon[[i]] %>% pull(prec_city) %>% unique()
    )
  
}


map_school <- 
  map_school %>%
  addCircles(
    lng         = ~school_lng,
    lat         = ~school_lat,
    radius      = ~count_student / 4, #  / 100
    color       = ~city_school_palette(count_student),
    popup       = ~glue("{school_name}, {city_name}, {street_name}, к-сть учнів - {count_student}"),
    weight      = 1,
    fillOpacity = 0.4
  ) %>% 
  leaflet::addLayersControl(
    baseGroups = c("dark", "light", "osm"),
    overlayGroups = city_polygon_file_name$precinct_city_name
  )


map_school
 






