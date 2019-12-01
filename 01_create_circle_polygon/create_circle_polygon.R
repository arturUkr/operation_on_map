
library(sf)
library(units)
library(leaflet)
library(leaflet.extras)



create_circle_polygon <-
  function(
    lon,
    lat,
    radius
  ) {
    
    point <- st_point(x = c(lon, lat))
    point <- st_sfc(point, crs = 4326) 
    
    radius <- units::set_units(radius, m)
    
    point <- st_transform(point, 32637)  # for calc in meters 
    point_buffer <- st_buffer(point, radius)
    
    point <- st_transform(point, 4326)
    point_buffer <- st_transform(point_buffer, 4326)
    
    return(point_buffer)
  }





create_circle_polygon(
  lon = 30.601773,
  lat = 50.427653,
  radius = 1000
) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(stroke = F) %>% 
  addMarkers(lng = 30.601773, lat = 50.427653) %>% 
  addMeasure(
    primaryLengthUnit = "meters"
  )