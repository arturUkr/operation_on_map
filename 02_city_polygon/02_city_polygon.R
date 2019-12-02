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

path_data <- "00_data"




tic("# read city for save")

city_for_save <-
  read.csv(here::here("02_city_polygon", "city_for_save.csv"))

toc()   # read city for save: 0.03 sec elapsed
# city_for_save
# A tibble: 34 x 2
# --------------------------------------------





tic("# read precinct address info")

df_precinct_address <-
  here::here(path_data, "precinct_address_list.xlsx") %>% 
  read_xlsx(sheet = "precinct_address_list")

toc()   # read precinct address info: 0.37 sec elapsed
# glimpse(df_precinct_address)
# df_precinct_address
# A tibble: 29,695 x 7   # without foreign precincts
# --------------------------------------------






tic("# read precinct info")

df_precinct <-
  read.csv(here::here(path_data, "df_dim_precinct_information.csv")) %>% 
  as_tibble()

toc() # read precinct info: 0.63 sec elapsed
# glimpse(df_precinct)
# df_precinct
# A tibble: 29,793 x 14


df_precinct <- 
  df_precinct %>% 
  mutate_if(is.factor, as.character)


df_precinct <-
  df_precinct %>% 
  inner_join(
    df_precinct_address %>% distinct(), 
    by = "precinct_address"
  )
# glimpse(df_precinct)
# A tibble: 29,693 x 20
# --------------------------------------------





tic()

precinct_polygon <- 
  geojsonio::geojson_read(here::here(path_data, "precincts.json"), what = "sp")

toc() # 45.64 sec elapsed

# --------------------------------------------




tic("# convert polygons to sf object")

pol_sf <- 
  sf::st_as_sf(precinct_polygon)
  
toc() # convert polygons to sf object: 4.28 sec elapsed

# --------------------------------------------


pol_sf <- 
  pol_sf %>% 
  mutate(name = as.integer(as.character(name))) %>%
  rename(precinct_id = name)
# dim(pol_sf)   # 32 421     2
# glimpse(pol_sf)
# glimpse(df_precinct)


pol_sf <- 
  pol_sf %>% 
  inner_join(
    df_precinct,
    by = "precinct_id"
  )
# dim(pol_sf)   # 28 943     15
# glimpse(pol_sf)


pol_sf %>% 
  filter(precinct_city_name == "м.Київ") %>%
  # filter(precinct_city_name == "м.Вінниця", precinct_region_name == "Вінницька обл.") %>%
  # filter(precinct_city_name == "м.Луцьк", precinct_region_name == "Волинська обл.") %>%
  # filter(precinct_city_name == "м.Кривий Ріг", precinct_region_name == "Дніпропетровська обл.") %>%
  # filter(precinct_city_name == "м.Кам’янське", precinct_region_name == "Дніпропетровська обл.") %>%
  # filter(precinct_city_name == "м.Дніпро", precinct_region_name == "Дніпропетровська обл.") %>%
  # filter(precinct_city_name == "м.Нікополь", precinct_region_name == "Дніпропетровська обл.") %>%
  # filter(precinct_city_name == "м.Павлоград", precinct_region_name == "Дніпропетровська обл.") %>%
  # filter(precinct_city_name == "м.Краматорськ", precinct_region_name == "Донецька обл.") %>%
  # filter(precinct_city_name == "м.Маріуполь", precinct_region_name == "Донецька обл.") %>%
  # filter(precinct_city_name == "м.Слов’янськ", precinct_region_name == "Донецька обл.") %>%
  # filter(precinct_city_name == "м.Житомир", precinct_region_name == "Житомирська обл.") %>%
  # filter(precinct_city_name == "м.Бердичів", precinct_region_name == "Житомирська обл.") %>%
  # filter(precinct_city_name == "м.Мукачево", precinct_region_name == "Закарпатська обл.") %>%
  # filter(precinct_city_name == "м.Ужгород", precinct_region_name == "Закарпатська обл.") %>%
  # filter(precinct_city_name == "м.Бердянськ", precinct_region_name == "Запорізька обл.") %>%
  # filter(precinct_city_name == "м.Запоріжжя", precinct_region_name == "Запорізька обл.") %>%
  # filter(precinct_city_name == "м.Мелітополь", precinct_region_name == "Запорізька обл.") %>%
  # filter(precinct_city_name == "м.Івано-Франківськ", precinct_region_name == "Івано-Франківська обл.") %>%
  # filter(precinct_city_name == "м.Бровари", precinct_region_name == "Київська обл.") %>%
  # filter(precinct_city_name == "м.Біла Церква", precinct_region_name == "Київська обл.") %>%
  # filter(precinct_city_name == "м.Кропивницький", precinct_region_name == "Кіровоградська обл.") %>%
  # filter(precinct_city_name == "м.Львів", precinct_region_name == "Львівська обл.") %>%
  # filter(precinct_city_name == "м.Лисичанськ", precinct_region_name == "Луганська обл.") %>%
  # filter(precinct_city_name == "м.Сєвєродонецьк", precinct_region_name == "Луганська обл.") %>%
  # filter(precinct_city_name == "м.Миколаїв", precinct_region_name == "Миколаївська обл.") %>%
  # filter(precinct_city_name == "м.Одеса", precinct_region_name == "Одеська обл.") %>%
  # filter(precinct_city_name == "м.Полтава", precinct_region_name == "Полтавська обл.") %>%
  # filter(precinct_city_name == "м.Кременчук", precinct_region_name == "Полтавська обл.") %>%
  # filter(precinct_city_name == "м.Рівне", precinct_region_name == "Рівненська обл.") %>%
  # filter(precinct_city_name == "м.Суми", precinct_region_name == "Сумська обл.") %>%
  # filter(precinct_city_name == "м.Тернопіль", precinct_region_name == "Тернопільська обл.") %>%
  # filter(precinct_city_name == "м.Харків", precinct_region_name == "Харківська обл.") %>%
  # filter(precinct_city_name == "м.Херсон", precinct_region_name == "Херсонська обл.") %>%
  # filter(precinct_city_name == "м.Хмельницький", precinct_region_name == "Хмельницька обл.") %>%
  # filter(precinct_city_name == "м.Кам’янець-Подільський", precinct_region_name == "Хмельницька обл.") %>%
  # filter(precinct_city_name == "м.Черкаси", precinct_region_name == "Черкаська обл.") %>%
  # filter(precinct_city_name == "м.Чернівці", precinct_region_name == "Чернівецька обл.") %>%
  # filter(precinct_city_name == "м.Чернігів", precinct_region_name == "Чернігівська обл.") %>%
  st_union() %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(stroke = F, fillOpacity = 0.5)






SAVE_FILE <- TRUE

tic("# cut city polygon")
polygon_for_save <- 
  city_for_save %>% 
  mutate(row_id = row_number()) %>% 
  nest(city_inf = c(precinct_city_name, precinct_region_name, file_name)) %>% 
  mutate(
    city_polygon = 
      map2(
        .x = city_inf,
        .y = row_id,
        .f = ~{
          tic(glue("# {.y} - {.x$precinct_city_name} - {.x$precinct_region_name}"))

          result <- 
            pol_sf  %>% 
            filter(
              precinct_city_name    == .x$precinct_city_name, 
              precinct_region_name  == .x$precinct_region_name
            ) %>% 
            select(
              prec_id    = precinct_id,
              distr_id   = district_id,
              vote_plan  = voter_count_plan,
              vote_real  = voter_count_real,
              reg_id     = region_id,
              reg_name   = region_name,
              prec_lat   = precinct_lat,
              prec_lng   = precinct_lng,
              prec_km    = precinct_area_km,
              prec_str   = precinct_street,
              prec_str_n = precinct_street_number,
              prec_city  = precinct_city_name,
              prec_area  = precinct_area_name,
              prec_reg   = precinct_region_name
            )
          
          if (SAVE_FILE) {
            
            result %>% 
              sf::write_sf(
                here::here("00_data", "00_city_polygon", paste0(.x$file_name, ".shp")), 
                layer_options = "ENCODING=UTF-8"
              ) 
            
          }

          toc()
          return(result)
        }
      )
  )
toc()   # cut city polygon: 2.14 sec elapsed
# polygon_for_save
# A tibble: 34 x 3


pol_sf %>% 
  filter(precinct_city_name == "м.Київ") %>% 
  select(
    prec_id    = precinct_id,
    distr_id   = district_id,
    vote_plan  = voter_count_plan,
    vote_real  = voter_count_real,
    reg_id     = region_id,
    reg_name   = region_name,
    prec_lat   = precinct_lat,
    prec_lng   = precinct_lng,
    prec_km    = precinct_area_km,
    prec_str   = precinct_street,
    prec_str_n = precinct_street_number,
    prec_city  = precinct_city_name,
    prec_area  = precinct_area_name,
    prec_reg   = precinct_region_name
  ) %>% 
  sf::write_sf(
    here::here("00_data", "00_city_polygon", paste0("kyiv", ".shp")), 
    layer_options = "ENCODING=UTF-8"
  )




