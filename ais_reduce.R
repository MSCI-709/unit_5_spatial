# Reduce AIS data to manageable size for student use
# Load in .csv file of Jan. 2017 AIS points in UTM Zone 17
# Crop to a bounding box around FL and GA
# Remove duplicate rows and rows with no CallSign
# Keep only ships with status "under way using engine"
# Collapse lat/lon points into spatial lines
# Write out as shapefile

library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(lubridate)  # ymd_hms

####################################################
#     Transform lat/lon point (ais detections) 
#       data into line (ship track) data
####################################################

## AIS data; Downloaded for January 2017, UTM Zone 17
# https://marinecadastre.gov/AIS/

ais = read.csv("data_too_big_for_students/raw/AIS_UTM17_Jan2017_4GB/2017_v2/AIS_2017_01_Zone17.csv") #data/AIS_ASCII_by_UTM_Month/2017_v2/AIS_2017_01_Zone17.csv")
head(ais)
dim(ais)  # 31,884,021 rows (woah!)

lat_bounds = c(25, 34)
lon_bounds = c( -82, -76)

Sys.time() # timestamp
ais_lines = ais %>% 
  distinct(.keep_all=TRUE) %>%
  filter(LAT <= 34, 
         LAT >= 25,
         LON >= -82,
         LON <= -76,
         Status == "under way using engine",
         CallSign != "") %>%
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  st_as_sf(coords=c("LON", "LAT"), crs='+proj=longlat +datum=WGS84') %>%
  arrange(date_time) %>% # ensure ship tracks points are in chronological order
  group_by(CallSign, Length) %>%
  summarise(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points
  st_cast("LINESTRING") 
Sys.time() # took 10.5 minutes to run

head(ais_lines)

st_write(ais_lines, dsn="data/too_big_for_students/processed/ais_lines_jan_2017/ais_lines_jan_2017.shp")


############################################
#         Plot AIS lines
############################################

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# plot critical habitats and carcass locations
ais_map_jan_2017_lines = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  # geom_polygon(data = CAN_crit_hab, aes(x = longitude, y = latitude, fill = critical_habitat), alpha = 0.3) + # add poly
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, color=NA, fill='yellow') +
  # geom_sf(data=ais1, aes(color=CallSign)) +
  geom_sf(data=ais_lines, aes(color=CallSign)) +
  # geom_point(data = ais1, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color=FALSE) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 


ggsave(ais_map_jan_2017_lines, filename='figures/ais_map_jan_lines.pdf', device="pdf", height=5, width=7)


####################################################
#  Filter original Jan 2017 AIS data to just
#  one day (2021-1-25), ships underway and 
#  ships with a call sign
####################################################

ais_filter = ais %>% 
  distinct(.keep_all=TRUE) %>%
  filter(LAT <= 34, 
         LAT >= 25,
         LON >= -82,
         LON <= -76,
         Status == "under way using engine",
         CallSign != "") %>%
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  filter(date_time > ymd_hms("2017-01-25 00:00:00"), 
         date_time < ymd_hms("2017-01-25 11:59:59")) 

head(ais_filter)
dim(ais_filter)  # 119,726 rows

write_csv(ais_filter %>% select(-date_time), file='data/processed_ais/ais_2017-01-25.csv')










