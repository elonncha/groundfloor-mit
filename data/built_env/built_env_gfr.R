#MIT GFR project Built Env Vars calculation
setwd("E:\\Ellen\\MIT_ground")
library(sf)
library(dplyr)
library(tidyverse)

#------------prepareed the source data---------------

#base map of two relavant counties w/ landuse (land_use_2), residential units# (du), office area (bldg_are_9)
ft_base <- st_read("fulton\\Base Canvas.shp")
dk_base <- st_read("dekalb\\Base Canvas.shp")
base <- rbind(ft_base, dk_base) #or read in the clipped base_DF.shp

#import GFR csv and convert to spatial points
gfr_df <- read.csv("data\\places_studyarea_retail.csv")
gfr_sf <- st_as_sf(gfr_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) 

#read building footprint
flt_bld <- st_read("E:\\Ellen\\MIT_ground\\fulton\\bld\\bld_cliped.shp")
dk_bld <- st_read("E:\\Ellen\\MIT_ground\\dekalb\\dk_bld.shp")
bld_fp <- st_read("data\\bld_DKFT.shp") # or read in the clipped bld_gfr_clip.shp file

#read road network
roads <- st_read("data\\rd22_5county.shp") #or read in the clipped rd_gfr_clip.shp file

#read transit stops
stp_df <- read.csv("data\\Transit_Stops_2019.csv")
stp_sf <- st_as_sf(stp_df, coords = c("ï..X", "Y"), crs = 32616) 

#read sidewalk/bike lane
trail <- st_read("E:\\Ellen\\MIT_ground\\dekalb\\BikePed\\Bicycle and Pedestrian.shp") #or read in the trail.shp file

#-------------Analysis-----------------------
#--------1.5 buffer around GFR--------
gfr_prj <- st_transform(gfr_sf, crs = 32616)
buffer_gfr <- st_buffer(gfr_prj, dist = 2414.01) #the unit is meter here
buffer_gfr_ori_crs <- st_transform(buffer_gfr, st_crs(gfr_sf)) #make this trans for every layer

#-----Average building fp size and built area ratio of each buffer-----
crs_target <- st_crs(buffer_gfr_ori_crs)
bld_fp <- st_transform(bld_fp, crs_target)
bld_intersect <- st_intersection(bld_fp, buffer_gfr_ori_crs) 
buildings_in_buffers <- st_join(bld_intersect, buffer_gfr_ori_crs, join = st_intersects)
building_summary <- buildings_in_buffers %>%
  group_by(PLACEKEY) %>% 
  summarise(total_area_sqft = sum(area, na.rm = TRUE)) %>%
  ungroup()

building_summary <- building_summary %>%
  mutate(
    total_area_sqmi = sum_Area_SQUAREFEET / 2.788e+7,  # 1 square mile is approximately 27,878,400 square feet
    ratio = total_area_sqmi / 7.07 #area of circle in sqmi with radius of 1.5 mi
  ) 

#------Total road length------
crs_target <- st_crs(buffer_gfr_ori_crs)
roads <- st_transform(roads, crs_target)
roads_in_buffers <- st_intersection(roads, buffer_gfr_ori_crs)
roads_in_buffers$length <- st_length(roads_in_buffers)
rd_sum <- roads_in_buffers %>%
  group_by(PLACEKEY) %>% 
  summarise(total_length = sum(length) * 0.000621371) %>% #meter to mile
  ungroup()
  
final_data <- left_join(building_summary, rd_sum, by = "PLACEKEY")

#--------calculate average setback.--------
rd_intersect <- roads
intersection <- buildings_in_buffers

distances <- vector("numeric", length = nrow(intersection))
for (i in seq_len(nrow(intersection))) {
  distances[i] <- min(st_distance(intersection[i, ], rd_intersect))
}
intersection$setback <- distances

mean_setback <- intersection%>%
  group_by(PLACEKEY) %>%
  summarize(mean_distance = mean(dist_to_street, na.rm = TRUE))

final_data <- left_join(final_data, mean_setback, by = "PLACEKEY")


#-------Landuse Shannon's Entropy--------
intersection <- read.csv("data\\performance-result\\gfr_base.csv") 
### this is big so stored outside, this is done in 1.5 mi buffer
landuse_areas_within_buffer <- intersection %>%
  group_by(PLACEKEY, land_use_2) %>% 
  summarise(total_area = sum(area_gross)) %>%
  ungroup()

buffer_areas <- landuse_areas_within_buffer %>%
  group_by(PLACEKEY) %>%
  summarise(total_buffer_area = sum(total_area))

landuse_areas_within_buffer <- left_join(landuse_areas_within_buffer, buffer_areas, by = "PLACEKEY") %>%
  mutate(proportion = total_area / total_buffer_area)

shannon_entropy <- landuse_areas_within_buffer %>%
  group_by(PLACEKEY) %>%
  summarise(entropy = -sum(proportion * log(proportion))) %>%
  ungroup()
final_data <- left_join(final_data, shannon_entropy, by = "PLACEKEY")

#---------intersection number in buffers-----------
combined_roads <- st_union(roads)
intersection_points <- st_intersection(combined_roads, combined_roads) %>% 
  st_cast("POINT")
intersections_within_buffer <- st_intersection(intersection_points, buffer_gfr_ori_crs)
count_intersections <- intersections_within_buffer %>%
  group_by(PLACEKEY) %>%  
  summarise(num_intersections = n()) %>%
  ungroup()

final_data <- left_join(final_data, count_intersections, by = "PLACEKEY")

#----------Transit stop number in buffers--------------
stops_in_buffer <- st_join(stp_sf, buffer_gfr_ori_crs, join = st_within)

count_stops <- stops_in_buffer %>%
  group_by(PLACEKEY) %>%
  summarise(num_stops = n())

final_data <- left_join(final_data, count_stops, by = "PLACEKEY")

#--------------Total length of sidewalk/bike lanes-----------
trail_intersection <- st_intersection(trail, buffer_gfr_ori_crs)
trail_intersection <- trail_intersection %>%
  mutate(length_m = st_length(geometry))

trail_length_summary <- trail_intersection %>%
  group_by(PLACEKEY) %>%
  summarise(total_length_m = sum(length_m) * 0.000621371) #meter to mile

final_data <- left_join(final_data, trail_length_summary, by = "PLACEKEY")

#----------residential unit number---------------
df <- read.csv("data\\performance-result\\bld_gfr.csv")
#intersection <- st_intersection(bld_fp, base)
#intersection <- intersection %>%
  #mutate(area_bld = st_area(geometry))

# Calculate the total area for each summary zone
total_area_per_zone <- df %>%
  group_by(id) %>%
  summarize(total_area = sum(area, na.rm = TRUE))

# Join total area back to the original df
df <- left_join(df, total_area_per_zone, by = "id")

# Calculate the ratio of each building's area to its summary zone's total area
df$ratio <- df$area / df$total_area

# Assign the du value to each building based on its ratio
df$assigned_du <- df$du * df$ratio

# Summarize the total assigned du value within each buffer zone
summary_per_buffer <- df %>%
  group_by(PLACEKEY) %>%
  summarize(total_assigned_du = sum(assigned_du, na.rm = TRUE))

final_data <- left_join(final_data, summary_per_buffer, by = "PLACEKEY")

#----write data----
#write.csv(final_data, "data\\final_BE_gfr.csv")
