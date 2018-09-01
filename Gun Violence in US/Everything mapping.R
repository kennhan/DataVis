library(tidyverse)
library(lubridate)
library(ggmap)
library(ggthemes)
library(viridis)
library(sp)
library(maps)
library(maptools)


# Function: convert long and lat to county --------------------------------

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Input -------------------------------------------------------------------
og_data = read.csv('gun-violence-data_01-2013_03-2018.csv', stringsAsFactors = T)
glimpse(og_data)

# Dim reduction + data type -----------------------------------------------
data = og_data %>%
  select(-c(incident_url, source_url, incident_url_fields_missing, incident_characteristics, 
            notes, contains("participant_"),sources))

colnames(data)[13] = 'long'
colnames(data)[11] = 'lat'
glimpse(data)

data = mutate(data, long = ifelse(long > 0, long * -1, long)) 

#data$state = ifelse(str_detect(data$state, "District of Columbia"), "District of Columbia", data$state)

data$state[str_detect(data$state,"District of Columbia")]
# data$state = as.character(data$state)
# data$city_or_county = as.character(data$city_or_county)

filter(data, is.na(long)) %>%
  select(state, city_or_county) %>%
  head()

# Mapping -----------------------------------------------------------------

# _ Making city df ----
us.cities = us.cities

# ___ make usState with state abbreviation and state name ----
usState = bind_rows(tibble(state.abb, state.name),
        tibble('state.abb' = 'DC',state.name = 'District of Columbia'))

# ___ test data$state vs. us.cities$state ----
identical(sort(unique(data$state)),
          sort(unique(usState$state.name)))

# ___ make county.stat df ----
county.stat = data %>%
  select(n_killed, n_injured, long, lat) %>%
  filter(!is.na(long) & !is.na(lat))

county.stat$county = latlong2county(select(county.stat,long,lat))
county.stat = separate(county.stat, county, c('state','county'), sep = ",")

county.stat = county.stat %>%
  group_by(state, county) %>%
  summarize(killed = sum(n_killed),
            injured = sum(n_injured))

#verify:
glimpse(county.stat)
unique(county.stat$state)

# _ remove state NA ----
filter(county.stat, is.na(state))
county.stat = filter(county.stat, !is.na(state))

# _ merge with county_df ----
county_df <- map_data("county", projection = "albers", parameters = c(39, 45))
county_df = left_join(county_df, county.stat, by = c('region' = 'state', 
                                                     'subregion' = 'county')) 


# _ get state_df ---- 
state_df <- map_data("state", projection = "albers", parameters = c(39, 45))

# _ now mapping ----
county_df2 = slice(county_df, 8000:10000)

centroid = county_df %>%
  group_by(region, subregion) %>%
  summarize(lat = max(lat),
            long = long[which.max(lat)],
            killed = mean(killed)) %>%
  arrange(desc(killed))
glimpse(centroid)

centroid$subregion[15:nrow(centroid)] = NA

ggplot() +
  geom_polygon(data=county_df, 
               mapping=aes(x=long, y=lat, group=group, fill = killed),
               color = alpha("grey50", 1/4), 
               size = 0.25) +
  geom_polygon(data = state_df,
               mapping = aes(x = long, y=lat, group=group),
               color = "grey30",
               size = 0.5,
               fill = NA) +
  geom_text(data = centroid,
            mapping = aes(x=long, y=lat, label = subregion),
            color=alpha('white',0.7),
            size = 4) +
  coord_map() +
  scale_fill_viridis(option = 'inferno', 
                     na.value = 'grey20',
                     name = 'Headcount') +
  scale_color_viridis(option = 'inferno') +
  ggtitle("Top 15 Counties by Number of People Killed by Gun Violence") +
  theme(line = element_blank(),
        title = element_text(color='white'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(color='white'),
        legend.text = element_text(color='white'),
        panel.background = element_rect('black'),
        plot.background = element_rect('black'),
        legend.background = element_rect('black')) +
  ggsave('killed_by_county.jpg', width = 15, height = 9)




# _ zoom in Cook ------------------------------------------------------------
cook_polyg = map_data(map = 'county', region = 'illinois', projection = "albers", parameters = c(39, 45)) %>%
  filter(subregion == 'cook')

cook_map = get_map(location = c(lon = -87.9415106, lat = 41.8372408), 
                   zoom = 9)
ggmap(cook_map)

cook_data = data %>%
  filter(state == 'Illinois')


ggmap(cook_map) +
  geom_polygon(data = cook_polyg, aes(x = long, y = lat)) + 
  geom_point(data = cook_data, mapping = aes(x = long, y = lat))


glimpse(data)
