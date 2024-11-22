library(tidyverse)
library(sf)
library(tigris)
library(maps)
library(ggthemes)
library(tidycensus)
library(httr)
library(jsonlite)
library(ggpubr)
options(tigris_use_cache = TRUE)

# LinkNYC Data and POI data download
x <- read_csv("/Users/jadenthomas/Downloads/LinkNYC_Kiosk_Locations (1).csv")
poi <- read_csv("/Users/jadenthomas/Downloads/Points_of_Interest_20241119.csv")

# Get Lat and Long in useful form
poi <- poi %>% mutate(coordinates=gsub("POINT \\(|\\)", "", the_geom),
               long=as.numeric(sub(" .*", "", coordinates)),
               lat=as.numeric(sub(".* ", "", coordinates)))

# Get tract geometry data for joins
nyc_tracts <- rbind(tracts(state="NY", county="005", year=2020),
                    tracts(state = "NY", county = "047", year = 2020), # Kings (Brooklyn)
                    tracts(state = "NY", county = "061", year = 2020), # New York (Manhattan)
                    tracts(state = "NY", county = "081", year = 2020), # Queens
                    tracts(state = "NY", county = "085", year = 2020))

# get population data NEEDS API KEY FOR CENSUS, mine is saved locally
population_data <- get_acs(
  geography = "tract",
  variables = "B01003_001E", # Total population
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
  year = 2021, # Most recent ACS data
  survey = "acs5"
)

# Get GEOID based on Long and Lat
v <- x %>% st_as_sf(coords=c("Longitude", "Latitude"), crs="NAD83")

results <- st_join(v, nyc_tracts, join=st_within) %>% dplyr::select(colnames(v), "GEOID") %>% data.frame() %>% dplyr::select(!geometry)
# Get kiosks per tract
total <- results %>% filter(`Installation.Status`=="Live") %>% group_by(GEOID) %>% summarize(kiosks=n()) %>% right_join(population_data, by=join_by("GEOID"=="GEOID"))

# Get GEOID based on long lat
poi_v <- poi %>% data.frame() %>% st_as_sf(coords=c("long", "lat"), crs="NAD83") %>% dplyr::select(!c(NAME))
poi_results <- st_join(poi_v, nyc_tracts, join=st_within) %>% dplyr::select(colnames(poi_v), "GEOID") %>% data.frame() %>% dplyr::select(!geometry)
# Get pois by tract
poi_total <- poi_results %>% group_by(GEOID) %>% summarize(pois=n())


# those that don't have any put to 0
poi_total[is.na(poi_total$pois),]$pois <- 0
total[is.na(total$kiosks),]$kiosks <- 0

# get people serviced by kiosk
total <- total %>% mutate(pop_per_kiosk=estimate/kiosks) %>% rename(ct_title=NAME, population=estimate, pop_moe=moe)


# Get it to plot
tract_pop_per_kiosk <- left_join(nyc_tracts, total, by=join_by("GEOID"=="GEOID"))
track_poi <- left_join(nyc_tracts, poi_total, by=join_by("GEOID"=="GEOID"))

link_locations_plot <- ggplot(data=nyc_tracts) +
  geom_sf(aes(fill=COUNTYFP), size=0.2, alpha=0.5) +
  theme_classic() +
  geom_point(data=x, aes(x=Longitude, y=Latitude), size=.5) +
  labs(title="LinkNYC Locations") +
  theme(panel.grid=element_blank(), axis.ticks =element_blank(), axis.title = element_blank(),
        axis.line = element_blank(), axis.text=element_blank(), legend.position="none", plot.title=element_text(size=25, hjust=.5))


# Final df with useful columns, may need to remove stuff
actual_total <- left_join(tract_pop_per_kiosk, poi_total, by=join_by("GEOID"=="GEOID")) %>% relocate(c(GEOID, kiosks, population, pop_moe, pop_per_kiosk, pois, ct_title))


write_csv(actual_total, "poi_and_kiosk_by_ct_data.csv")

nyc_tracts %>% filter(COUNTYFP=="047") %>% ggplot() + geom_sf(size=0.2) +
  geom_point(data=filter(x, Borough=="Brooklyn"), aes(x=Longitude, y=Latitude), size=0.5)

x$Borough %>% unique()

ggplot(data=nyc_tracts) +
  geom_sf(aes(fill=COUNTYFP), size=0.2, alpha=0.5) +
  theme_classic() +
  geom_point(data=poi, aes(x=long, y=lat), size=.2) +
  labs(title="Points of Interest in NYC") +
  theme(panel.grid=element_blank(), axis.ticks =element_blank(), axis.title = element_blank(),
        axis.line = element_blank(), axis.text=element_blank(), legend.position="none", plot.title=element_text(size=25, hjust=.5))

ggplot(data=track_poi) +
  geom_sf(aes(fill=n)) +
  theme_classic()

pop_tract <- left_join(nyc_tracts, population_data, by=join_by("GEOID"=="GEOID"))



ggplot(data=tract_pop_per_kiosk) +
  geom_sf(aes(fill=pop_per_kiosk), size=0.2) +
  theme_classic() +
  geom_point(data=x, aes(x=Longitude, y=Latitude), size=0.1) +
  scale_fill_gradient(low="skyblue", high="red") +
  labs("People per Kiosk by Census Tract") +
  theme(panel.grid=element_blank(), axis.ticks =element_blank(), axis.title = element_blank(),
        axis.line = element_blank(), axis.text=element_blank(), plot.title=element_text(size=25, hjust=.5))


pop_tract %>% filter(COUNTYFP=="047") %>% ggplot() +
  geom_sf(aes(fill=estimate), size=0.2) +
  theme_classic() +
  scale_fill_gradient(low="white", high="red") +
  labs(title="Brooklyn Population and LinkNYC Locations") +
  geom_point(data=filter(x, Borough=="Brooklyn"), aes(x=Longitude, y=Latitude), size=0.3) +
  theme(panel.grid=element_blank(), axis.ticks =element_blank(), axis.title = element_blank(),
        axis.line = element_blank(), axis.text=element_blank(), plot.title=element_text(size=25, hjust=.5))


pop_plot

ggarrange(link_locations_plot, pop_plot, nrow=2)

x %>% filter(`Installation Status`=="Live") %>% group_by(Borough) %>% summarize(n=n())

x$`Census Tract (CT)`

