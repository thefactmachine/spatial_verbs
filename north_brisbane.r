# preliminaries
options(stringsAsFactors = FALSE)
rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(sp)
library(rgdal)

# not sure which of the following is really required
library(rgeos)
library(sf)
library(geojsonio)
library(Cairo)


# ===========================================================
# ======= Read in Selected Polygons -- hand selected 8 areas.
# 1) Read in the SA3....
sp_poly_df_nth <- rgdal::readOGR(dsn = "input_data/north_brisbane", 
                                              layer = "north_of_brisbane")

# 2) Set the projection system explicitly.. this the sames as in the *.prj file
proj4string(sp_poly_df_nth) <- CRS("+init=epsg:4283")

# 3) reproject the sucker to WGS 84
sp_poly_df_nth <- sp::spTransform(sp_poly_df_nth, 
                                               CRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# ===========================================================

# do a very basic plot
plot(sp_poly_df_nth)

# CSV Fun ================================================
# Read in ABS CSV data and process it....
df_time_series_all <- read.csv(file="input_data/csv_data/2011Census_T01_AUST_SA2_long.csv", header=TRUE, sep=",")

# select and filter appropriate columns
vct_columns <- c("region_id", "Total_persons_2001_Census_Persons", 
                 "Total_persons_2006_Census_Persons", "Total_persons_2011_Census_Persons")
df_time_series_all <- df_time_series_all[, names(df_time_series_all) %in% vct_columns]
# rename
names(df_time_series_all) <- c("id", "2001_p", "2006_p", "2011_p")
# ===========================================================

# get a vector of region ids for north brisbane
vct_north_bris <- sp_poly_df_nth@data$SA2_MAIN %>% as.numeric()

length(vct_north_bris) == 85

# subset the timeseries by north of brisbane
df_ts_north <- df_time_series_all %>% filter(id %in% vct_north_bris)

df_geometry <- sp_poly_df_nth@data
df_geometry$SA2_MAIN <- as.numeric(df_geometry$SA2_MAIN)

# join time series with geometry data
df_data <- df_geometry %>% inner_join(df_ts_north, by = c("SA2_MAIN" = "id"))
df_data$STATE_CODE <- NULL
df_data$STATE_NAME <- NULL

# pc growth
df_data$pc_growth <- ((df_data$`2011_p` - df_data$`2001_p`) / df_data$`2001_p`) * 100
df_data$pc_growth <- df_data$pc_growth %>% round(., 2)

# abs growth 
df_data$abs_growth <- df_data$`2011_p` - df_data$`2001_p`

df_data <- df_data %>% arrange(desc(abs_growth))




















