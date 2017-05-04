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



# this was created in QGIS by hand selecting the polygons...righthand click..save 
# selected features...

# loads the shapefile  dsn = folder name; layer = filename without extension
# data type is SpatialPolygonsDataFrame...only has 7 rows.

# setting the projection for the shapefile....
# look at the *.prj file and copy its definition into here:
# http://prj2epsg.org/search..... in the case of the shapefile below...
# the following was returned...."4283 - GDA94"
# you can do a lookup for the EPSG here:  https://www.epsg-registry.org/


# ===========================================================
# ======= Read in Selected Polygons -- hand selected 8 areas.
# 1) Read in the SA3 file....
sp_poly_df_CBR_selected_SA3 <- rgdal::readOGR(dsn = "input_data/sa3_selected_CBR_regions", 
                                          layer = "SA3_selected_CBR_regions")

# 2) Set the projection system explicitly.. this the sames as in the *.prj file
proj4string(sp_poly_df_CBR_selected_SA3) <- CRS("+init=epsg:4283")

# 3) reproject the sucker to WGS 84
sp_poly_df_CBR_selected_SA3 <- sp::spTransform(sp_poly_df_CBR_selected_SA3, 
                                           CRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# do a very basic plot
plot(sp_poly_df_CBR_selected_SA3)

# ===========================================================
# ======= Read in SA3  -- Alles Australia 351 rows =======

sp_poly_df_AUST_SA3 <- rgdal::readOGR(dsn = "input_data/2011_SA3_shape", 
                                          layer = "SA3_2011_AUST")

proj4string(sp_poly_df_AUST_SA3) <- CRS("+init=epsg:4283")

sp_poly_df_AUST_SA3 <- sp::spTransform(sp_poly_df_AUST_SA3, 
                       CRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# this erases the plot
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_AUST_SA3)


# ===========================================================
# Subset the large SA3 File into the smaller SA3 file based on PK

sp_poly_df_CBR_SA3 <- sp_poly_df_AUST_SA3[sp_poly_df_AUST_SA3@data$SA3_CODE %in% 
                                        sp_poly_df_CBR_selected_SA3@data$SA3_CODE,]

# this is how to clear the current plot in R_Studio
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_CBR_SA3) 


# also check the size of All Australia and then Just Canberra:
object.size(sp_poly_df_AUST_SA3)
object.size(sp_poly_df_CBR_SA3)

# ===========================================================
# Dissolve the 8 polygons. 

# the trick for id is to use an attribute that is the
# the same for all rows.  If we used SA3_CODE ... then it would return 8 rows
sp_poly_df_CBR_dissolve <- rgeos::gUnaryUnion(sp_poly_df_CBR_SA3, 
                                  id = sp_poly_df_CBR_SA3@data$STATE_CODE)

# check the plot
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_CBR_dissolve)

# ===========================================================
# Intersect the detailed SA2 with the SA3 dissolve (created above)

# step 1 - load the data
# load in SA2 more granular than SA3...has 2214 rows; SA3 has 351 rows
sp_poly_df_AUST_SA2 <- rgdal::readOGR(dsn = "input_data/2011_SA2_shape", 
                                  layer = "SA2_2011_AUST")

# step 1.a
proj4string(sp_poly_df_AUST_SA2) <- CRS("+init=epsg:4283")

# step 1.c -- reproject to wgs84
sp_poly_df_AUST_SA2 <- sp::spTransform(sp_poly_df_AUST_SA2, 
                      CRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


# step 2 -- blocking ...cut the sucker down to ACT or NSW 
sp_poly_df_NSW_SA2 <- sp_poly_df_AUST_SA2[sp_poly_df_AUST_SA2@data$STATE_CODE == "1" | 
                                            sp_poly_df_AUST_SA2@data$STATE_CODE == "8", ]



# step 3 -- doing the spatial_join --- polygon to polygon...

# logical vector of all the SA2 polygons within NSW (length is same as NSW)
# byid = TRUE for some reason needs to be there...this took a long time to find out
# gContains returns a matrix... .[,1] converts to a vector
vct_contained_in_CBR <- gContains(sp_poly_df_CBR_dissolve, sp_poly_df_NSW_SA2, 
                                  byid=TRUE) %>% .[, 1] 

# step 3.a get rid of the names
names(vct_contained_in_CBR) <- NULL

# step 4 executate the join
sp_poly_df_CBR_SA2 <- sp_poly_df_NSW_SA2[vct_contained_in_CBR,]


if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_CBR_SA2)



# ===========================================================
# We have an additional 4 polygons at SA2 level..we use these to
# display Queenbean.

sp_poly_df_qb_SA2 <- rgdal::readOGR(dsn = "input_data/additonal_sa2", 
                                    layer = "additional_sa2")


# step 1.a
proj4string(sp_poly_df_qb_SA2) <- CRS("+init=epsg:4283")

# step 1.c -- reproject to wgs84
sp_poly_df_qb_SA2 <- sp::spTransform(sp_poly_df_qb_SA2, 
                                       CRS = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# we have a look at what we just imported....
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_qb_SA2)

# to get greater Canberra we club the queenbeyan stuff together...how easy!!
sp_poly_df_greater_CBR_SA2 <- rbind(sp_poly_df_CBR_SA2, sp_poly_df_qb_SA2)

# and check the result
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_greater_CBR_SA2)

# ========================================================
# CSV Fun ================================================
# Read in ABS CSV data and process it....
df_time_series_all <- read.csv(file="input_data/csv_data/2011Census_T01_AUST_SA2_long.csv", header=TRUE, sep=",")

# select and filter appropriate columns
vct_columns <- c("region_id", "Total_persons_2001_Census_Persons", 
                 "Total_persons_2006_Census_Persons", "Total_persons_2011_Census_Persons")
df_time_series_all <- df_time_series_all[, names(df_time_series_all) %in% vct_columns]

# rename
names(df_time_series_all) <- c("id", "2001_p", "2006_p", "2011_p")

# get a vector of canberra id's -- we want to join this data to SA2 level data
vct_sa2_id_cbr <- sp_poly_df_greater_CBR_SA2@data$SA2_MAIN

# filter all of australia for just greater canberra only...
df_time_series_cbr <- df_time_series_all[df_time_series_all$id %in% vct_sa2_id_cbr,]

# now calculate change between 2001 and 2011
df_time_series_cbr$change_10 <- df_time_series_cbr$`2011_p` - df_time_series_cbr$`2001_p`

# cast the id column to a character...so we can join with the shapefile...
df_time_series_cbr$id <- as.character(df_time_series_cbr$id)

# club back into the shapefile ----- THIS PART HERE IS WHERE THE DATA GET JOINED TO THE GEOMETRY
sp_poly_df_greater_CBR_SA2@data <- sp_poly_df_greater_CBR_SA2@data %>% 
  inner_join(df_time_series_cbr, by = c("SA2_MAIN" = "id"))

# some clean up stuff here -------------------------------------------------

# clean things up...
sp_poly_df_greater_CBR_SA2@data$`2001_p` <- NULL
sp_poly_df_greater_CBR_SA2@data$`2006_p` <- NULL
sp_poly_df_greater_CBR_SA2@data$STATE_NAME <- NULL
# this identifies "Oxley (ACT)" and converts it to "Oxley" 
sp_poly_df_greater_CBR_SA2@data$SA2_NAME <- gsub("\\(ACT\\)", "", sp_poly_df_greater_CBR_SA2@data$SA2_NAME)

sp_poly_df_greater_CBR_SA2@data[sp_poly_df_greater_CBR_SA2@data$SA2_NAME == 
                                  "Queanbeyan West - Jerrabomberra", "SA2_NAME"] <- "Jerrabomberra"

# ========================================================
# Plot a simple bar chart ================================
# Highest growing suburbs

df_highest_growing <- sp_poly_df_greater_CBR_SA2@data %>% 
                      select(SA2_NAME, change_10) %>%
                      arrange(desc(change_10)) %>%
                      slice(1:20) %>%
                      rename(Suburb = SA2_NAME, Growth = change_10) 

# need to fiddle with factors -- convert suburb into a factor.
# ..its order is based on the value of growth.  Can put a minus sign
# next to the second variable in order to flip around its sort order....
df_highest_growing$Suburb <- reorder(df_highest_growing$Suburb, 
                                     df_highest_growing$Growth)



g <- ggplot(df_highest_growing, aes(x = Suburb, y = Growth))
g <- g + geom_bar(width = 0.7, stat = "identity", fill = "#3da456")
g <- g + coord_flip()
g <- g + theme_light(13, base_family = "Calibri")
g <- g + theme(panel.grid.minor.y = element_blank())
g <- g + theme(panel.grid.major.y = element_blank())
g <- g + theme(panel.grid.minor.x = element_blank())
g <- g + theme(panel.grid.major.x = element_blank())
g <- g + theme(axis.ticks.y = element_blank())
g <- g + scale_y_continuous(label = scales::comma)
g <- g + theme(panel.border = element_blank())
g <- g + theme(panel.background = element_blank())
g <- g + theme(axis.line = element_line(colour = "black"))
g <- g + labs(y = "Absolute increase in number of people (2001 ~ 2011)")
g <- g + labs(x = "SA2 Region (i.e suburb)")
g 


g
cairo_pdf("growth_by_sa2.pdf", width = 11.69, height = 8.27)
print(g)
dev.off() 


# ========================================================














# ======= clean up stuff and get it ready for geolayers. 



# clean up a name --- dont really need this ......it is at SA3 level.......
sp_poly_df_CBR_selected_SA3[sp_poly_df_CBR_selected_SA3@data$SA3_CODE == "80103", "SA3_NAME"] <- "Pialligo"




# ====================================
# ==============================================================
# === Following adds quantiles and exports to geojson....======
# ==============================================




# filter out negative values and then calculate 4 quantiles
df_cbr_positive <- df_time_series_cbr %>% filter(change_10 > 0)
quant_positive <- quantile(df_cbr_positive$change_10, probs = c(0, 0.25, 0.5, 0.75))

df_cbr_positive$quant <- -1
df_cbr_positive$quant <- ifelse(df_cbr_positive$change_10 < quant_positive[2], 1, df_cbr_positive$quant)
df_cbr_positive$quant <- ifelse(df_cbr_positive$change_10 >= quant_positive[2] &  
                                  df_cbr_positive$change_10 < quant_positive[3], 2, df_cbr_positive$quant)
df_cbr_positive$quant <- ifelse(df_cbr_positive$change_10 >= quant_positive[3] &  
                                  df_cbr_positive$change_10 < quant_positive[4], 3, df_cbr_positive$quant)
df_cbr_positive$quant <- ifelse(df_cbr_positive$change_10 >= quant_positive[4], 4, df_cbr_positive$quant)
table(df_cbr_positive$quant)

df_cbr_negative <- df_time_series_cbr %>% filter(change_10 <= 0)
df_cbr_negative$quant <- 0

# club humpty dumpty back together again
df_cbr_quant <- rbind(df_cbr_positive, df_cbr_negative)
df_cbr_quant$id <- as.character(df_cbr_quant$id)




 
# lets take out all places with zero population....108 polygons...
sp_poly_df_greater_CBR_SA2 <- sp_poly_df_greater_CBR_SA2[sp_poly_df_greater_CBR_SA2@data$`2011_p` != 0, ]


# lets also take out the areas with non-positive growth -- 65 polygons.
sp_poly_df_greater_CBR_SA2 <- sp_poly_df_greater_CBR_SA2[sp_poly_df_greater_CBR_SA2@data$change_10 > 0, ]



# convert to geo_json
#  https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d
json_CRB_SA2 <- geojsonio::geojson_json(sp_poly_df_greater_CBR_SA2)
geojsonio::geojson_write(json_CRB_SA2, file = "CBR_SA2.geojson")







# Centroids  Digression ====================================
# ===========================================================
# We are calculating centroids here.....just for 8 polygons
# if byid was false ... then there would only be a single centroid for the entire shape

sp_points_df_CBR_centroids <- sp::SpatialPointsDataFrame(
  coords = rgeos::gCentroid(sp_poly_df_CBR_selected_SA3, byid = TRUE),  
  data = sp_poly_df_CBR_selected_SA3@data, 
  proj4string = sp_poly_df_CBR_selected_SA3@proj4string)

# now check things by plotting....
if (length(dev.list()) > 0) {dev.off(dev.list()["RStudioGD"])}
plot(sp_poly_df_CBR_selected_SA3)
plot(sp_points_df_CBR_centroids, pch = 16 , col = 'red', add = TRUE)

# ===========================================================











