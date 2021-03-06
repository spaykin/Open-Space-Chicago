# Load Libraries
library("rgdal")
library("rgeos")
library("sp")
library(tmap)
library(leaflet)
library(raster) # Needed for grid and kernel density surface
library(tmaptools)
library(RColorBrewer)

##### Load data #####

# Load non-spatial data - City-Owned Vacant Land Inventory data (csv)
vacant <- read.csv("City-Owned_Land_Inventory.csv")
head(vacant)

# Subset vacant land for North Lawndale
vacant_NL <- subset(vacant, Community.Area.Number == "29")

# Load non-sptial data - income
income <- read.csv("Per_Capita_Income.csv")

##### COMMUNITY AREAS #####

# Subset income for North Lawndale
income_NL <- subset(income, Community.Area.Number == "29")

# Load spatial data - community areas
community.areas <-
  readOGR(
    "Boundaries - Community Areas (current)/",
    "geo_export_122a50a2-7802-46f7-8418-bf806c1cb6fd"
  )
plot(community.areas)
str(community.areas@data)

# Check coordinate reference system
crs(community.areas)

# Create new crs EPSG 3435
crs.new <- CRS("+init=EPSG:3435")

# Transform into new crs
community.areas <- spTransform(community.areas, crs.new)
crs(community.areas)
plot(community.areas)

# Create North Lawndale community area
community.area.NL <- subset(community.areas, area_num_1 == "29")

##### PARKS #####

# Load spatial data - parks
parks <-
  readOGR(
    "Parks - Chicago Park District Park Boundaries (current)/",
    "geo_export_1222679b-f01a-4a20-8892-26998609e5e5"
  )
plot(parks)

# Check crs
crs(parks)

# Transform crs
parks <- spTransform(parks, crs.new)
crs(parks)

# Sum park acreage
sum(parks@data$acres)

# Subset parks for North Lawndale
parks_NL <- subset(parks, zip == c("60608", "60623", "60624"))

# Sum of North Lawndale park acreage
sum(parks_NL@data$acres)

##### HABITATS #####

# Load spatial data - habitats
habitats <-
  readOGR("Openspaces_Habitat/", "DATA_ADMIN_OPNSP_HABITAT")
plot(habitats)
crs(habitats)

# Transform crs
habitats <- spTransform(habitats, crs.new)

habitats.df <- as.data.frame(habitats@data)

# Sum habitat acreage
sum(habitats.df$ACRES)

# Join income data to community area
CA.income <-
  merge(community.areas, income, by.x = "area_num_1", by.y = "Community.Area.Number")
head(CA.income)

# Merge for North Lawndale
CA.income.NL <-
  merge(community.area.NL,
        income_NL,
        by.x = "area_num_1",
        by.y = "Community.Area.Number")

# Remove data where lat long coordinates = 0
vacant <- subset(vacant, X.Coordinate != "0" & Y.Coordinate != "0")

# Create spatial points data frame using lat long columns - Chicago
vacant.pts <-
  SpatialPointsDataFrame(vacant[, 15:16], vacant, proj4string = CRS("+init=EPSG:3435"))
plot(vacant.pts)

# Create spatial points data frame - North Lawndale
vacant.NL.pts <-
  SpatialPointsDataFrame(vacant_NL[, 15:16], vacant_NL, proj4string = CRS("+init=EPSG:3435"))
plot(vacant.NL.pts)

# Write shapefile - vacant land
writeOGR(vacant.pts,
         dsn = ".",
         layer = "Vacant Lands Chicago",
         driver = "ESRI Shapefile")

# Rename "Per Capita Income" for Chicago
names(CA.income@data)[names(CA.income@data) == "PER.CAPITA.INCOME"] <-
  "Per Capita Income"

# Rename "Per Capita Income" for North Lawndale
names(CA.income.NL@data)[names(CA.income.NL@data) == "PER.CAPITA.INCOME"] <-
  "Per Capita Income"

############################
##### MAP: VACANT LAND #####
############################

# Map vacant lots on community areas and income data
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "brown")

##### BUFFERS #####

# Create buffers around vacant land
vacant_buffers <- gBuffer(vacant.pts, width = 500, byid = TRUE)

# Plot with borders
tm_shape(CA.income) + tm_borders() +
  tm_shape(vacant_buffers) + tm_borders(col = "blue") +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "brown")

# Merge buffers
union.buffers <- gUnaryUnion(vacant_buffers)

# Add dataframe with IDs the length of your buffer
unionbuff.df <- data.frame(ID = 1:length(union.buffers))

# Use SpatialPolygonsDataFrame function with polygons and new dataframe
unionbuff.sdf <-
  SpatialPolygonsDataFrame(union.buffers, unionbuff.df)

# Write a shapefile
writeOGR(unionbuff.sdf,
         dsn = ".",
         layer = "BufferUnion",
         driver = "ESRI Shapefile")

# Map housing buffers and income
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income", id = "community") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(vacant.pts) + tm_dots(alpha = .3,
                                 col = "blue",
                                 legend.show = TRUE) +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "blue",
    alpha = .3,
    size = .4
  ) +
  tm_shape(union.buffers) + tm_borders(col = "darkblue", alpha = .7) +
  tm_add_legend(
    type = c("symbol"),
    labels = "500 ft buffer",
    border.col = "darkblue",
    col = "#ffffff",
    size = .4
  )

############################
######## MAP: PARKS ########
############################

# Create spatial data frame
parks.df <- as.data.frame(parks@data)

# Create spatial points data frame
parks.points <-
  SpatialPointsDataFrame(parks[, 46], parks.df, proj4string = CRS("+init=EPSG:3435"))

# Map parks as points on community areas and income data
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(parks.points) + tm_dots(alpha = .7, col = "darkgreen")

# Map park boundaries on community areas and income data
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(parks) + tm_fill(col = "darkgreen")

##### BUFFERS - PARKS #####

# Create buffers around parks - quarter mile (1320 feet)
parks_buffers <- gBuffer(parks, width = 1320, byid = TRUE)

# Create buffers around parks in North Lawndale - quarter mile (1320 feet)
parks_NL_buffers <- gBuffer(parks_NL, width = 1320, byid = TRUE)

# Plot parks with individual buffers
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(parks) + tm_fill(col = "darkgreen") +
  tm_shape(parks_buffers) + tm_borders(col = "gray", alpha = .5)

# Merge parks buffers
union.parks.buff <- gUnaryUnion(parks_buffers)

# Add dataframe with IDs the length of your buffer
union.parks.buff.df <- data.frame(ID = 1:length(union.parks.buff))

# Use SpatialPolygonsDataFrame function with polygons and new dataframe
union.parks.sdf <-
  SpatialPolygonsDataFrame(union.parks.buff, union.parks.buff.df)

# Write a shapefile
writeOGR(union.parks.sdf,
         dsn = ".",
         layer = "ParksBufferUnion",
         driver = "ESRI Shapefile")

# Map parks with joined buffers
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(union.parks.buff) + tm_fill(col = "lightgray", alpha = .3) +
  tm_shape(parks) + tm_fill(col = "darkgreen")

############################
###### MAP: HABITATS #######
############################

# Map habitat data on community area and income data
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(habitats) + tm_borders(col = "magenta", alpha = .5)

############################
##### PREP: FINAL MAPS #####
############################

# Map all Chicago
tm_shape(CA.income) +
  tm_fill(col = "Per Capita Income",
          alpha = .8,
          palette = "Greys") +
  tm_borders(alpha = .3, col = "white") +
  tm_shape(union.parks.buff) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_shape(parks) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    size = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue", alpha = .5) +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "orange") +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "darkorange",
    size = .4
  )

# Subset North Lawndale income data
CA.income.NEW <- readOGR(".", "CA.income")

# Projections
crs(CA.income.NEW)
CA.income.NEW <- spTransform(CA.income.NEW, crs.new)

# Subset North Lawndale
N.Lawndale <- subset(CA.income.NEW@data, area_num_1 == "29")

# Merge data
N.Lawndale <-
  merge(community.areas, N.Lawndale, by.x = "area_num_1", by.y = "area_num_1")

# Change PerCapInc variable to numerics
N.Lawndale@data$PerCapInc <- as.numeric(N.Lawndale@data$PerCapInc)

######################
##### FINAL MAPS #####
######################

# Map open space in Chicago (NO INCOME)
tm_shape(community.areas) +
  tm_layout(
    main.title = "Open Space in Chicago",
    main.title.size = 1.2,
    main.title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "center")
  ) +
  tm_borders(alpha = .3, col = "black") +
  tm_fill(col = "gray", alpha = .3) +
  tm_shape(parks) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    border.col = "darkgreen",
    size = .3
  ) +
  tm_shape(parks_buffers) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_add_legend(
    type = c("symbol"),
    labels = "1/4 Mile Buffer",
    col = "lightgreen",
    border.col = "lightgreen",
    alpha = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue") +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    border.col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "orange") +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "orange",
    border.col = "orange",
    size = .4
  ) +
  tm_credits(
    "Created by Susan Paykin (2019). \n Data from Chicago Data Portal.",
    position = c("left", "bottom")
  )

# Map open space with per capita income comparison
tm_shape(CA.income) +
  tm_layout(
    main.title = "Open Space in Chicago",
    main.title.size = 1.3,
    main.title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "center")
  ) +
  tm_borders(alpha = .3, col = "black") +
  tm_fill(col = "Per Capita Income",
          alpha = .8,
          palette = "Greys") +
  tm_shape(parks) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    size = .3
  ) +
  tm_shape(union.parks.buff) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_add_legend(
    type = c("symbol"),
    labels = "1/4 Mile Parks Buffer",
    col = "lightgreen",
    border.col = "lightgreen",
    alpha = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue", alpha = .5) +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "orange") +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "darkorange",
    border.col = "darkorange",
    size = .4
  ) +
  tm_credits(
    "Created by Susan Paykin (2019). \n Data from Chicago Data Portal.",
    position = c("left", "bottom")
  )

# Write shapefile
writeOGR(Chicago,
         dsn = ".",
         layer = "Chicago Open Space",
         driver = "ESRI Shapefile")

# Map open space in North Lawndale
tm_shape(community.area.NL) +
  tm_layout(
    main.title = "Open Space in North Lawndale",
    main.title.size = 1.3,
    main.title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("right", "bottom")
  ) +
  tm_borders(alpha = .3, col = "black") +
  tm_fill(col = "gray", alpha = .4) +
  tm_shape(parks_NL) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    border.col = "darkgreen",
    size = .3
  ) +
  tm_shape(parks_NL_buffers) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_add_legend(
    type = c("symbol"),
    labels = "1/4 Mile Parks Buffer",
    col = "lightgreen",
    border.col = "lightgreen",
    alpha = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue") +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    border.col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.NL.pts) + tm_dots(alpha = .7, col = "darkorange", size = .1) +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "darkorange",
    border.col = "darkorange",
    size = .4
  ) +
  tm_credits(
    "Created by Susan Paykin (2019). \n Data from Chicago Data Portal.",
    position = c("center", "BOTTOM")
  ) +
  tm_scale_bar(position = c("center", "BOTTOM"))

# Rename "PERCENT.HOUSEHOLDS.BELOW.POVERTY"
names(CA.income@data)[names(CA.income@data) == "PERCENT.HOUSEHOLDS.BELOW.POVERTY"] <-
  "Percent Below Poverty"

# Map open space with percent of households below poverty line
tm_shape(CA.income) +
  tm_layout(
    main.title = "Open Space in Chicago",
    main.title.size = 1.3,
    main.title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "center")
  ) +
  tm_borders(alpha = .3, col = "black") +
  tm_fill(col = "Percent Below Poverty",
          alpha = .8,
          palette = "Greys") +
  tm_shape(parks) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    size = .3
  ) +
  tm_shape(union.parks.buff) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_add_legend(
    type = c("symbol"),
    labels = "1/4 Mile Parks Buffer",
    col = "lightgreen",
    border.col = "lightgreen",
    alpha = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue", alpha = .5) +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.pts) + tm_dots(alpha = .7, col = "orange") +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "darkorange",
    border.col = "darkorange",
    size = .4
  ) +
  tm_credits(
    "Created by Susan Paykin (2019). \n Data from Chicago Data Portal.",
    position = c("left", "bottom")
  )

#################################
# no credits map - North Lawndale
#################################

# Map open space in North Lawndale - no credits
tm_shape(community.area.NL) +
  tm_layout(
    main.title = "Open Space in North Lawndale",
    main.title.size = 1.3,
    main.title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("right", "bottom")
  ) +
  tm_borders(alpha = .3, col = "black") +
  tm_fill(col = "gray", alpha = .4) +
  tm_shape(parks_NL) + tm_fill(col = "darkgreen") +
  tm_add_legend(
    type = c("fill"),
    labels = "Parks",
    col = "darkgreen",
    border.col = "darkgreen",
    size = .3
  ) +
  tm_shape(parks_NL_buffers) + tm_fill(col = "lightgreen", alpha = .3) +
  tm_add_legend(
    type = c("symbol"),
    labels = "1/4 Mile Parks Buffer",
    col = "lightgreen",
    border.col = "lightgreen",
    alpha = .3
  ) +
  tm_shape(habitats) + tm_fill(col = "darkblue") +
  tm_add_legend(
    type = c("fill"),
    labels = "Habitat Areas",
    col = "darkblue",
    border.col = "darkblue",
    size = .3
  ) +
  tm_shape(vacant.NL.pts) + tm_dots(alpha = .7, col = "darkorange", size = .1) +
  tm_add_legend(
    type = c("symbol"),
    labels = "Vacant Land",
    col = "darkorange",
    border.col = "darkorange",
    size = .4
  ) +
  tm_scale_bar(position = c("center", "BOTTOM"))
