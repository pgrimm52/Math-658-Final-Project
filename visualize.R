##############################
# Visualization script
##############################

##############################
# STEP 1: Prepare environment
##############################

# Load libraries
rm(list=ls())
library(dplyr)
library(ggmap)
library(scales)
library(rgeos)
library(rgdal)
library(maptools)
devtools::install_github("dkahle/ggmap") # need newewst version for stamen jpeg conflict

# Full dataset split by Injury boolean (30k vs 6k)
data <- read.csv("Crashes_in_DC.csv") %>% 
	tbl_df() %>%
	mutate(
		FATAL       = ifelse(FATAL_DRIVER | FATAL_BICYCLIST | FATAL_PEDESTRIAN, 1, 0),
		MAJORINJURY = ifelse(MAJORINJURIES_DRIVER | MAJORINJURIES_BICYCLIST | MAJORINJURIES_PEDESTRIAN, 1, 0),
		MINORINJURY = ifelse(MINORINJURIES_DRIVER | MINORINJURIES_BICYCLIST | MINORINJURIES_PEDESTRIAN, 1, 0),
		YEAR        = substr(FROMDATE, 1,4),
		INJURY      = ifelse(FATAL | MAJORINJURY | MINORINJURY, 1, 0)) %>%
	filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
	select(OBJECTID, LATITUDE, LONGITUDE, WARD, INJURY)

data.hospitals <- data.frame(
	HOSP.NAME = c(
		"Howard University",
		"MedStar Washington",
		"Providence Hospital",
		"MedStar Georgetown",
		"GWU Hospital",
		"Sibley Memorial",
		"Adventist Health Washington",
		"United Medical Center",
		"Prince George"),
	HOSP.LATITUDE = c(
		38.917823,
		38.929901,
		38.943891,
		38.911631,
		38.901313,
		38.936805,
		38.985554,
		38.835939,
		38.930904),
	HOSP.LONGITUDE = c(
		-77.020815,
		-77.014155,
		-76.991492,
		-77.075540,
		-77.050646,
		-77.108526,
		-77.001902,
		-76.984961,
		-76.920999))

# Grab administrative boundaries
wards <- readOGR(dsn = "./Ward_from_2012/", layer = "Ward_from_2012") %>% 
	fortify()
levels(wards$group) <- c(8, 6, 7, 2, 1, 5, 3, 4, 0) # ID to ward crosswalk

# Grab ward centroids for text
wards.centroid <- data.frame(
	ward = c(1, 2, 3, 4, 5, 6, 7, 8),
	long = c(-77.03033, -77.05086, -77.075, -77.025, -76.99, -76.998, -76.948, -77.015),
	lat = c(38.92642, 38.9, 38.94, 38.96, 38.92642, 38.888, 38.888, 38.83922))

# Append sampling results to mapping data
create_plot_data <- function(result_name, result_vector){
	temp <- data.frame(
		group = as.factor(c(1:8)),
		result_vector)
	names(temp) <- c("group", result_name)
	
	wards %>% 
		left_join(temp)
}

results <- data.frame(
	group = as.factor(c(1:8)),
	driving_dist = c(0.7116066, 1.2569679, 2.1191933, 2.2078342, 1.5637462, 2.5459868, 3.5562012, 2.4240003),
	driving_time = c(5.158333, 8.508333, 9.097059, 9.872917, 7.957778, 13.804762, 8.883333, 8.399275))

data.plot <- wards %>% left_join(results)

##############################
# STEP 2: Output general graphics
##############################

# Map of hospitals with ward overlay
general.1 <- ggmap(
	get_map("washington dc", zoom = 11, source = "stamen", maptype="terrain-background", color="bw"), 
	extent = "device", maprange = FALSE) +
		geom_polygon(data = data.plot, 
								 aes(x = long, y = lat, group = group, fill = group), 
								 color = "grey", alpha = 0.3, size = 0.25) +
		scale_x_continuous(limits = c(-77.15, -76.89), expand = c(0, 0)) +
		scale_y_continuous(limits = c(38.79, 39), expand = c(0, 0)) +
		geom_point(data = data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE), size = I(4), color = I("red")) +
		geom_text(data = data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE, label=HOSP.NAME), vjust = -1.1) +
		guides(fill=guide_legend(title="Ward"))

# Map of all crashes
general.2 <- qmplot(LONGITUDE, LATITUDE, data = data, geom = "blank", maptype = "toner-lite", darken = .3, legend = "topright") +
	stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .25, color = "black", size = 0.1) +
	scale_fill_gradient("Crash\nFrequency", high = "blue")

# Injuries only
general.3 <- qmplot(LONGITUDE, LATITUDE, data = data %>% filter(INJURY == 1), geom = "blank", maptype = "toner-lite", darken = .3, legend = "topright") +
	stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .2, color = "black", size = 0.1) +
	scale_fill_gradient2("Crash w/Injury\nFrequency", low = "white", mid = "yellow", high = "red", midpoint = 50) 
	# + geom_polygon(data = data.plot, 
	# 						 aes(x = long, y = lat, group = group), 
	# 						 fill = NA, color = "white", alpha = 0.3, size = 0.25) 

# Front page graphic
general.4 <- qmplot(x = LONGITUDE, y = LATITUDE,
	data = data %>% filter(INJURY == 1),
	maptype = "toner",
	geom = "density2d",
	color = I("red")) +
	geom_point(data = data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE), size = 7, color="red") 

##############################
# STEP 3: Output results graphics
##############################

# Plot results data
results.1 <- ggplot() +
	geom_polygon(data = data.plot, 
							 aes(x = long, y = lat, group = group, fill=driving_dist), 
							 color = "black", size = 0.25) +
	scale_fill_distiller(palette = "PiYG", direction = -1) +
	geom_text(data = wards.centroid, aes(x = long, y = lat, label = ward), color="black", size=7) + 
	theme_nothing(legend = TRUE) +
	coord_equal(ratio=1.25) +
	guides(fill=guide_legend(title="Average\ndriving\ndistance\n(miles)"))

results.2 <- ggplot() +
	geom_polygon(data = data.plot, 
							 aes(x = long, y = lat, group = group, fill=driving_time), 
							 color = "black", size = 0.25) +
	scale_fill_distiller(palette = "RdYlGn", direction = -1, values = c(0, 0.4, 1)) +
	geom_text(data = wards.centroid, aes(x = long, y = lat, label = ward), color="black", size=7) + 
	theme_nothing(legend = TRUE) +
	coord_equal(ratio=1.25) +
	guides(fill=guide_legend(title="Average\ndriving\ntime (mins)"))

##############################
# STEP 4: Save graphics
##############################

setwd("/Users/philipp/Google Drive/Courses/Math 658/project/") # setwd("C:/Users/GrimmPh/Desktop")
ggsave(general.1, file = "1.png")
ggsave(general.2, file = "2.png")
ggsave(general.3, file = "3.png")
ggsave(general.4, file = "6.png")
ggsave(results.1, file = "4.png")
ggsave(results.2, file = "5.png")


##############################
# BACKUP CODE
##############################

# display.brewer.all() # for color palettes

# c("terrain", "terrain-background", 
# 	"terrain-labels", "terrain-lines", "toner", "toner-2010", 
# 	"toner-2011", "toner-background", "toner-hybrid", 
# 	"toner-labels", "toner-lines", "toner-lite", "watercolor"

# 
# ggmap(get_map(location = "washington dc", zoom = 11, source = "stamen", maptype = "terrain", color="bw")) +
# 	geom_polygon(data = wards,
# 						   aes(x = long, y = lat, group = group),
# 						   color = "red", size = 0.25, fill = NA) +
# 	scale_x_continuous(limits = c(-77.13, -76.9), expand = c(0, 0)) +
# 	scale_y_continuous(limits = c(38.79, 39), expand = c(0, 0))

# > min(wards$long)
# [1] -77.1198
# > min(wards$lat)
# [1] 38.79164
# > max(wards$lat)
# [1] 38.99597
# > max(wards$long)
# [1] -76.90915

# # # Ward outlines and numbers, background
# qmplot(LONGITUDE, LATITUDE, data = data, geom = "blank", maptype="terrain") +
# 	geom_polygon(data = wards,
# 							 aes(x = long, y = lat, group = group),
# 							 color = "red", size = 0.25, fill=NA) +
# 	geom_text(data = wards.centroid, aes(x = long, y = lat, label = ward), color="red", size=8)

# 
# # Ward outlines and numbers, no background
# ggplot() +
# 	geom_polygon(data = wards,
# 							 aes(x = long, y = lat, group = group),
# 							 color = "grey", size = 0.25, fill=NA) +
# 	theme_nothing() +
# 	coord_equal(ratio = 1.2) +
# 	geom_point(data= data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE), size = 4, color="red") +
# 	geom_text(data = wards.centroid, aes(x = long, y = lat, label = ward), color="black", size=7)
# 	
# # geom_text(data = data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE, label=HOSP.NAME), 
# #		  			color = "grey", vjust = 2.3) +
#
# # Map of hospitals
# qmplot(
# 	x = HOSP.LONGITUDE, 
# 	y = HOSP.LATITUDE, 
# 	data = data.hospitals,
# 	maptype = "terrain", 
# 	color = I("red"),
# 	size = I(8)) +
# 	geom_point(aes(HOSP.LONGITUDE, HOSP.LATITUDE), shape = 3, size = 4, color="white") 
# 
# qmplot(
# 	x = HOSP.LONGITUDE,
# 	y = HOSP.LATITUDE,
# 	data = data.hospitals,
# 	maptype = "toner",
# 	color = I("red"),
# 	size = I(4))
# 
# Map of injuries
# qmplot(x = LONGITUDE, y = LATITUDE,
# 	data = data %>% filter(INJURY == 1),
# 	maptype = "terrain-lines",
# 	geom = "density2d",
# 	color = I("black")) +
# 	geom_point(data = data.hospitals, aes(HOSP.LONGITUDE, HOSP.LATITUDE), size = 3, color="red")
