library(gmodels)
library(dplyr)
setwd("/Users/philipp/Google Drive/Courses/Math 658/project/") # setwd("C:/Users/GrimmPh/Desktop")

# Explore

data <- read.csv("Crashes_in_DC.csv") %>% 
	tbl_df() %>%
	mutate(
		YEAR        = substr(FROMDATE, 1,4),
		FATAL       = ifelse(FATAL_DRIVER | FATAL_BICYCLIST | FATAL_PEDESTRIAN, 1, 0),
		MAJORINJURY = ifelse(MAJORINJURIES_DRIVER | MAJORINJURIES_BICYCLIST | MAJORINJURIES_PEDESTRIAN, 1, 0),
		MINORINJURY = ifelse(MINORINJURIES_DRIVER | MINORINJURIES_BICYCLIST | MINORINJURIES_PEDESTRIAN, 1, 0),
		INJURY      = ifelse(FATAL | MAJORINJURY | MINORINJURY, 1, 0),
		DRIVER      = ifelse(TOTAL_VEHICLES, 1, 0),
		BICYCLE     = ifelse(TOTAL_BICYCLES, 1, 0),
		PEDESTRIAN  = ifelse(TOTAL_PEDESTRIANS, 1, 0))

data
str(data)

# Year
data %>%
	mutate(YEAR = substr(FROMDATE, 1,4)) %>%
	count(YEAR) %>%
	arrange(desc(YEAR)) %>%
	View()

# Loc Error
data %>%
	filter(YEAR == 2016) %>%
	count(LOCERROR)

# Injury
data %>%
	filter(YEAR == 2016, LOCERROR == "NO ERROR")

data %>%
	filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
	summarize(
		sum(FATAL), 
		sum(MAJORINJURY), 
		sum(MINORINJURY), 
		sum(INJURY))

# Ward location of injury

data %>%
	filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
	filter(INJURY == 1) %>%
	count(WARD)

# Type of injured entity

data %>%
	filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
	filter(INJURY == 1) %>%
	count(DRIVER)

CrossTable(
	data %>%
		filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
		filter(INJURY == 1) %>%
		pull(BICYCLE),
	data %>%
		filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
		filter(INJURY == 1) %>%
		pull(PEDESTRIAN)
)

# Export

write.csv(
	data %>%
		filter(YEAR == 2016, LOCERROR == "NO ERROR") %>%
		filter(INJURY == 1) %>%
		select(OBJECTID, LATITUDE, LONGITUDE, WARD),
	"injury_data.csv",
	row.names=FALSE)
	