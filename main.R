##############################
# Math 658: Survey Sampling
# DC Crash Data Project
# Dunn, Grimm, Roring
# Jul 24 2017
##############################

##############################
# STEP 1: Prepare environment
##############################

# Clear workspace, load libraries & extras
rm(list=ls())
library(dplyr)
library(ggmap)
library(geosphere)
setwd("/Users/philipp/Google Drive/Courses/Math 658/project/") # setwd("C:/Users/GrimmPh/Desktop")
set.seed(2) # for reproducibility

# Grab extra functions
source("extra.R")

# Include Google API key, if necessary
# fix(mapdist) # PWGAIzaSyBdjwwvJmdYAXzlLWG8klC-DVXCQHeIvtg & make sure
register_google(key = "AIzaSyBdjwwvJmdYAXzlLWG8klC-DVXCQHeIvtg")

##############################
# STEP 2: Determine sample size & allocation
##############################

# # Load pre-processed dataset (made one manual change for wrong long sign)
# data <- read.csv("hillary_data.csv") %>% 
# 	tbl_df()

# Load injury-only dataset
data <- read.csv("injury_data.csv") %>%
	tbl_df()

data <- data %>% 
	bind_cols(random = runif( nrow(data) ) ) %>% 
	arrange(random)

# Define stratum characteristics
stratum_name <- levels(data$WARD)
stratum_size <- data %>% group_by(WARD) %>% summarize(n = n()) %>% select(n) %>% pull()

# Perform pilot
s2_pilot <- perform_pilot(
	pilot_size_ward = 5, 
	data = data, 
	target_var = quo(driving_dist))

# Calculate optimal total n
optimal_n <- calc_optimal_n(
	Nh = stratum_size, 
	S2h = s2_pilot, 
	e = 0.1, 
	alpha = 0.05)

# Allocate n across strata
stratum_samplesize_prop <- proportional_allocation(
	Nh = stratum_size,
	n = optimal_n)

stratum_samplesize <- exact_optimal_allocation(
	Nh = stratum_size,
	s2h = s2_pilot,
	n = optimal_n)

# stratum_samplesize <- c(15, 85, 17, 15, 32, 49, 23, 33) # From Hillary's pilot

##############################
# STEP 3: Build sample
##############################

# Generate sampled dataset
data.sample <- sampler(full_df = data, 
											 stratum_name = stratum_name, 
											 stratum_samplesize = stratum_samplesize)

# Append parameter data (first generate data in temp data, then rejoin with data.sample)
data.sample_temp1 <- closest_hospitals(unique_id = data.sample$OBJECTID, 
																			 lat = data.sample$LATITUDE, 
																			 long = data.sample$LONGITUDE, 
																			 num_closest = 3)

data.sample_temp2 <- add_googlemaps_mod(df = data.sample_temp1) #; write.csv(data.sample_temp2, "google_data3.csv", row.names = FALSE)
# data.sample_temp2 <- read.csv("google_data.csv") # to avoid excessive API calls

data.sample_temp3 <- data.sample_temp2 %>%
	group_by(unique_id) %>%
	summarize(
	  straight_dist = min(straight_dist)/1609.34,
	  driving_dist  = min(miles),
		driving_time  = min(seconds))

data.sample <- left_join(
	data.sample, 
	data.sample_temp3, 
	by=c("OBJECTID"  = "unique_id"))

##############################
# STEP 4: Estimation
##############################

# Calculate estimates
estimate <- sum(tapply(data.sample$driving_dist, data.sample$WARD, mean) * stratum_size / sum(stratum_size))
estimate.time <- sum(tapply(data.sample$driving_time/60, data.sample$WARD, mean) * stratum_size / sum(stratum_size))
estimate_variance <- sum((stratum_size/sum(stratum_size))^2 * 
												 	(stratum_size-stratum_samplesize)/stratum_size * 
												 	1/stratum_samplesize *
												 	tapply(data.sample$driving_dist, data.sample$WARD, alt_var))
estimate_variance.time <- sum((stratum_size/sum(stratum_size))^2 * 
												 	(stratum_size-stratum_samplesize)/stratum_size * 
												 	1/stratum_samplesize *
												 	tapply(data.sample$driving_time/60, data.sample$WARD, alt_var))
ci <- estimate + c(-1,1) * 1.96 * sqrt(estimate_variance)
ci.time <- estimate.time + c(-1,1) * 1.96 * sqrt(estimate_variance.time)

# Results (driving distance)
rbind(
	N_h = stratum_size, 
	n_h = stratum_samplesize,
	s2_pilot = s2_pilot,
	s2_h = tapply(data.sample$driving_dist, data.sample$WARD, alt_var),
	ybar_h = tapply(data.sample$driving_dist, data.sample$WARD, mean)) # stratum information

optimal_n # n
estimate # ybar_str
estimate_variance # Var-hat(ybar_str)
sqrt(estimate_variance) # SE(ybar_str)
ci # 95% CI for ybar_str

# Secondary results (driving time)
rbind(
	N_h = stratum_size,
	s2_h = tapply(data.sample$driving_time/60, data.sample$WARD, alt_var),
	ybar_h = tapply(data.sample$driving_time/60, data.sample$WARD, mean)) # stratum information for time
estimate.time # ybar_str
estimate_variance.time # Var-hat(ybar_str)
sqrt(estimate_variance.time) # SE(ybar_str)
ci.time # 95% CI for ybar_str

##############################
# BACKUP COPY: RESULTS
##############################


# RESULTS FOR ALL 2016 (30400 crash reports) (Seed=1; Pilot 8 per ward)
##############################

# > # Results (driving distance)
# Ward 1       Ward 2       Ward 3       Ward 4       Ward 5       Ward 6       Ward 7      Ward 8
# N_h      2580.0000000 7.318000e+03 2164.0000000 2323.0000000 4518.0000000 4814.0000000 3471.0000000 3212.000000
# n_h        25.0000000 1.600000e+01    9.0000000   14.0000000   19.0000000   68.0000000   51.0000000   39.000000
# s2_pilot    0.9552309 5.221928e-02    0.2204004    0.3846267    0.1858755    1.9263569    2.1257269    1.438370
# s2_h        0.4475983 1.653444e-01    0.1755642    0.2682010    0.3900370    0.9196527    0.6457094    1.231145
# ybar_h      1.0067674 1.176116e+00    2.2916542    2.2047716    1.5664840    2.5626079    3.8746727    2.383467
# > 
# 	> optimal_n # n
# [1] 241
# > estimate # ybar_str
# [1] 2.033012
# > estimate_variance # Var-hat(ybar_str)
# [1] 0.002231477
# > sqrt(estimate_variance) # SE(ybar_str)
# [1] 0.04723851
# > ci # 95% CI for ybar_str
# [1] 1.940424 2.125599
# > 
# > # Secondary results (driving time)
# Ward 1      Ward 2      Ward 3     Ward 4      Ward 5      Ward 6      Ward 7      Ward 8
# N_h    2580.000000 7318.000000 2164.000000 2323.00000 4518.000000 4814.000000 3471.000000 3212.000000
# s2_h     11.641244    5.044212    1.441667    6.09848   11.443228    6.969506    8.574338   12.245076
# ybar_h    6.138667    8.005208    9.233333   10.22143    8.470175   13.412500   10.317647    8.963248
# > estimate.time # ybar_str
# [1] 9.394201
# > estimate_variance.time # Var-hat(ybar_str)
# [1] 0.04628958
# > sqrt(estimate_variance.time) # SE(ybar_str)
# [1] 0.2151501
# > ci.time # 95% CI for ybar_str
# [1] 8.972507 9.815896


# RESULTS FOR INJURIES ONLY (5990 reports) (Seed=1; Pilot 5 per ward)
##############################
# > # Results (driving distance)
# 						Ward 1       Ward 2      Ward 3      Ward 4      Ward 5      Ward 6      Ward 7      Ward 8
# N_h      487.0000000 1253.0000000 429.0000000 529.0000000 960.0000000 905.0000000 748.0000000 679.0000000
# n_h       12.0000000   20.0000000  17.0000000   8.0000000  45.0000000  56.0000000  35.0000000  23.0000000
# s2_pilot   0.3071398    0.1161087   0.7537060   0.1143175   0.9600483   1.6530868   0.9744525   0.5294248
# s2_h       0.2712146    0.3555450   0.6319334   0.1697019   0.5084101   0.7527157   0.6214640   1.9358762
# ybar_h     0.7116066    1.2569679   2.1191933   2.2078342   1.5637462   2.5459868   3.5562012   2.4240003
# > 
# 	> optimal_n # n
# [1] 216
# > estimate # ybar_str
# [1] 2.02168
# > estimate_variance # Var-hat(ybar_str)
# [1] 0.003130481
# > sqrt(estimate_variance) # SE(ybar_str)
# [1] 0.0559507
# > ci # 95% CI for ybar_str
# [1] 1.912016 2.131343
# > 
# 	> # Secondary results (driving time)
# 					Ward 1      Ward 2     Ward 3     Ward 4     Ward 5     Ward 6     Ward 7     Ward 8
# N_h    487.000000 1253.000000 429.000000 529.000000 960.000000 905.000000 748.000000 679.000000
# s2_h     8.964268    3.237939   9.031918   6.856662  10.649919   7.727684  10.118676  11.906881
# ybar_h   5.158333    8.508333   9.097059   9.872917   7.957778  13.804762   8.883333   8.399275
# > estimate.time # ybar_str
# [1] 9.145088
# > estimate_variance.time # Var-hat(ybar_str)
# [1] 0.04046097
# > sqrt(estimate_variance.time) # SE(ybar_str)
# [1] 0.2011491
# > ci.time # 95% CI for ybar_str
# [1] 8.750836 9.539341


### DATASET DESCRIPTION (BAREBONES SAMPLING FRAME)
##############################
# > data
# # A tibble: 5,990 x 5
# OBJECTID LATITUDE LONGITUDE   WARD       random
# <int>    <dbl>     <dbl> <fctr>        <dbl>
# 	1  2900938 38.92129 -77.07215 Ward 3 2.298248e-05
# 2  2984494 38.88221 -77.02115 Ward 6 4.321919e-05
# 3  2962667 38.87847 -76.96131 Ward 7 1.953009e-04
# 4  2935129 38.91297 -76.99276 Ward 5 3.338496e-04
# 5  3024243 38.89394 -76.95434 Ward 7 4.404834e-04
# 6  2830757 38.90459 -77.02794 Ward 2 6.632213e-04
# 7  2974649 38.87678 -77.01002 Ward 6 6.804196e-04
# 8  2950517 38.84616 -76.98336 Ward 8 7.544663e-04
# 9  2876267 38.90523 -77.06078 Ward 2 7.619553e-04
# 10  2931820 38.90928 -76.93250 Ward 7 8.399892e-04
# # ... with 5,980 more rows

# > str(data)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	5990 obs. of  5 variables:
# 	$ OBJECTID : int  2900938 2984494 2962667 2935129 3024243 2830757 2974649 2950517 2876267 2931820 ...
# $ LATITUDE : num  38.9 38.9 38.9 38.9 38.9 ...
# $ LONGITUDE: num  -77.1 -77 -77 -77 -77 ...
# $ WARD     : Factor w/ 8 levels "Ward 1","Ward 2",..: 3 6 7 5 7 2 6 8 2 7 ...
# $ random   : num  2.30e-05 4.32e-05 1.95e-04 3.34e-04 4.40e-04 ...


### DATASET DESCRIPTION (FULL DATASET WITH ALL FIELDS)
##############################
# > data
# # A tibble: 216,728 x 57
# X        Y OBJECTID  CRIMEID      CCN               REPORTDATE  ROUTEID  MEASURE OFFSET STREETSEGID ROADWAYSEGID
# <dbl>    <dbl>    <int>    <int>   <fctr>                   <fctr>   <fctr>    <dbl>  <int>       <int>        <int>
# 	1 -77.01618 38.89581  2822509 26859658 16024137 2016-02-22T18:34:51.000Z 11000402  384.353      0        5688         6230
# 2 -77.01818 38.92137  2822510 26746345 15175128 2015-11-03T14:36:36.000Z 11000402 3034.894      0       10928        11313
# 3 -77.01758 38.91749  2822511 26738379 15170342 2015-10-26T15:09:30.000Z 11000402 2601.468      0        3241         3881
# 4 -77.01788 38.95767  2822512 26738320 15170314 2015-10-26T12:58:53.000Z 11000402 5807.964      0        9632        10350
# 5        NA       NA  2822513 24353849 12173080 2012-12-11T05:00:00.000Z 11000402 2923.032      0       10928        11313
# 6 -77.01803 38.92037  2822514 24353849 12173080 2012-12-11T05:00:00.000Z 11000402 2923.032      0       10928        11313
# 7 -77.01616 38.90956  2822515 26918660 16062133 2016-04-21T16:29:08.000Z 11000402 1762.963      0        2317         2787
# 8        NA       NA  2822516 24346348 12170576 2012-12-06T12:52:00.000Z 11000402 5807.964      0        9632        10350
# 9 -77.01788 38.95767  2822517 24346348 12170576 2012-12-06T12:52:00.000Z 11000402 5807.964      0        9632        10350
# 10        NA       NA  2822518 24196591 12125815 2012-09-05T22:27:00.000Z 11000402 5518.099      0       11517        11747
# # ... with 216,718 more rows, and 46 more variables: FROMDATE <fctr>, TODATE <fctr>, MARID <int>, ADDRESS <fctr>, LATITUDE <dbl>,
# #   LONGITUDE <dbl>, XCOORD <dbl>, YCOORD <dbl>, WARD <fctr>, EVENTID <fctr>, MAR_ADDRESS <fctr>, MAR_SCORE <dbl>,
# #   MAJORINJURIES_BICYCLIST <int>, MINORINJURIES_BICYCLIST <int>, UNKNOWNINJURIES_BICYCLIST <int>, FATAL_BICYCLIST <int>,
# #   MAJORINJURIES_DRIVER <int>, MINORINJURIES_DRIVER <int>, UNKNOWNINJURIES_DRIVER <int>, FATAL_DRIVER <int>, MAJORINJURIES_PEDESTRIAN <int>,
# #   MINORINJURIES_PEDESTRIAN <int>, UNKNOWNINJURIES_PEDESTRIAN <int>, FATAL_PEDESTRIAN <int>, TOTAL_VEHICLES <int>, TOTAL_BICYCLES <int>,
# #   TOTAL_PEDESTRIANS <int>, PEDESTRIANSIMPAIRED <int>, BICYCLISTSIMPAIRED <int>, DRIVERSIMPAIRED <int>, TOTAL_TAXIS <int>,
# #   TOTAL_GOVERNMENT <int>, SPEEDING_INVOLVED <int>, NEARESTINTROUTEID <fctr>, NEARESTINTSTREETNAME <fctr>, OFFINTERSECTION <dbl>,
# #   INTAPPROACHDIRECTION <fctr>, LOCERROR <fctr>, YEAR <chr>, FATAL <dbl>, MAJORINJURY <dbl>, MINORINJURY <dbl>, INJURY <dbl>, DRIVER <dbl>,
# #   BICYCLE <dbl>, PEDESTRIAN <dbl>
# > str(data)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	216728 obs. of  57 variables:
# 	$ X                         : num  -77 -77 -77 -77 NA ...
# $ Y                         : num  38.9 38.9 38.9 39 NA ...
# $ OBJECTID                  : int  2822509 2822510 2822511 2822512 2822513 2822514 2822515 2822516 2822517 2822518 ...
# $ CRIMEID                   : int  26859658 26746345 26738379 26738320 24353849 24353849 26918660 24346348 24346348 24196591 ...
# $ CCN                       : Factor w/ 141825 levels "_1407439","00052854",..: 105424 98626 98016 98010 53515 53515 110016 53292 53292 49601 ...
# $ REPORTDATE                : Factor w/ 59486 levels "","1975-05-26T05:00:00.000Z",..: 23418 16306 15683 15676 7070 7070 27699 6934 6934 4673 ...
# $ ROUTEID                   : Factor w/ 7396 levels "","11000102",..: 9 9 9 9 9 9 9 9 9 9 ...
# $ MEASURE                   : num  384 3035 2601 5808 2923 ...
# $ OFFSET                    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ STREETSEGID               : int  5688 10928 3241 9632 10928 10928 2317 9632 9632 11517 ...
# $ ROADWAYSEGID              : int  6230 11313 3881 10350 11313 11313 2787 10350 10350 11747 ...
# $ FROMDATE                  : Factor w/ 3603 levels "","1900-01-01T00:00:00.000Z",..: 3099 2993 2985 2985 1937 2323 3163 1932 2323 1840 ...
# $ TODATE                    : Factor w/ 259 levels "","2000-11-20T00:00:00.000Z",..: 1 1 1 1 109 1 1 109 1 109 ...
# $ MARID                     : int  301151 311622 229646 249396 218661 218661 238268 249396 249396 248690 ...
# $ ADDRESS                   : Factor w/ 69309 levels ""," 1 CHEVY CHASE CIRCLE N.W.",..: 43506 46263 21804 65393 46262 46262 66783 64520 64520 49279 ...
# $ LATITUDE                  : num  38.9 38.9 38.9 39 38.9 ...
# $ LONGITUDE                 : num  -77 -77 -77 -77 -77 ...
# $ XCOORD                    : num  398596 398423 398476 398451 398437 ...
# $ YCOORD                    : num  136440 139277 138847 143306 139166 ...
# $ WARD                      : Factor w/ 10 levels "","Null","Ward 1",..: 8 3 3 6 3 3 7 6 6 6 ...
# $ EVENTID                   : Factor w/ 145052 levels "{00002A3C-F0F4-4CB7-A853-356E1D807559}",..: 15999 69928 82659 38787 96116 96116 86849 136907 136907 63650 ...
# $ MAR_ADDRESS               : Factor w/ 38821 levels "","1 13TH STREET NE",..: 17756 22974 20792 20364 13961 13961 6332 20364 20364 28854 ...
# $ MAR_SCORE                 : num  200 200 200 200 200 200 200 200 200 200 ...
# $ MAJORINJURIES_BICYCLIST   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ MINORINJURIES_BICYCLIST   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ UNKNOWNINJURIES_BICYCLIST : int  0 0 0 0 0 0 0 0 0 0 ...
# $ FATAL_BICYCLIST           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ MAJORINJURIES_DRIVER      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ MINORINJURIES_DRIVER      : int  0 1 1 0 1 1 1 0 0 0 ...
# $ UNKNOWNINJURIES_DRIVER    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ FATAL_DRIVER              : int  0 0 0 0 0 0 0 0 0 0 ...
# $ MAJORINJURIES_PEDESTRIAN  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ MINORINJURIES_PEDESTRIAN  : int  0 0 0 0 0 0 0 0 0 0 ...
# $ UNKNOWNINJURIES_PEDESTRIAN: int  0 0 0 0 0 0 0 0 0 0 ...
# $ FATAL_PEDESTRIAN          : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TOTAL_VEHICLES            : int  2 2 2 2 2 2 2 2 2 2 ...
# $ TOTAL_BICYCLES            : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TOTAL_PEDESTRIANS         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ PEDESTRIANSIMPAIRED       : int  0 0 0 0 0 0 0 0 0 0 ...
# $ BICYCLISTSIMPAIRED        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ DRIVERSIMPAIRED           : int  0 0 0 0 0 0 0 0 0 0 ...
# $ TOTAL_TAXIS               : int  0 0 0 0 0 0 0 1 1 0 ...
# $ TOTAL_GOVERNMENT          : int  1 0 0 0 0 0 0 0 0 0 ...
# $ SPEEDING_INVOLVED         : int  0 0 0 0 0 0 0 0 0 0 ...
# $ NEARESTINTROUTEID         : Factor w/ 1665 levels "","11000102",..: 220 181 236 379 144 144 472 379 379 335 ...
# $ NEARESTINTSTREETNAME      : Factor w/ 1624 levels "","10TH PL SE",..: 622 522 657 994 430 430 1182 994 994 903 ...
# $ OFFINTERSECTION           : num  37.4964 0.0293 14.9983 0.0287 0.0444 ...
# $ INTAPPROACHDIRECTION      : Factor w/ 9 levels "","East","North",..: 6 6 3 3 3 3 6 3 3 6 ...
# $ LOCERROR                  : Factor w/ 5 levels "INVALID DATES",..: 3 3 3 3 5 3 3 5 3 5 ...
# $ YEAR                      : chr  "2016" "2015" "2015" "2015" ...
# $ FATAL                     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MAJORINJURY               : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MINORINJURY               : num  0 1 1 0 1 1 1 0 0 0 ...
# $ INJURY                    : num  0 1 1 0 1 1 1 0 0 0 ...
# $ DRIVER                    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ BICYCLE                   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ PEDESTRIAN                : num  0 0 0 0 0 0 0 0 0 0 ...