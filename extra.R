##############################
# Extra scripts for main.R
##############################

message("Extra functions loaded")

# FUNCTION (sampler): samples from full dataset according to given strata
sampler <- function(full_df, stratum_name, stratum_samplesize, hillarys_randomization=FALSE){
	
	if (hillarys_randomization) {
		full_df <- full_df %>% arrange(RANDOM.VALUES)
	} else {
		full_df <- full_df %>% arrange(random)
	}
	
	lapply(1:length(stratum_name), function(x){
		full_df[full_df$WARD == stratum_name[x], ][1:stratum_samplesize[x], ]
	}) %>%
		do.call(rbind, .)
}

# FUNCTION (closest_hospitals): grab shortlist of closest hospitals based on straightline distance
closest_hospitals <- function(unique_id, lat, long, num_closest=3){
	
	data.hospitals <- data.frame(
		HOSP.NAME = c(
			"Howard University Hospital",
			"MedStar Washington Hospital Center",
			"Providence Hospital",
			"MedStar Georgetown University Hospital",
			"George Washington University Hospital",
			"Sibley Memorial Hospital – Emergency Room",
			"Adventist Health Washington Hospital",
			"United Medical Center – Emergency Room",
			"Prince George’s Hospital Center"),
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
	
	df_merge <- merge(
		cbind(unique_id, lat, long),
		cbind(hosp.lat = data.hospitals$HOSP.LATITUDE, hosp.long = data.hospitals$HOSP.LONGITUDE),
		by=NULL)
	
	cbind(df_merge, straight_dist = diag(distm(df_merge[, 3:2], df_merge[, 5:4]))) %>%
		group_by(unique_id) %>%
		top_n(-num_closest) %>%
		ungroup()
}

# FUNCTION (add_googlemaps_mod): adds google maps driving time/distances
# Requires data.frame with lat, long, hosp.lat, hosp.long

add_googlemaps_mod <- function(df){
  
  lapply(1:nrow(df), function(x){
    mapdist(
      from = paste(df[x, "lat"], df[x, "long"], sep=" "),
      to   = paste(df[x, "hosp.lat"], df[x, "hosp.long"], sep=" "),
      mode = "driving")
  }) %>% 
    do.call(rbind, .) %>%
    data.frame(df, .) 
}

# add_googlemaps <- function(df){
# 	google_data <- mapdist(
# 		from = paste(df$lat, df$long, sep=" "),
# 		to   = paste(df$hosp.lat, df$hosp.long, sep=" "),
# 		mode = "driving")
# 	
# 	return(list(df, driving_dist=google_data$m, driving_time=google_data$seconds, google_data)) 
# }
# 
# add_googlemaps_alt <- function(df){
# 	
# 	driving_dist <- sapply(1:nrow(df), function(x){
# 		origin <- paste(df$lat[x], df$long[x], sep="+")
# 		destination <- paste(df$hosp.lat[x], df$hosp.long[x], sep="+")
# 		result.long <- gmapsdistance::gmapsdistance(origin = origin,
# 																 destination = destination,
# 																 mode = "driving",
# 																 shape = "long",
# 																 key = "AIzaSyBdjwwvJmdYAXzlLWG8klC-DVXCQHeIvtg")
# 		Sys.sleep(1)
# 		result.long$Distance
# 	})
# 	cbind(df, driving_dist)
# }

# FUNCTION (alt_var): calculates alternative population/sample variance (S^2 or s^2)
alt_var <- function(x){
	sum((x-mean(x))^2)/(length(x)-1)
}

# FUNCTION (perform_pilot): returns vector of sample variances for target var (remember quosure!)
perform_pilot <- function(pilot_size_ward, data, target_var) {

	data.pilot <- data %>%
		arrange(random) %>%
		group_by(WARD) %>%
		slice(1:pilot_size_ward)
	
	s2_pilot <- closest_hospitals(data.pilot$OBJECTID, data.pilot$LATITUDE, data.pilot$LONGITUDE, 1) %>%
		add_googlemaps_mod() %>%
		group_by(unique_id) %>%
		summarize(
			straight_dist = min(straight_dist)/1609.34,
			driving_dist  = min(miles),
			driving_time  = min(seconds)) %>%
		left_join(data.pilot, by=c("unique_id" = "OBJECTID")) %>%
		group_by(WARD) %>%
		summarize(s2 = alt_var(!!target_var)) %>%
		pull(s2)
	
	s2_pilot
}

# FUNCTION (calc_optimal_n): returns value for optimal total sample size (via Neyman allocation)
calc_optimal_n <- function(Nh, S2h, e, alpha){
	
	part1 <- (Nh*sqrt(S2h))/sum(Nh*sqrt(S2h))
	part2 <- sum(Nh^2*S2h/part1)
	part3 <- sum(Nh*S2h)
	part4 <- sum(Nh)^2*e^2/(qnorm(alpha/2)^2)
	
	ceiling(part2/(part3+part4))
}
# # Optional plot
# e_range <- seq(0.05, 0.25, 0.01)
# e_sample_size <- sapply(e_range, function(x){calc_optimal_n(stratum_size, s2_pilot, x, 0.05)})
# lo <- loess(e_sample_size~e_range)
# plot(e_range, e_sample_size,
# 		 xlab = "Tolerated error margin (in miles)",
# 		 ylab = "Necessary sample size")
# lines(x, predict(lo), col='red')

# FUNCTION (proportional_allocation): returns vector of proportionally allocated sample sizes
proportional_allocation <- function(Nh, n) {
	p.a <- round(n * Nh / sum(Nh))
	if (sum(p.a) != n) message("WARNING: Rounding not successful")
	p.a
}

# FUNCTION (exact_optimal_allocation): returns vector of exactly optimal allocated sample sizes
exact_optimal_allocation <- function(Nh, s2h, n){
	
	roots <- sapply(1:n, function(x){ sqrt( x*(x+1) ) })
	priority_values <- sapply(Nh*sqrt(s2h), function(x){ x/roots })
	
	p_vec <- as.vector(priority_values)
	threshold <- p_vec[order(p_vec, decreasing=TRUE)][n+1]
	
	colSums(priority_values>threshold)
}
