# Script to estimate mean and maximum fan-head entrenchment during experiments 
# with variable feed.

# Anya Leenman
# 8 July 2021

# comments added 14 March 2022... why didn't past me think this was important!!
#---------------------------------------
# Housekeeping
drive <- "E"
run <- 7
repe <- 1

library(raster)
library(dplyr)
library(lubridate)

# working dir
setwd(paste0(drive, "://Experiments/Processing/Run", run, "/Run", run, "_rep", 
             repe, "/_1min_intervals"))

# DEMs to read in
DEM_list <- list.files("./summary_data/DEM_7mm_smoothed", pattern = ".tif")
# radii at which to sample entrenchment
radii <- seq(0.25, 1.5, by = 0.25)
# empty list to save profiles
prof_list <- list()
# for each radius:
for(j in 1:length(radii)){
  rad <- radii[j] # radius at which to extract profile
  # generate an arc profile
  x_line <- seq(0, rad, 0.0001)
  y_line <- sqrt(rad ^ 2 - x_line ^ 2)
  cds <- cbind(x_line, y_line)
  l <- spLines(cds)
  l_length <- 1/4 * 2 * pi * rad
  # extract pts at 1mm intervals
  l_pts <- rgeos::gInterpolate(l, d = seq(0, l_length, 0.001), normalized = FALSE) 
  prof_list[[j]] <- l_pts # save extracted pts to list - this will be used to sample DEM
}
# prep header of dataframe
out_df <- as.data.frame(matrix(NA, nrow = 0, ncol = 7))
colnames(out_df) <- c("t_step", radii)

# Extract arc profile from DEM (or pointcloud) at 0.25m intervals downfan,
# using profiles of 1mm-spaced points generated in loop above.
# loop through DEMS:
for (i in 1:length(DEM_list)){
  t_step <- substring(DEM_list[i], 1, 4) # get time
  print(t_step)
  DEM <- raster(paste0("./summary_data/DEM_7mm_smoothed/", DEM_list[i])) #import DEM
  outrow <- t_step # why did I rename this? Oh well
  
  # loop through the 0.25 m spaced profiles
  for(k in 1:length(prof_list)){
    prof <- extract(DEM, prof_list[[k]]) # sample DEM at profile
    rad <- radii[k] # get the name of the radius we are sampling
    # if we wanted to plot we could uncomment lines below:
    # if(k == 1){
    #   plot(prof, ylim = c(-0.25, -0.1), type = "l", main = t_step)
    # } else {
    #   lines(prof)
    # }
    
    # apply some smoothing? NO - DEMs already smoothed before import.
    
    # Estimate depth of trenching - look at max depth difference within short width?
    # plot these points on the profile and watch it while running to look for discrepancies
    hmax <- max(prof, na.rm = T)
    hmin <- min(prof, na.rm = T)
    relief <- hmax - hmin # trenching measured as max diff across profile 
    # (only works for very trenched fanhead!)
    relief_mm <- relief * 1000 # convert m to mm
    
    # for each timestep, save difference (i.e. depth) to csv, e.g.
    # timestep | Depth at 0.25 m downfan | depth at 0.5m | depth 0.75 | etc
    outrow <- cbind(outrow, relief_mm)
    outrow2 <- as.numeric(outrow)
    
    # OR apply flow mask and find max depth outside channel and min depth inside ?
    # plot points on profile and watch while running to look for discrepancies again.
    # maybe too complicated...
    
    # 14 Mar 2022: Looks like I decided it was too complicated!
  }
  
  # rbind the cbinded row to the df, starting with the header we made earlier
  out_df <- rbind(out_df, outrow2)
  colnames(out_df) <- c("t_step", radii) # rename so there are no df rbinding complaints
}

# save csv
write.csv(out_df, file = paste0("./t_series/Run", run, "rep", repe, 
          "_trenching_depths_in_mm.csv"), 
          row.names = F)

# for each profile (dist downfan), work out mean + max 
minz <- apply(out_df, 2, min) # the 2 signifies "column" (Vs row)
meanz <- colMeans(out_df, na.rm = F)
maxz <- apply(out_df, 2, max) # the 2 signifies "column" (Vs row)

summary_df <- as.data.frame(rbind(minz, meanz, maxz)) %>%
  select(-t_step, -t_h, -t_m, -time, -t_sec) # why am I time wrangling? Huh.

write.csv(summary_df, file = paste0("./t_series/Run", run, "rep", repe, 
                                    "_trenching_depths_in_mm_summary_data.csv"), 
          row.names = F)

# then some time wrangling:
out_df <- mutate(out_df, t_h = floor(t_step/100)) %>% # get hour
  mutate(t_m = t_step - t_h * 100) %>% # get min
  mutate(time = paste(t_h, t_m, sep = ":")) %>% # concatenate to hh:mm format
  mutate(time = hm(time)) %>% # convert to time format using lubridate
  mutate(t_sec = period_to_seconds(time)) %>%
  mutate(t_m = t_sec / 60) %>%
  mutate(t_h = t_m / 60)

if(run == 7){
  # divide into 10 minute periods (coded so that high feed is START):
  out_df <- mutate(out_df, cycle = ceiling((t_m - 715) / 10)) %>% # assign cycle
    mutate(time_in_cycle = t_m - 705 - 10 * cycle)
} else if (run == 8) {
  # divide into 10 minute periods (coded so that high feed is START):
  out_df <- mutate(out_df, cycle = ceiling((t_m - 710) / 20)) %>% # assign cycle
    mutate(time_in_cycle = t_m - 690 - 20 * cycle)
  
} else if (run == 9) {
  # divide into 10 minute periods (coded so that high feed is START):
  out_df <- mutate(out_df, cycle = ceiling((t_m - 700) / 40)) %>% # assign cycle
    mutate(time_in_cycle = t_m - 660 - 40 * cycle)
}

# make a nice plot of trench depth vs time, at 0.25 m (and 0.5m?) profile
plot(out_df$t_h, out_df$"0.25",
     type = "l",
     xlab = "Time",
     ylab = "Cross-fan elevation difference (mm)")


