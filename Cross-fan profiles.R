# Script to plot cross-fan profiles at 0.25, 0.5, 1 and 1.5 m downfan
# Compares aggrading and degrading conditions

# Anya Leenman
# 8 July 2021
#---------------------------------------
# Housekeeping
rm(list = ls())

drive <- "E"
run <- 5
repe <- 1

radii <- c(0.25, 0.5, 1, 1.5) # radii at which to extract profile

library(raster)
library(dplyr)
library(lubridate)

setwd(paste0(drive, "://Experiments/Processing/Run", run, "/Run", run, "_rep", repe,
             "/_1min_intervals"))

DEM_list <- list.files("./summary_data/DEM_7mm_smoothed", pattern = ".tif")
t_list <- lapply(DEM_list, FUN = substring, 1, 4)
t_df <- data.frame(tvec = as.numeric(unlist(t_list))) %>%
  mutate(t_h = ifelse((tvec %% 100) == 0, tvec / 100, floor (tvec / 100))) %>% # get hour
  mutate(t_m = ifelse((tvec %% 100) == 0, 0, tvec - t_h * 100)) %>% # get minute
  mutate(time = paste(t_h, t_m, sep = ":")) %>%    # concatenate to hh:mm format
  mutate(time = hm(time)) %>% # convert to time format using lubridate
  mutate(t_sec = period_to_seconds(time)) %>% # Convert to number of seconds since start
  mutate(t_min = t_sec / 60) %>% # time in minutes
  mutate(t_hr = t_min / 60) %>% # time in hours
  dplyr::select(-time, -t_h, -t_m) # remove unnecessary columns
  #filter(complete.cases(.)) %>% # get rid of rows with NA.
  
# divide into high-low feed cycles (coded so that high feed is START):
if(run <= 7){
  t_df <- mutate(t_df, cycle = ceiling((t_min - 715) / 10)) %>% # assign cycle
    mutate(time_in_cycle = t_min - 705 - 10 * cycle) # assign minute in cycle
} else if(run == 8){
  t_df <- mutate(t_df, cycle = ceiling((t_min - 710) / 20)) %>% # assign cycle
    mutate(time_in_cycle = t_min - 690 - 20 * cycle) # assign minute in cycle
} else if(run == 9) {
  t_df <- mutate(t_df, cycle = ceiling((t_min - 700) / 40)) %>% # assign cycle
    mutate(time_in_cycle = t_min - 660 - 40 * cycle) # assign minute in cycle
}

# get cycle length
cycle_len = max(unique(t_df$time_in_cycle))

# list of timesteps at end of low-feed (degraded state) or high-feed (aggraded state)
deg_steps <- #timesteps at end of low-feed, when fan degraded
  filter(t_df, time_in_cycle == cycle_len)
agg_steps <- # timesteps at end of high-feed, when fan aggraded.
  filter(t_df, time_in_cycle == cycle_len/2)

# filter to every 80 minutes (data too close in time otherwise)
if(run <= 7){
  deg_steps <- 
    filter(deg_steps, cycle %% 8 == 0) # get every 8th cycle
  agg_steps <- 
    filter(agg_steps, cycle %% 8 == 0) # get every 8th cycle
} else if(run == 8){
  deg_steps <- 
    filter(deg_steps, cycle %% 4 == 0) # get every 4th cycle
  agg_steps <- 
    filter(agg_steps, cycle %% 4 == 0) # get every 4th cycle
} else if(run == 9){
  deg_steps <- 
    filter(deg_steps, cycle %% 2 == 0) # get every 2nd cycle
  agg_steps <- 
    filter(agg_steps, cycle %% 2 == 0) # get every 2nd cycle
}

# set up profile lines along which to extract DEM
prof_list <- list()
for(j in 1:length(radii)){
  rad <- radii[j] 
  x_line <- seq(0, rad, 0.0001)
  y_line <- sqrt(rad ^ 2 - x_line ^ 2)
  cds <- cbind(x_line, y_line)
  l <- spLines(cds)
  # lines(l)
  l_length <- 1/4 * 2 * pi * rad
  # extract pts at 1 centimetre intervals (adjust if needed)
  l_pts <- rgeos::gInterpolate(l, d = seq(0, l_length, 0.01), normalized = FALSE) 
  prof_list[[j]] <- l_pts
  # points(prof_list[[j]])
}

# set up blank plots
if(run >=7){
  png(
    filename = paste0(
      drive, ":/Experiments/writing/3_feed_duration/figures/arc-DEM-profiles_run",
      run, ".png"),
    units = "in",
    width = 6,
    height = 4,
    res = 300,
    type = "cairo-png")
} else if(run <7){
  png(
    filename = paste0(
      drive, ":/Experiments/writing/2_floods/Figs/arc-DEM-profiles_run",
      run, ".png"),
    units = "in",
    width = 6,
    height = 4,
    res = 300,
    type = "cairo-png")
}


pars <- c('plt','usr')
par(mfrow = c(2, 4), # 2*4 grid
    mai = rep(0.2, 4),
    oma = c(2,2.5,0,0)) 

ylims = c(-0.23, -0.1)
axis_lims = data.frame(x1 = rep(0, 4), # get axis lengths right
                       x2 = c(0.4, 0.8, 1.6, 2.4),
                       y1 = rep(ylims[1], 4),
                       y2 = rep(ylims[2], 4))

# upper row
plot(c(axis_lims$x1[1], axis_lims$x2[1]), 
     c(axis_lims$y1[1], axis_lims$y2[1]), 
     type = "n", 
     xlab = "", ylab = "")
par1 <- c(list(mfg=c(1,1,2,4)), par(pars))
plot(c(axis_lims$x1[2], axis_lims$x2[2]), 
     c(axis_lims$y1[2], axis_lims$y2[2]), 
     type = "n", 
     xlab = "", ylab = "")
par2 <- c(list(mfg=c(1,2,2,4)), par(pars))
plot(c(axis_lims$x1[3], axis_lims$x2[3]), 
     c(axis_lims$y1[3], axis_lims$y2[3]), 
     type = "n", 
     xlab = "", ylab = "")
par3 <- c(list(mfg=c(1,3,2,4)), par(pars))
plot(c(axis_lims$x1[4], axis_lims$x2[4]), 
     c(axis_lims$y1[4], axis_lims$y2[4]), 
     type = "n", 
     xlab = "", ylab = "")
par4 <- c(list(mfg=c(1,4,2,4)), par(pars))
# rep for lower row
plot(c(axis_lims$x1[1], axis_lims$x2[1]), 
     c(axis_lims$y1[1], axis_lims$y2[1]), 
     type = "n", 
     xlab = "", ylab = "")
par5 <- c(list(mfg=c(2,1,2,4)), par(pars))
plot(c(axis_lims$x1[2], axis_lims$x2[2]), 
     c(axis_lims$y1[2], axis_lims$y2[2]), 
     type = "n", 
     xlab = "", ylab = "")
par6 <- c(list(mfg=c(2,2,2,4)), par(pars))
plot(c(axis_lims$x1[3], axis_lims$x2[3]), 
     c(axis_lims$y1[3], axis_lims$y2[3]), 
     type = "n", 
     xlab = "", ylab = "")
par7 <- c(list(mfg=c(2,3,2,4)), par(pars))
plot(c(axis_lims$x1[4], axis_lims$x2[4]), 
     c(axis_lims$y1[4], axis_lims$y2[4]), 
     type = "n", 
     xlab = "", ylab = "")
par8 <- c(list(mfg=c(2,4,2,4)), par(pars))
mtext("Cross-fan distance (m)", side = 1, line = 1, outer = T)
mtext("Elevation (m) relative to top of walls", side = 2, line = 1, outer = T)

par_list <- list(
  par1, par2, par3, par4, par5, par6, par7, par8
)

# set up colours for lines
colz <- gray.colors(nrow(deg_steps), start = 0, end = 0.8, 
                    gamma = 2.2, rev = T)

# Extract arc profiles from DEM at specified radii
# first for degraded state (end of low-feed)
# for (i in 50:60){ # debug version
for (i in 1:nrow(deg_steps)){ # real version
  t_step <- deg_steps$tvec[i]
  print(t_step)
  # import chosen DEM
  DEM <- raster(paste0("./summary_data/DEM_7mm_smoothed/", t_step, ".tif") )
  
  # extract profiles from DEM
  for(k in 1:length(prof_list)){
    prof <- extract(DEM, prof_list[[k]])
    xcoord <- seq(0, length(prof)/100, 0.01)[1:length(prof)]
    par(par_list[[k]])
    lines(xcoord, prof, col = colz[i])
  }
}

# Extract arc profiles from DEM at specified radii
# Rep for aggraded state
for (i in 1:nrow(agg_steps)){ 
  t_step <- agg_steps$tvec[i]
  print(t_step)
  # import chosen DEM
  DEM <- raster(paste0("./summary_data/DEM_7mm_smoothed/", t_step, ".tif") )
  
  # extract profiles from DEM
  for(k in 1:length(prof_list)){
    prof <- extract(DEM, prof_list[[k]])
    xcoord <- seq(0, length(prof)/100, 0.01)[1:length(prof)]
    par(par_list[[k+4]])
    lines(xcoord, prof, col = colz[i])
  }
}
dev.off()
