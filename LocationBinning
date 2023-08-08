# Setup
# Package installs
# CHECK LINE 149 IF YOU END UP WITH AN EMPTY DATETIME COLUMN
suppressPackageStartupMessages({
  library("dplyr")
  library("ggplot2")
  library("maps")
  library("ggmap")
  library("mapdata")
  library("readxl")
  library("secr")
})

# Import data - this example is young of the year sand tiger shark data
# Data import
# st = sand tiger, IM = immature, MA = mature, OYO = one year olds, IMM = young of the year, M and F are for sex
tigerpath <- "/Users/ellac/Dropbox/Mac/Downloads/Summer Research/2023-Wetherbee Lab/DataAnalysis/Sandtiger_data/Sandtiger_csvs" # Wherever you put your data (should be formatted like IMMc.csv in main branch)
tigerpath_png <- "/Users/ellac/Dropbox/Mac/Downloads/Summer Research/2023-Wetherbee Lab/DataAnalysis/Sandtiger_data/Sandtiger_pngs"

# Import cleaned data
IMMc <- read.csv(paste(tigerpath, sep = "/", "IMMc.csv"))

# Define polygons
# Use Google mymaps to draw a polygon, export as csv, copy/paste the coordinates into a vector, add commas manually (it uses a mix of commas and spaces it sucks)
db_2 <- c(-75.570124, 39.7413254, -75.6635078, 39.6398771, -75.5426582, 39.0920418, -75.1361641, 38.7201487, -74.6857246, 39.1133557, -75.1196846, 39.2623728, -75.4492744, 39.4662249, -75.4273018, 39.6863927, -75.570124, 39.7413254)
db_4 <- db_2[c(TRUE, FALSE)]
db_5 <- db_2[c(F, T)]
delaware_bay <- data.frame(x=db_4,y=db_5)

dbg_1 <- c(-74.6857246, 39.1133557, -75.1361641, 38.7201487, -75.2846626, 38.2586059, -73.9992622, 38.2693885, -74.6857246, 39.1133557)
dbg_2 <- dbg_1[c(TRUE, FALSE)]
dbg_3 <- dbg_1[c(F, T)]
delaware_gate <- data.frame(x=dbg_2,y=dbg_3)

dh_1 <- c(-70.49348284, 42.7216897, -71.2281935, 42.4345009, -70.5442945, 41.6687016, -69.6132033, 41.7281734, -70.4934828, 42.7216897)
dh_2 <- dh_1[c(TRUE, FALSE)]
dh_3 <- dh_1[c(F, T)]
duxbury_harbor <- data.frame(x=dh_2, y=dh_3)

nc_1 <- c(-76.3153163, 36.530089, -78.5345545, 33.8276823, -75.8099452, 32.845595, -73.6895838, 36.5830389, -76.3153163, 36.530089)
nc_2 <- nc_1[c(TRUE, FALSE)]
nc_3 <- nc_1[c(F, T)]
nc <- data.frame(x=nc_2, y=nc_3)

fl_1 <- c(-87.5389735, 31.016691, -82.3973719, 24.8081769, -79.1893641, 24.9676338, -79.6727625, 30.6393296, -87.5389735, 31.016691)
fl_2 <- fl_1[c(TRUE, FALSE)]
fl_3 <- fl_1[c(F, T)]
fl <- data.frame(x=fl_2, y=fl_3)

# Plot polygon
# nc plot limits
NCxlim <- c(-79,-73)
NCylim <- c(33,37)

# Mapping dataframes
states_df <- map_data("state")
east_coast <- subset(states_df, region %in% c("new hampshire","vermont","maine","massachusetts","rhode island","connecticut","new york","new jersey","pennsylvania","delaware","maryland","virginia","north carolina","south carolina","georgia","florida"))
world_df <- map_data("world")

ggplot(data=east_coast) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="gray90",color="black") +
  coord_fixed(xlim=NCxlim,ylim=NCylim, ratio = 1.3) +
  geom_polygon(data=nc, aes(x=x, y=y), color="black",size=1, fill=NA) +
  theme_bw() +
  theme(text = element_text(size=20),
        panel.background = element_rect(fill = "azure1", colour = "azure1"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))

# Binning IMM - this chunk can be copied with find & replace

### Delaware Bay
# Create T/F vector and attach to IMMc
IMM_coords <- data_frame(IMMc$lon, IMMc$lat)
IMM_del <- pointsInPolygon(IMM_coords, delaware_bay)
# Results in a huge list of true/false nonsense
IMM_del <- as.data.frame(IMM_del)
# m is for mapping
IMMm <- bind_cols(IMMc, IMM_del)
IMMm <- rename(IMMm, Delaware_Bay = IMM_del)


### Delaware Gate
# Create T/F vector and attach to IMMc
IMM_delg <- pointsInPolygon(IMM_coords, delaware_gate)
# Results in a huge list of true/false nonsense
IMM_delg <- as.data.frame(IMM_delg)
# m is for mapping
IMMm <- bind_cols(IMMm, IMM_delg)
IMMm <- rename(IMMm, Delaware_Gate = IMM_delg)


### Duxbury Harbor
IMM_dux <- pointsInPolygon(IMM_coords, duxbury_harbor)
# Results in a huge list of true/false nonsense
Duxbury_Harbor <- as.data.frame(IMM_dux)
# m is for mapping
IMMm <- bind_cols(IMMm, Duxbury_Harbor)
IMMm <- rename(IMMm, Duxbury_Harbor = IMM_dux)


### North Carolina
IMM_nc <- pointsInPolygon(IMM_coords, nc)
# Results in a huge list of true/false nonsense
NC <- as.data.frame(IMM_nc)
# m is for mapping
IMMm <- bind_cols(IMMm, NC)
IMMm <- rename(IMMm, NC = IMM_nc)


### Florida
IMM_fl <- pointsInPolygon(IMM_coords, fl)
# Results in a huge list of true/false nonsense
FL <- as.data.frame(IMM_fl)
# m is for mapping
IMMm <- bind_cols(IMMm, FL)
IMMm <- rename(IMMm, FL = IMM_fl)

# Verification map - points inside and outside the polygon should be different shapes
ggplot(data=east_coast) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="palegreen3",color="black") +
  coord_fixed(xlim=NCxlim,ylim=NCylim,ratio = 1.3) +
  geom_polygon(data=nc, aes(x=x, y=y), color="black",size=1, fill=NA) +
  geom_point(data=IMMm, aes(x=lon,y=lat, shape = NC), color="blue",size=1)

# Assign all other locations to "in transit"
make_transit=function(Data)
{
  Data <- Data %>% mutate(In_Transit = case_when(Delaware_Bay == F & Delaware_Gate == F & Duxbury_Harbor == F & NC == F & FL == F ~ T, TRUE ~ F))
  return(Data)
}

IMMm2 <- make_transit(IMMm) # Run the loop

# Make all the logical nonsense into one column
loc=function(Data)
{
  Data <- Data %>% mutate(Location = case_when(Delaware_Bay == T ~ "db", Delaware_Gate == T ~ "dg", Duxbury_Harbor == T ~ "dh", NC == T ~ "nc", FL == T ~ "fl", In_Transit == T ~ "it", TRUE ~ "ERROR")) # Create new column that combines all the logical location columns into one with easy abbreviations
  print(Data %>% filter(Location == "ERROR")) # Print any issues
  return(Data)
}

# b is for binned
IMMb <- loc(IMMm2)

# Data cleaning
IMMb <- select(IMMb, -c("X"))
# *********CHECK THIS LINE IF YOUR DATETIME COLUMN IS EMPTY*******
IMMb$DateandTimeUTC <- as.POSIXct(IMMb$DateandTimeUTC, format = "%m/%d/%Y %H:%M:%OS") # Format may change
# Remove columns I don't care about ("logical nonsense")
IMMb <- select(IMMb, -c(Receiver, Transmitter, BethLocation, Delaware_Bay, Delaware_Gate, Duxbury_Harbor, NC, FL, In_Transit))
