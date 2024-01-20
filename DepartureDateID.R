# Setup
# Package installs
suppressPackageStartupMessages({
  library("dplyr")
  library("ggplot2")
  library("maps")
  library("ggmap")
  library("mapdata")
  library("readxl")
  library("secr")
  library("lubridate")
  library("tidyr")
  library("scales")
  library("ggrepel")
})

### Import data - this example is immature male sand tiger shark data
# Data import
# st = sand tiger, IM = immature, MA = mature, OYO = one year olds, IMM = young of the year, M and F are for sex
# *********CHANGE TO Wherever you put your data (should be formatted like IMMc.csv in main branch)
tigerpath <- "/Users/ellac/Dropbox/Mac/Downloads/Summer Research/2023-Wetherbee Lab/DataAnalysis/SharkTracking2023/Gittest" 
# **********CHANGE TO Wherever you want your graphs going
tigerpath_png <- "/Users/ellac/Dropbox/Mac/Downloads/Summer Research/2023-Wetherbee Lab/DataAnalysis/SharkTracking2023/Gittest/ST_Gittest_PNGs"

# Import binned data
IMMb <- read.csv(paste(tigerpath, sep = "/", "IMMb.csv"))

# if females involved - Fix the whole "Sex = FALSE" thingy
# df$Sex <- "F"

### Define mapping parameters - these are example polygons denoting locations on the East Coast of the US
# Define states
# Map data
states_df <- map_data("state")
east_coast <- subset(states_df, region %in% c("new hampshire","vermont","maine","massachusetts","rhode island","connecticut","new york","new jersey","pennsylvania","delaware","maryland","virginia","north carolina","south carolina","georgia","florida"))

# Define map zooms for easy mapping
DBxlim <- c(-77,-73)
DBylim <- c(38,40.7)
DHxlim <- c(-72,-69)
DHylim <- c(41,43)
NCxlim <- c(-79,-73)
NCylim <- c(33,37)
FLxlim <- c(-89,-79)
FLylim <- c(24,31)

# Define polygons
# Use Google mymaps to draw a polygon, export as csv, copy/paste the coordinates into a vector, add commas manually (it uses a mix of commas and spaces it sucks)
db_2 <- c(-75.570124, 39.7413254, -75.6635078, 39.6398771, -75.5426582, 39.0920418, -75.1361641, 38.7201487, -74.6857246, 39.1133557, -75.1196846, 39.2623728, -75.4492744, 39.4662249, -75.4273018, 39.6863927, -75.570124, 39.7413254)
db_4 <- db_2[c(TRUE, FALSE)] # Select every other number to get latitudes
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

### Clean time ******** May need to change format!!
IMMb$DateandTimeUTC <- as.POSIXct(IMMb$DateandTimeUTC, format = "%Y-%m-%d %H:%M:%S")
# Remove anything with NA datetime
IMMb <- IMMb %>% drop_na(DateandTimeUTC)

### In this example, we are investigating departures from DB specifically, so we will filter for that
bay_only <- IMMb %>% 
  filter(Location == "db")

bay_sharks <- unique(bay_only$SharkID)
length(bay_sharks)

IMM_db <- IMMb %>% 
  filter(SharkID %in% bay_sharks)

IMM_db %>% 
  group_by(SharkID) %>% 
  dplyr::summarize(observations = n()) %>% # something else has loaded plyr and now we need to specify which summarize idk
  arrange(observations)
# Down to 226 sharks

# Now filter for at least one DG observation - DG stands for "Delaware Gate," the area on the way out of the bay. In hindsight this is unecessarily complicated but here we are.
gate_only <- IMM_db %>% 
  filter(Location == "dg")

gate_sharks <- unique(gate_only$SharkID)
length(gate_sharks)

IMM_dbg <- IMM_db %>% 
  filter(SharkID %in% gate_sharks)

IMM_dbg %>% 
  group_by(SharkID) %>% 
  dplyr::summarize(observations = n()) %>% 
  arrange(observations)
# Down to 194 sharks

# Simplify the location column to focus on Delaware Bay - "ot" = anything not DB or DG
IMM_dbg <- IMM_dbg %>% 
  arrange(SharkID, DateandTimeUTC) %>% 
  mutate(loc2 = case_when(Location == "dh" | Location == "it" | Location == "nc" | Location == "fl" ~ "ot", Location == "db" ~ "db", Location == "dg" ~ "dg"))

# Remove count column that isn't even in order any more and order correctly
IMM_dbg <- select(IMM_dbg, -c(X))
head(IMM_dbg)
IMM_dbg <- select(IMM_dbg, c(Sex, DateandTimeUTC, SharkID, lat, lon, Location, loc2))

# Function from https://psyteachr.github.io/tutorials/detecting-runs-in-a-sequence.html, thanks Dale Barr
detect_runs <- function(x) {  
  if (!is.logical(x[[1]])) stop("'x' must be a tibble whose first column is of type 'logical'")
  runs <- rle(x[[1]]) # ID runs like earlier
  run_start_fr <- c(1L, cumsum(runs$lengths[-length(runs$lengths)]) + 1L)
  run_end_fr <- run_start_fr + (runs$lengths - 1L) # Find run start & end
  
  tgt_start <- run_start_fr[runs$values]
  tgt_end <- run_end_fr[runs$value]
  tibble(run = seq_along(tgt_start),
         start_x = tgt_start,
         end_x = tgt_end) # Return a tibble of runs
}

### Create the dataframe of runs
# Split runs by shark
dbg_split <- split(IMM_dbg, IMM_dbg$SharkID)
sharknames <- c()
allruns_lst <- list()

# Return each shark's df of runs using Epic Terrifying Loop
for (i in 1:length(dbg_split)) {
  df <- dbg_split[[i]]
  name <- as.character(df[1,3]) # Extract shark name
  dem <- as.character(df[1,1])
  sharknames <- c(sharknames, name) # Add shark name to the big vector
  
  ## DB Create logical
  db_tgt <- df %>% 
    mutate(is_db = (loc2=="db"))
  ## DB Add subtables to use to detect runs
  db_runs_nest <- db_tgt %>% 
    nest(subtbl = c(is_db))
  ## DB Find runs
  db1_runs <- db_tgt %>% pull(is_db)
  db_runs <- detect_runs(tibble(lvec=db1_runs))
  ## DB Make dataframe
  db_runs <- db_runs %>% 
    mutate(start_d = df[start_x,]$DateandTimeUTC, end_d = df[end_x,]$DateandTimeUTC, SharkID = df[start_x,]$SharkID, SharkID = name, Demographic=dem) %>% 
    relocate(SharkID, .after = run)
  
  ## dg Create logical
  dg_tgt <- df %>% 
    mutate(is_dg = (loc2=="dg"))
  ## dg Add subtables to use to detect runs
  dg_runs_nest <- dg_tgt %>% 
    nest(subtbl = c(is_dg))
  ## dg Find runs
  dg1_runs <- dg_tgt %>% pull(is_dg)
  dg_runs <- detect_runs(tibble(lvec=dg1_runs))
  ## dg Make dataframe
  dg_runs <- dg_runs %>% 
    mutate(start_d = df[start_x,]$DateandTimeUTC, end_d = df[end_x,]$DateandTimeUTC, SharkID = df[start_x,]$SharkID, SharkID = name, Demographic=dem) %>% 
    relocate(SharkID, .after = run)
  
  ## ot Create logical
  ot_tgt <- df %>% 
    mutate(is_ot = (loc2=="ot"))
  ## ot Add subtables to use to detect runs
  ot_runs_nest <- ot_tgt %>% 
    nest(subtbl = c(is_ot))
  ## ot Find runs
  ot1_runs <- ot_tgt %>% pull(is_ot) 
  ot_runs <- detect_runs(tibble(lvec=ot1_runs))
  ## ot Make dataframe
  ot_runs <- ot_runs %>% 
    mutate(start_d = df[start_x,]$DateandTimeUTC, end_d = df[end_x,]$DateandTimeUTC, SharkID = df[start_x,]$SharkID, SharkID = name, Demographic=dem) %>% 
    relocate(SharkID, .after = run)
  
  # Put all three of those together
  three_runs <- bind_rows(lst(db_runs, dg_runs, ot_runs), .id="id") %>% 
    arrange(start_x)
  
  allruns_lst[[length(allruns_lst) + 1]] <- three_runs
  #assign(paste(name, sep = "_", "all_runs"), three_runs) # Return the split dataframe with shark ID as the df name
}

# You should now have a list of dataframes called allruns_lst, with colums 
# "id" (location df that the row came from), "run" (numbered in order of date), "SharkID", "start_x" and "end_x" (used by the code, not readable), 
# "start_d" and "end_d" (start and end of the shark's stay in that location, by date and time), 
# and "Demographic" (whatever you put in as the first column of the original dataframe)

### Now some cleanup
all_runs_unfiltered = do.call(rbind, allruns_lst)
# Eliminate runs of length less than one (require at least 2 observations of the shark in the same place to qualify as a run)
all_runs_unfiltered <- all_runs_unfiltered %>%
  mutate(obs = end_x-start_x) # Create "obs" - number of tag pings in the run
all_runs <- all_runs_unfiltered %>% 
  filter(obs > 1)
all_runs[1:100,] #%>% arrange(obs)
# Spot checks confirm that this is consistent with the individual run dfs

# Rename ID column to locations
all_runs["id"][all_runs["id"] == "ot_runs"] <- "OT"
all_runs["id"][all_runs["id"] == "db_runs"] <- "DB"
all_runs["id"][all_runs["id"] == "dg_runs"] <- "DG"

# Split runs by shark
runs_split <- split(all_runs, all_runs$SharkID)

### Now identify departure dates using a variety of patterns - in this case, we want 
# to consider sharks that were outside the bay, then inside, then outside again, 
# in addition to sharks that entered and exited the "gate" and sharks that started 
# in the bay, then passed through the "gate" into another location.
# For this example, I have only included pattern 1, OT-DG-OT. 

# Identify pattern 1 - OT-DG-OT
## Remember to group by SharkID
# Initialize list
pattern1_list <- list()

# Loop to ID patterns
for (i in 1:length(runs_split)) {
  df <- runs_split[[i]] # Extract one shark's runs
  name <- as.character(df[1,3]) # Extract shark name
  
  # x <- df[2,4] # Test row (prints the second start_x for verification, used as placeholder to check binding)
  
  vec <- df$id # Extract the list of locations in order as a vector
  # Search for OT-DG-OT
  a <- 1 # Iterator for while loop
  extract_lst <- data.frame(id=character(),
                            run=integer(),
                            SharkID=character(),
                            start_x=integer(),
                            end_x=integer(),
                            start_d=POSIXct(),
                            end_d=POSIXct(),
                            stringsAsFactors=FALSE) # Initialize extract list (for adding to pattern)
  
  while (a < nrow(df)) {
    reading_frame <- vec[a:as.numeric(a+2)]
    collapsedframe <- paste(reading_frame, collapse = '')
    # print(collapsedframe) # Print reading frame as one string
    
    if (collapsedframe == "OTDGOT") {
      # print("Match found!") 
      # Extract the three rows in question - [a:as.numeric(a+2)]
      extract <- df[a:as.numeric(a+2),]
      # print(extract) # extract is working correctly here
      
      # ******HERE IS WHERE FOUR HOURS GET ADDED IF YOU USE BIND_ROWS. USE rbind()*****
      # Original: ST0902 starts on 2010-05-30 01:24:00 UTC
      
      extract_lst <- rbind(extract_lst, extract) # adding 3 rows to extract_lst
      a <- a + 1
    } else {
      a <- a + 1
    }
  }
  
  
  pattern1_list[[length(pattern1_list) + 1]] <- extract_lst # Add result to pattern1_list for later binding
}

# Unsplit
pattern_1 = do.call(rbind, pattern1_list) # 144 obs
# Delete duplicate rows (OT-DG-OT-DG-OT could cause a duplicate row)
pattern_1_trimmed <- pattern_1 %>% distinct() # 137 obs, spot checks indicate that it's working

pattern_1_dates <- data.frame(SharkID=character(),
                              Demographic=character(),
                              DepartureDate=POSIXct(),
                              TransitDate=POSIXct(),
                              stringsAsFactors=F)
### Identify the departure dates!
b <- 2
while (b < nrow(pattern_1_trimmed)) {
  if (pattern_1_trimmed[b,1] == "DG") { # Start on any row that is DG
    name <- pattern_1_trimmed[b,3] # Extract SharkID
    dem <- pattern_1_trimmed[b,8]
    depart <- pattern_1_trimmed[b,7] # Identify DG exit date
    transit <- pattern_1_trimmed[as.numeric(b+1),6] # Identify next observation
    
    vec <- c(name, dem, depart, transit)
    vec2 <- as.data.frame(t(vec))
    names(vec2)[names(vec2) == 'SharkID'] <- 'SharkID'
    names(vec2)[names(vec2) == 'Demographic'] <- 'Demographic'
    names(vec2)[names(vec2) == 'end_d'] <- 'DepartureDate'
    names(vec2)[names(vec2) == 'start_d'] <- 'TransitDate'
    
    #print(vec2)
    
    pattern_1_dates <- rbind(pattern_1_dates, vec2)
    
    b <- b+1
  } else {
    b <- b+1
  }
}

# Return pattern 1 dates to legible (check against all_runs to make sure this doesn't add any hours back)
pattern_1_dates$DepartureDate <- as.POSIXct(as.numeric(pattern_1_dates$DepartureDate, origin = "1970-01-01"))
pattern_1_dates$TransitDate <- as.POSIXct(as.numeric(pattern_1_dates$TransitDate,  origin = "1970-01-01"))

# Fix the weirdass list thing
pattern_1_dates$SharkID <- as.character(pattern_1_dates$SharkID)
pattern_1_dates$Demographic <- as.character(pattern_1_dates$Demographic)

# Filter for less than 30 days between DG and OT 2
pattern_1_dates <- pattern_1_dates %>%
  mutate(window = round(difftime(as.POSIXct(TransitDate, tryFormats = "%Y-%m-%d %H:%M:%OS"), as.POSIXct(DepartureDate, tryFormats = "%Y-%m-%d %H:%M:%OS"), units="days"), digits=4))
head(pattern_1_dates)
pattern_1_dates_trimmed <- pattern_1_dates %>% 
  filter(window < 30) %>%# Mess around with this - do we have enough data at 7 days? 14? 21?
  select(SharkID, DepartureDate, TransitDate, window) %>%  # Remove demographic to make the adding back work
  filter(SharkID != "ST0986") # Teleporting Shark
head(pattern_1_dates_trimmed)

### ******Demographic tends to get lost here - in this case it could simply be restored by adding an "IMM" column, but it can also be restored with code such as this:
# Add demographic back by startdate
#pat1_Date_Sharks <- unique(pattern_1_dates$SharkID)
#pat1DatesRef <- alltiger_dbg %>% 
#  filter(SharkID %in% pat1_Date_Sharks)

#pattern_1_dates_trimmed <- inner_join(x = pat1DatesRef, y = pattern_1_dates_trimmed, join_by(SharkID == SharkID, DateandTimeUTC == DepartureDate))
#head(pattern_1_dates_trimmed)
#pattern_1_dates_trimmed <- pattern_1_dates_trimmed %>% 
#  rename(DepartureDate = DateandTimeUTC, Demographic = id)
#head(pattern_1_dates_trimmed)
#pattern_1_dates_trimmed <- pattern_1_dates_trimmed %>% 
#  select(SharkID, Demographic, DepartureDate, TransitDate, window) # Several of the combined columns mean nothing when applied to a departure date
#head(pattern_1_dates_trimmed)

mapsharks <- pattern_1_dates_trimmed$SharkID
mapsharks_full <- IMM_dbg %>% filter(SharkID %in% mapsharks)

# Map data
states_df <- map_data("state")
east_coast <- subset(states_df, region %in% c("new hampshire","vermont","maine","massachusetts","rhode island","connecticut","new york","new jersey","pennsylvania","delaware","maryland","virginia","north carolina","south carolina","georgia","florida"))

mapsharks_split <- split(mapsharks_full, mapsharks_full$SharkID)

mapsharks_split[[1]] <- mapsharks_split[[1]] %>% filter(grepl("2012", DateandTimeUTC, fixed = TRUE) == T | grepl("2013", DateandTimeUTC, fixed = TRUE) == T) 


# Map the sharks of interest
ggplot(data=east_coast) +
  geom_polygon(aes(x=long,y=lat,group=group),fill="palegreen",color="black") +
  coord_fixed(1.3) +
  coord_fixed(ylim = c(33,43), xlim = c(-80,-70), ratio = 1.3) +
  geom_path(data=mapsharks_split[[1]], aes(x=lon, y=lat, color=DateandTimeUTC),linewidth=1, alpha = 0.7)  +
  ggtitle(mapsharks_split[[1]][1,3])

### Combining and cleaning
# Add quick pattern ID
#pattern_1_dates_trimmed <- pattern_1_dates_trimmed %>% 
#  mutate(Pattern = "Transit-Gate-Transit")
#pattern_2_dates_trimmed <- pattern_2_dates_trimmed %>% 
#  mutate(Pattern = "Bay-Gate-Transit")
#pattern_3_dates_trimmed <- pattern_3_dates_trimmed %>% 
#  mutate(Pattern = "Transit-Bay-Transit")

# Combine all into useful info for the next step
#st_db_departures <- rbind(pattern_1_dates_trimmed, pattern_2_dates_trimmed, pattern_3_dates_trimmed)
st_db_departures <- pattern_1_dates_trimmed
# st_db_departures %>% arrange(window)

# Create a df of all sharks where departures can be narrowed down within a month
st_db_dMonth <- st_db_departures %>% 
  filter(window < 30) %>% 
  arrange(DepartureDate)
# Create a df of all sharks where departures can be narrowed down within a week
st_db_dWeek <- st_db_departures %>% 
  filter(window < 7) %>% 
  arrange(DepartureDate)
# Create a df of sharks where there is less than a day window between DB run and OT run
st_db_dDay <- st_db_departures %>% 
  filter(window < 1)

# Export departures + runs for reference
write.csv(st_db_dWeek, file = paste(tigerpath, sep = "/", "ST_DB_WeekDepartures.csv"))
write.csv(all_runs, file = paste(tigerpath, sep = "/", "ST_DB_RUNS.csv"))
