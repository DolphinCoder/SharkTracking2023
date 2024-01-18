# SharkTracking2023
Summer 2023 REU code - all code used to study shark migration

Email ellacrotty@reed.edu with any questions

## Files List
### Functions
**ERDDAP_Import: A function to clean data imported from NOAA's ERDDAP data repository**
- Currently tested on Grid DAP Data from here: https://www.ncei.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000
- Not tested on Table DAP Data so far
- Allows you to select variables to keep and "essential variables"
- Converts everything to numeric without totally losing the units by combining the units row with the header

**mapTrack: A function to map shark tracks with time color coding and arrows**
- Accepts many parameters
- Can be adapted, current function is to make a shark track with or without arrows, colored by month or season and optionally faceted by year
- Tag ID number must be first column, Demographic must be second column
- In order to work, variable names MUST BE: Month (optional), DateandTimeUTC (POSIXct class), SharkID (unique identifier with no spaces), lon (longitude), lat (latitude), Season (optional)

Example code to run before running mapTrack:
df <- df %>% 
  select(c(SharkID, DateandTimeUTC, id, lat, lon, Season))
df$DateandTimeUTC <- as.POSIXct(df$DateandTimeUTC, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dx"), tz = "UTC")

**Small Functions: A catch-all document for the simpler functions referenced throughout the codebase**
  - addmonth() adds Month & Seasons columns to a dataframe with a POSIXct datetime column
  - polygonify() converts a POLYGON geometric object (the output of a Google MyMaps layer export) to a dataframe of coordinates

### Example data - input into LocationBinning and then DepartureDateID

**IMMc.csv**
- Sand tiger shark acoustic tagging data from University of Rhode Island Wetherbee lab
- IMM = Immature Males, c = cleaned
- Fixes a few flipped latitudes
- Deletes several incomplete or extraneous columns
- Date column is in UTC time zone and of type chr, so it must be converted to Date or POSIXct for date operations (exporting and importing documents with POSIXct format didn't work so here we are)

**YOYc.csv**
- Sand tiger shark acoustic tagging data from University of Rhode Island Wetherbee lab
- YOY = Young Of the Year, c = cleaned

### Larger code documents, in order 

**LocationBinning**
An R script that demonstrates:
- how to create polygons bounding aggregations sites
- how to bin individual tracker pings into these polygons
- how to clean the data afterwards

**DepartureDateID**
An R script that demonstrates:
- how to create a dataframe of "runs" (Dates that a shark spent in one location) with a function from https://psyteachr.github.io/tutorials/detecting-runs-in-a-sequence.html (Thank you Dale Barr)
- how to pull departure dates & certainties from this dataframe of runs
