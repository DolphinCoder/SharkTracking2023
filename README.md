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
- Variable names MUST BE: Month, DateandTimeUTC, SharkID, lon, lat, Season, 

**Small Functions: A catch-all document for the simpler functions referenced throughout the codebase**
  - addmonth() adds Month & Seasons columns to a dataframe with a POSIXct datetime column
  - polygonify() converts a POLYGON geometric object (the output of a Google MyMaps layer export) to a dataframe of coordinates

### Larger code documents

**LocationBinning**
An R script that demonstrates:
- how to create polygons bounding aggregations sites
- how to bin individual tracker pings into these polygons
- how to clean the data afterwards

**DepartureDateID**
An R script that demonstrates:
- how to create a dataframe of "runs" (Dates that a shark spent in one location) with a function from https://psyteachr.github.io/tutorials/detecting-runs-in-a-sequence.html (Thank you Dale Barr)
- how to pull departure dates & certainties from this dataframe of runs
