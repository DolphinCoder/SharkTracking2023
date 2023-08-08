# SharkTracking2023
Summer 2023 REU code - all code used to study shark migration

## Files List
### Functions
**ERDDAP_Import: A function to clean data imported from NOAA's ERDDAP data repository**
- Currently tested on Grid DAP Data from here: https://www.ncei.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000
- Not tested on Table DAP Data so far
- Allows you to select variables to keep and "essential variables"
- Converts everything to numeric without totally losing the units by combining the units row with the header

**mapTrack: A function to map shark tracks with time color coding and arrows**
- Accepts many parameters

**Small Functions: A catch-all document for the simpler functions referenced throughout the codebase**
  - addmonth() adds Month & Seasons columns to a dataframe with a POSIXct datetime column

### Larger code documents
