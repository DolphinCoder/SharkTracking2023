## R code
## Function to clean a dataset from the NOAA ERDDAP data repository
ERDDAP_Import <- function(Data, vars = NULL, dropNAvars = NULL, avname = "ERDDAPavgs"){
  # Data - a data frame imported from the ERDDAP website
  # vars - a vector of column names you want to keep (time, lat, and lon will be kept automatically)
  # dropNAvars - a vector of column names you want to have no NA values after processing
  
  # Check that Data is a dataframe
  if (class(Data) != "data.frame") {
    stop("ERROR: Data must be an object of type data.frame")
  }
  # Check that dropNAvars is only one argument
  if(length(dropNAvars) > 1) {
    stop("ERROR: dropNAvars only accepts one argument at this time")
  }
  
  Name <- deparse(substitute(Data))
  print(Name)
  print(head(Data[,vars]))
  Data_clean <- filter(Data, Data[,dropNAvars] != "NaN") # I think this only works for one dropNAvar at this time
  Data_clean <- Data_clean[,c("time", "latitude","longitude", vars)]
  print(Data_clean)
  
  strcheck <- c(letters, "_", LETTERS)
  
  newvars <- c()
  # Add units to vars so that later vars calls work
  for (i in 1:length(vars)) {
    var <- vars[i]
    #print(var)
    unit <- (Data[1,var])
    newvar <- paste(var, sep="_", unit)
    #print(newvar)
    newvars <- c(newvars, newvar)
  }
  print(newvars)
  
  newdropNAvars <- c()
  # Add units to vars so that later vars calls work
  for (i in 1:length(dropNAvars)) {
    var <- dropNAvars[i]
    #print(var)
    unit <- (Data[1,var])
    newvar <- paste(var, sep="_", unit)
    #print(newvar)
    newdropNAvars <- c(newdropNAvars, newvar)
  }
  print(newdropNAvars)
  
  if (sum(!(grepl(pattern = paste(strcheck, collapse="|"), x = Data[1,]))) == 0) { # If units row is still present
    # (if all of the values in the first row contain letters and/or underscores)
    colnames(Data_clean) <- gsub(" ", "", paste(colnames(Data_clean), sep = "_", Data_clean[1,])) # Add units + remove spaces
    Data_clean <- Data_clean[-c(1),] # Remove first row (units) now that it's in the column names
    print("Line 31: Units removed")
    print(head(Data_clean))
  } else {
    print("Line 34: Units already integrated")
  }
  
  timename <- "time_UTC" # Time_units
  
  # Clean up the time column so that it can be converted to POSIXct
  Data_clean[,timename] <- gsub("T", " ", Data_clean[,timename])
  Data_clean[,timename] <- gsub("Z", "", Data_clean[,timename])
  # Convert to POSIXct (datetime format)
  Data_clean[,timename] <- as.POSIXct(Data_clean[,timename], tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"))
  
  for (i in 1:length(newvars)){
    print(newvars[i])
    Data_clean[,newvars[i]] <- as.numeric(Data_clean[,newvars[i]])
  }
  
  print(head(Data_clean))
  Data_clean$latitude_degrees_north <- as.numeric(Data_clean$latitude_degrees_north)
  Data_clean$longitude_degrees_east <- as.numeric(Data_clean$longitude_degrees_east)
  
  # Clean up date column name
  names(Data_clean)[names(Data_clean) == "time_UTC"] <- "Date"
  
  print(head(Data_clean))
  
  return(Data_clean)
  #names(Data_clean)[names(Data_clean) == "chlorophyll_mgm-3"] <- "chlorophyll_mgm_3" # I think it would be neat to add newnames = oldnames as a parameter, but not right now
}
