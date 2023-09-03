## Fairly simple functions referenced throughout this code base
addmonth=function(Data){ # Adds month and season columns
  print("WARNING: Remember to assign the result of this to a new dataframe!")
  print("WARNING: datetime must be of class POSIXct and called exactly DateandTimeUTC")
  Data <- Data %>% 
    mutate(Month = as.factor(month(DateandTimeUTC)))
  Data <- Data %>% # Mutate to add columns for month and season
    mutate(Season = case_when(Month == "12" | Month == "1" | Month == "2" ~ "Winter", Month == "3" | Month == "4" | Month == "5" ~ "Spring", Month == "6" | Month == "7" | Month == "8" ~ "Summer", Month == "9" | Month == "10" | Month == "11" ~ "Fall"))
}

polygonify1=function(Data) { # Converts a POLYGON object (output of Google MyMaps polygon) to a vector of coordinates
  Data <- gsub("POLYGON\\ ", "", Data)
  Data <- gsub("(", "", Data, fixed = T)
  Data <- gsub(")", "", Data, fixed = T) # Remove all non-number stuff, fixed = T makes () work
  Data <- gsub(", ", ",", Data, fixed = T) # Compress commas with spaces - this will prevent ,, on next line
  Data <- gsub(" ", ",", Data, fixed = T)
  print("Remember to assign this to a variable")
  Data <- as.numeric(strsplit(Data,split=",",fixed=TRUE)[[1]]) # Convert to vector
  d2 <- Data[c(T, F)]
  d3 <- Data[c(F, T)]
  d4 <- data.frame(x=d2,y=d3)
  return(d4)
}
