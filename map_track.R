library(tidyverse)
library(lubridate) # Just in case

## Function to map shark tracks color coded by season with arrows
map_track=function(Data, trackTitle, exportPath, yearFacet = F, arrow = T, by = c("Month", "Season"), xlim = c(-90,-40), ylim = c(20,50)) {
  print("WARNING: Tag ID number must be first column, Demographic must be second column, datetime must be called DateandTimeUTC")
  print("WARNING: Latitude column name must be exactly 'lat' and longitude must be 'lon'")
  # Check classes
  if (!("data.frame" %in% class(Data))) { # Some data have multiple classes for some reason
    stop("ERROR: Data must be an object of type data.frame")
  }
  if (class(trackTitle) != "character") {
    stop("ERROR: trackTitle must be an object of type character")
  }
  if (!("POSIXct" %in% class(Data$DateandTimeUTC))) {
    stop("ERROR: DateandTimeUTC must be named exactly that and be of type POSIXct. God have mercy on your soul and all that.")
  }
  
  # Define palettes
  season_pal <- c("Fall" = "orangered3", Summer = "forestgreen", Spring = "hotpink2", Winter = "turquoise3")
  month_pal <- c("6"="red4", "7"="orangered3", "8"="orangered", "9"="gold2", "10"="chartreuse2", "11"="forestgreen", "12"="turquoise3", "1"="steelblue3", "2"="royalblue3", "3"="mediumorchid4", "4"="hotpink2", "5"="violetred3")
  
  if (!("Season" %in% colnames(Data))){ # Add month and season columns for color if not already present
    Data <- Data %>% 
      mutate(Month = as.factor(month(DateandTimeUTC)))
    Data <- Data %>% # Mutate to add columns for month and season
      mutate(Season = case_when(Month == "12" | Month == "1" | Month == "2" ~ "Winter", Month == "3" | Month == "4" | Month == "5" ~ "Spring", Month == "6" | Month == "7" | Month == "8" ~ "Summer", Month == "9" | Month == "10" | Month == "11" ~ "Fall"))
  }
  
  # Set it up so that if yearFacet = T, faceting will occur
  if(yearFacet == T) {
    faceting = vars(year(DateandTimeUTC))
  } else {
    faceting = vars(SharkID) # Since this is always done by shark, this just adds a lil subtitle
  }
  # If arrow = T then arrows will happen
  if(arrow == T) {
    arrowPar <- arrow(length = unit(0.005, "npc"), type="closed")
    segThicc <- 1
  } else {
    arrowPar <- NULL
    segThicc <- 2
  }
  
  Data2 <- Data %>% 
    mutate(xend=lead(lon), yend=lead(lat)) # Mutate to add a leading column to divide segments in half
  
  if(by == "Season"){
    ggplot(data=world_df) +
      geom_polygon(aes(x=long,y=lat,group=group),fill="gray90",color="black") +
      coord_fixed(xlim = xlim, ylim = ylim, ratio = 1.3) +
      geom_segment(data=Data2, aes(x=(lon+xend)/2,y=(lat+yend)/2,xend=xend,yend=yend, color = Season), linewidth = segThicc) +
      # Plot second half of segment, then plot first half with arrow, color by season
      geom_segment(data=Data2, aes(x=lon,y=lat,xend=(xend+lon)/2,yend=(yend+lat)/2, color = Season), arrow=arrowPar, linewidth = segThicc) +
      scale_color_manual(values = season_pal) + # Color by season
      ggtitle(paste(trackTitle, sep = " ", Data[1,1])) + # Title by ID number
      labs(subtitle = Data[1,2]) + # Subtitle by demographic
      theme(text = element_text(size=20), # Change background and line colors
            panel.background = element_rect(fill = "azure1", colour = "azure1"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")) +
      facet_wrap(facets = faceting) ## Facets by year if faceting exists
    if(yearFacet == T) { # Change filename if yearFacet is true
      ggsave(path=exportPath, filename=paste(trackTitle, sep = "_", paste(paste(Data[1,1], sep = "_", "SeasonPath_byYear"), sep=".", "png")), width = 3000, height = 3000, units = "px")
    } else {
      ggsave(path=exportPath, filename=paste(trackTitle, sep = "_", paste(paste(Data[1,1], sep = "_", "SeasonPath"), sep=".", "png")), width = 3000, height = 3000, units = "px")
    }
  } else {
    if(by == "Month") {
      ggplot(data=world_df) +
        geom_polygon(aes(x=long,y=lat,group=group),fill="gray90",color="black") +
        coord_fixed(xlim = xlim, ylim = ylim, ratio = 1.3) +
        geom_segment(data=Data2, aes(x=(lon+xend)/2,y=(lat+yend)/2,xend=xend,yend=yend, color = Month), linewidth = segThicc) +
        # Plot second half of segment, then plot first half with arrow, color by month
        geom_segment(data=Data2, aes(x=lon,y=lat,xend=(xend+lon)/2,yend=(yend+lat)/2, color = Month), arrow=arrowPar, linewidth = segThicc) +
        scale_color_manual(values = month_pal) + # Color by month
        ggtitle(paste(trackTitle, sep = " ", Data[1,1])) + # Title by ID number
        labs(subtitle = Data[1,2]) + # Subtitle by demographic
        theme(text = element_text(size=20),
              panel.background = element_rect(fill = "azure1", colour = "azure1"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white")) +
        facet_wrap(facets = faceting) ## Facets by year if faceting exists
      if(yearFacet == T) {
        ggsave(path=exportPath, filename=paste(trackTitle, sep = "_", paste(paste(Data[1,1], sep = "_", "MonthPath_byYear"), sep=".", "png")), width = 3000, height = 3000, units = "px")
      } else {
        ggsave(path=exportPath, filename=paste(trackTitle, sep = "_", paste(paste(Data[1,1], sep = "_", "MonthPath"), sep=".", "png")), width = 3000, height = 3000, units = "px")
      }
    } else {
      stop("ERROR: 'by' not specified, please specify by = 'Month' or by = 'Season'")
    }
  }
}