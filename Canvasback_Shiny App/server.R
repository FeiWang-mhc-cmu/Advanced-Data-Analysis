server <- function(input, output){
  
  # Read boundaries shapefile into R (PS: please include all the shapefiles in the same directory)
  ducks <- readOGR(".", layer = "MAS_stratum_boundaries", verbose = FALSE)
  
  # Read segments shapefile into R (PS: please include all the shapefiles in the same directory)
  seg <- readOGR(".", layer = "2013_MAS_segments", verbose = FALSE)
  
  # Convert segment shapefile into dataframe
  df <- data.frame(seg)
  
  # Load population estimates and harvest data into R
  pop <- read.csv("CopyOfcanvasbackEstimates.csv")
  
  # Change the name of hunting policy 
  harvest <- read.csv("CopyOfcanvasbackHarvest.csv")
  harvest$season <- as.character(harvest$season)
  harvest$season[harvest$season == "rc"] <- "close"
  harvest$season[harvest$season == "c"] <- "close"
  harvest$season[harvest$season == "r"] <- "restricted"
  harvest$season[harvest$season == "mr"] <- "restricted"
  harvest$season[harvest$season == "m"] <- "restricted"
  harvest$season[harvest$season == "l"] <- "liberal (1-bird-bag)"
  harvest$season[harvest$season == "l2"] <- "liberal (2-bird-bag)"
  
  # Create marker icon
  duckIcon <- makeIcon(
    iconUrl = "http://www.gschneiderphoto.com/gallery3/var/albums/birds/ducks/canvasback/canvasback-drake-flying_2011.jpg",
    iconWidth = 25, iconHeight = 25)
  
  
  #2005 Map
  
  ## Manipulation of Data
  # Select the data of year 2005
  pop2005 <- pop %>%
    filter(year == 2005)
  
  harvest2005 <- harvest %>%
    filter(year == 2005)
  
  # Segment estimated population and coordinates 
  df2005 <- merge(df, pop2005, by = "stratum") 
  
  df2005 <- df2005 %>%
    mutate(lat = (latto + latfrom)/2, long = (longto + longfrom)/2) %>%
    select(stratum, segment, lat, long, popEst, sePopEst, VCF, seVCF)
  
  ## Create Leaflet
  # Function for color: change the color according to estimated population 
  pal <- colorNumeric(
    palette = "Blues", 
    domain = df2005$popEst
  )  
  # PS: If it is "liberal", color will be "Greens". If it is "restricted", color will be "Blues". If it is "closed", color will be "Reds".
  
  # Create marker geographical coordinates
  marker2005 <- df2005[!duplicated(df2005$stratum),]
  # PS: We only have the coordinates for Traditional Survey Strata
  
  # Create leaflet
  y2005 <- leaflet(ducks) %>%
    addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
    addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(pop2005$popEst)) %>%
    addLegend("bottomright", pal = pal, values = pop2005$popEst, 
              title = "Est. Population", opacity = 1) %>%
    addMarkers(lat = marker2005$lat,  lng = marker2005$long, icon = duckIcon, 
               popup=paste("Year:", 2005, "<br>", "Stratum:", marker2005$stratum,"<br>", 
                           "Population:", marker2005$popEst, "<br>",
                           "Standard Error", marker2005$sePopEst, "<br>")) 
  
  
  # 2008 Map
  
  ## Manipulation of Data
  # Select the data of year 2008
  pop2008 <- pop %>%
    filter(year == 2008)
  
  harvest2008 <- harvest %>%
    filter(year == 2008)
  
  # Segment estimated population and coordinates 
  df2008 <- merge(df, pop2008, by = "stratum") 
  
  df2008 <- df2008 %>%
    mutate(lat = (latto + latfrom)/2, long = (longto + longfrom)/2) %>%
    select(stratum, segment, lat, long, popEst, sePopEst, VCF, seVCF)
  
  
  ## Create Leaflet
  # Function for color: change the color according to estimated population 
  pal <- colorNumeric(
    palette = "Reds", 
    domain = df2008$popEst
  )  
  # PS: If it is "liberal", color will be "Greens". If it is "restricted", 
  # color will be "Blues". If it is "closed", color will be "Reds".
  
  # Create marker geographical coordinates
  marker2008 <- df2008[!duplicated(df2008$stratum),]
  # PS: We only have the coordinates for Traditional Survey Strata
  
  # Create leaflet
  y2008 <- leaflet(ducks) %>%
    addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
    addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0, 
                color = ~pal(pop2008$popEst)) %>%
    addLegend("bottomright", pal = pal, values = pop2008$popEst, 
              title = "Est. Population", opacity = 1) %>%
    addMarkers(lat = marker2008$lat,  lng = marker2008$long, icon = duckIcon, 
               popup=paste("Year:", 2008,"<br>", "Stratum:", marker2008$stratum,"<br>", 
                           "Population:", marker2008$popEst, "<br>",
                           "Standard Error", marker2008$sePopEst, "<br>")) 
  
  
  #2012 Map
  
  ## Manipulation of Data
  # Load population estimates and harvest data into R
  pop <- read.csv("CopyOfcanvasbackEstimates.csv")
  harvest <- read.csv("CopyOfcanvasbackHarvest.csv")
  
  # Select the data of year 2012
  pop2012 <- pop %>%
    filter(year == 2012)
  
  harvest2012 <- harvest %>%
    filter(year == 2012)
  
  # Segment estimated population and coordinates 
  df2012 <- merge(df, pop2012, by = "stratum") 
  
  df2012 <- df2012 %>%
    mutate(lat = (latto + latfrom)/2, long = (longto + longfrom)/2) %>%
    select(stratum, segment, lat, long, popEst, sePopEst, VCF, seVCF)
  
  
  ## Create Leaflet
  # Function for color: change the color according to estimated population 
  pal <- colorNumeric(
    palette = "Greens", 
    domain = df2012$popEst
  )  
  # PS: If it is "liberal", color will be "Greens". If it is "restricted", color will be "Blues". If it is "closed", color will be "Reds".
  
  # Create marker geographical coordinates
  marker2012 <- df2012[!duplicated(df2012$stratum),]
  # PS: We only have the coordinates for Traditional Survey Strata
  
  # Create leaflet
  y2012 <- leaflet(ducks) %>%
    addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
    addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~pal(pop2012$popEst)) %>%
    addLegend("bottomright", pal = pal, values = pop2012$popEst, 
              title = "Est. Population", opacity = 1) %>%
    addMarkers(lat = marker2012$lat,  lng = marker2012$long, icon = duckIcon, 
               popup=paste("Year:", 2012, "Stratum:", marker2012$stratum,"<br>", 
                           "Population:", marker2012$popEst, "<br>",
                           "Standard Error", marker2012$sePopEst, "<br>")) 
  
  
  
  
  
  
  output$mymap1 <- renderLeaflet(y2005)
  
  output$mymap2 <- renderLeaflet(y2008)
  
  output$mymap3 <- renderLeaflet(y2012)
  
  output$table <- renderTable({
    harvest %>% 
      filter(year==input$year)})
  
  trellis.par.set(theme=theme.mosaic())   
  output$plot <- renderPlot({
    ggplot({data = pop %>% 
      filter(year==input$year)}, aes(x = as.factor(stratum), y = popEst)) +
      geom_bar(stat = "identity") + 
      xlab("Stratum") +
      ylab("Population") +
      ggtitle("Population Estimate")
  })
  
}





