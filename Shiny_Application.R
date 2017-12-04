#load libraries
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(viridis)
library(plotly)
library(shiny)
library(readr)
library(ggthemes)  
library(sqldf)
library(scales)

# Reading Crashes File
crashes <- read.csv('crashes.csv')

# UI File
ui <-  shinyUI(
  navbarPage("Accident Analysis",
             #Tab 1
              tabPanel("Top City",
                      sidebarLayout(
                        sidebarPanel(
                            fluidPage(
                              titlePanel("Accident Analysis"),
                                fluidRow(
                                
                                  column(8, wellPanel
                                          (
                                            # Problem Selection Select Input
                                            selectInput("input_type", "Problem Selection:",
                                              c("Street Light Problem", "Other Problem"))
                                          )
                                        ),
                                
                                  column(8, wellPanel
                                          (
                                            # Generates dynamic UI component
                                            uiOutput("ui")
                                          )
                                        )
                                       )
                                      ),
                                # Number of City Slider
                                sliderInput("city_count", "Number of City:", 
                                        min = 1, max = 10, value = 5, step= 1)
                                      ),
                                mainPanel
                                (
                                  h1(textOutput("caption_1")),
                                  h1(textOutput("caption_8")),
                                  br(),
                                  # Accident by City
                                  plotOutput("accidentPlot"),
                                  br(),
                                  h1(textOutput("caption_9")),
                                  br(),
                                  # Major Accident Suburb on Leaflet Map
                                  leafletOutput("accidentMap"),
                                  br(),
                                  h1(textOutput("caption_10")),
                                  br(),
                                  # Accident locations in Major Suburb on Leaflet Map
                                  leafletOutput("accidentIncident")
                                )
                              )
                            ),
             # Tab 2
             tabPanel("Time Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("Year", "Year Range:",
                                      min = 2012, max = 2017, value = c(2012,2017),sep = "")
                                      ),
                          mainPanel(
                            h1(textOutput("caption_2")),
                            h1(textOutput("caption_11")),
                            h1(textOutput("caption_12")),
                            # Heat map of Month VS Accident Type
                            h3(textOutput("caption_18")),
                            plotOutput("heatmap"),
                            fluidRow(
                              column(12,
                                     "",
                                     fluidRow(
                                       
                                       # Accidents by week day
                                       column(width = 5, h1(textOutput("caption_13")), br(),
                                              plotOutput("DayPlot"), height = "100px"),
                                       
                                       # Accidents by Hour
                                       column(width = 7, h1(textOutput("caption_14")), br(),
                                              plotOutput("HourPlot"), height = "100px")
                                     )
                                     
                              ))
                          )
                      )
             ),
             # Tab 3
             tabPanel("Demographics",
                      sidebarLayout(
                        sidebarPanel(
                          #Year selection slider
                          sliderInput("Year_1", "Year Range:",min = 2012, max = 2017, value = c(2012,2017),sep = ""),
                          # Gender Checkbox
                          checkboxInput(inputId = "demographics_gender",
                                        label = strong("Show Gender Demographics"),
                                        value = FALSE),
                          # Injury Checkbox
                          checkboxInput(inputId = "demographics_injury",
                                        label = strong("Show Injury Demographics"),
                                        value = FALSE),
                          # People Checkbox
                          checkboxInput(inputId = "demographics_people_type",
                                        label = strong("Show People Demographics"),
                                        value = FALSE),
                          # Injury Type Checkbox
                          checkboxInput(inputId = "demographics_injury_type",
                                        label = strong("Show Injury Type Demographics"),
                                        value = FALSE),
                          # Vehicle Checkbox
                          checkboxInput(inputId = "demographics_vehicle_type",
                                        label = strong("Show Vehicle Demographics"),
                                        value = FALSE)
                        ),
                        mainPanel(
                          h1(textOutput("caption_3")),
                          h1(textOutput("caption_19")),
                          br(),
                          fluidRow(
                            column(12,
                                   "",
                                   fluidRow(
                                     # Pie chart by Gender
                                     column(width = 6, plotOutput("GenderPlot"), height = "100px"),
                                     
                                     # Pie chart by Injury
                                     column(width = 6, plotOutput("InjuryPlot"), height = "100px"),
                                     
                                     # Pie chart by People
                                     column(width = 6, plotOutput("PeoplePlot"), height = "100px"),
                                     
                                     # Pie chart by Injury Type
                                     column(width = 6, plotOutput("InjuryTypePlot"), height = "100px"),
                                     
                                     # Pie chart by Vehicle
                                     column(width = 6, plotOutput("VehiclePlot"), height = "100px")
                                   )
                                   
                            ))
                        )
                      )
             ),
             # Tab 4
             tabPanel("Important Parameters",
                      sidebarLayout(
                        sidebarPanel(
                          # Year selection slider
                          sliderInput("Year_2", "Year Range:",min = 2012, max = 2017, value = c(2012,2017),sep = ""),
                          # Speed graph Checkbox
                          checkboxInput(inputId = "speed_type",
                                        label = strong("Show Speed Analysis"),
                                        value = FALSE),
                          # Road Type Checkbox
                          checkboxInput(inputId = "road_type",
                                        label = strong("Show Road Type Analysis"),
                                        value = FALSE),
                          # Road Geometry Checkbox
                          checkboxInput(inputId = "geometry_type",
                                        label = strong("Show Road Type Analysis"),
                                        value = FALSE)
                        ),
                        mainPanel(
                          h1(textOutput("caption_4")),
                          h1(textOutput("caption_20")),
                          br(),
                          fluidRow(
                            column(12,
                                   "",
                                   fluidRow(
                                     
                                     # Accident by speed in top city
                                     column(width = 6,plotOutput("SpeedPlot"), height = "100px"),
                                     
                                     # Accident by road type in top city
                                     column(width = 6,plotOutput("RoadPlot"), height = "100px"),
                                     
                                     # Accident by road geometry in top city
                                     column(width = 6,plotOutput("GeometryPlot"), height = "100px")
                                   )
                                   
                            ))
                        )
                      )
             ),
             # Tab 5
             navbarMenu("More Feature",
             # Sub Menu 1
             tabPanel("Accident Parameters",
                      sidebarLayout(
                        sidebarPanel(
                          # Year selection slider
                          sliderInput("Year_3", "Year Range:",
                                      min = 2012, max = 2017, value = c(2012,2017),sep = ""),
                          # Analysis selection select input
                          selectInput("select_condition", "Analysis Type:", 
                                      c("Alcohol Involved" = "alcohol", 
                                        "Hit And Run" = "hit",
                                        "Police Involved" = "police",
                                        "Both Alcohol Involved and Hit And Run" = "both_ah",
                                        "Both Alcohol Involved and Police Involved" = "both_ap",
                                        "Both Police Involved and Hit And Run" = "both_hp",
                                        "Alcohol Involved, Police Involved and Hit And Run" = "all_ahp")
                          )
                        ),
                        mainPanel(
                          h1(textOutput("caption_5")),
                          h1(textOutput("caption_21")),
                          br(),
                          plotOutput("AnalysisPlot")
                        )
                      )
             ),
             # Sub Menu 2
             tabPanel("Accident Count by City",
                      h1(textOutput("caption_6")),
                      h1(textOutput("caption_15")),
                      br(),
                      # All accident suburb shown in leaflet graph
                      column(8,leafletOutput("map", height="500px")),
                      # Accident count by city
                      column(4,h3(textOutput("caption_17")),
                             plotOutput("plot", height="400px"))
                      
             ),
             # Sub Menu 3
             tabPanel("Frequent Accident Locations",
                      
                      h1(textOutput("caption_7")),
                      h1(textOutput("caption_16")),
                      br(),
                      # Frequent accident locations by suburb
                      leafletOutput("frequentAccident")
                      
             )
             )
             )
  
)

server <- (
  
  
  function(input, output) 
    {
#####################################################################################################
# Step 1: Rendering Dynamic UI
####################################################################################################  
    # Rendering Dynamic Input
    output$ui <- renderUI({
      if (is.null(input$input_type))
        return()
      
      # UI component Depending on input$input_type and send it to the client.
      switch(input$input_type,
             # Light Condition Select Input
             "Street Light Problem" = selectInput("light_condition", "Light Condition Type:", 
                                                         c("Dark  No Street Lights" = "Dark Streets without Street lights", 
                                                           "Dark Street Lights Off" = "Dark Street with Street lights off",
                                                           "Dark Street Lights On" = "Dark Street with Street lights on",
                                                           "Dark Street Lights Unknown" = "Dark Street with Street lights condition unknown",
                                                           "Dusk/Dawn" = "Dusk/Dawn Time",
                                                           "Unknown Light Conditions" = "Unknown Light Condition",
                                                           "Day" = "Day Time"),
                                                         selected = "Dark Streets without Street lights"
                                                         
             ),
             # Other Problem Select Input
             "Other Problem" = selectInput("select_type", "Accident Cause:", 
                                           c("Alcohol Involved" = "Alcohol Consumption", 
                                             "Hit And Run Involved" = "Hit and Run Involvement",
                                             "Police Involved" = "Police Attention Involved",
                                             "Vehicle Run Off Road" = "Vehicle Ran Off Road"
                                           ),
                                           selected = "Alcohol Consumption"
             )
      )
      
    })
    

#####################################################################################################
# Step 2: Generating Captions
####################################################################################################
    # Generating Caption 1 Dynamically
    output$caption_1 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 2 Dynamically
    output$caption_2 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 3 Dynamically
    output$caption_3 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 4 Dynamically
    output$caption_4 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 5 Dynamically
    output$caption_5 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 6 Dynamically
    output$caption_6 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Caption 7 Dynamically
    output$caption_7 <- reactiveText(function() {
      
      switch(input$input_type,
             "Street Light Problem" =
             {
               paste("Light Condition Type:", input$light_condition)
             },
             "Other Problem" =
             {
               paste("Accident Cause:", input$select_type)
             }
      )
      
    })
    
    # Generating Accidents per City title
    output$caption_8 <- reactiveText(function() {
      paste("Accidents per City:")
    })
    
    # Generating Major accident suburbs title
    output$caption_9 <- reactiveText(function() {
      paste("Major accident suburbs:")
    })
    
    # Generating Accidents incidents title
    output$caption_10 <- reactiveText(function() {
      paste("Accidents incidents:")
    })
    
    # Generating year limits caption dynamically
    output$caption_11 <- reactiveText(function() 
      {
        paste("From Year: ", input$Year[1]," to Year: ", input$Year[2])
      })
    
    # Generating year limits caption dynamically
    output$caption_19 <- reactiveText(function() 
    {
      paste("From Year: ", input$Year_1[1]," to Year: ", input$Year_1[2])
    })
    
    # Generating year limits caption dynamically
    output$caption_20 <- reactiveText(function() 
    {
      paste("From Year: ", input$Year_2[1]," to Year: ", input$Year_2[2])
    })
    
    # Generating year limits caption dynamically
    output$caption_21 <- reactiveText(function() 
    {
      paste("From Year: ", input$Year_3[1]," to Year: ", input$Year_3[2])
    })
    
    # Generating Heat Map of Accident Type Vs Month title
    output$caption_12 <- reactiveText(function() 
    {
      
      paste("Heat Map of Accident Type Vs Month:")
      
    })
    
    # Generating Accident by Week Day title
    output$caption_13 <- reactiveText(function() 
    {
      paste("Accident by Week Day:")
    })
    
    # Generating Accident by Hour title
    output$caption_14 <- reactiveText(function() 
    {
      paste("Accident by Hour:")
    })
    
    # Generating Accident Count by City title
    output$caption_15 <- reactiveText(function() 
    {
      paste("Accident Count by City:")
    })
    
    # Generating Frequent Accident Spots title
    output$caption_16 <- reactiveText(function() 
    {
      paste("Frequent Accident Spots:")
    })
    
    # Generating additional information for heat map title
    output$caption_18 <- reactiveText(function() 
    {
      paste("(Note: Empty tiles means no accident took place)")
    })

#####################################################################################################
# Step 3: Plotting Top city graph
####################################################################################################    
# Reactive function for Top City graph    
output$accidentPlot <- reactivePlot(function() {
      # Selecting crashes subset according to select input conditions dynamically 
      switch(input$input_type,
             "Street Light Problem" =
             {
               # Check for the input variable = Dark No Street Lights
               if (input$light_condition == "Dark Streets without Street lights") 
                 {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Dark No street lights"), ]
                 
                }
               # Check for the input variable = Dark Street Lights Off
               else if (input$light_condition == "Dark Street with Street lights off") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Dark Street lights off"), ]
                 
                }
               # Check for the input variable = Dark Street Lights On
               else if (input$light_condition == "Dark Street with Street lights on") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Dark Street lights on"), ]
                 
                }
               # Check for the input variable = Dark Street Lights Unknown
               else if (input$light_condition == "Dark Street with Street lights condition unknown") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Dark Street lights unknown"), ]
                 
                }
               # Check for the input variable = Dusk/Dawn
               else if (input$light_condition == "Dusk/Dawn Time") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Dusk/Dawn"), ]
                 
                }
               # Check for the input variable = Unknown
               else if (input$light_condition == "Unknown Light Condition") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Unknown"), ]
                 
                }
               # Check for the input variable = Day
               else if (input$light_condition == "Day Time") 
                {
                 
                 crashes_sub <- crashes[ which(crashes$LIGHT_CONDITION == "Day"), ]
                 
                }
             },
            "Other Problem" =
            {
              
              switch(input$select_type,
                     # Check for the input variable = Alcohol Consumption
                     "Alcohol Consumption" =
                     {
                       crashes_sub <- crashes[ which(crashes$ALCOHOL_RELATED == "Yes"), ]
                     },
                     # Check for the input variable = Hit and Run Involvement
                     "Hit and Run Involvement" =
                     {
                       crashes_sub <- crashes[ which(crashes$HIT_RUN_FLAG == "Yes"), ]
                     },
                     # Check for the input variable = Police Attention Involved
                     "Police Attention Involved" =
                     {
                       crashes_sub <- crashes[ which(crashes$POLICE_ATTEND == "Yes"), ]
                     },
                     # Check for the input variable = Vehicle Ran Off Road
                     "Vehicle Ran Off Road" =
                     {
                       crashes_sub <- crashes[ which(crashes$RUN_OFFROAD == "Yes"), ]
                     }
              )
            }
      )
      
      accident_places <- crashes_sub %>%
        group_by(LGA_NAME) %>%
        summarize(total_accident = n()) %>%
        arrange(desc(total_accident)) %>%
        top_n(n = input$city_count)

      # Plotting graph for top city
      top_city <- ggplot(accident_places, aes(x = LGA_NAME, y = total_accident, fill = LGA_NAME )) +
        geom_bar(stat = "identity") +
        labs(x="City Name", y = "Total Accidents") + 
        geom_text(aes(label = total_accident), vjust =  -.5) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        # In order to add title can use below code but adding caption instead
        # ggtitle("Accident per City") +
        # theme(plot.title = element_text(size = 30, face = "bold")) +
      print(top_city)
      
      
      # create a reactive value that will store the click position
      city_name <- reactiveValues(clickedMarker=NULL)
      
#####################################################################################################
# Step 3: Plotting Leaflet map with markers for accident cities
####################################################################################################    
      
      output$map <- renderLeaflet({
        leaflet() %>%
          setView(lng=144 , lat =-37, zoom=6) %>%
          addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
          addCircleMarkers(data=crashes_sub, ~LONGITUDE , ~LATITUDE, 
                           layerId=~LGA_NAME, popup=~LGA_NAME, radius=8 , 
                           color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8)
      })

      # store the click
      observeEvent(input$map_marker_click,{
        city_name$clickedMarker <- input$map_marker_click
      })
      
#####################################################################################################
# Step 4: Make a bar graph depending of the selected point
####################################################################################################

      output$plot=renderPlot({
        accident_place=city_name$clickedMarker$id
        if(is.null(accident_place))
        {
          accident_place="Not Selected"
        }
        if(accident_place=="Not Selected")
        {
          # Generating caption
          output$caption_17 <- reactiveText(function() {
            paste("Click on individual point for accident count")
          })

          counts <- table(crashes_sub$LGA_NAME)
          barplot(counts, main="Accidents by City",
                  xlab="City Name",
                  ylab="Number of Accidents")
          
        }
        else
        {
          accident_by_city <- crashes_sub[(crashes_sub$LGA_NAME  == accident_place),]

          accident_summary <- accident_by_city %>%
            group_by(LGA_NAME) %>%
            summarize(total_accident = n())

          accident_count <- ggplot(accident_summary, aes(x = LGA_NAME, y = total_accident,fill =LGA_NAME)) +
            geom_bar(stat = "identity", width=0.2) +
            labs(x="City Name", y = "Total Accidents") +
            ggtitle("Accident per City") +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            geom_text(aes(label = total_accident), vjust =  -.5)
          print(accident_count)
        }
      })
    
#####################################################################################################
# Step 5: Plotting Leaflet map with markers for top accident cities
####################################################################################################    
    # Create leaflet map
    output$accidentMap <- renderLeaflet({ 
      
      unique_location <- unique(crashes_sub[c("LGA_NAME", "LONGITUDE","LATITUDE")])
      selectedCityRows <- (unique_location$LGA_NAME %in% accident_places$LGA_NAME)
      uniqueCityRows <- unique_location[selectedCityRows,]
      uniqueCityAddress <- uniqueCityRows %>% group_by(LGA_NAME) %>%
        summarise(Longitude=mean(LONGITUDE), Latitude=mean(LATITUDE))
      
      leaflet(data = uniqueCityAddress) %>%
        setView(lng=144 , lat =-37, zoom=7) %>%
        addTiles() %>%
        addMarkers(~Longitude,
                   ~Latitude,
                   popup = ~as.character(LGA_NAME))
    })
    
    
#####################################################################################################
# Step 6: Plotting Leaflet map with cluster of actual site locations
####################################################################################################  
    # Create leaflet map
    output$accidentIncident <- renderLeaflet({ 
      
      unique_location <- crashes_sub[c("LGA_NAME", "LONGITUDE","LATITUDE")]
      selectedCityRows <- (unique_location$LGA_NAME %in% accident_places$LGA_NAME)
      uniqueCityRows <- unique_location[selectedCityRows,]
      leaflet(uniqueCityRows) %>%
        setView(lng=144 , lat =-37, zoom=6) %>%
        addTiles() %>% 
        addMarkers(
        clusterOptions = markerClusterOptions()
      )
      
    })
    
#####################################################################################################
# Step 7: Plotting Leaflet map with markers for frequent accident locations
####################################################################################################  
    #Create leaflet map
    output$frequentAccident <- renderLeaflet({
      unique_location <- crashes_sub[c("LGA_NAME", "LONGITUDE","LATITUDE")]
      unique_location_2 <- unique_location[c(duplicated(unique_location) | duplicated(unique_location, fromLast = T)),]
      unique_location_3 <- unique(unique_location_2)

      leaflet(data = unique_location_3) %>%
        setView(lng=144 , lat =-37, zoom=6) %>%
        addTiles() %>%
        addMarkers(~LONGITUDE,
                   ~LATITUDE,
                   popup=paste ("CITY :",unique_location_3$LGA_NAME,"<br>", 
                                "LONGITUDE :",unique_location_3$LONGITUDE,"<br>",
                                "LATITUDE :",unique_location_3$LATITUDE))
    })
    
      
#####################################################################################################
# Step 8: Plotting Heat map of Month VS Accident Type
####################################################################################################      
      output$heatmap <- reactivePlot(function() {
        date <- crashes_sub$ACCIDENT_DATE
        df1 <- data.frame(DateTime = date)
        crashes_sub$YEAR <- c()
        for (j in df1)
        {
          date_lub <- dmy(j)
          y <- as.numeric(year(date_lub))
          crashes_sub$YEAR <- as.data.frame(y)
        }
        
        crashes_year <- crashes_sub[crashes_sub$YEAR >= input$Year[1] & crashes_sub$YEAR <= input$Year[2],]
        crashes_year$Month_name <- months(as.Date(crashes_year$ACCIDENT_DATE))
        group_type <- subset(crashes_year, select=c(5,15,45))
        selectedCityRows <- (group_type$LGA_NAME %in% accident_places$LGA_NAME)
        uniqueCityRows_6 <- group_type[selectedCityRows,]

        
        accident_type <- uniqueCityRows_6 %>%
          group_by(ACCIDENT_TYPE,Month_name) %>%
          summarize(total_accident = n()) %>%
          arrange(ACCIDENT_TYPE)
        accident_type$Month_name <- factor(accident_type$Month_name, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
        
        
        heat <- ggplot(accident_type, aes(x=Month_name, y=ACCIDENT_TYPE, fill=total_accident)) +
          geom_tile(color="yellow", size=0.1) + 
          geom_text(aes(label = total_accident), vjust =  -.5) +
          coord_equal() + labs(x=NULL, y=NULL) +
          theme_tufte(base_family="Arial") +  
          theme(axis.text=element_text(size=12)) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          theme(legend.title=element_text(size=10))
        
        print(heat)

#####################################################################################################
# Step 9: Plotting graph of Accident count vs Weekday
####################################################################################################        
        output$DayPlot <- renderPlot({
          
          
          accident_day <- crashes_year[c("DAY_OF_WEEK", "LGA_NAME")]
          selectedCityRows <- (accident_day$LGA_NAME %in% accident_places$LGA_NAME)
          #View(selectedCityRows)
          uniqueCityRows_7 <- accident_day[selectedCityRows,]
          
          accident_day <- uniqueCityRows_7 %>%
            group_by(DAY_OF_WEEK) %>%
            summarize(total_accident = n()) %>%
            arrange(desc(total_accident))
          
          accident_day$DAY_OF_WEEK <- factor(accident_day$DAY_OF_WEEK, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
          
          top_day <- ggplot(accident_day, aes(x = DAY_OF_WEEK, y = total_accident)) + 
            geom_bar(aes(fill=total_accident), stat = "identity") + 
            labs(x="Day of Week", y = "Total Accidents") + 
            geom_text(aes(label = total_accident), vjust =  -.5) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
          print(top_day)
        })

#####################################################################################################
# Step 10: Plotting graph of Accident count vs Hour
####################################################################################################         
        output$HourPlot <- renderPlot({
          
          accident_hour <- crashes_year[c("ACCIDENT_TIME", "LGA_NAME")]
          selectedCityRows <- (accident_hour$LGA_NAME %in% accident_places$LGA_NAME)
          uniqueCityRows_8 <- accident_hour[selectedCityRows,]
          
          time <- uniqueCityRows_8$ACCIDENT_TIME
          df2 <- data.frame(DateTime = time)
          uniqueCityRows_8$HOUR <- c()
          for (i in df2){
            time.lub <- hms(i)
            h <- as.numeric(hour(time.lub))
            uniqueCityRows_8$HOUR <- as.data.frame(h)
          }
          counts <- table(uniqueCityRows_8$HOUR)
          top_hour <- barplot(counts,
                              xlab="City Name",
                              ylab="Number of Accidents")
          text(top_hour, counts, labels = counts, pos = 3)
        })
        
})
      
#####################################################################################################
# Step 11: Plotting Demographics of Gender
####################################################################################################       
      
output$GenderPlot <- renderPlot({
        
        date <- crashes_sub$ACCIDENT_DATE
        df1 <- data.frame(DateTime = date)
        crashes_sub$YEAR <- c()
        for (j in df1)
        {
          date_lub <- dmy(j)
          y <- as.numeric(year(date_lub))
          crashes_sub$YEAR <- as.data.frame(y)
        }
        crashes_year <- crashes_sub[crashes_sub$YEAR >= input$Year_1[1] 
                                    & crashes_sub$YEAR <= input$Year_1[2],]
        
        if (input$demographics_gender) 
          {
          
          # code for printing it using pie function
          # Slices <- c(sum(crashes_year$MALES),sum(crashes_year$FEMALES),
          #             sum(crashes_year$UNKNOWN_GENDER))
          # View(Slices)
          # lbls <- c("Males", "Females", "Unknown")
          # pct <- round(Slices/sum(Slices)*100)
          # lbls <- paste(lbls, pct) # add percents to labels 
          # lbls <- paste(lbls,"%",sep="") # ad % to labels 
          # x <-pie(Slices,labels = lbls, col=rainbow(length(lbls)),
          #         main="Pie Chart of Gender")
          # print(x)
          
            df <- data.frame(
              group = c("Males", "Females", "Unknown"),
              value = c(sum(crashes_year$MALES),sum(crashes_year$FEMALES),
                        sum(crashes_year$UNKNOWN_GENDER))
            )
            df$percentage = round((df$value / sum(df$value))*100)
            genderplot <- ggplot(df, aes(x = "", y=percentage, fill=group)) +
              geom_bar(width = 1, stat = "identity") +
              ggtitle("Demographics by Gender") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label = percent(percentage/100)), 
                        position = position_stack(vjust = 0.5)) +
              coord_polar("y", start=0) 
            print(genderplot)
        }
       
#####################################################################################################
# Step 11: Plotting Demographics of Injury
####################################################################################################  
        output$InjuryPlot <- renderPlot({
          
          if (input$demographics_injury) 
            {
              df <- data.frame(
                group = c("Injured", "Not Injured"),
                value = c(sum(crashes_year$INJ_OR_FATAL),
                          sum(crashes_year$NONINJURED))
              )
              df$percentage = round((df$value / sum(df$value))*100)
              injuryplot <- ggplot(df, aes(x = "", y=percentage, fill=group)) +
                geom_bar(width = 1, stat = "identity") +
                ggtitle("Demographics by Injury") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                geom_text(aes(label = percent(percentage/100)), 
                          position = position_stack(vjust = 0.5)) +
                coord_polar("y", start=0)
              print(injuryplot)
            } 
          
        })
        
#####################################################################################################
# Step 12: Plotting Demographics of People
####################################################################################################         
        output$PeoplePlot <- renderPlot({
          
          if (input$demographics_people_type) 
            {

            
              df <- data.frame(
                group = c("Bicyclist", "Passenger","Driver","Pedestrian", "Pillion","Unknown","Motorist"),
                value = c(sum(crashes_year$BICYCLIST), sum(crashes_year$PASSENGER),
                          sum(crashes_year$DRIVER), sum(crashes_year$PEDESTRIAN),
                          sum(crashes_year$PILLION), sum(crashes_year$UNKNOWN_PERSON_TYPE),
                          sum(crashes_year$MOTORIST))
              )
              df$percentage = round((df$value / sum(df$value))*100)
              peopleplot <- ggplot(df, aes(x = "", y=percentage, fill=group)) +
                geom_bar(width = 1, stat = "identity") +
                ggtitle("Demographics by People") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                geom_text(aes(label = percent(percentage/100)), 
                          position = position_stack(vjust = 0.5)) +
                coord_polar("y", start=0)
              print(peopleplot)
            
            } 
          
        })
        
#####################################################################################################
# Step 13: Plotting Demographics of Injury Type
####################################################################################################         
        output$InjuryTypePlot <- renderPlot({
          
          if (input$demographics_injury_type) 
            {
            
              df <- data.frame(
                group = c("Fatality", "Serious","Not Serious"),
                value = c(sum(crashes_year$FATALITY), sum(crashes_year$SERIOUSINJURY),
                          sum(crashes_year$OTHERINJURY))
              )
              df$percentage = round((df$value / sum(df$value))*100)
              injurytypeplot <- ggplot(df, aes(x = "", y=percentage, fill=group)) +
                geom_bar(width = 1, stat = "identity") +
                ggtitle("Demographics by Injury Type") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                geom_text(aes(label = percent(percentage/100)), 
                          position = position_stack(vjust = 0.5)) +
                coord_polar("y", start=0)
              print(injurytypeplot)
            
          } 
          
        })

#####################################################################################################
# Step 14: Plotting Demographics of Vehicle
####################################################################################################         
        output$VehiclePlot <- renderPlot({
          
          if (input$demographics_vehicle_type) 
            {

              df <- data.frame(
                group = c("Passenger", "Motorcycle","Public","Heavy", "Bicycle","Unknown"),
                value = c(sum(crashes_year$PASSENGERVEHICLE), sum(crashes_year$MOTORCYCLE),
                          sum(crashes_year$PUBLICVEHICLE), sum(crashes_year$HEAVYVEHICLE),
                          sum(crashes_year$BICYCLE),
                          sum(crashes_year$UNKNOWN_VEHICLE_TYPE))
              )
              df$percentage = round((df$value / sum(df$value))*100)
              vehicleplot <- ggplot(df, aes(x = "", y=percentage, fill=group)) +
                geom_bar(width = 1, stat = "identity") +
                ggtitle("Demographics by Vehicles") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                geom_text(aes(label = percent(percentage/100)), 
                          position = position_stack(vjust = 0.5)) +
                coord_polar("y", start=0)
              print(vehicleplot)
          } 
          
        })
        
        
        
        
      })
#####################################################################################################
# Step 15: Plotting graph of Accident count vs Speed
####################################################################################################       
output$SpeedPlot <- renderPlot({
        
        date <- crashes_sub$ACCIDENT_DATE
        df1 <- data.frame(DateTime = date)
        crashes_sub$YEAR <- c()
        for (j in df1)
        {
          date_lub <- dmy(j)
          y <- as.numeric(year(date_lub))
          crashes_sub$YEAR <- as.data.frame(y)
        }
        crashes_year <- crashes_sub[crashes_sub$YEAR >= input$Year_2[1] 
                                    & crashes_sub$YEAR <= input$Year_2[2],]
        
        if (input$speed_type) 
          {
            accident_speed <- crashes_year[c("SPEED_ZONE", "LGA_NAME")]
            selectedCityRows <- (accident_speed$LGA_NAME %in% accident_places$LGA_NAME)
            uniqueCityRows_2 <- accident_speed[selectedCityRows,]
            accident_places_2 <- uniqueCityRows_2 %>%
              group_by(SPEED_ZONE) %>%
              summarize(total_accident = n()) %>%
              arrange(desc(total_accident))
            
            accident_places_2$SPEED_ZONE <- factor(accident_places_2$SPEED_ZONE, levels = c("30 km/hr","40 km/hr","50 km/hr","60 km/hr","70 km/hr","75 km/hr","80 km/hr","90 km/hr","100 km/hr","110 km/hr","Camping grounds or off road","Unknown Speed"))
            
            speed_count <- ggplot(accident_places_2, aes(x = SPEED_ZONE, y = total_accident, fill = SPEED_ZONE )) + 
              geom_bar(stat = "identity") + 
              ggtitle("Accident by Speed") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              labs(x="Speed", y = "Total Accidents") + 
              geom_text(aes(label = total_accident), vjust =  -.5) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
            print(speed_count)
        }
#####################################################################################################
# Step 16: Plotting graph of Accident count vs Road Type
####################################################################################################        
        output$RoadPlot <- renderPlot({
          
          if (input$road_type) 
            {
              accident_road <- crashes_year[c("RMA", "LGA_NAME")]
              selectedCityRows <- (accident_road$LGA_NAME %in% accident_places$LGA_NAME)
              uniqueCityRows_3 <- accident_road[selectedCityRows,]

              accident_places_3 <- uniqueCityRows_3 %>%
                group_by(RMA) %>%
                summarize(total_accident = n()) %>%
                arrange(desc(total_accident))
              
              road_count <- ggplot(accident_places_3, aes(x = RMA, y = total_accident, fill = RMA)) + 
                geom_bar(stat = "identity") + 
                ggtitle("Accident by Road Type") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                labs(x="Road Type", y = "Total Accidents") + 
                geom_text(aes(label = total_accident), vjust =  -.5) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
              print(road_count)
          }
          
        })
#####################################################################################################
# Step 17: Plotting graph of Accident count vs Road Geometry
####################################################################################################         
        output$GeometryPlot <- reactivePlot(function() {
          
          if (input$geometry_type) 
            {
              accident_geometry <- crashes_year[c("ROAD_GEOMETRY", "LGA_NAME")]
              selectedCityRows <- (accident_geometry$LGA_NAME %in% accident_places$LGA_NAME)
              uniqueCityRows_1 <- accident_geometry[selectedCityRows,]

              accident_places_1 <- uniqueCityRows_1 %>%
                group_by(ROAD_GEOMETRY) %>%
                summarize(total_accident = n()) %>%
                arrange(desc(total_accident))
              
              geometry_count <- ggplot(accident_places_1, aes(x = ROAD_GEOMETRY, y = total_accident, fill = ROAD_GEOMETRY )) + 
                geom_bar(stat = "identity") + 
                ggtitle("Accident by Road Geometry") +
                theme(plot.title = element_text(size = 25, face = "bold")) +
                labs(x="Road Geometry", y = "Total Accidents") + 
                geom_text(aes(label = total_accident), vjust =  -.5) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
              print(geometry_count)
          }
        })
        
      })
#####################################################################################################
# Step 10: Plotting graph of Accident count vs Hour
####################################################################################################       
output$AnalysisPlot <- renderPlot({
        
        date <- crashes_sub$ACCIDENT_DATE
        df1 <- data.frame(DateTime = date)
        crashes_sub$YEAR <- c()
        for (j in df1)
        {
          date_lub <- dmy(j)
          y <- as.numeric(year(date_lub))
          crashes_sub$YEAR <- as.data.frame(y)
        }
        crashes_year <- crashes_sub[crashes_sub$YEAR >= input$Year_3[1] 
                                    & crashes_sub$YEAR <= input$Year_3[2],]
        accident_analysis <- crashes_year[c("HIT_RUN_FLAG","POLICE_ATTEND","ALCOHOL_RELATED", "LGA_NAME")]
        selectedCityRows <- (accident_analysis$LGA_NAME %in% accident_places$LGA_NAME)
        uniqueCityRows_2 <- accident_analysis[selectedCityRows,]

      
        # Check for the input variable = alcohol
        if (input$select_condition == "alcohol") 
          {
          
            Analysis_data_1 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,ALCOHOL_RELATED) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)
            
            a <- ggplot(Analysis_data_1, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = ALCOHOL_RELATED, size = 3)) +
              ggtitle("Accident Type: Alcohol Consumption") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            print(a) 
          
        }
        
        # Check for the input variable = hit
        else if (input$select_condition == "hit") 
          {
          
            Analysis_data_2 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,HIT_RUN_FLAG) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)
            
            h <- ggplot(Analysis_data_2, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = HIT_RUN_FLAG, size = 3)) +
              ggtitle("Accident Type: Hit & Run Case") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            print(h) 
           }
        
        # Check for the input variable = police
        else if (input$select_condition == "police") 
          {
          
            Analysis_data_3 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,POLICE_ATTEND) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)
            
            p <- ggplot(Analysis_data_3, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = POLICE_ATTEND, size = 3)) +
              ggtitle("Accident Type: Police Involvement Needed") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            print(p) 
          
          }
        else if (input$select_condition == "both_ah") 
          {
          
            Analysis_data_4 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,ALCOHOL_RELATED,HIT_RUN_FLAG) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)
            
            ah <- ggplot(Analysis_data_4, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = ALCOHOL_RELATED, color = HIT_RUN_FLAG, size = 3)) +
              ggtitle("Accident Type: Alcohol Consumption and Hit & Run Case") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            # library(plotly)
            # p <- ggplotly(p)
            print(ah)
          
          }
        else if (input$select_condition == "both_ap") 
          {
          
            Analysis_data_5 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,ALCOHOL_RELATED,POLICE_ATTEND) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)

            
            ap <- ggplot(Analysis_data_5, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = ALCOHOL_RELATED, color = POLICE_ATTEND, size = 3)) +
              ggtitle("Accident Type: Alcohol Consumption and Police Involvement Needed") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            print(ap)
          
          }
        else if (input$select_condition == "both_hp") 
          {
          
            Analysis_data_6 <- uniqueCityRows_2 %>%
              group_by(LGA_NAME,HIT_RUN_FLAG,POLICE_ATTEND) %>%
              summarize(total_accident = n()) %>%
              arrange(total_accident)

            
            hp <- ggplot(Analysis_data_6, aes(LGA_NAME, total_accident)) +
              geom_point(aes(shape = HIT_RUN_FLAG, color = POLICE_ATTEND, size = 3)) +
              ggtitle("Accident Type: Hit & Run Case and Police Involvement Needed") +
              theme(plot.title = element_text(size = 25, face = "bold")) +
              geom_text(aes(label=total_accident),hjust= 5, vjust=0)
            print(hp)
          
          }
        else if (input$select_condition == "all_ahp") 
        {
          Analysis_data_7 <- uniqueCityRows_2 %>%
            group_by(LGA_NAME,ALCOHOL_RELATED,HIT_RUN_FLAG,POLICE_ATTEND) %>%
            summarize(total_accident = n()) %>%
            arrange(total_accident)

          
          ahp <- ggplot(Analysis_data_7, aes(LGA_NAME, total_accident)) +
            geom_point(aes(shape = HIT_RUN_FLAG, color = POLICE_ATTEND, size = ALCOHOL_RELATED)) +
            ggtitle("Accident Type: Alcohol Involvement and Hit & Run Case and Police Involvement Needed") +
            theme(plot.title = element_text(size = 25, face = "bold")) +
            geom_text(aes(label=total_accident),hjust= 5, vjust=0)
          print(ahp)
        }

        
      })
      
    })
    
  })

shinyApp(ui, server)