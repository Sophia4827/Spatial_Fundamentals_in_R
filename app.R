# DIDA 370 Final Project: 311 Data Analysis

library(shiny)
library(sf)
library(bslib)
library(urbnmapr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(tidyr)
library(spData) 
library(paletteer)
library(shinydashboard)
library(readr)
# Source: https://readr.tidyverse.org/

# Define UI using page_navbar + layout_sidebar (theme-ready)
ui <- page_navbar(
  title = "311 Service Requests Analysis",
  theme = bs_theme(preset = "lux"),
  
  # Introduction 
  nav_panel(
    title = "Introduction",
    navset_bar(padding = 0,
               title = "Introduction",
               p("311 service requests offer a unique and powerful lens into the daily challenges faced by New Yorkers. Started in 2003, 
                 311 allows individuals to report a wide range of local issues—from noise disturbances and housing violations to sanitation 
                 complaints and illegal parking. It is more than just a service tool - the 311 data reveals patterns of inequality, 
                 highlights underserved neighborhoods, and provides agencies with the information needed to respond effectively.",
                 p("Our project explores how 311 complaints vary across boroughs and neighborhoods. First, a stacked bar chart visualizes 
                   the top 10 complaint types by borough, which reveals that while Brooklyn receives the highest number of complaints overall, 
                   and each borough experiences its own dominant issues. A heatmap of six major complaint categories further shows these 
                   differences, with Brooklyn and the Bronx emerging as the hotspots for residential noise and illegal parking. Additional bar
                   charts and maps then break down the complaint types within each borough, showing local trends that wouldn’t be as easily
                   seen in citywide summaries.",
                   p("We then focus on illegal parking, which is one of the most frequently reported issues. We identify the top streets
                     where these complaints occur, the most common types of violations, and where they cluster geographically through bar
                     charts and maps. These visualizations offer both high-level and street-specific insights, giving city agencies the tools
                     they need for data-driven interventions.",
                     p("Our findings show that some neighborhoods face more urgent and frequent service issues than others. For example, 
                     residential noise and heat/hot water complaints are heavily concentrated in Brooklyn and the Bronx, indicating potential
                     gaps in housing quality or tenant support. Streets like Broadway and Union Street consistently appear at the top of illegal
                     parking reports, with violations ranging from blocked sidewalks to improper use of parking permits. Ultimately, 311 data serves
                     as more than a record of complaints—it is a map of lived experience. When interpreted well, it can guide better policy and more
                     equitable urban planning.")
                     )
                   )
                 )
               )
    ),
  
  # Background
  nav_panel(
    title = "Background",
    navset_bar(padding = 0,
               title = "Background",
               p("311 service request data offers a unique and underutilized lens into the daily experiences of New Yorkers. As a platform for
                 residents to report quality-of-life concerns—ranging from housing violations to noise complaints and illegal parking—311 captures
                 a real-time, ground-level perspective of what communities need and where they are being underserved. Because these reports originate
                 from the public, they often reflect hyper-local issues that traditional datasets or government records may miss, making 311 an especially
                 valuable tool for understanding the social and spatial inequalities that shape urban life.",
                 p("As highlighted by the Hunter Urban Review, 311 data can reveal which neighborhoods are receiving attention and which are being ignored. 
                 When mapped and analyzed properly, complaint patterns can indicate systemic disinvestment, over-policing, or inequitable service delivery.
                 For example, higher volumes of complaints about heat or hot water in certain neighborhoods may signal gaps in housing maintenance or tenant
                 protections—not just isolated incidents (He, Z. K. H., 2023). At the same time, uneven complaint rates across neighborhoods may reflect
                 disparities in civic engagement or access to city resources, raising important questions about who feels empowered to use the system in the
                 first place.",
                   p("Further, as demonstrated in a data science case study by Avonlea Fisher, NYC 311 data is not only useful for mapping complaints but also
                   for predictive modeling and trend detection. The study showed that complaint volumes vary significantly across time and boroughs, and that
                   certain types of complaints—such as noise and illegal parking—are persistently high in specific areas. These patterns can help city agencies
                   allocate resources more effectively by targeting interventions where they're most needed (Fisher, 2021).",
                     p("Building on this understanding, our project uses 311 service request data to explore how different types of complaints vary across
                     boroughs and neighborhoods. By analyzing and mapping the distribution of complaints such as illegal parking, residential noise, and
                     heating issues, we aim to identify both widespread and localized trends in community concerns. Our goal is to show how 311 data—when
                     interpreted critically—can offer a powerful, people-centered approach to assessing neighborhood needs and informing equitable policy
                     decisions.")))
                 )
    ),
    navset_bar(padding = 0,
               title = "Research Questions",
               p("1. What neighborhoods/boroughs get the most noise complaints, and how does this correlate with time of day?",
                 p("2. Which streets have the most illegal parking? What kinds of illegal parking complaints are most common on each street?",
                   p("3. How do complaint types vary across boroughs or community boards? And are there patterns in complaints that can help 
                     agencies allocate resources more efficiently?",
                     p("4. Are some neighborhoods receiving quicker responses than others, even for similar complaint types?")))
               )
    )
  ),
  
  # Noise Complaints Dashboard
  nav_panel(
    title = "Research Question 1",
    dashboardPage(
      dashboardHeader(title = "Noise Complaints"),
      dashboardSidebar(disable = T),
      dashboardBody(
        fluidRow(
          column(width = 6,
                 box(width = NULL, solidHeader = T, title = "Map – Geographic Distribution of Noise Complaints",status = "primary", 
                     height = 400, leafletOutput(outputId = "map_plot", height = 300)),
                 box(width = NULL,
                     selectInput(
                       "map_time_filter",
                       label = "Choose a variable to display on the map:",
                       choices = c("All", "Morning", "Afternoon", "Evening"),
                       selected = "All"
                     )),
                 box(width = NULL, solidHeader = F, 
                     height = 200, status = "primary", background = "light-blue",
                     strong("Map – Geographic Distribution of Noise Complaints"),
                     p("The map plots the location of noise complaints throughout NYC, again split
                     by time of day. Each dot represents a complaint, which is color coded the same
                     way as the bar chart. Using the dropdown menu, users can filter to view only
                       morning, afternoon, or evening complaints."))),
            column(width = 6,
                   box(width = NULL, solidHeader = T, title = "Bar Chart – Noise Complaints by Borough and Time of Day",status = "primary", 
                       height = 400, plotOutput(outputId = "bar_plot", height = 300)),
                   box(width = NULL,
                       selectInput(
                         "bar_time_filter",
                         label = "Choose a variable to display on the map:",
                         choices = c("All", "Morning", "Afternoon", "Evening"),
                         selected = "All"
                       )),
                   box(width = NULL, solidHeader = F, 
                       height = 200, status = "primary", background = "light-blue",
                       strong("Bar Chart – Noise Complaints by Borough and Time of Day"),
                       p("This chart shows the number of noise complaints across NYC boroughs, broken
                  down by morning (blue), afternoon (green), and evening (purple). The dropdown
                  menu lets users filter by time of day. Evening complaints are the most common,
                  especially in the Bronx and Brooklyn, while morning and afternoon complaints 
                  are lower across boroughs. This helps show not just where complaints are happening,
                  but when they are happening."))),
          )
        )
      )
    ),
  
  # Illegal Parking Dashboard
  nav_panel(
    title = "Research Question 2",
    dashboardPage(
      dashboardHeader(title = "Illegal Parking"),
      dashboardSidebar(disable = T),
      dashboardBody(
        fluidRow(
        column(width = 6,
               box(plotOutput(outputId = "bar_plot2", height = 300), width = NULL, solidHeader = T, 
                   height = 400, title = "Bar Plot - NYC Streets with most Illegal Parking Complaints",status = "primary"),
               box(plotOutput(outputId = "bar_plot3", height = 300), width = NULL, solidHeader = T, 
                   height = 400, title = "Bar Chart - Parking Complaint Types Received",status = "primary"),
               box(width = NULL, solidHeader = T, title = "Map - Illegal Parking on NYC Streets by Complaint Type",status = "primary",
                   height = 400, plotOutput(outputId = "map_plot3", height = 300))),
          column(width = 6,
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Bar Plot - NYC Streets with Most Illegal Parking Complaints"),
                     p("In order to visualize illegal parking issues facing NYC, the top 10 streets that received the most illegal parking
              complaints in the past year were plotted on a bar chart. These streets were Broadway, Union Street, Schroeders Avenue,
              Adams Street, Grand Concourse, 7 Avenue, West 115 Street, Classon Avenue, Dean Street, and Gold Street.")),
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Bar Chart - Parking Complaint Types Received"),
                     p("The bar plot further improves this analysis through filtering by complaint type to see which complaint types were
                the most prevalent. The top 10 illegal parking complaint types on these streets were blocked sidewalks, posted parking
                sign violation, blocked bike lane, blocked hydrant, blocked crosswalk, double parked blocking traffic, double parked
                blocking vehicle, parking permit improper use, license plate obscured, and commercial overnight parking.")),
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Map - Illegal Parking on NYC Streets by Complaint Type"),
                     p("The map plots the top 3 complaint types on the top 10 streets on top of a map of the 5 boroughs. The color coding
                of the boroughs allows us to see that Manhattan has the most parking sign violations, while Brooklyn has more blocked
                bike lanes. Visualizing how complaint types vary across boroughs can also reveal patterns to better allocate resources
                by the city government. This can include specific solutions to problems, including stricter enforcement of posted parking
                signs in Brooklyn or more underground parking garages in Manhattan to reduce blocked bike lanes.")))
        )
      )
    )
    ),
  
  # Complaint Types Across Boroughs
  nav_panel(
    title = "Research Question 3",
    dashboardPage(
      dashboardHeader(title = "Complaint Types Across Boroughs"),
      dashboardSidebar(disable = T),
      dashboardBody(
        fluidRow(
          column(width = 6,
                 box(plotOutput(outputId = "stack_plot", height = 300), width = NULL, solidHeader = T, 
                     height = 400, title = "Stacked Bar Chart - Top 10 Complaint Types by Borough",status = "primary"),
                 box(width = NULL, solidHeader = T, title = "Heatmap - Complaint by Borough",status = "primary", 
                     height = 400, plotOutput(outputId = "heat_map", height = 300)),
                 box(width = NULL, solidHeader = T, title = "Bar Graphs - Complaint Types by Borough",status = "primary", 
                     height = 400, plotOutput(outputId = "facet_plot", height = 300))
          ),
            column(width = 6,
                   box(width = NULL, solidHeader = F, 
                       height = 400, status = "primary", background = "light-blue",
                       strong("Stacked Bar Chart - Top 10 Complaint Types by Borough"),
                       p("This stacked bar chart explains how complaint types vary across boroughs and reveals patterns that can
                inform more efficient resource allocation by city agencies. The chart displays the top 10 complaint types
                reported through NYC's 311 system, grouped by borough. The graph clearly shows that Brooklyn has the highest
                total number of complaints, followed by Queens and the Bronx. Staten Island, by contrast, has significantly
                fewer complaints overall. The chart also highlights how specific complaint types are distinguishable across
                boroughs. For example, Illegal Parking is a leading issue in Brooklyn and Queens, suggesting that traffic enforcement
                resources could be focused more heavily in these areas. Residential Noise and Heat/Hot Water are also common in
                several boroughs, indicating a potential need for increased housing inspections or tenant support services. By
                identifying which types of complaints are most frequent in each borough, this chart provides valuable insights that
                agencies can use to strategically allocate staff, schedule inspections, or launch targeted public awareness campaigns.")),
                   box(width = NULL, solidHeader = F, 
                       height = 400, status = "primary", background = "light-blue",
                       strong("Heatmap - Complaint by Borough"),
                       p("The heatmap shows the distribution of six major complaint types: Unsanitary Condition, 
                  Noise (Street/Sidewalk and Residential), Illegal Parking, Heat/Hot Water, and Blocked Driveway, across four New York
                  City boroughs: Bronx, Brooklyn, Manhattan, and Queens. The intensity of the red shading represents the volume of complaints,
                  with darker red indicating a higher number. This graph highlights that Noise - Residential is the most frequently reported
                  complaint in all boroughs, with particularly high levels in Brooklyn and the Bronx. Brooklyn also stands out for its high
                  volume of Illegal Parking complaints. The Bronx has more 'Heat/Hot Water' issues, and Queens has significant levels of both
                  Illegal Parking and Noise - Residential complaints. The heatmap indicates a high-level overview of complaint concentrations
                  and makes it easy to identify which boroughs struggle most with specific issues.")),
                   box(width = NULL, solidHeader = F, 
                       height = 400, status = "primary", background = "light-blue",
                       strong("Bar Graphs - Complaint Types by Borough"),
                       p("The horizontal bar charts for each borough: Bronx, Brooklyn, Manhattan, and Queens, display the number of complaints per
                    type. Each subplot is color-coded by borough and allows for easier comparison of complaint types within each location. 
                    In Brooklyn, Noise - Residential and Illegal Parking, complaints dominate, far exceeding the other categories. The Bronx
                    similarly shows a high number of Noise - Residential complaints, followed by 'Heat/Hot Water' and 'Illegal Parking.'
                    Manhattan has a more balanced distribution, with no single complaint type overwhelming the others. Queens displays a high
                    frequency of Noise - Residential and Blocked Driveway complaints. This breakdown provides a more detailed view of how issues
                    differ across boroughs and can be useful for agencies to adjust interventions based on localized concerns."))
                   
            )
        )
      )
    )
  ),
  
  # Response Times
  nav_panel(
    title = "Research Question 4",
    dashboardPage(
      dashboardHeader(title = "Response Times"),
      dashboardSidebar(disable = T),
      dashboardBody(
        fluidRow(
          column(width = 6,
                 box(plotOutput(outputId = "noise_plot", height = 300), width = NULL, solidHeader = T, 
                     height = 400, title = "Bar Chart – Average Response Time for Noise Complaints by Borough",status = "primary"),
                 box(width = NULL, solidHeader = T, title = "Bar Chart – Average Response Time for Illegal Parking by Borough",status = "primary", 
                     height = 400, plotOutput(outputId = "parking_plot", height = 300)),
                 box(width = NULL, solidHeader = T, title = "Bar Chart – Average Response Time for Heat/Hot Water by Borough",status = "primary", 
                     height = 400, plotOutput(outputId = "water_plot", height = 300))
          ),
          column(width = 6,
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Bar Chart – Average Response Time for Noise Complaints by Borough"),
                     p("The bar chart shows the average response time for noise complaints across the five boroughs. Manhattan has the longest
                       response time, with complaints taking over 60 hours to resolve on average. Queens and Staten Island follow, with slightly
                       shorter times. In contrast, the Bronx has the shortest average response time, suggesting faster handling of noise-related
                       issues in that borough. This visualization highlights how response speed for similar complaint types can vary widely 
                       depending on location.")),
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Bar Chart – Average Response Time for Illegal Parking by Borough"),
                     p("This chart displays the average response time for illegal parking complaints. The Bronx and Queens experience the longest 
                       delays, with times approaching 4 hours, while Manhattan has one of the shortest response times for this issue. The borough-level
                       differences in enforcement response may reflect how traffic violations are prioritized or where resources are more heavily 
                       concentrated. These trends can help identify areas that may benefit from more active parking enforcement.")),
                 box(width = NULL, solidHeader = F, 
                     height = 400, status = "primary", background = "light-blue",
                     strong("Bar Chart – Average Response Time for Heat/Hot Water by Borough"),
                     p("The third chart shows response times for heat and hot water complaints, which are essential services for residents. 
                       The data indicates more consistent response times across boroughs, with averages ranging between 40 and 48 hours. Manhattan
                       has the quickest responses, while the Bronx has the slowest. This suggests that while service delivery for heat-related 
                       issues may be relatively standardized, certain boroughs like the Bronx still face slight delays that could signal a need
                       for improved housing response protocols."))
                 
          )
                     
              )
            )
          )
        ),
  
  # Conclusion
  nav_panel(
    title = "Conclusion",
    navset_bar(padding = 0,
               title = "Conclusion",
               p("Overall, the culmination of graphs, visualizations, and maps created from the 311 Service Request Dataset has allowed for an in-depth
                 understanding of the problems facing residents of the 5 boroughs of NYC and provided us with thorough answers to our research questions.
                 Our first research question, which asks “What neighborhoods/boroughs get the most noise complaints, and how does this correlate with time
                 of day?” was answered through both a bar plot and a map, filtered by time of day. We found that the Bronx received the most service requests,
                 and the majority of complaints were received in the evening. This pattern of the evening being the most popular time to receive complaints,
                 followed by afternoon, and then morning, was seen throughout all five boroughs. Our second question, “Which streets have the most illegal 
                 parking? What are the most common illegal parking complaint types received?” was also answered through both bar plots and maps. We found that 
                 Broadway was the street that received the most parking complaints and that Manhattan and Brooklyn were the boroughs with the most congestion. 
                 We also found that the most popular parking complaint type was “Blocked Sidewalks,” and this was the most popular parking complaint type for 
                 Manhattan. Our third research question, “How do complaint types vary across boroughs or community boards? And are there patterns in complaints
                 that can help agencies allocate resources more efficiently?” was answered with a stacked bar chart, a heatmap, and bar graphs. We found that
                 complaint types do vary across boroughs, as residential noise complaints are more common in the Bronx and Brooklyn, while illegal parking issues
                 are more common in Brooklyn and Queens. Furthermore, agencies can allocate resources more efficiently by focusing traffic enforcement resources
                 more heavily in Brooklyn and Queens, and increasing housing inspections or tenant support services in the Bronx and Brooklyn. Finally, our fourth
                 question, “Are some neighborhoods receiving quicker responses than others, even for similar complaint types?” was answered with bar charts. We found
                 that Manhattan received the longest response times for noise and heat/hot water complaints, while the Bronx received the longest response times for
                 illegal parking. Overall, these conclusions can help city agencies better allocate resources and focus on which areas are experiencing the most
                 problems to better serve the boroughs.")
    )
  ),
  
  # References
  nav_panel(
    title = "References",
    navset_bar(padding = 0,
               title = "References",
               p("Fisher, A. (2021, January 24). Analyzing and modelling NYC 311 Service requests. Medium.
                 https://medium.com/data-science/analyzing-and-modelling-nyc-311-service-requests-eb6a9c9adc7c",
                 p("He, Z. K. H. (2023). The value of 311 data. Hunter Urban Review. 
                   https://hunterurbanreview.commons.gc.cuny.edu/the-value-of-311-data/"))
    )
  )
)


# Server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 160*1024^2)
  
  # Preprocessing for Research Question 1: What neighborhoods/boroughs get the most noise complaints, and how does this correlate with time of day?
  
  # Load CSV file with the specified path
  data_noise <- read.csv("311 datasets/311 Requests (2024-2025, Noise Complaints).csv")
  
  # Extract Hour from Created.Date using strptime
  # Format is "MM/DD/YYYY H:MM", e.g., "1/1/2024 0:00"
  # data$Created.Date for the character string
  # format for timestamp format
  # $hour to extract hour
  # Source: https://www.geeksforgeeks.org/how-to-convert-character-to-a-timestamp-in-r/
  data_noise$Hour <- strptime(data_noise$Created.Date, format = "%m/%d/%Y %H:%M")$hour
  
  # Remove rows with NA values in Longitude, Latitude, and Hour using drop_na
  data_clean_noise <- drop_na(data_noise, Longitude, Latitude, Hour)
  
  # Create sf object using Longitude and Latitude columns
  data_sf <- st_as_sf(data_clean_noise, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Create a lookup table for time of day
  # Morning: 5 AM to 11:59 AM (hours 5 to 11)
  # Afternoon: 12 PM to 6:59 PM (hours 12 to 18)
  # Evening: 7 PM to 4:59 AM (hours 19 to 23, and 0 to 4)
  time_lookup <- c(
    "0" = "Evening", "1" = "Evening", "2" = "Evening", "3" = "Evening", "4" = "Evening",
    "5" = "Morning", "6" = "Morning", "7" = "Morning", "8" = "Morning", "9" = "Morning",
    "10" = "Morning", "11" = "Morning",
    "12" = "Afternoon", "13" = "Afternoon", "14" = "Afternoon", "15" = "Afternoon",
    "16" = "Afternoon", "17" = "Afternoon", "18" = "Afternoon",
    "19" = "Evening", "20" = "Evening", "21" = "Evening", "22" = "Evening", "23" = "Evening"
  )
  
  # Map hours to time of day using the lookup table
  data_sf$Time_of_Day <- time_lookup[as.character(data_sf$Hour)]
  
  # Sample the sf data for the map to reduce the number of points
  # Sample 500 rows (as previously confirmed to work)
  data_sf_sampled <- data_sf[sample(length(data_sf$Hour), 500), ]
  
  # Precompute datasets for the bar plot
  data_valid <- st_drop_geometry(data_sf)
  data_valid$Time_of_Day <- factor(data_valid$Time_of_Day, levels = c("Morning", "Afternoon", "Evening"))
  # Use aggregate() to count number of rows by Borough and Time_of_Day
  # Source: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/aggregate
  bar_data_all <- aggregate(Unique.Key ~ Borough + Time_of_Day, data = data_valid, length)
  # Name the result column "Count
  colnames(bar_data_all)[3] <- "Count"
  
  # Create a named list for bar plot data
  bar_data_list <- list(
    "All" = bar_data_all,
    "Morning" = bar_data_all[bar_data_all$Time_of_Day == "Morning", ],
    "Afternoon" = bar_data_all[bar_data_all$Time_of_Day == "Afternoon", ],
    "Evening" = bar_data_all[bar_data_all$Time_of_Day == "Evening", ]
  )
  
  # Precompute sf datasets for the map using the sampled data
  data_sf_list <- list(
    "All" = data_sf_sampled,
    "Morning" = data_sf_sampled[data_sf_sampled$Time_of_Day == "Morning", ],
    "Afternoon" = data_sf_sampled[data_sf_sampled$Time_of_Day == "Afternoon", ],
    "Evening" = data_sf_sampled[data_sf_sampled$Time_of_Day == "Evening", ]
  )
  
  # Bar plot rendering
  output$bar_plot <- renderPlot({
    # Get the pre-aggregated data
    plot_data <- bar_data_list[[input$bar_time_filter]]
    
    # Create bar plot with custom colors
    ggplot(plot_data, aes(x = Borough, y = Count, fill = Time_of_Day)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = c("Morning" = "blue", "Afternoon" = "green", "Evening" = "purple")
      ) +
      labs(title = "Noise Complaints by Borough and Time of Day",
           x = "Borough", y = "Number of Complaints") +
      theme_minimal()
  })
  
  # Map rendering
  output$map_plot <- renderLeaflet({
    # Get the filtered sf data
    map_data <- data_sf_list[[input$map_time_filter]]
    
    # Define a color palette for Time_of_Day
    pal <- colorFactor(
      palette = c("blue", "green", "purple"),
      domain = c("Morning", "Afternoon", "Evening")
    )
    
    # Render the map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        data = map_data,
        radius = 5,
        color = ~pal(Time_of_Day),
        fillOpacity = 0.7,
        popup = ~paste("Borough:", Borough, "<br>Time of Day:", Time_of_Day)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = map_data$Time_of_Day,
        title = "Time of Day",
        opacity = 0.7
      ) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)  # Center on NYC
      # Source: https://aagarw30.gitbooks.io/r-leaflet/content/setView.html
  })
  
  ######################################################################################
  
  # Preprocessing for Research Question 2: Which streets have the most illegal parking? What kinds of illegal parking complaints are most common on each street?
  
  data_illegal <- read.csv("311 datasets/311 Requests (2024-2025, Illegal Parking Complaints).csv")
  nyc <- st_read("311 datasets/nyc_neighborhoods/NYC_Nhood ACS2008_12.shp")
  
  # Filter data to only include necessary columns, add geometry column, add borough column
  data_clean_illegal <- drop_na(data_illegal, Longitude, Latitude)
  data_sf <- st_as_sf(data_clean_illegal, coords = c("Longitude", "Latitude"), crs = 4326)
  
  data_illegal <- data_illegal %>% select(Complaint.Type, Descriptor, Incident.Address, Street.Name, Borough, Latitude, Longitude, Location)
  
  boro_names <- c("1" = "Manhattan",
                  "2" = "Bronx",
                  "3" = "Brooklyn",
                  "4" = "Queens",
                  "5" = "Staten Island")
  
  nyc <- nyc %>% 
    mutate(borough = boro_names[as.character(borocode)])
  
  # Which streets have the most complaints?
  street_counts10 <- data_illegal %>% 
    group_by(Street.Name) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)) %>% 
    head(10)
  
  # Bar Plot Top Ten Streets most Parking Complaints 
  output$bar_plot2 <- renderPlot({
    ggplot(street_counts10, aes(y = count, x = reorder(Street.Name, -count), fill= Street.Name))+
      geom_bar(stat="identity")+
      labs(x = "Street Names", y = "Count", title = "NYC Streets w Most Illegal Parking Complaints") +
      scale_fill_paletteer_d("MetBrewer::Cassatt2") +
      theme(legend.position="none", 
            plot.title = element_text(hjust = 0.5, size=15), 
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Filter data to only include the 10 ten streets with the most complaints
  streets10 <- data_illegal %>% 
    filter(Street.Name %in% c("BROADWAY", "UNION STREET", "SCHROEDERS AVENUE", "ADAMS STREET", "GRAND CONCOURSE",
                            "7 AVENUE", "WEST 115 STREET", "CLASSON AVENUE", "DEAN STREET", "GOLD STREET"))
  
  # Map Top 10 streets with most parking complaints
  data_clean_illegal <- drop_na(streets10, Longitude, Latitude)
  data_sf <- st_as_sf(data_clean_illegal, coords = c("Longitude", "Latitude"), crs = 4326)
  
  output$map_plot2 <- renderPlot({
    ggplot() +
      geom_sf(data = nyc, aes(fill = boroname))+
      geom_sf(data = data_sf, color = "black", size = 0.5)+
      scale_fill_manual(
        name = "NYC Borough",
        values = c(
          "Bronx" = "#a2eafa",       
          "Brooklyn" = "#5dd7fc",    
          "Manhattan" = "#6b88e8",   
          "Queens" = "#b28bf7",      
          "Staten Island" = "#cc72f2" 
        )
      )+
      theme_minimal()+
      labs(title = "Illegal Parking on NYC Street")+
      theme(legend.title.align = 0.5,plot.title = element_text(hjust = 0.5, size=15))
  })
  
  # What kinds of illegal parking complaints are most common on each street?
  complaint_types <- streets10 %>% 
    group_by(Descriptor) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)) %>% 
    head(10)
  
  # Bar plot of Types of Parking Complaints Received
  output$bar_plot3 <- renderPlot({
    # Create bar plot with custom colors
    ggplot(complaint_types, aes(y = count, x = reorder(Descriptor, -count), fill= Descriptor))+
      geom_bar(stat="identity")+
      labs(x = "Complaint Types", y = "Count", title = "Types of Parking Complaints Received") +
      scale_fill_paletteer_d("MexBrewer::Ronda") +
      theme(legend.position="none", 
            plot.title = element_text(hjust = 0.5, size=15), 
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Where does each type of illegal parking complaint occur most?
  top3_complaint_types <- streets10 %>% 
    filter(Descriptor %in% c("Blocked Sidewalk", "Posted Parking Sign Violation", "Blocked Bike Lane"))  %>%
    drop_na(Longitude, Latitude) %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  output$map_plot3 <- renderPlot({
    # Create bar plot with custom colors
    ggplot() +
      geom_sf(data = nyc, aes(fill = boroname))+
      geom_sf(data = top3_complaint_types, aes(color = Descriptor), size = 2)+
      scale_fill_manual(
        name = "NYC Borough",
        values = c(
          "Bronx" = "#a2eafc",       
          "Brooklyn" = "#5dd7fd",    
          "Manhattan" = "#6b88e9",   
          "Queens" = "#b28bf8",      
          "Staten Island" = "#cc72f3" 
        )
      )+
      scale_color_manual(
        name = "Complaint Type",
        values = c(
          "Blocked Sidewalk" = "black",
          "Posted Parking Sign Violation" = "red",
          "Blocked Bike Lane" = "darkgray"
        )
      ) +
      theme_minimal()+
      labs(title = "Illegal Parking on NYC Streets by Complaint Type")+
      theme(legend.title.align = 0.5,plot.title = element_text(hjust = 0.5, size=15))
  })
  
  ######################################################################################
  
  # Preprocessing for Research Question 3: How do complaint types vary across boroughs or community boards? 
    # And are there patterns in complaints that can help agencies allocate resources more efficiently?
  
  jan_april <- read.csv("311 datasets/311 Requests (Jan 01 2024 - Apr 01 2024).csv")
  april_jul <- read.csv("311 datasets/311 Requests (Apr 01 2024 - Jul 01 2024).csv")
  jul_oct <- read.csv("311 datasets/311 Requests (Jul 01 2024 - Oct 01 2024).csv")
  oct_dec <- read.csv("311 datasets/311 Requests (Oct 01 2024 - Dec 01 2024).csv")
  
  # Combining all the data
  # Source: https://forum.posit.co/t/merge-multiple-csv-into-one-df-in-r/146554
  all_data <- rbind(jan_april, april_jul, jul_oct, oct_dec)
  
  # Remove NAs
  clean_data <- na.omit(all_data)
  
  # Stacked Bar Chart 
  
  # Total Count of top 10 complaint type
  top_10_complaints <- clean_data %>%
    count(Complaint.Type) %>%
    top_n(10, n) %>%
    pull(Complaint.Type)
  
  # Filter data to only include top 10 complaints
  stacked_data <- clean_data %>%
    filter(Complaint.Type %in% top_10_complaints) %>%
    group_by(Borough, Complaint.Type) %>%
    summarise(Count = n(), .groups = 'drop')
  
  output$stack_plot <- renderPlot({
    ggplot(stacked_data, aes(x = Borough, y = Count, fill = reorder(Complaint.Type, -Count))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Top 10 Complaint Types by Borough",
           x = "Borough",
           y = "Number of Complaints",
           fill = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(hjust = 0.5, size = 7.5), 
            legend.position = "right") 
  })
  
  # Heat Map
  heatmap_data <- clean_data %>%
    group_by(Borough, Complaint.Type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    top_n(20, count)
  
  output$heat_map <- renderPlot({
    ggplot(heatmap_data, aes(x = Borough, y = Complaint.Type, fill = count)) +
      geom_tile() +
      labs(title = "Complaint Heatmap by Borough",
           x = "Borough",
           y = "Complaint Type") +
      scale_fill_gradient(low = "pink", high = "red") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  # Facet by Borough
  complaint_summary <- clean_data %>%
    group_by(Borough, Complaint.Type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    top_n(20, count)
  
  output$facet_plot <- renderPlot({
    ggplot(complaint_summary, aes(x = Complaint.Type, y = count, fill = Borough)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Borough) +
      coord_flip() +
      labs(
        title = "Complaint Types by Borough",
        x = "Complaint Type",
        y = "Number of Complaints"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 7))
  })
  
  ######################################################################################
  
  # Preprocessing for Research Question 4: Are some neighborhoods receiving quicker responses than others, even for similar complaint types?
  
  # Make sure dates are in Date/Time format
  # Source: https://stackoverflow.com/questions/38594177/changing-date-time-format-in-r
  clean_data$Created.Date <- as.POSIXct(clean_data$Created.Date, format="%m/%d/%Y %I:%M:%S %p")
  clean_data$Closed.Date <- as.POSIXct(clean_data$Closed.Date, format="%m/%d/%Y %I:%M:%S %p")
  
  # Add new column with calculated response time
  # Source: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/difftime
  clean_data$response_time_hours <- as.numeric(difftime(clean_data$Closed.Date, clean_data$Created.Date, units = "hours"))
  
  # Convert to spatial points
  data_sf <- st_as_sf(clean_data, coords = c("Longitude", "Latitude"), crs = st_crs(nyc))
  
  # Spatial join
  data_neighborhoods <- st_join(data_sf, nyc, join = st_within)
  
  # Top Complaint: Noise Complaint
  noise_only <- data_neighborhoods %>%
    filter(grepl("Noise", `Complaint.Type`, ignore.case = TRUE))
  
  neighborhood_response_noise <- noise_only %>%
    group_by(Borough) %>%  # replace with your actual neighborhood column
    summarize(avg_response = mean(response_time_hours, na.rm = TRUE),
              n_requests = n()) %>%
    arrange(desc(avg_response))
  
  output$noise_plot <- renderPlot({
    ggplot(neighborhood_response_noise, aes(x = reorder(Borough, avg_response), y = avg_response, fill = Borough)) +
      geom_col(width = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = "Average Response Time for Noise Complaints by Borough",
        x = "Borough",
        y = "Avg Response Time (hrs)",
        fill = "Borough"
      ) +
      theme_minimal()
  })
  
  # Second Complaint: Illegal Parking
  illegal_parking_only <- data_neighborhoods %>%
    filter(grepl("Illegal Parking", `Complaint.Type`, ignore.case = TRUE))
  
  neighborhood_response_parking <- illegal_parking_only %>%
    filter(Borough != "Unspecified") %>% 
    group_by(Borough) %>%  # replace with your actual neighborhood column
    summarize(avg_response = mean(response_time_hours, na.rm = TRUE),
              n_requests = n()) %>%
    arrange(desc(avg_response))
  
  output$parking_plot <- renderPlot({
    ggplot(neighborhood_response_parking, aes(x = reorder(Borough, avg_response), y = avg_response, fill = Borough)) +
      geom_col(width = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = "Average Response Time for Illegal Parking by Borough",
        x = "Borough",
        y = "Avg Response Time (hrs)",
        fill = "Borough"
      ) +
      theme_minimal()
  })
  
  # Third Complaint: Heat/Hot Water
  water_only <- data_neighborhoods %>%
    filter(grepl("HEAT/HOT WATER", `Complaint.Type`, ignore.case = TRUE))
  
  neighborhood_response_water <- water_only %>%
    group_by(Borough) %>%  # replace with your actual neighborhood column
    summarize(avg_response = mean(response_time_hours, na.rm = TRUE),
              n_requests = n()) %>%
    arrange(desc(avg_response))
  
  output$water_plot <- renderPlot({
    ggplot(neighborhood_response_water, aes(x = reorder(Borough, avg_response), y = avg_response, fill = Borough)) +
      geom_col(width = 0.8) +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = "Average Response Time for Heat/Hot Water by Borough",
        x = "Borough",
        y = "Avg Response Time (hrs)",
        fill = "Borough"
      ) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)