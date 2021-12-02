#################################################
# author: "Zheyan Liu"
# date: "11/18/2021"
#################################################

# change working directory

# R packages
library(shiny)
library(shinythemes)
library(shinyTime)
library(tidyverse)
library(reticulate)
library(leaflet)

PYTHON_DEPENDENCIES = c('pip', 'numpy','pandas','googlemaps','datetime')
# use local python
# use_python('/Users/jimmy/anaconda3/python.exe')

# ------------------ App virtualenv setup (Do not edit) ------------------- #

# virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
# python_path = Sys.getenv('PYTHON_PATH')
# 
# # Create virtual env and install dependencies
# reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
# reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
# reticulate::use_virtualenv(virtualenv_dir, required = T)


# ------------------ App server logic (Edit anything below) --------------- #

# Import python functions to R
reticulate::source_python('GetRoute.py')


subwayIcons <- icons(
  iconUrl = "https://maps.gstatic.com/mapfiles/transit/iw2/6/subway2.png",
  iconWidth = 12, iconHeight = 12,
  iconAnchorX = 12, iconAnchorY = 12,
)

# mygoogle_routes$start_location = '168 st, NY'
# mygoogle_routes$destination = '24 st, NY'
# # it is a R dataframe 
# df = 
#   mygoogle_routes$get_directions() %>% 
#   mutate(num_stops = as.integer(num_stops),
#          route_num = as.integer(route_num))
# 
# 
# mygoogle_routes$directions_df = reticulate::r_to_py(df)
# mygoogle_routes$get_stops()


####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("No crime Navigation:",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      
                                      HTML("<h5>Who are you?</h5>"),
                                      selectInput("gender", "Your Gender",
                                                  list("Female", "Male")),
                                      selectInput("age", "Your Age",
                                                  list("<18", "18-30",'30-50','>50')),
                                      selectInput("race", "Your Race",
                                                  list(`Hispanic` = list('Hispanic'),
                                                       `Non-Hispanic` = list("White", "Black",'Asian'))),
                                      
                                      
                                      HTML("<h5>When you leave?</h5>"),
                                      dateInput("start_date", "Date:", value = Sys.Date(), min =  Sys.Date(), max = Sys.Date() + 14),
                                      
                                      # Default is current time + 3 min
                                      timeInput("time_input", "Time", value = strptime(unlist(strsplit(as.character(Sys.time() + 180), split = ' '))[2], "%T")),
                                      
                                      HTML("<h5>Where to go?</h5>"),
                                      textInput("start_location", "Your Location:", "168 st"),
                                      textInput("destination", "Place of Interest:", "Prospect Park"),
                                      
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Routes')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      DT::dataTableOutput("tabledata"), # Results DT table
                                      # Leaflet map
                                      verbatimTextOutput('routes_selection_box'),
                                      DT::dataTableOutput("tabledata2"), # Results DT table
                                      DT::dataTableOutput("tabledata3"), # Results DT table
                                      leafletOutput("mymap")
                                    ) # mainPanel()
                                    
                           )
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {

  departure_time = reactive({  
    
    input$submitbutton
    
    paste(input$start_date, unlist(strsplit(as.character(input$time_input), split = ' '))[2], sep = ' ')
  })
  
  
  # Input Data
  directions_raw = reactive({  
    
    mygoogle_routes = google_routes()
    
    # take dependence on button
    input$submitbutton
    
    mygoogle_routes$start_location = isolate(paste(input$start_location, 'New York', sep = ','))
    mygoogle_routes$destination = isolate(paste(input$destination, 'New York', sep = ','))
    # it is a R dataframe 
    
    df = 
      mygoogle_routes$get_directions(departure_time()) %>% 
      mutate(num_stops = as.integer(num_stops),
             route_num = as.integer(route_num))
             
    print(df)
    
  })
  
  directions_grouped = reactive({  
    # take dependence on button
    input$submitbutton
    df2 = 
      directions_raw() %>% 
      mutate(
        # change time into minuates
        time = round(time/60),
        # change distance into miles
        distance = round(distance/1609, 1),
        walking_distance = round(walking_distance/1609, 2)) %>% 
        group_by(route_num) %>% 
        # mutate(line = paste0(line, num_stops, collapse = '(')) %>%
        summarise(time = mean(time),
                  distance = mean(distance),
                  walking_distance = mean(walking_distance),
                  line = paste0(line, '[', as.character(num_stops), ']', collapse = " - "),
                  n = n()) %>% 
        mutate(
          crime_score = round(runif(n),2),
          crowdness_score = round(runif(n),2)
        ) %>% 
        # distinct(line, .keep_all = TRUE) %>% 
        select(-n, -distance) %>% 
        relocate(route_num, time, walking_distance, crime_score, crowdness_score, line) %>% 
        rename('line[stops]' = line,
               'time(min)' = time,
               # 'distance(mile)' = distance,
               'walking_distance(mile)' = walking_distance)  
      
    print(df2)
      
  })
  
  
  
  df_map = reactive({
    print(class(directions_raw()))
    mygoogle_routes = google_routes()
    mygoogle_routes$directions_df = reticulate::r_to_py(directions_raw())
    
    mygoogle_routes$get_stops() %>% 
      filter(route_num  %in% input$tabledata_rows_selected) %>%
      mutate(line = ifelse(line == '7X', '7', line)) %>% 
      mutate(group = paste(as.character(route_num), line)) %>% 
      # add subway service
      mutate(service = 
               case_when(  line %in% c('A', 'C', 'E')  ~ "8 Avenue(ACE)",
                           line %in% c('S') ~ "Shuttle(S)",
                           line %in% c('B', 'D', 'F', 'M') ~ "6 Avenue(BDFM)",
                           line %in% c('G') ~ "Brooklyn-Queens Crosstown(G)",
                           line %in% c('L') ~ "14 St-Canarsie(L)",
                           line %in% c('N', 'Q', 'R', 'W') ~ "Broadway(NQRW)",
                           line %in% c('1', '2', '3') ~ "7 Avenue(123)",
                           line %in% c('4', '5', '6') ~ "Lexington Av(456)",
                           line %in% c('7') ~ "Flushing(7)",
                           TRUE ~ 'other_line')) %>% 
      relocate(route_num)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Routes Found") 
      isolate(paste('Departures at',departure_time()))
      # isolate(input$time_input)
    } else {
      return("Please enter your start location and destination")
    }
  })
  
  # print the selected indices
  output$routes_selection_box = renderPrint({
    s = input$tabledata_rows_selected
    if (length(s)) {
      cat('You have selected ')
      cat(paste('Route',s), sep = ', ')
    }
  })
  
  
  
  output$tabledata <- DT::renderDataTable({
      if (input$submitbutton>0) {
        DT::datatable(directions_grouped(),
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })
  
  # output$tabledata2 <- DT::renderDataTable({
  #   print(input$tabledata_rows_selected)
  #   if (input$submitbutton>0) {
  # 
  #     DT::datatable(df_map() %>%  filter(route_num  %in% input$tabledata_rows_selected) ,
  #                   options = list(scrollX = TRUE),
  #                   rownames = FALSE)
  #   }
  # })
  # 
  # output$tabledata3 <- DT::renderDataTable({
  #   if (input$submitbutton>0) {
  #     
  #     DT::datatable(directions_raw(),
  #                   options = list(scrollX = TRUE),
  #                   rownames = FALSE)
  #   }
  # })
  # 
  
  output$mymap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% 
      # Stamen.TonerLite
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(-73.8399986, 40.746739, zoom = 10)
  })
  
  pal <- 
    colorFactor(palette = c("blue", "azure4", "orange",'green','brown','yellow','red','forestgreen','purple'), 
                levels = c('8 Avenue(ACE)',
                           'Shuttle(S)',
                           '6 Avenue(BDFM)',
                           'Brooklyn-Queens Crosstown(G)',
                           '14 St-Canarsie(L)',
                           'Broadway(`NQRW`)',
                           '7 Avenue(123)',
                           'Lexington Av(456)',
                           'Flushing(7)'))
  
  observe({


    leafletProxy("mymap", data = df_map()) %>%
      clearShapes()  %>%
      clearMarkers()

    # print('I am here')
    # print(df_map() %>%  distinct(group) %>%  pull(group))
    for(group in df_map() %>%  distinct(group) %>%  pull(group)){

      leafletProxy("mymap", data = df_map()) %>%
        addPolylines(lng = ~long, lat = ~lat, data=df_map()[df_map()$group==group,], color=~pal(service))
    }

    leafletProxy("mymap", data = df_map()) %>%
      # addCircles(lng = ~lng , lat = ~lat, weight = 1, stroke = FALSE,
      #                                      radius = 400, opacity = 1, fillOpacity = 1)
      addMarkers(lng = ~long, lat = ~lat, icon = subwayIcons)


  })
  

  
}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server) 

