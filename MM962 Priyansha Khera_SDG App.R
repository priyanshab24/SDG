library(shiny)
library(dplyr)
library(tidyverse)
library(maps)
library(shinydashboard)
library(RColorBrewer)
library(dygraphs)
library(readxl)
library(shinymanager)
library(rvest)
library(devtools)
library(plotly)
library(shinya11y)
library(DT)
library(ggplot2)
library(dplyr)
library(forecast)
library(shinycssloaders)
library(rsconnect)


#devtools::install_github("ewenme/shinya11y")

# Read the sustainable development dataset 
sdg 

# Load the world data   
library(sf)

# Install and load required package
library(rnaturalearth)
library(rnaturalearthdata)

# Load countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data 
world <- merge(world, sdg, by.x = "admin", by.y = "country", all.x = TRUE)

# App login Credentials
credentials <- data.frame(
  user = c("priyansha"),
  password = c("shiny"),
  logo_path = c("strathlogo.png"),
  stringsAsFactors = FALSE
)

# Create UI 
ui <- function(request) {
  
    
  dashboardPage(     
    dashboardHeader(
      title = span(
        style = "font-size: 18px; font-weight: bold; color: #001f3f;",
        "SDG Dashboard",
        img(src = "UNlogo.png", height = 40,alt="UNited Nations logo")
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Sustainable Development Goals",tabName="sdggoals"),
        menuItem("User Guide", tabName = "user_guide",icon=icon("book")), #user guide for users
        menuItem("Overall Score by Country", tabName = "overallscore", icon = icon("line-chart")),
        menuItem("Goal Scores by Country", tabName = "Goal_Scores", icon = icon("bar-chart")),
        menuItem("Continental Progress", tabName = "continental", icon = icon("globe")),
        menuItem("Regional Comparisons", tabName = "Regional", icon = icon("table")),
        menuItem("Download Dataset", tabName = "download_data", icon = icon("download")), 
        menuItem("Share SDG App",tabName ="share" ),
        bookmarkButton(id="bookmark")
      )
    ),
    dashboardBody(
      use_tota11y(),
      tags$head(
        tags$style(
          HTML(
            "
      /* Logo */
      .skin-blue .main-header .logo {
        background-color: #89cff0; /* Soft blue */
      }
      /* Logo when hovered */
      .skin-blue .main-header .logo:hover {
        background-color: #89cff0;
      }
      /* Rest of the header */
      .skin-blue .main-header .navbar {
        background-color: #0081a2;
      }
      /* Main sidebar */
      .skin-blue .main-sidebar {
        background-color: #aec6cf; /* Soft gray-blue */
      }
      /* Active selected tab in the sidebar menu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #d7b9c0; /* Soft pink */
      }
      /* Highlight links in the sidebar menu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #d1d1d1; /* Light gray */
        color: #000000;
      }
      /* Toggle button */
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #d7b9c0; /* Soft pink */
      }
      /* Main body */
      .skin-blue .content-wrapper {
        background-color: #f0e5d8; /* Cream */
      }
      "
          )
        )
      ),
      tabItems(
        # Inside the UI function
        tabItem(
          tabName = "sdggoals",
          fluidPage(
              h1("Sustainable Development Goals"),
            fluidRow(
              column(width = 12,
                     img(src="TrSDGUN.png",height=200,alt="SDG logo"),
                     h2("Welcome to the SDG Dashboard!"),
                     p("This page provides a look into the Sustainable Development Goals (SDGs).")
              )
            ),
            fluidRow(
              column(
                width=12,
                     h3("17 SDGs"),
                     p("Check below different SDGs:"),
                     img(src = "Goal1.gif", height = 200,alt="goal1"), 
                     img(src = "Goal2.gif", height = 200,alt="goal2"),
                     img(src = "Goal3.gif", height = 200,alt="goal3"),
                     img(src = "Goal4.gif", height = 200,alt="goal4"),
                     img(src = "Goal5.gif", height = 200,alt="goal5"),
                     img(src = "Goal6.gif", height = 200,alt="goal6"),
                     img(src = "Goal7.gif", height = 200,alt="goal7"),
                     img(src = "Goal8.gif", height = 200,alt="goal8"),
                     img(src = "Goal9.gif", height = 200,alt="goal9"),
                     img(src = "Goal10.gif", height = 200,alt="goal10"),
                     img(src = "Goal11.gif", height = 200,alt="goal11"),
                     img(src = "Goal12.gif", height = 200,alt="goal12"),
                     img(src = "Goal13.gif", height = 200,alt="goal13"),
                     img(src = "Goal14.gif", height = 200,alt="goal14"),
                     img(src = "Goal15.gif", height = 200,alt="goal15"),
                     img(src = "Goal16.gif", height = 200,alt="goal16"),
                     img(src = "Goal17.gif",height = 200,alt="goal17")
              )  
              )
            
          )
        ),
        
        # User Guide Tab
        tabItem(
          tabName = "user_guide",
          fluidRow(
            box(
              title = "User Guide",
              width = 12,
              tags$img(src = "strathlogo.png", height = 40,alt= "strathclyde logo"),
              column(6,
                     h1("Welcome to the SDG Dashboard User Guide!"),
                     p("This guide provides instructions on how to navigate and utilize the features of the SDG Dashboard."),
                     br(),
                     h2(" SDG Dashboard Overview"),
                     p("The SDG Dashboard provides access to information on the progress of countries towards the Sustainable Development Goals (SDGs) set by the United Nations."),
                     br(),
                     h3("Navigation"),
                     p("Use the sidebar menu on the left to navigate between different sections of the dashboard."),
                     br(),
                     h4("Sustainable Development Goals tab"),
                     p("In this tab you can see the basic information through visuals of 17 sustainable development goals"),
                     br(),
                     h4("User Guide Tab"),
                     p("This tab contains instructions and information on how to use the dashboard effectively."),
                     br(),
                     h4("Download Dataset Tab"),
                     p("This tab allows you to download the dataset used to generate the dashboard visualizations."),
                     br(),
                     h4("Interactive Elements"),
                     p("Explore the interactive elements such as dropdown menus, filters, and plots to analyze the data."),
                     br(),
                     h4("Sharing"),
                     p("Use the provided option to share the current state of the dashboard with other staff members."),
                     br()
                   
              ),
              column(6,
                     h1("Getting Started"),
                     p("To begin, select a tab from the sidebar menu to explore different aspects of the dashboard."),
                     br(),
                     h2("Download Dataset Tab"),
                     p("Click on this tab to download the dataset used in the dashboard."),
                     br(),
                     h3("Interacting with Visualizations"),
                     p("Hover over data points in plots to view additional information."),
                     br(),
                     h4("Bookmarking"),
                     p("Use the bookmark button to save and share the current state of the dashboard"),
                     br(),
                     h4("Thank You"),
                     p("Thank you for using the SDG Dashboard! We hope you find it informative and useful.")
           ) )
          )
        ),
        # Download Dataset Tab
        tabItem(
          tabName = "download_data",
          fluidRow(
            box(
              title = "Download SDG Dataset",
              width = 12,
              downloadButton("download_data_button", "Download SDG Dataset")
            )
          )
        ),
        tabItem(
          tabName = "overallscore",
          
          fluidPage(
            box(
              title = "Select year",
              selectInput(inputId = "year",
                          label = NULL,
                          choices = unique(world$year),
                          selected = "2015"),
              width = 12
            ),
            
            box(
              width=12,
              titlePanel("Overall Score by Country and Continents over time: "),
              withSpinner( plotlyOutput("overall_score_plot"),color ="#000103",
                         
                           color.background ="#757575"
            )
          )
        )
        ),
        
        #'Goal Scores tab
        tabItem(
          tabName = "Goal_Scores",
          fluidPage(
            box(width=12,
                selectInput("country", "Select Country:", choices = unique(sdg$country), multiple = TRUE,selected="India")
                
            ),
            box(
              plotlyOutput("goal_score_plot"),
              width=12
            )
          )
          
        ),
        #Continental progress tab 
        tabItem(
          tabName = "continental",
          fluidPage(
            width=12,
            withSpinner( plotlyOutput("choropleth_map",height = "800px", width = "1200px"))
        )),
        
        #Regional comparison tab 
        tabItem(
          tabName = "Regional",
          fluidPage(
            titlePanel("Comparisons between United Nations Regional Groups"),
            fluidRow(
              tabsetPanel(
                tabPanel("Africa", plotlyOutput("plot_africa")),
                tabPanel("Asia", plotlyOutput("plot_asia")),
                tabPanel("Europe", plotlyOutput("plot_europe")),
                tabPanel("Americas", plotlyOutput("plot_americas")),
                tabPanel("Oceania", plotlyOutput("plot_oceania"))
              ), 
              titlePanel("Data table showing UN Regional Groups score by countries over time "),
              DTOutput("regional_table")
            )
          )
        ),
        tabItem(
          tabName = "share",
          fluidRow(
            box(
              title = "Share SDG App",
              width = 12
            )
      )
    )
    )
    )
  )
}

ui <- secure_app(ui)

# Define server
server <- function(input, output,session) {
 
  observe(session$doBookmark())
  output$bookmark_ui <- renderUI({
    bookmarkButton(id="bookmark")
  })

  #Render overall score plot
  output$overall_score_plot <- renderPlotly({
    req(input$year)
    
    plot1 <- world %>%
      filter(year == input$year) %>% 
      ggplot(aes(x = admin, y = sdg_index_score, fill = continent)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Country", y = "SDG Index Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggplotly(plot1) %>%
      layout(title = paste("Overall Score for the year", input$year))
  })
  
  
  # Create choropleth map for continental progress
  output$choropleth_map <- renderPlotly({
    choropleth_map <- plot_ly() %>% 
      add_trace(
        data = world,
        type = "choropleth",
        locations = ~admin,
        z = ~sdg_index_score,
        text = ~paste("Country: ", admin, "<br>Continent: ", continent, "<br>Score: ", sdg_index_score),  # Custom tooltip text
        locationmode = "country names",
        colorscale = "Viridis",
        colorbar = list(title = "Score"),
        showscale = TRUE
      ) %>%
      layout(
        title = "Scores by Country in Each Continent",
        geo = list(
          showframe =FALSE,
          showcoastlines = TRUE,
          projection = list(type = "mercator")
        )
      )
    # Customize layout
    choropleth_map <- choropleth_map %>%
      layout(
        title = list(text = "Scores by Country in Each Continent", font = list(size = 24)),  # Larger title font
        margin = list(l = 50, r = 50, b = 50, t = 100),  # Adjust margins for better layout
        legend = list(x = 0.8, y = 0.1)  # Position legend
      )
    
    # Display map
    return(choropleth_map)
  })
  
  #Render plot for Individual scores for each goal by country over time
  output$goal_score_plot <- renderPlotly({
    
    # Define the color palette
    palette <- RColorBrewer::brewer.pal(3, "Set2")
    
    # Filter and prepare data
    plot_data <- sdg %>%
      filter(country %in% input$country) %>%
      pivot_longer(
        cols = starts_with("goal_"),
        names_to = "goal",
        values_to = "score"
      ) %>%
      arrange(country, goal)  # Arrange the data by country and goal
    
    # Create the plotly plot
    plot_ly(plot_data, x = ~goal, y = ~score, color = ~country, text = ~paste("Country: ", country, "<br>Year: ", year, "<br>Score: ", score), type = "bar") %>%
      layout(title = paste("Score by SDG Goal for country",input$country),
             xaxis = list(title = "SDG Goal"),
             yaxis = list(title = "Score"),
             colorway = palette,
             barmode = "group", 
             legend = list(orientation = "h", x = 0.5, y = -0.2), 
             hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)),  # Customize hover label appearance
             showlegend = TRUE)
    
  })
  
  ####Regional Groups comparison       
  
  centroid <- st_centroid(world)
  world$longitude <- st_coordinates(centroid)[, 1]
  world$latitude <- st_coordinates(centroid)[, 2]
  
  # Filter data for each regional group
  africa_data <- world %>%
    filter(region_un == "Africa")
  
  asia_data <- world %>%
    filter(region_un == "Asia")
  
  europe_data <- world %>%
    filter(region_un == "Europe")
  
  americas_data <- world %>%
    filter(region_un == "Americas")
  
  oceania_data <- world %>%
    filter(region_un == "Oceania")
  
  # Create plots for each region
  plot_africa <- plot_ly(africa_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                         text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
    add_markers() %>%
    layout(title = "SDG Score for Africa", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  
  plot_asia <- plot_ly(asia_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                       text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
    add_markers() %>%
    layout(title = "SDG Score for Asia", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  
  plot_europe <- plot_ly(europe_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                         text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
    add_markers() %>%
    layout(title = "SDG Score for Europe", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  
  plot_americas <- plot_ly(americas_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                           text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
    add_markers() %>%
    layout(title = "SDG Score for Americas", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  
  plot_oceania <- plot_ly(oceania_data, x = ~longitude, y = ~latitude, color = ~sdg_index_score, 
                          text = ~paste(iso_a3, "<br>SDG Score:", sdg_index_score)) %>%
    add_markers() %>%
    layout(title = "SDG Score for Oceania", xaxis = list(title = "Longitude"), yaxis = list(title = "Latitude"))
  
  # Render the plots
  output$plot_africa <- renderPlotly({
    suppressWarnings(plot_africa)
  })
  
  output$plot_asia <- renderPlotly({
    suppressWarnings(plot_asia)
  })
  
  output$plot_europe <- renderPlotly({
    suppressWarnings(plot_europe)
  })
  
  output$plot_americas <- renderPlotly({
    suppressWarnings(plot_americas)
  })
  
  output$plot_oceania <- renderPlotly({
    suppressWarnings(plot_oceania)
  })
  
  
  output$regional_table <- renderDT({
    datatable(
      world[,c("admin","iso_a3","sdg_index_score","region_un","year")],
      options = list(
        dom = 'tip', 
        paging = TRUE, # Enable pagination
        searching = TRUE, # Enable search box
        ordering = TRUE, # Enable sorting
        pageLength = 10, # Number of rows per page
        lengthMenu = c(5, 10, 15, 20), # Control the number of rows per page
        autoWidth = TRUE, # Adjust table width automatically
        rownames = FALSE, # Hide row names
        language = list(
          search = "Search:", # Customize search box label
          info = "Showing _START_ to _END_ of _TOTAL_ entries", # Customize info text
          paginate = list(
            first = "First", 
            previous="Previous",
            
            last = "Last"
          )
        )
      )
    )
  })
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # Placeholder for download button
  output$download_data_button <- downloadHandler(
    filename = function() {
      "Sustainale_Development_Goals.RData"
    },
    content = function(file) {
      write.csv(sdg, file, row.names = FALSE)
    }
  )
  
}
# Run the app
shinyApp(ui, server,enableBookmarking = "url")

