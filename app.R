# Polish Sejm Members Visualization App
# This app visualizes members of the Polish Sejm in a circular layout
# with hover functionality to display additional information

# Load required libraries
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        border-left: 5px solid #dc3545;
      }
      .info-box {
        margin-top: 20px;
        padding: 10px;
        background-color: #f8f9fa;
        border-radius: 5px;
      }
    "))
  ),
  titlePanel("Polish Sejm Members Visualization"),
  
  div(class = "title-box",
      p("Interactive visualization of the members of the Polish Sejm"),
      p("Hover over each point to see detailed information about the Sejm member")
  ),
  
  fluidRow(
    column(9,
           plotlyOutput("memberPlot", height = "700px")
    ),
    column(3,
           div(class = "info-box",
               h4("Filter Options"),
               selectInput("partyFilter", "Filter by Party:", choices = c("All" = ""), multiple = TRUE),
               hr(),
               h4("Selected Member Details"),
               verbatimTextOutput("memberDetails")
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to fetch data from the Sejm API
  fetchSejmData <- reactive({
    # Fetch the list of members from the current term
    response <- tryCatch({
      GET("https://api.sejm.gov.pl/sejm/term10/MP")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(response) || http_status(response)$category != "Success") {
      return(data.frame(
        id = character(),
        firstName = character(),
        lastName = character(),
        club = character(),
        interpellationCount = integer(),
        x = numeric(),
        y = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    
    members <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Get additional data for each member (like interpellations count)
    members_data <- lapply(members$id, function(member_id) {
      # Get interpellations count
      interpellations_response <- tryCatch({
        GET(paste0("https://api.sejm.gov.pl/sejm/term10/MP/", member_id, "/interpellations"))
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(interpellations_response) || http_status(interpellations_response)$category != "Success") {
        interpellation_count <- 0
      } else {
        interpellations <- fromJSON(content(interpellations_response, "text", encoding = "UTF-8"))
        interpellation_count <- length(interpellations)
      }
      
      return(list(id = member_id, interpellationCount = interpellation_count))
    })
    
    # Convert list to dataframe
    interpellations_df <- do.call(rbind, lapply(members_data, function(x) {
      data.frame(id = x$id, interpellationCount = x$interpellationCount, stringsAsFactors = FALSE)
    }))
    
    # Merge with members data
    members_df <- merge(members, interpellations_df, by = "id")
    
    # Update party filter choices
    if (!is.null(members_df)) {
      clubs <- sort(unique(members_df$club))
      updateSelectInput(session, "partyFilter", choices = c("All" = "", clubs))
    }
    
    # Calculate positions for circle layout
    n <- nrow(members_df)
    angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    radius <- 10
    
    members_df$x <- radius * cos(angles)
    members_df$y <- radius * sin(angles)
    
    return(members_df)
  })
  
  # Create the plot with members arranged in a circle
  output$memberPlot <- renderPlotly({
    members_df <- fetchSejmData()
    
    if (nrow(members_df) == 0) {
      return(ggplotly(ggplot() + 
                       annotate("text", x = 0, y = 0, label = "Unable to fetch data from the API") +
                       theme_void()))
    }
    
    # Apply party filter if selected
    if (!is.null(input$partyFilter) && length(input$partyFilter) > 0) {
      members_df <- members_df %>% filter(club %in% input$partyFilter)
    }
    
    # Define colors for different clubs
    clubs <- unique(members_df$club)
    n_clubs <- length(clubs)
    
    if (n_clubs <= 8) {
      colors <- brewer.pal(max(3, n_clubs), "Set1")
    } else {
      colors <- colorRampPalette(brewer.pal(8, "Set1"))(n_clubs)
    }
    
    club_colors <- setNames(colors[1:n_clubs], clubs)
    
    # Create the plot
    p <- ggplot(members_df, aes(x = x, y = y, color = club,
                                text = paste0("<b>", firstName, " ", lastName, "</b><br>",
                                             "Party: ", club, "<br>",
                                             "Interpellations: ", interpellationCount, "<br>",
                                             "ID: ", id))) +
      geom_point(size = 4, alpha = 0.8) +
      scale_color_manual(values = club_colors) +
      theme_void() +
      theme(legend.title = element_text(size = 10),
            legend.text = element_text(size = 8),
            legend.position = "bottom",
            plot.margin = unit(c(1, 1, 1, 1), "cm")) +
      labs(color = "Party") +
      coord_fixed(ratio = 1)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", 
                              bordercolor = "black", 
                              font = list(family = "Arial", size = 12)),
             legend = list(orientation = "h", y = -0.1))
  })
  
  # Display detailed information about selected member
  output$memberDetails <- renderPrint({
    event_data <- event_data("plotly_hover")
    if (!is.null(event_data)) {
      members_df <- fetchSejmData()
      point_index <- event_data$pointNumber[1] + 1
      
      if (point_index <= nrow(members_df)) {
        member <- members_df[point_index, ]
        cat("Name: ", member$firstName, " ", member$lastName, "\n")
        cat("Party: ", member$club, "\n")
        cat("Interpellations: ", member$interpellationCount, "\n")
        cat("Member ID: ", member$id, "\n")
      }
    } else {
      cat("Hover over a point to see details")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
