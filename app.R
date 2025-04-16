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
      
      return(members_df)
    })
    
    # Create the plot with members arranged in a hemicycle
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
      
      # Group by party
      members_by_party <- members_df %>%
        group_by(club) %>%
        arrange(lastName) %>%
        mutate(party_size = n()) %>%
        ungroup()
      
      # Order parties by size (optional)
      party_order <- members_by_party %>%
        group_by(club) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        pull(club)
      
      members_by_party$club <- factor(members_by_party$club, levels = party_order)
      members_arranged <- members_by_party %>% arrange(club)
      
      # Calculate positions for hemicycle layout
      # Start with the largest radius (outer edge)
      base_radius <- 10
      # Number of rows in the hemicycle
      num_rows <- 5
      # Calculate radius for each row (decreasing as we move inward)
      radii <- seq(base_radius, base_radius * 0.6, length.out = num_rows)
      
      # Calculate positions for each MP
      n <- nrow(members_arranged)
      
      # Function to assign positions in hemicycle
      create_hemicycle_coords <- function(df) {
        # Define the hemicycle angle range (pi radians = 180 degrees)
        angle_range <- pi
        
        # Get unique parties and their sizes
        party_sizes <- df %>%
          group_by(club) %>%
          summarize(size = n()) %>%
          arrange(club)
        
        # Allocate angles proportionally to party sizes
        total_mps <- sum(party_sizes$size)
        party_angles <- party_sizes %>%
          mutate(
            angle_start = c(0, cumsum(head(size / total_mps * angle_range, -1))),
            angle_end = cumsum(size / total_mps * angle_range)
          )
        
        # Create result dataframe
        result <- data.frame()
        
        # For each party, create points in the hemicycle
        for (i in 1:nrow(party_sizes)) {
          party_df <- df %>% filter(club == party_sizes$club[i])
          party_size <- nrow(party_df)
          
          # Get angle range for this party
          start_angle <- party_angles$angle_start[i]
          end_angle <- party_angles$angle_end[i]
          
          # Distribute MPs across rows
          mps_per_row <- ceiling(party_size / num_rows)
          
          # Create positions for each MP in this party
          party_positions <- data.frame()
          mp_count <- 0
          
          for (row in 1:num_rows) {
            row_radius <- radii[row]
            row_mps <- min(mps_per_row, party_size - mp_count)
            
            if (row_mps <= 0) break
            
            # Calculate angles for this row
            if (row_mps == 1) {
              row_angles <- (start_angle + end_angle) / 2
            } else {
              row_angles <- seq(start_angle, end_angle, length.out = row_mps)
            }
            
            # Calculate coordinates
            x_coords <- row_radius * cos(row_angles)
            y_coords <- row_radius * sin(row_angles)
            
            # Add to positions dataframe
            row_positions <- data.frame(
              x = x_coords,
              y = y_coords,
              row = row
            )
            
            party_positions <- rbind(party_positions, row_positions)
            mp_count <- mp_count + row_mps
            
            if (mp_count >= party_size) break
          }
          
          # Ensure we have the correct number of positions
          party_positions <- party_positions[1:party_size, ]
          
          # Add to party dataframe
          party_df$x <- party_positions$x
          party_df$y <- party_positions$y
          party_df$row <- party_positions$row
          
          # Add to result
          result <- rbind(result, party_df)
        }
        
        return(result)
      }
      
      # Apply the function to create coordinates
      plotData <- create_hemicycle_coords(members_arranged)
      
      # Adjust y-coordinates to make sure all points are in the upper half-plane
      plotData$y[plotData$y < 0] <- 0.1
      
      # Define colors for different clubs
      clubs <- unique(plotData$club)
      n_clubs <- length(clubs)
      
      if (n_clubs <= 8) {
        colors <- brewer.pal(max(3, n_clubs), "Set1")
      } else {
        colors <- colorRampPalette(brewer.pal(8, "Set1"))(n_clubs)
      }
      
      club_colors <- setNames(colors[1:n_clubs], clubs)
      
      # Create the plot
      p <- ggplot(plotData, aes(x = x, y = y, color = club,
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
        coord_fixed(ratio = 1) +
        # Flip coordinates to make the hemicycle open at the bottom like in the image
        scale_y_reverse() +
        # Set limits to ensure we see the proper hemicycle shape
        xlim(c(-base_radius-1, base_radius+1)) +
        ylim(c(base_radius+1, -1))
      
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
        if (!is.null(input$partyFilter) && length(input$partyFilter) > 0) {
          members_df <- members_df %>% filter(club %in% input$partyFilter)
        }
        
        # Order by party
        party_order <- members_df %>%
          group_by(club) %>%
          summarize(count = n()) %>%
          arrange(desc(count)) %>%
          pull(club)
        
        members_df$club <- factor(members_df$club, levels = party_order)
        members_arranged <- members_df %>% arrange(club, lastName)
        
        point_index <- event_data$pointNumber[1] + 1
        
        if (point_index <= nrow(members_arranged)) {
          member <- members_arranged[point_index, ]
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
