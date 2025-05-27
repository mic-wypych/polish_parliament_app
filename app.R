# Polish Sejm Members Visualization App
# This app visualizes members of the Polish Sejm in a circular layout
# with hover functionality to display additional information

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(bslib)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(dplyr)
library(reactable)
library(stringr)
library(dplyr)
library(tidyr)
library(glue)

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


    dashboardPage(
      dashboardHeader(title = "Polish parliament application"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("parties", tabName = "parties", icon = icon("landmark")),
          menuItem("Members", tabName = "members", icon = icon("user")),
          menuItem("Votes", tabName = "votes", icon = icon("envelope"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "members",
          div(class = "title-box",
        p("Interactive visualization of the members of the Polish Sejm"),
        p("Hover over each point to see detailed information about the Sejm member")),
    
        
        fluidRow(
        column(9,
               withSpinner(
                plotlyOutput("memberPlot", height = "700px")
              )
        ),
        column(3,
               div(class = "info-box",
                   h4("Filter Options"),
                   selectInput("partyFilter", "Filter by Party:", choices = c("All" = ""), multiple = TRUE),
                   hr(),
                   h4("Selected Member Details"),
                   uiOutput("photo"),
                   verbatimTextOutput("memberDetails")
               )
        )
      ),
      fluidRow(
        withSpinner(
          reactableOutput("mptable")
      )
      )

          ),
          tabItem(tabName = "parties",
          fluidRow(
            navset_card_underline(
              nav_panel("Komisje",
              withSpinner(
                plotlyOutput("committeePlot")
              )
            ),
              nav_panel("Grupy",
              withSpinner(
                plotlyOutput("groupPlot")
              )
            )
            )

            
          )),
          
          tabItem(tabName = "votes",
          withSpinner(
            plotlyOutput("v_timePlot")
          ),
          withSpinner(
            reactableOutput("vtable")
          )

          )
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
        
       
        
        return(list(
          id = member_id, 
          interpellationCount = interpellation_count
        ))
      })
      
      # Convert list to dataframe
      members_df <- do.call(rbind, lapply(members_data, function(x) {
        data.frame(
          id = x$id, 
          interpellationCount = x$interpellationCount,
          stringsAsFactors = FALSE
        )
      }))
      
      # Join with your original members dataframe to keep all the other information
      members_df <- merge(members, members_df, by = "id")
      members_df <- members_df |> filter(active == TRUE)
      # Update party filter choices
      if (!is.null(members_df)) {
        clubs <- sort(unique(members_df$club))
        updateSelectInput(session, "partyFilter", choices = c("All" = "", clubs))
      }
      
      return(members_df)
    })
    fetchVoteData <- reactive({
      proc_response <- tryCatch({
        GET("https://api.sejm.gov.pl/sejm/term10/proceedings/")
      }, error = function(e) {
        return(NULL)
      })
      
      proc_response <- fromJSON(content(proc_response, "text", encoding = "UTF-8"))
      
      proc_response
      
      v_response <- tryCatch({
        GET("https://api.sejm.gov.pl/sejm/term10/votings/1")
      }, error = function(e) {
        return(NULL)
      })
      
      v_response <- fromJSON(content(v_response, "text", encoding = "UTF-8"))
      
      
      v_response
      
      voting_list <- list()
      
      for (i in proc_response$number) {
        v_response <- tryCatch({
          GET(paste0("https://api.sejm.gov.pl/sejm/term10/votings/", i))
        }, error = function(e) {
          return(NULL)
        })
        
        v_response <- fromJSON(content(v_response, "text", encoding = "UTF-8"))
        v_response <- list(v_response)
        voting_list <- append(voting_list, v_response)
      }
      
      
      vote_df <- voting_list[[1]]
      
      vote_df <- vote_df[,c("yes", "no", "abstain","date", "title", "topic", "totalVoted")]
      vote_df$date <- as.Date(vote_df$date)
      for (i in 2:length(voting_list)) {
      
        df_i <- data.frame(voting_list[[i]][c("yes", "no", "abstain","date", "title", "topic", "totalVoted")])
        df_i$date <- as.Date(df_i$date)
        vote_df <- rbind(vote_df, df_i)
      
      }
      
      #do we need to get proceedings and for each proceeding get the votes?
      
      vote_df$yes <- as.numeric(vote_df$yes )
      vote_df$no <- as.numeric(vote_df$no )
      vote_df$abstain <- as.numeric(vote_df$abstain )

      return(vote_df)
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
      
      members_by_party$club <- factor(members_by_party$club, levels = rev(c("Razem", "Lewica", "Polska2050-TD", "PSL-TD", "KO", "PiS", "Republikanie", "Konfederacja", "niez.")))
      members_arranged <- members_by_party %>% arrange(club, lastName) %>% ungroup()
      
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
        result <- result %>% arrange(club, lastName)

        return(result)
      }
      
      # Apply the function to create coordinates
      plotData <- create_hemicycle_coords(members_arranged)
      
      # Adjust y-coordinates to make sure all points are in the upper half-plane
      plotData$y[plotData$y < 0] <- 0.1
      

      plotData$row <- row.names(plotData)

      # Create the plot
      p <- ggplot(plotData, aes(x = x, y = y, color = club, key = rownames(plotData),
                                 text = paste0("<b>", firstName, " ", lastName, "</b><br>",
                                              "Party: ", club))) +
        geom_point(size = 4, alpha = 0.8) +
        scale_color_manual(values = c("PiS" = "#012b7f", "KO" = "#d41c3c", "PSL-TD" = "#3cb43c",
        "Polska2050-TD" = "#f9c300", "Lewica" = "#a81849", "Razem" = "#870f57",
        "Konfederacja" = "#1b263f", "Republikanie" = "#749cbc", "niez." = "#000000")) +
        theme_void() +
        theme(legend.title = element_text(size = 10),
              legend.text = element_text(size = 8),
              legend.position = "bottom",
              plot.margin = unit(c(1, 1, 1, 1), "cm")) +
        labs(color = "Party") +
        coord_fixed(ratio = 1) +

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


    output$photo <- renderUI({
      event_data <- event_data("plotly_hover")

      if (!is.null(event_data)) {
        members_df <- fetchSejmData()
        if (!is.null(input$partyFilter) && length(input$partyFilter) > 0) {
            members_df <- members_df %>% filter(club %in% input$partyFilter)
          }
        
          members_df$club <- factor(members_df$club, levels = rev(c("Razem", "Lewica", "Polska2050-TD", "PSL-TD", "KO", "PiS", "Republikanie", "Konfederacja", "niez.")))
          members_arranged <- members_df %>% dplyr::arrange(club, lastName)
          
          point_index <- event_data$key
          
  
            member <- members_arranged[point_index, ]

        tags$img(src = paste0("https://api.sejm.gov.pl/sejm/term10/MP/", member$id, "/photo"), height = "180px", style = "max-width: 140px; border-radius: 20%;")
      } else {
        cat("Hover over a point to see details")
      }
      })

    output$memberDetails <- renderPrint({
      event_data <- event_data("plotly_hover")
      

      if (!is.null(event_data)) {
        members_df <- fetchSejmData()
        if (!is.null(input$partyFilter) && length(input$partyFilter) > 0) {
            members_df <- members_df %>% filter(club %in% input$partyFilter)
          }
          
        
        members_df$club <- factor(members_df$club, levels = rev(c("Razem", "Lewica", "Polska2050-TD", "PSL-TD", "KO", "PiS", "Republikanie", "Konfederacja", "niez.")))
        members_arranged <- members_df %>% dplyr::arrange(club, lastName)
        
        point_index <- event_data$key
        

          member <- members_arranged[point_index, ]
          cat("Name: ", member$firstName, " ", member$lastName, "\n")
          cat("Party: ", as.character(member$club), "\n")
          cat("Proffesion: ", member$profession, "\n")
          cat("District: ", member$districtName, "\n")
          cat("Birth date: ", member$birthDate, "\n")
          cat("Number of votes: ", as.character(member$numberOfVotes), "\n")
          cat("Member ID: ", member$id, "\n")

      } else {
        cat("Hover over a point to see details")
      }
    })

    output$mptable <- renderReactable({
      bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height, transition = "width 0.6s ease"))
        chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
      }
      mps <- fetchSejmData()

      q_response <- tryCatch({
        GET('https://api.sejm.gov.pl/sejm/term10/writtenQuestions')
      }, error = function(e) {
        return(NULL)
      })
      
      q_response <- fromJSON(content(q_response, "text", encoding = "UTF-8"))
      q_response <- tibble(q_response)

      q_count <- q_response |>
        count(from, name = "q_count") %>%
        mutate(from = as.integer(from))

      inter_response <- tryCatch({
        GET('https://api.sejm.gov.pl/sejm/term10/interpellations')
      }, error = function(e) {
        return(NULL)
      })
      
      
      inter_response <- fromJSON(content(inter_response, "text", encoding = "UTF-8"))
      inter_response <- tibble(inter_response)


      inter_response <- unnest(inter_response, from )

      inter_count <- inter_response |>
        count(from, name = "inter_count") %>%
        mutate(from = as.integer(from))
      
      mps <- mps %>%
        left_join(q_count, by = c("id" = "from")) |>
          left_join(inter_count, by = c("id" = "from")) |>
        mutate(q_count = replace_na(q_count, 0),
               inter_count = replace_na(inter_count, 0))



      mps |>
        mutate(photo = paste0("https://api.sejm.gov.pl/sejm/term10/MP/", id, "/photo")) %>%
        select(firstLastName, photo, club, birthDate, districtName, numberOfVotes, q_count, inter_count) |>
        reactable(
          columns = list(
            numberOfVotes = colDef(name = "number of votes", align = "left", cell = function(value) {
              width <- paste0(value / max(mps$numberOfVotes) * 100, "%")
              bar_chart(value, width = width)
            }),
            photo = colDef(cell = function(value) {
              image <- img(src = sprintf(value), style = "height: 50px; border-radius: 20%;")
              tagList(
                div(style = "display: inline-block; width: 45px;", image)
              )
            }),
            q_count = colDef(name = "number of questions", align = "left", cell = function(value) {
              width <- paste0(value / max(mps$q_count) * 100, "%")
              bar_chart(value, width = width, fill = "#2a9d8f")
            }),
            inter_count = colDef(name = "number of interpellations", align = "left", cell = function(value) {
              width <- paste0(value / max(mps$inter_count) * 100, "%")
              bar_chart(value, width = width, fill = "#8338ec")
            })


  ))
    })

    output$committeePlot <- renderPlotly({
      com_response <- tryCatch({
        GET("https://api.sejm.gov.pl/sejm/term10/committees")
      }, error = function(e) {
        return(NULL)
      })
      
      committees <- fromJSON(content(com_response, "text", encoding = "UTF-8"))
      
      #there are 39 committees which is an ok number I guess
      
      # the members are already listed inside nice dataframes so it should be fairly easy to get it to work
      members <- committees$members
      
      coms_members_df <-committees$members[[1]]
      coms_members_df$name <- committees$name[1]
      for (i in 2:length(members)) {
         temp <- members[[i]]
         temp$name <- committees$name[i]
         coms_members_df <- rbind(coms_members_df, temp)
      }
      
      
      waffle_df <- coms_members_df %>%
        group_by(name) %>%
        arrange(club, lastFirstName) %>%
        mutate(
          id = row_number(),        # unique ID per MP in committee
          row = (id - 1) %/% 10,    # 10 tiles per row
          col = (id - 1) %% 10,
          position = `function`,
          name = str_wrap(name, 50),
          position = replace_na(position, " ")
        ) %>%
        ungroup() 
      
      # Tooltip text
      waffle_df <- waffle_df %>%
        mutate(tooltip = glue("MP: {lastFirstName}<br>club: {club}<br>Function: {position}"))
      
      # ggplot
      p <- ggplot(waffle_df, aes(x = col, y = -row, fill = club, text = tooltip)) +
        geom_tile(color = "white", width = 0.9, height = 0.9) +
        facet_wrap(~name) +
          scale_fill_manual(values = c("PiS" = "#012b7f", "KO" = "#d41c3c", "PSL-TD" = "#3cb43c",
        "Polska2050-TD" = "#f9c300", "Lewica" = "#a81849", "Razem" = "#870f57",
        "Konfederacja" = "#1b263f", "Republikanie" = "#749cbc", "niez." = "#000000")) +
        coord_equal() +
        labs(title = "Committees and their members") +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size= 4)
        )
      
      # Convert to interactive plotly
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white", 
            bordercolor = "black", 
            font = list(family = "Arial", size = 12)
          ),
          legend = list(orientation = "h", y = -0.1)
        )
    })

    output$groupPlot <- renderPlotly({
      group_response <- tryCatch({
        GET("https://api.sejm.gov.pl/sejm/term10/bilateralGroups")
      }, error = function(e) {
        return(NULL)
      })
      
      
      group_response <- fromJSON(content(group_response, "text", encoding = "UTF-8"))
      
      
      group_list <- list()
      
      for (i in group_response[["id"]]) {
      
        group_i_response <- tryCatch({
          GET(paste0("https://api.sejm.gov.pl/sejm/term10/bilateralGroups/", i))
        }, error = function(e) {
          return(NULL)
        })
      
        group_i <- fromJSON(content(group_i_response, "text", encoding = "UTF-8"))
        group_i <- list(group_i)
        group_list <- append(group_list, group_i)
      
      }
      
      
      name <- group_list[[1]]$name
      
      group_list[[1]]$members$group_name <- name
      
      df_group_members <- group_list[[1]]$members[,c("club", "id", "name","type", "senator", "group_name")]
      
      for (i in 2:length(group_list)) {
        name <- group_list[[i]]$name
      
        group_list[[i]]$members$group_name <- name
        df_i <- data.frame(group_list[[i]]$members[,c("club", "id", "name","type", "senator", "group_name")])
        
        df_group_members <- rbind(df_group_members, df_i)
      
      }
      
      df_group_members <- df_group_members %>%
        filter(senator == FALSE)
      
      
      waffle_df <- df_group_members %>%
        group_by(group_name) %>%
        arrange(club, name) %>%
        mutate(
          id = row_number(),        # unique ID per MP in committee
          row = (id - 1) %/% 10,    # 10 tiles per row
          col = (id - 1) %% 10,
          name = str_wrap(name, 50),
          type = replace_na(type, " ")
        ) %>%
        ungroup() 
      
      # Tooltip text
      waffle_df <- waffle_df %>%
        mutate(tooltip = glue("MP: {name}<br>club: {club}<br>Function: {type}"))
      
      # ggplot
      p <- ggplot(waffle_df, aes(x = col, y = -row, fill = club, text = tooltip)) +
        geom_tile(color = "white", width = 0.9, height = 0.9) +
        facet_wrap(~group_name) +
          scale_fill_manual(values = c("PiS" = "#012b7f", "KO" = "#d41c3c", "PSL-TD" = "#3cb43c",
        "Polska2050-TD" = "#f9c300", "Lewica" = "#a81849", "Razem" = "#870f57",
        "Konfederacja" = "#1b263f", "Republikanie" = "#749cbc", "niez." = "#000000")) +
        coord_equal() +
        labs(title = "Groups and their members") +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size= 4)
        )
      
      # Convert to interactive plotly
      ggplotly(p, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white", 
            bordercolor = "black", 
            font = list(family = "Arial", size = 12)
          ),
          legend = list(orientation = "h", y = -0.1)
        )
    })

    output$vtable <- renderReactable({
      vote_df <- fetchVoteData()
      
      bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
              bar <- div(style = list(background = fill, width = width, height = height, transition = "width 0.6s ease"))
              chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
              div(style = list(display = "flex", alignItems = "center"), label, chart)
            }
      
      vote_df |>
        reactable(
          columns = list(
            yes = colDef(name = "yes votes", align = "left", cell = function(value) {
              width <- paste0(value / max(vote_df$yes) * 100, "%")
              bar_chart(value, width = width, fill = "#0081a7")
            }),
            abstain = colDef(name = "abstain votes", align = "left", cell = function(value) {
              width <- paste0(value / max(vote_df$abstain) * 100, "%")
              bar_chart(value, width = width, fill = "#8d99ae")}),
            no = colDef(name = "no votes", align = "left", cell = function(value) {
              width <- paste0(value / max(vote_df$no) * 100, "%")
              bar_chart(value, width = width, fill = "#780000")
            })
            
      ),
      filterable = TRUE)
    })

    output$v_timePlot <- renderPlotly({
      vote_df <- fetchVoteData()

      vote_time_plot <- vote_df %>%
        count(date) |>
        ggplot(aes(x = date, y = n, text = glue("data: {date}<br>votes: {n}"))) +
        geom_col(fill = "#780000", width = 1.5) +
        scale_x_date(date_breaks = "month") +
        theme_minimal() +
        labs(title = "Number of votes per day", y = "ilosć głosowań", x = NULL) +
        theme(axis.text.x = element_text(angle = 90),
            panel.grid = element_blank(),
            panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"))

      ggplotly(vote_time_plot, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white", 
            bordercolor = "black", 
            font = list(family = "Arial", size = 12)
          ),
          legend = list(orientation = "h", y = -0.1)
        )

    })
  }

# Run the app
shinyApp(ui = ui, server = server)