library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)

miles_app <- function(...) {
  # Custom theme
  my_theme <- bs_theme(
    version = 5,
    primary = "#2C3E50",    # Dark blue
    "bg-body" = "#F8F9FA",  # Light gray background
    "card-bg" = "#FFFFFF",  # White cards
    "font-size-base" = "0.95rem"
  )
  
  ui <- page_fluid(
    theme = my_theme,
    title = "Internal Medicine Milestones 2.0 Analysis",
    style = "min-height: 100vh;",
    
    # Title card at the top
    card(
      height = "80px",
      class = "mb-3",
      style = "background: linear-gradient(90deg, #2C3E50 0%, #3498DB 100%);",
      div(
        style = "color: white; padding: 15px;",
        h2("Internal Medicine Milestones 2.0 Analysis",
           style = "margin: 0; font-weight: 300;")
      )
    ),
    
    layout_columns(
      col_widths = c(3, 9),
      
      # Control panel card
      card(
        full_screen = TRUE,
        height = "calc(66vh - 40px)",
        class = "shadow-sm",
        card_header(
          class = "bg-light",
          "Analysis Controls"
        ),
        fileInput("files", "Upload CSV Files", 
                  multiple = TRUE,
                  accept = c(".csv")),
        uiOutput("period_selector"),
        radioButtons("plot_type", "Visualization Type:",
                     choices = c("Faceted by Category" = "facet",
                                 "Lollipop Chart" = "lollipop"))
      ),
      
      layout_columns(
        col_widths = c(12),
        
        # Main visualization card
        card(
          full_screen = TRUE,
          height = "calc(66vh - 40px)",
          class = "shadow-sm",
          card_header(
            class = "bg-light",
            span(icon("chart-line"), "Milestone Score Distribution", 
                 style = "font-weight: 500;")
          ),
          plotOutput("milestone_plot", height = "250px")
        ),
        
        # Data preview and summary in a row
        layout_columns(
          col_widths = c(6, 6),
          card(
            height = "calc(33vh - 40px)",
            class = "shadow-sm",
            card_header(
              class = "bg-light",
              span(icon("table"), "Data Preview", 
                   style = "font-weight: 500;")
            ),
            tableOutput("data_preview")
          ),
          card(
            height = "calc(33vh - 40px)",
            class = "shadow-sm",
            card_header(
              class = "bg-light",
              span(icon("chart-pie"), "Score Summary", 
                   style = "font-weight: 500;")
            ),
            tableOutput("score_summary")
          )
        )
      )
    )
  )
  
  server <- function(input, output) {
    data <- reactive({
      req(input$files)
      file_paths <- input$files$datapath
      df <- import_and_select_columns(file_paths)
      
      df <- df %>%
        filter(
          grepl("Year-End", period),
          Resident.Year == 3
        )
      
      reorder_milestones(df)
    }) %>% bindCache(input$files)
    
    output$period_selector <- renderUI({
      req(data())
      periods <- unique(data()$period)
      selectInput("period_filter", "Academic Year:",
                  choices = c("All", sort(periods)),
                  selected = "All")
    })
    
    filtered_data <- reactive({
      req(data())
      df <- data()
      
      if (input$period_filter != "All") {
        df <- df %>% 
          filter(period == input$period_filter)
      }
      
      df
    })
    
    graduation_analysis <- reactive({
      req(filtered_data())
      grad_data <- filtered_data()
      
      milestone_cols <- names(grad_data)[grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(grad_data))]
      
      results <- grad_data %>%
        select(all_of(milestone_cols)) %>%
        pivot_longer(everything(), names_to = "Milestone", values_to = "Score") %>%
        group_by(Milestone) %>%
        summarise(
          Below7 = mean(Score < 7) * 100,
          At7 = mean(Score >= 7 & Score < 8) * 100,
          Above8 = mean(Score >= 8) * 100,
          MedianScore = median(Score)
        )
      
      results %>%
        mutate(
          Category = sub("\\d+$", "", Milestone),
          Milestone = factor(Milestone, levels = milestone_cols)
        ) %>%
        arrange(Category, Milestone)
    }) %>% bindCache(filtered_data())
    
    output$data_preview <- renderTable({
      head(filtered_data())
    })
    
    output$milestone_plot <- renderPlot({
      req(graduation_analysis())
      
      data <- graduation_analysis()
      
      period_title <- if(input$period_filter == "All") {
        "All Academic Years"
      } else {
        paste("Academic Year", input$period_filter)
      }
      
      base_theme <- theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 16, face = "bold", color = "#2C3E50"),
          axis.title = element_text(size = 12, color = "#2C3E50"),
          axis.text.y = element_text(size = 11),
          legend.position = "right",
          panel.grid.major = element_line(color = "#E1E5EA"),
          panel.grid.minor = element_line(color = "#F1F4F8")
        )
      
      if(input$plot_type == "facet") {
        # Define blue color palette for different categories
        blue_palette <- c(
          PC = "#1f77b4",    # Dark blue
          MK = "#3498db",    # Medium blue
          SBP = "#5dade2",   # Light blue
          PBL = "#85c1e9",   # Very light blue
          PROF = "#2874a6",  # Navy blue
          ICS = "#21618c"    # Deep blue
        )
        
        ggplot(data) +
          geom_bar(aes(x = reorder(Milestone, -MedianScore), y = Below7, fill = Category),
                   stat = "identity") +
          geom_text(aes(x = reorder(Milestone, -MedianScore), y = Below7 + 2,
                        label = round(MedianScore, 1)), size = 3) +
          facet_wrap(~Category, scales = "free_x") +
          scale_fill_manual(values = blue_palette) +
          base_theme +
          theme(strip.background = element_rect(fill = "#2C3E50"),
                strip.text = element_text(color = "white", face = "bold")) +
          labs(title = paste("Milestone Performance by Category -", period_title),
               x = "Milestone", y = "Percentage Below 7",
               fill = "Category")
        
      } else if(input$plot_type == "lollipop") {
        # Define gradient of blues for categories
        blue_palette <- c(
          PC = "#1f77b4",    # Dark blue
          MK = "#3498db",    # Medium blue
          SBP = "#5dade2",   # Light blue
          PBL = "#85c1e9",   # Very light blue
          PROF = "#2874a6",  # Navy blue
          ICS = "#21618c"    # Deep blue
        )
        
        ggplot(data) +
          geom_segment(aes(x = reorder(Milestone, -MedianScore), 
                           xend = reorder(Milestone, -MedianScore),
                           y = 0, yend = Below7, color = Category), 
                       size = 1.2) +
          geom_point(aes(x = reorder(Milestone, -MedianScore), y = Below7, 
                         fill = Category),
                     size = 3, shape = 21, color = "white") +
          coord_flip() +
          scale_fill_manual(values = blue_palette) +
          scale_color_manual(values = blue_palette) +
          base_theme +
          labs(title = paste("Milestone Performance -", period_title),
               y = "Percentage Below 7",
               x = "Milestone")
      }
    })
    
    output$score_summary <- renderTable({
      req(graduation_analysis())
      
      graduation_analysis() %>%
        mutate(
          across(where(is.numeric), round, 1),
          Risk = case_when(
            Below7 < 2.5 ~ "Minimal",
            Below7 < 5 ~ "Low",
            Below7 < 7.5 ~ "Moderate",
            TRUE ~ "High"
          )
        )
    })
  }
  
  shinyApp(ui, server, ...)
}


