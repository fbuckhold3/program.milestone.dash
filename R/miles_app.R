library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(fmsb)

miles_app <- function(...) {
        # [Previous UI code remains the same up until the server function]
  my_theme <- bs_theme(
    version = 5,
    primary = "#2C3E50",
    "bg-body" = "#F8F9FA",
    "card-bg" = "#FFFFFF",
    "font-size-base" = "0.95rem"
  )
  
  ui <- page_fluid(
    theme = my_theme,
    title = "Residency/Fellowship Program Milestones 2.0 Analysis",
    style = "min-height: 100vh;",
    
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
      
      card(
        full_screen = TRUE,
        height = "calc(50vh - 60px)",  # Adjusted height
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
        
        card(
          full_screen = TRUE,
          height = "calc(50vh - 60px)",  # Adjusted height
          class = "shadow-sm",
          card_header(
            class = "bg-light",
            "Milestone Score Distribution"
          ),
          plotOutput("milestone_plot", height = "calc(50vh - 120px)")  # Adjusted height
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          card(
            height = "calc(50vh - 60px)",  # Adjusted height
            class = "shadow-sm",
            card_header(
              class = "bg-light",
              "Milestone Spider Plot"
            ),
            plotOutput("spider_plot", height = "calc(50vh - 120px)")  # Adjusted height
          ),
          card(
            height = "calc(50vh - 60px)",
            class = "shadow-sm",
            card_header(
              class = "bg-light",
              "Score Summary"
            ),
            div(
              style = "height: calc(50vh - 120px); overflow-y: auto;",
              DTOutput("score_summary", height = "100%")
            )
          )
        )
      )
    )
  )
    
    server <- function(input, output, session) {
      # [Previous reactive expressions remain the same up until the milestone_plot]
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
      })
      
      output$period_selector <- renderUI({
        req(data())
        periods <- unique(data()$period)
        selectInput("period_filter", "Academic Year:",
                    choices = c("All", sort(periods)),
                    selected = "All")
      })
      
      filtered_data <- reactive({
        req(data(), input$period_filter)
        df <- data()
        
        if (input$period_filter != "All") {
          df <- df %>% 
            filter(period == input$period_filter)
        }
        
        df
      })
      
      total_medians <- reactive({
        req(filtered_data())
        milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(filtered_data()), value = TRUE)
        
        filtered_data() %>%
          select(all_of(milestone_cols)) %>%
          summarise(across(everything(), median)) %>%
          pivot_longer(everything(), names_to = "Milestone", values_to = "MedianScore")
      })
      
      graduation_analysis <- reactive({
        req(filtered_data())
        grad_data <- filtered_data()
        
        milestone_cols <- grep("^(PC|MK|SBP|PBL|PROF|ICS)", names(grad_data), value = TRUE)
        
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
      })
      # Updated milestone plot
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
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 18, face = "bold", color = "#2C3E50"),
            axis.title = element_text(size = 14, color = "#2C3E50"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            panel.grid.major = element_line(color = "#E1E5EA"),
            panel.grid.minor = element_line(color = "#F1F4F8")
          )
        
        if(input$plot_type == "facet") {
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
                          label = sprintf("%.1f%%", Below7)), 
                      size = 4) +  # Added percentage symbol and increased text size
            facet_wrap(~Category, scales = "free_x") +
            scale_fill_manual(values = blue_palette) +
            base_theme +
            theme(
              strip.background = element_rect(fill = "#2C3E50"),
              strip.text = element_text(color = "white", face = "bold", size = 12),
              panel.spacing = unit(1, "lines")
            ) +
            labs(title = paste("Milestone Performance by Category -", period_title),
                 x = "Milestone", 
                 y = "Percentage Below 7",
                 fill = "Category")
          
        } else if(input$plot_type == "lollipop") {
          blue_palette <- c(
            PC = "#1f77b4",
            MK = "#3498db",
            SBP = "#5dade2",
            PBL = "#85c1e9",
            PROF = "#2874a6",
            ICS = "#21618c"
          )
          
          # Sort data by Below7 value
          data <- data %>%
            arrange(desc(Below7))
          
          ggplot(data) +
            geom_segment(aes(x = reorder(Milestone, Below7), 
                             xend = reorder(Milestone, Below7),
                             y = 0, yend = Below7, color = Category), 
                         size = 1.2) +
            geom_point(aes(x = reorder(Milestone, Below7), y = Below7, 
                           fill = Category),
                       size = 4, shape = 21, color = "white") +
            geom_text(aes(x = reorder(Milestone, Below7), y = Below7,
                          label = sprintf("%.1f%%", Below7)),
                      hjust = -0.2, size = 4) +  # Added percentage labels
            coord_flip() +
            scale_fill_manual(values = blue_palette) +
            scale_color_manual(values = blue_palette) +
            base_theme +
            labs(title = paste("Milestone Performance -", period_title),
                 y = "Percentage Below 7",
                 x = "Milestone") +
            scale_y_continuous(limits = c(0, max(data$Below7) * 1.2))  # Adjust y-axis for labels
        }
      })
      
      # Updated milestone plot
      output$spider_plot <- renderPlot({
        req(graduation_analysis())
        
        # Get data for all periods first (unfiltered)
        all_data <- data() %>%
          select(matches("^(PC|MK|SBP|PBL|PROF|ICS)")) %>%
          summarise(across(everything(), median)) %>%
          pivot_longer(everything(), 
                       names_to = "Milestone", 
                       values_to = "MedianScore") %>%
          pivot_wider(names_from = Milestone, 
                      values_from = MedianScore)
        
        # Get filtered data
        filtered_data <- graduation_analysis() %>%
          select(Milestone, MedianScore) %>%
          pivot_wider(names_from = Milestone, 
                      values_from = MedianScore)
        
        # Set up the plot data
        plot_data <- rbind(
          rep(9, ncol(filtered_data)),  # Max
          rep(1, ncol(filtered_data)),  # Min
          as.data.frame(filtered_data),  # Current selection
          as.data.frame(all_data)       # All periods
        )
        
        # Set up layout for plot and legend
        layout(matrix(c(1, 2), ncol = 2), widths = c(4, 1))
        
        # Draw radar chart
        par(mar = c(1, 1, 2, 1))
        radarchart(
          plot_data,
          pcol = c("#2C3E50", "#3498DB"),  # Colors for filtered and all data
          pfcol = c(scales::alpha("#2C3E50", 0.3), scales::alpha("#3498DB", 0.1)),
          plwd = c(2, 2),
          plty = c(1, 2),  # Solid line for filtered, dashed for all
          cglcol = "grey",
          cglty = 1,
          axislabcol = "grey",
          caxislabels = seq(1, 9, 2),
          title = if(input$period_filter == "All") 
            "All Years" 
          else 
            paste(input$period_filter, "vs All Years"),
          maxmin = TRUE,
          centerzero = FALSE,
          vlcex = 0.9,
          cex.main = 1.2
        )
        
        # Add legend
        par(mar = c(1, 0, 2, 1))
        plot.new()
        legend("center", 
               legend = c(
                 if(input$period_filter == "All") "All Years" else input$period_filter,
                 "All Years Median"
               ),
               col = c("#2C3E50", "#3498DB"),
               lty = c(1, 2),
               lwd = 2,
               bty = "n",
               cex = 0.8)
      })
      
      # Updated table with better sizing
      output$score_summary <- renderDT({
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
          ) %>%
          datatable(
            options = list(
              pageLength = 50,
              dom = 't',
              scrollY = "100%",
              scrollCollapse = TRUE,
              autoWidth = TRUE,
              scroller = TRUE,
              deferRender = TRUE
            ),
            rownames = FALSE,
            class = 'cell-border stripe compact',
            style = 'bootstrap',
            selection = 'none'
          ) %>%
          formatStyle(
            0:5,
            target = 'row',
            backgroundColor = 'white',
            color = '#2C3E50'
          )
      }, server = FALSE)
    }
  shinyApp(ui, server, ...)
}




