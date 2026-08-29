# ============================================================
# CS5803 Data Visualisation
# R Shiny Second Implementation
# Student: Mohammad Ahmad Author
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", dependencies = TRUE)
}

library(shiny)

data_file <- "Final_DV_Dataset.csv"

if (!file.exists(data_file)) {
  stop(
    paste(
      "Final_DV_Dataset.csv not found.",
      "Place app.R and Final_DV_Dataset.csv in the same folder.",
      "Current working directory:",
      getwd()
    )
  )
}

raw_data <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(raw_data) <- trimws(names(raw_data))

find_col <- function(possible_names, data_names) {
  match_name <- possible_names[possible_names %in% data_names]
  if (length(match_name) == 0) return(NA)
  match_name[1]
}

month_col <- find_col(c("Month_Label", "Month Label", "Month"), names(raw_data))
force_col <- find_col(c("Police_Force", "Police Force", "Reported by"), names(raw_data))
lsoa_col  <- find_col(c("LSOA name", "LSOA_Name", "LSOA.name"), names(raw_data))
type_col  <- find_col(c("Crime type", "Crime_type", "Crime.type"), names(raw_data))
group_col <- find_col(c("Crime_Group", "Crime Group", "Crime.Group"), names(raw_data))
count_col <- find_col(c("Crime_Count", "Crime Count", "Crime.Count"), names(raw_data))
lat_col   <- find_col(c("Latitude"), names(raw_data))
lon_col   <- find_col(c("Longitude"), names(raw_data))

missing <- c()

if (is.na(month_col)) missing <- c(missing, "Month_Label")
if (is.na(force_col)) missing <- c(missing, "Police_Force")
if (is.na(lsoa_col))  missing <- c(missing, "LSOA name")
if (is.na(type_col))  missing <- c(missing, "Crime type")
if (is.na(group_col)) missing <- c(missing, "Crime_Group")
if (is.na(count_col)) missing <- c(missing, "Crime_Count")
if (is.na(lat_col))   missing <- c(missing, "Latitude")
if (is.na(lon_col))   missing <- c(missing, "Longitude")

if (length(missing) > 0) {
  stop(paste("Missing required columns:", paste(missing, collapse = ", ")))
}

crime <- data.frame(
  Month_Label  = raw_data[[month_col]],
  Police_Force = raw_data[[force_col]],
  LSOA_Name    = raw_data[[lsoa_col]],
  Crime_Type   = raw_data[[type_col]],
  Crime_Group  = raw_data[[group_col]],
  Crime_Count  = as.numeric(raw_data[[count_col]]),
  Latitude     = as.numeric(raw_data[[lat_col]]),
  Longitude    = as.numeric(raw_data[[lon_col]]),
  stringsAsFactors = FALSE
)

crime <- crime[
  !is.na(crime$Month_Label) &
    !is.na(crime$Police_Force) &
    !is.na(crime$LSOA_Name) &
    !is.na(crime$Crime_Type) &
    !is.na(crime$Crime_Group) &
    !is.na(crime$Crime_Count) &
    !is.na(crime$Latitude) &
    !is.na(crime$Longitude),
]

if (nrow(crime) == 0) {
  stop("Dataset loaded, but no valid rows remain after cleaning.")
}

comma_number <- function(x) {
  format(round(x, 0), big.mark = ",", scientific = FALSE)
}

safe_unique <- function(x) {
  sort(unique(x[!is.na(x)]))
}

no_data_plot <- function(message = "No data available for the selected filters.") {
  plot.new()
  text(0.5, 0.5, message, cex = 0.9)
}

group_sum <- function(data, group_col) {
  result <- aggregate(
    data$Crime_Count,
    by = list(data[[group_col]]),
    FUN = sum,
    na.rm = TRUE
  )
  names(result) <- c(group_col, "Total_Crime")
  result[order(result$Total_Crime, decreasing = TRUE), ]
}

month_choices <- c("(All)", safe_unique(crime$Month_Label))
force_choices <- c("(All)", safe_unique(crime$Police_Force))
group_choices <- c("(All)", safe_unique(crime$Crime_Group))
type_choices  <- c("(All)", safe_unique(crime$Crime_Type))

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background-color: #f5f6f8;
        font-size: 12px;
      }

      .container-fluid {
        max-width: 1500px;
        padding-left: 12px;
        padding-right: 12px;
      }

      .main-title {
        background-color: #1f2937;
        color: white;
        padding: 6px 12px;
        border-radius: 8px;
        margin-bottom: 6px;
      }

      .main-title h2 {
        margin-top: 0px;
        margin-bottom: 2px;
        font-size: 22px;
        font-weight: 700;
      }

      .main-title p {
        margin-bottom: 0px;
        font-size: 12px;
      }

      .kpi-box {
        background-color: white;
        border-radius: 8px;
        padding: 5px;
        margin-bottom: 6px;
        text-align: center;
        box-shadow: 0 1px 4px rgba(0,0,0,0.12);
      }

      .kpi-number {
        font-size: 18px;
        font-weight: bold;
        color: #1f2937;
      }

      .kpi-label {
        font-size: 11px;
        color: #555;
      }

      .chart-box {
        background-color: white;
        border-radius: 8px;
        padding: 5px;
        margin-bottom: 6px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.12);
      }

      .note-box {
        background-color: #eef2ff;
        border-left: 4px solid #1f2937;
        padding: 6px;
        margin-top: 6px;
        font-size: 11px;
      }

      .form-group {
        margin-bottom: 6px;
      }

      .control-label {
        font-size: 11px;
        font-weight: bold;
      }

      select {
        font-size: 11px;
      }

      h4 {
        font-size: 16px;
        margin-top: 6px;
        margin-bottom: 8px;
      }
    "))
  ),
  
  div(
    class = "main-title",
    h2("Crime Pattern Analysis Dashboard"),
    p("R Shiny second implementation of the Tableau dashboard using base R and reactive filters.")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Filters"),
      
      selectInput(
        inputId = "month_filter",
        label = "Month",
        choices = month_choices,
        selected = "(All)"
      ),
      
      selectInput(
        inputId = "force_filter",
        label = "Police Force",
        choices = force_choices,
        selected = "(All)"
      ),
      
      selectInput(
        inputId = "group_filter",
        label = "Crime Group",
        choices = group_choices,
        selected = "(All)"
      ),
      
      selectInput(
        inputId = "type_filter",
        label = "Crime Type",
        choices = type_choices,
        selected = "(All)"
      ),
      
      sliderInput(
        inputId = "top_n",
        label = "Top LSOA Areas",
        min = 5,
        max = 15,
        value = 9,
        step = 1
      ),
      
      div(
        class = "note-box",
        "Filters update the charts reactively using the same dataset structure as the Tableau dashboard."
      )
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(
          width = 4,
          div(
            class = "kpi-box",
            div(class = "kpi-number", textOutput("kpi_total")),
            div(class = "kpi-label", "Total Crime Count")
          )
        ),
        column(
          width = 4,
          div(
            class = "kpi-box",
            div(class = "kpi-number", textOutput("kpi_lsoa")),
            div(class = "kpi-label", "Unique LSOA Areas")
          )
        ),
        column(
          width = 4,
          div(
            class = "kpi-box",
            div(class = "kpi-number", textOutput("kpi_types")),
            div(class = "kpi-label", "Unique Crime Types")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(class = "chart-box", plotOutput("police_plot", height = "145px"))
        ),
        column(
          width = 6,
          div(class = "chart-box", plotOutput("trend_plot", height = "145px"))
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(class = "chart-box", plotOutput("lsoa_plot", height = "165px"))
        ),
        column(
          width = 6,
          div(class = "chart-box", plotOutput("group_plot", height = "165px"))
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          div(class = "chart-box", plotOutput("hotspot_plot", height = "185px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    df <- crime
    
    if (!is.null(input$month_filter) && input$month_filter != "(All)") {
      df <- df[df$Month_Label == input$month_filter, ]
    }
    
    if (!is.null(input$force_filter) && input$force_filter != "(All)") {
      df <- df[df$Police_Force == input$force_filter, ]
    }
    
    if (!is.null(input$group_filter) && input$group_filter != "(All)") {
      df <- df[df$Crime_Group == input$group_filter, ]
    }
    
    if (!is.null(input$type_filter) && input$type_filter != "(All)") {
      df <- df[df$Crime_Type == input$type_filter, ]
    }
    
    df
  })
  
  output$kpi_total <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    comma_number(sum(df$Crime_Count, na.rm = TRUE))
  })
  
  output$kpi_lsoa <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    comma_number(length(unique(df$LSOA_Name)))
  })
  
  output$kpi_types <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0")
    comma_number(length(unique(df$Crime_Type)))
  })
  
  output$police_plot <- renderPlot({
    
    df <- filtered_data()
    if (nrow(df) == 0) return(no_data_plot())
    
    summary_df <- group_sum(df, "Police_Force")
    summary_df <- summary_df[order(summary_df$Total_Crime), ]
    
    par(mar = c(3, 8, 2, 1))
    
    barplot(
      summary_df$Total_Crime,
      names.arg = summary_df$Police_Force,
      horiz = TRUE,
      las = 1,
      col = "#4c78a8",
      border = NA,
      main = "Crime Volume by Police Force",
      xlab = "Total Crime Count",
      cex.names = 0.7,
      cex.main = 0.85,
      cex.lab = 0.7,
      cex.axis = 0.7
    )
  })
  
  output$trend_plot <- renderPlot({
    
    df <- filtered_data()
    if (nrow(df) == 0) return(no_data_plot())
    
    trend <- aggregate(
      df$Crime_Count,
      by = list(Month_Label = df$Month_Label, Police_Force = df$Police_Force),
      FUN = sum,
      na.rm = TRUE
    )
    
    names(trend)[3] <- "Total_Crime"
    
    months <- sort(unique(trend$Month_Label))
    forces <- sort(unique(trend$Police_Force))
    
    if (length(months) == 0 || length(forces) == 0) return(no_data_plot())
    
    y_max <- max(trend$Total_Crime, na.rm = TRUE)
    colours <- c("#4c78a8", "#f58518", "#e45756", "#72b7b2", "#54a24b")
    colours <- colours[seq_along(forces)]
    
    par(mar = c(4, 4, 2, 1))
    
    plot(
      seq_along(months),
      rep(NA, length(months)),
      ylim = c(0, y_max * 1.1),
      xaxt = "n",
      xlab = "Month",
      ylab = "Total Crime Count",
      main = "Monthly Crime Trend by Police Force",
      cex.main = 0.85,
      cex.lab = 0.7,
      cex.axis = 0.7
    )
    
    axis(1, at = seq_along(months), labels = months, las = 2, cex.axis = 0.6)
    
    for (i in seq_along(forces)) {
      force_data <- trend[trend$Police_Force == forces[i], ]
      y_values <- rep(NA, length(months))
      y_values[match(force_data$Month_Label, months)] <- force_data$Total_Crime
      
      lines(
        seq_along(months),
        y_values,
        type = "o",
        lwd = 2,
        col = colours[i],
        pch = 16
      )
    }
    
    legend(
      "topright",
      legend = forces,
      col = colours,
      lwd = 2,
      pch = 16,
      bty = "n",
      cex = 0.55
    )
  })
  
  output$lsoa_plot <- renderPlot({
    
    df <- filtered_data()
    if (nrow(df) == 0) return(no_data_plot())
    
    summary_df <- group_sum(df, "LSOA_Name")
    summary_df <- head(summary_df, input$top_n)
    summary_df <- summary_df[order(summary_df$Total_Crime), ]
    
    par(mar = c(3, 9, 2, 1))
    
    barplot(
      summary_df$Total_Crime,
      names.arg = summary_df$LSOA_Name,
      horiz = TRUE,
      las = 1,
      col = "#59a14f",
      border = NA,
      main = paste("Top", input$top_n, "LSOA Areas by Crime Volume"),
      xlab = "Total Crime Count",
      cex.names = 0.62,
      cex.main = 0.85,
      cex.lab = 0.7,
      cex.axis = 0.7
    )
  })
  
  output$group_plot <- renderPlot({
    
    df <- filtered_data()
    if (nrow(df) == 0) return(no_data_plot())
    
    summary_df <- group_sum(df, "Crime_Group")
    summary_df <- summary_df[order(summary_df$Total_Crime), ]
    
    group_colours <- c(
      "Acquisitive crime" = "#4c78a8",
      "High harm crime" = "#e45756",
      "Public order / ASB" = "#f58518",
      "Other crime" = "#9d9d9d"
    )
    
    colours <- group_colours[summary_df$Crime_Group]
    colours[is.na(colours)] <- "#7f7f7f"
    
    par(mar = c(3, 8, 2, 1))
    
    barplot(
      summary_df$Total_Crime,
      names.arg = summary_df$Crime_Group,
      horiz = TRUE,
      las = 1,
      col = colours,
      border = NA,
      main = "Distribution of Crime Groups",
      xlab = "Total Crime Count",
      cex.names = 0.68,
      cex.main = 0.85,
      cex.lab = 0.7,
      cex.axis = 0.7
    )
  })
  
  output$hotspot_plot <- renderPlot({
    
    df <- filtered_data()
    if (nrow(df) == 0) return(no_data_plot())
    
    spatial_mean <- aggregate(
      cbind(Latitude, Longitude) ~ LSOA_Name,
      data = df,
      FUN = mean
    )
    
    spatial_count <- aggregate(
      df$Crime_Count,
      by = list(LSOA_Name = df$LSOA_Name),
      FUN = sum,
      na.rm = TRUE
    )
    
    names(spatial_count)[2] <- "Total_Crime"
    
    spatial <- merge(spatial_mean, spatial_count, by = "LSOA_Name", all.x = TRUE)
    
    spatial <- spatial[
      !is.na(spatial$Longitude) &
        !is.na(spatial$Latitude) &
        !is.na(spatial$Total_Crime),
    ]
    
    if (nrow(spatial) == 0) return(no_data_plot())
    
    size_values <- sqrt(spatial$Total_Crime)
    size_values <- 1 + 6 * (size_values - min(size_values)) /
      (max(size_values) - min(size_values) + 0.0001)
    
    par(mar = c(3, 4, 2, 1))
    
    plot(
      spatial$Longitude,
      spatial$Latitude,
      pch = 16,
      cex = size_values,
      col = rgb(0.1, 0.3, 0.8, 0.30),
      xlab = "Longitude",
      ylab = "Latitude",
      main = "Spatial Distribution of Crime Hotspots",
      cex.main = 0.85,
      cex.lab = 0.7,
      cex.axis = 0.7
    )
    
    grid()
    
    legend(
      "bottomleft",
      legend = "Larger circles indicate higher recorded crime concentration",
      pch = 16,
      col = rgb(0.1, 0.3, 0.8, 0.30),
      bty = "n",
      cex = 0.6
    )
  })
}

shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  launch.browser = TRUE
)
