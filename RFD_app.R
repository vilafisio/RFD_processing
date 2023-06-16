library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(signal)
library(stringr)
library(shinyjs)

options(shiny.maxRequestSize=1024^3)  # Set upload limit to 1GB

ui <- fluidPage(
  titlePanel("Force Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose txt files",
                multiple = TRUE,
                accept = c(".txt")),
      actionButton("calculate", "Calculate RFDs and Load Next File"),
      actionButton("save_results", "Save Results")
    ),
    mainPanel(
      plotlyOutput("forcePlot"),
      tableOutput("rfdTable"),
      textOutput("restMean"),
      downloadButton("downloadResults", "Download Results")
    )
  )
)

extract_info <- function(filename) {
  filename <- strsplit(filename, " ")[[1]][1]
  split_filename <- strsplit(filename, "_")[[1]]
  
  participant <- as.numeric(substr(split_filename[1], 4, nchar(split_filename[1])))
  task <- as.numeric(split_filename[3])
  session <- as.numeric(split_filename[4])
  trial <- as.numeric(split_filename[5])
  
  list(participant = participant, task = task, session = session, trial = trial)
}

get_impulse <- function(start, end, force_df) {
  if (start < min(force_df$Time) || end > max(force_df$Time)) {
    return(NA)
  }
  
  impulse <- sum(force_df$Force[force_df$Time >= start & force_df$Time <= end])
  return(impulse)
}

server <- function(input, output, session) {
  results_all <- reactiveVal(data.frame())
  click_time <- reactiveVal(0)
  file_index <- reactiveVal(1)
  
  data <- reactive({
    req(input$files)
    read_delim(input$files$datapath[[file_index()]], delim = "\t", skip = 8, show_col_types = FALSE)
  })
  
  observe({
    req(data())
    info <- extract_info(input$files$name[[file_index()]])
    
    force <- data()$Force*14.9
    force_df <- data.frame(Time = (1:length(force)) / 2000, Force = force)
    force_df <- force_df[complete.cases(force_df), ]
    
    Fs <- 2000  # Sampling
    Fc <- 10    # Define your desired cutoff frequency in Hz here
    
    Wn <- Fc / (Fs / 2)
    filter_order <- 4 
    b <- butter(filter_order, Wn)
    force_filtered <- filter(b, force_df$Force)
    
    force_df$Force <- force_filtered
    
    output$forcePlot <- renderPlotly({
      plot_ly(force_df, x = ~Time, y = ~Force, type = 'scatter', mode = 'lines') %>%
        add_trace(x = c(click_time(), click_time()), y = c(min(force_df$Force), max(force_df$Force)), mode = "lines", line = list(color = "red")) %>%
        layout(clickmode = "event+select")
    })
    
    observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      click_time(click_data$x)
    })
  })
  
  get_rate <- function(start, end, force_df) {
    if (start < min(force_df$Time) || end > max(force_df$Time)) {
      return(NA)
    }
    
    start_force <- approx(force_df$Time, force_df$Force, xout = start)$y
    end_force <- approx(force_df$Time, force_df$Force, xout = end)$y
    
    if (is.na(start_force) || is.na(end_force)) {
      return(NA)
    }
    
    print(paste("Start force:", start_force))
    print(paste("End force:", end_force))
    
    return((end_force - start_force) / (end - start))
  }
  
  observeEvent(input$calculate, {
    req(data())
    force_df <- data.frame(Time = (1:length(data()$Force)) / 2000, Force = data()$Force*14.9)
    force_df <- force_df[complete.cases(force_df), ]
    Wn <- 0.01
    filter_order <- 4
    b <- butter(filter_order, Wn)
    force_filtered <- filter(b, force_df$Force)
    force_df$Force <- force_filtered
    
    rest_end <- click_time()
    rest_start <- max(0, rest_end - 0.5)
    
    print(paste("Rest Start:", rest_start))
    print(paste("Rest End:", rest_end))
    
    rest_mean <- mean(force_df$Force[force_df$Time >= rest_start & force_df$Time <= rest_end])
    rest_sd <- sd(force_df$Force[force_df$Time >= rest_start & force_df$Time <= rest_end])  # compute the standard deviation
    force_df$Force <- force_df$Force - rest_mean
    peak_force <- max(force_df$Force, na.rm = TRUE)
    
    print(paste("Peak Force:", peak_force))
    
    output$restMean <- renderText({  
      paste("Resting mean force:", rest_mean)
    })
    
    observe({
      # If all files have been processed, disable the 'calculate' button
      if (file_index() >= length(input$files$datapath)) {
        shinyjs::disable("calculate")
      } else {
        shinyjs::enable("calculate")
      }
    })
    
    onset_time <- min(force_df$Time[force_df$Time > rest_end & force_df$Force > 0.1], na.rm = TRUE)
    
    print(paste("Onset Time:", onset_time))
    
    rate_0_50 <- get_rate(onset_time, onset_time + 0.05, force_df)
    rate_0_50 <- get_rate(onset_time, onset_time + 0.05, force_df)
    rate_50_100 <- get_rate(onset_time + 0.05, onset_time + 0.1, force_df)
    rate_100_200 <- get_rate(onset_time + 0.1, onset_time + 0.2, force_df)
    
    impulse_0_50 <- get_impulse(onset_time, onset_time + 0.05, force_df)
    impulse_50_100 <- get_impulse(onset_time + 0.05, onset_time + 0.1, force_df)
    impulse_100_200 <- get_impulse(onset_time + 0.1, onset_time + 0.2, force_df)
    
    info <- extract_info(input$files$name[[file_index()]])
    
    results <- data.frame(Participant = info$participant, Task = info$task, Session = info$session, Trial = info$trial, 
                          Onset_Time = onset_time-rest_end,
                          Rate_0_50 = rate_0_50, Rate_50_100 = rate_50_100, Rate_100_200 = rate_100_200,
                          Impulse_0_50 = impulse_0_50, Impulse_50_100 = impulse_50_100, Impulse_100_200 = impulse_100_200, 
                          Peak_Force=peak_force)
    
    
    results_all(rbind(results_all(), results))
    
    output$rfdTable <- renderTable({
      results_all()
    })
    
    if (file_index() < length(input$files$datapath)) {
      file_index(file_index() + 1)
    } else {
      shinyjs::disable("calculate")
      shinyjs::enable("save_results")
    }
  })
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      "RFD_results.csv"
    },
    content = function(file) {
      write.csv(results_all(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
