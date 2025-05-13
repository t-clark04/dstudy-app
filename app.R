# Load packages ---
library(shiny)
library(bslib)
library(tidyverse)
library(dplyr)
library(gtheory)
library(dtheory)
library(stringr)
library(DT)
library(plotly)

# User interface ----
ui <- page_navbar(
  theme = bs_theme(version = 5, preset = "yeti"),
  bg = "#008CBA",
  title = "Running a D-Study",
  sidebar = sidebar(
    helpText(
      "Run your own random p x t D-study from Generalizability Theory for a specified number of trials."),
    helpText(
      "Please upload a .csv or .txt file which includes a column called 'Person', a column called 'Trial',
      and one or more columns for metrics."),
    fileInput("file1", "Please choose a .csv or .txt file.", accept = c(".csv", ".txt")),
    selectInput(
      "var",
      "Select Variable(s) for Reliability Testing",
      NULL,
      multiple = TRUE,
      selectize = TRUE),
    numericInput(
      "from",
      "From (Starting # of Trials)",
      value = 1,
      min = 1),
    numericInput(
      "to",
      "To (Final # of Trials)",
      value = 10,
      min = 1),
    numericInput(
      "by",
      "By (Interval for Testing Trials)",
      value = 1,
      min = 1),
    numericInput(
      "rounded",
      "Number of Decimal Places in Outputted Values",
      value = 3,
      min = 1
    )
  ),
    card(
      card_header("D-Study Table"),
      DT::dataTableOutput("g_coefs")
    ),
    layout_columns(
    card(
      card_header("Reliability Plot"),
      plotlyOutput("plot")),
    card(
      card_header("Error Variance Plot"),
      plotlyOutput("plot2"))
    )
    
)

# Server logic ----
server <- function(input, output, session) {
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else if (str_sub(inFile$datapath, -3) == "csv") {
      data <- read.csv(inFile$datapath)
    }
    else if(str_sub(inFile$datapath, -3) == "txt") {
      data <- read.csv(inFile$datapath, sep = "")
    }
    return(data)
  })
  
  excl <- c("Person", "Trial")
  
  observe({
    updateSelectInput(session, "var",
                      label = "Select Variable(s) for Reliability Testing",
                      choices = colnames(myData())[!(colnames(myData()) %in% excl)],
                      selected = NULL)
    })
  
  output$g_coefs <- DT::renderDataTable({
    DT::datatable(
    if (length(input$var) == 0) {
      return(NULL)
      } else if (length(input$var) == 1) {
          final_df <- dtheory::dstudy(myData(), col.scores = input$var, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
          return(final_df)
      } else if (length(input$var) > 1) {
          final_df <- dtheory::dstudy(myData(), col.scores = input$var[1], from = input$from, to = input$to, by = input$by, rounded = input$rounded)
          for (elem in input$var[2:length(input$var)]) {
            intermed <- dtheory::dstudy(myData(), col.scores = elem, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
            final_df <- rbind(final_df, intermed)}
          return(final_df)
          }
    )
  })
  
  output$plot <- renderPlotly({
    if (length(input$var) == 0) {
      return(NULL)
    } else if (length(input$var) == 1) {
      new_df <- dtheory::dstudy(myData(), col.scores = input$var, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
      new_df$Metric <- rownames(new_df)
      new_df <- new_df %>%
        pivot_longer(cols = -Metric, names_to = "trial_number", values_to = "Phi Coefficient") %>% 
        separate(col = trial_number, into = c("n", "Number of Trials"), sep = " = ") %>%
        select(-"n")
      new_df$`Number of Trials` <- as.integer(new_df$`Number of Trials`)
      dplot <- ggplot(data = new_df, aes(x = `Number of Trials`, y = `Phi Coefficient`, color = Metric)) + 
        geom_point(size = 2.5) + 
        labs(x = "Number of Trials", y = "Phi Coefficient") + 
        scale_x_continuous(breaks = seq(input$from, input$to, input$by)) + 
        theme(text = element_text(family = "Open Sans", color = "black", size = 13)) + 
        guides(color = F)
      dplotly <- ggplotly(dplot, tooltip = c("x", "y", "Metric"))
      return(dplotly)
    } else if (length(input$var) > 1) {
      final_df <- dtheory::dstudy(myData(), col.scores = input$var[1], from = input$from, to = input$to, by = input$by, rounded = input$rounded)
      for (elem in input$var[2:length(input$var)]) {
        intermed <- dtheory::dstudy(myData(), col.scores = elem, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
        final_df <- rbind(final_df, intermed)}
      final_df$Metric <- rownames(final_df)
      new_df <-  final_df %>%
        pivot_longer(cols = -Metric, names_to = "trial_number", values_to = "Phi Coefficient") %>% 
        separate(col = trial_number, into = c("n", "Number of Trials"), sep = " = ") %>%
        select(-"n")
      new_df$`Number of Trials` <- as.integer(new_df$`Number of Trials`)
      dplot <- ggplot(data = new_df, aes(x = `Number of Trials`, y = `Phi Coefficient`, color = Metric, shape = Metric)) + 
        geom_point(size = 2.5) + 
        labs(x = "Number of Trials", y = "Phi Coefficient") + 
        scale_x_continuous(breaks = seq(input$from, input$to, input$by)) + 
        theme(text = element_text(family = "Open Sans", color = "black", size = 13)) + 
        guides(color = "none", shape = "none")
      dplotly <- ggplotly(dplot, tooltip = c("colour", "x", "y"))
      return(dplotly)
    }
  })
  
  output$plot2 <- renderPlotly({
    if (length(input$var) == 0) {
      return(NULL)
    } else if (length(input$var) == 1) {
      new_df <- dtheory::dstudy(myData(), col.scores = input$var, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
      new_df$Metric <- rownames(new_df)
      new_df <- new_df %>%
        pivot_longer(cols = -Metric, names_to = "trial_number", values_to = "Phi Coefficient") %>% 
        separate(col = trial_number, into = c("n", "Number of Trials"), sep = " = ") %>%
        select(-"n")
      new_df$`Number of Trials` <- as.integer(new_df$`Number of Trials`)
      new_df <- new_df %>%
        mutate(`Error Variance (%)` = (1 - `Phi Coefficient`))
      dplot2 <- ggplot(data = new_df, aes(x = `Number of Trials`, y = `Error Variance (%)`, color = Metric)) + 
        geom_point(size = 2.5) + 
        labs(x = "Number of Trials", y = "Error Variance (% of Total)") + 
        scale_x_continuous(breaks = seq(input$from, input$to, input$by)) + 
        theme(text = element_text(color = "black", size = 13)) + 
        guides(color = "none")
      dplotly2 <- ggplotly(dplot2, tooltip = c("x", "y", "Metric"))
      return(dplotly2)
    } else if (length(input$var) > 1) {
      final_df <- dtheory::dstudy(myData(), col.scores = input$var[1], from = input$from, to = input$to, by = input$by, rounded = input$rounded)
      for (elem in input$var[2:length(input$var)]) {
        intermed <- dtheory::dstudy(myData(), col.scores = elem, from = input$from, to = input$to, by = input$by, rounded = input$rounded)
        final_df <- rbind(final_df, intermed)}
      final_df$Metric <- rownames(final_df)
      new_df <-  final_df %>%
        pivot_longer(cols = -Metric, names_to = "trial_number", values_to = "Phi Coefficient") %>% 
        separate(col = trial_number, into = c("n", "Number of Trials"), sep = " = ") %>%
        select(-"n")
      new_df$`Number of Trials` <- as.integer(new_df$`Number of Trials`)
      new_df <- new_df %>%
        mutate(`Error Variance (%)` = (1 - `Phi Coefficient`))
      dplot2 <- ggplot(data = new_df, aes(x = `Number of Trials`, y = `Error Variance (%)`, color = Metric, shape = Metric)) + 
        geom_point(size = 2.5) + 
        labs(x = "Number of Trials", y = "Error Variance (% of Total)") + 
        scale_x_continuous(breaks = seq(input$from, input$to, input$by)) + 
        theme(text = element_text(family = "Open Sans", color = "black", size = 13)) + 
        guides(color = "none", shape = "none")
      dplotly2 <- ggplotly(dplot2, tooltip = c("colour", "x", "y"))
      return(dplotly2)
    }
  })
  
}

# Run the app
shinyApp(ui, server)
