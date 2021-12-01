library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidytext)
library(stringr)

ui <- fluidPage(
  title = "Analisis Sentimen Pengguna Twitter Terhadap Pandemi Covid19",
  headerPanel(""),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h2("Analisis Sentimen Pengguna Twitter Terhadap Pandemi Covid19")
      ),
      br(),
      sliderInput(inputId = "n_data",
                  label = "Banyak Data",
                  value = 60,
                  min = 20,
                  max = 100
      ),
      actionButton("refresh_button", "Reload"
      ),
      br(),
      br(),
      br(),
      br(),
      fluidRow(
        column(width = 1, plotlyOutput("d_plot", height = "220px", width = "300px"))
      ),
      fluidRow(
        column(width = 1, plotOutput("pie_plot", height = "210px", width = "300px"))
      )
    
    ),
    mainPanel(
      fluidRow(
        tabsetPanel(type = "tab",
                    id = "tabset",
                    tabPanel("All Data", dataTableOutput("table_all")),
                    tabPanel("Cleaning Data", dataTableOutput("cleaning_data")),
                    tabPanel("Hasil Sentimen", dataTableOutput("sentiment")),
        )
      )
    )
  )
)

#server
server <- function(input, output, session) {
  observeEvent(input$refresh_button, {
    source("Server.R")
  })

  d_view <- reactive({
    jumlah = input$n_data
    return(jumlah)
  })

  dp <- reactiveFileReader(1000, session, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict.csv", read.csv)
  result_predict <- reactiveFileReader(1000, session, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict_result.csv", read.csv)
  dp_clean <- reactiveFileReader(1000, session, "D:/Kuliah/Semester 5/Prak DS/pprojet/data_predict_clean.csv", read.csv)

  d_plot <- reactive({
    result_predict_count <- result_predict() %>%
      head(d_view()) %>%
      count(sentiment)
    result_predict_count %>%
      ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col() +
      geom_text(aes(label = n), color = "Black") +
      labs(
        x = "",
        y = "Jumlah"
      )
  })


  output$d_plot <- renderPlotly({
    ggplotly(d_plot())
  })

  output$pie_plot <- renderPlot({
    result_predict_count <- result_predict() %>%
      head(d_view()) %>%
      count(sentiment)
    prop <- round(result_predict_count$n * 100 / sum(result_predict_count$n), 1)
    ggplot(result_predict_count, aes(x = "", y = prop, fill = sentiment)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = cumsum(prop) - 0.99 * prop, label = paste0(prop, "%")), color = "Black", size = 4) +
      labs(
        x = "",
        y = "persentase"
      )
  })


  output$table_all <- renderDataTable({
    dp() %>% head(d_view()) %>% select(-sentiment)
  })

  output$cleaning_data <- renderDataTable({
    dp_clean() %>% head(d_view())
  })

  output$sentiment <- renderDataTable({
    result_predict() %>% head(d_view())
  })

}

#call shiny app
shinyApp(ui = ui, server = server, option = list(height = "500px"))
