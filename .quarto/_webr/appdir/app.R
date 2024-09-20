library(shiny)
library(shinyMatrix)
library(bslib)
library(tibble)
library(tidyr)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----

m <- matrix(rep(NA, 60), 6, 10, dimnames = list(c(LETTERS[1:6]), sprintf("%s", seq(1:10))))

ui <- page_sidebar(
  sidebar = sidebar(
    title = "Estimated age (est_age) for each card by group",
    matrixInput("mat",
                value = m,
                inputClass = "numeric",
                rows = list(names = TRUE, extend = FALSE), 
                cols = list(names = TRUE, extend = FALSE)),
    fill = TRUE,
    fillable = TRUE,
    width = 600
  ),
  fillable = TRUE,
  fillable_mobile = TRUE,
  # Main panel for displaying outputs ----
  # Output: A tabset that combines three panels ----
  navset_card_underline(
    nav_panel("Data", tableOutput("data")),
    nav_panel("Variation", tableOutput("variation")),
    nav_panel("Bias", plotOutput("bias"))
  )
)


# Define server logic ----
server <- function(input, output, session) {
  rv <- reactiveValues(truthmat = t(matrix(rep(c(36L, 57L,  8L, 87L, 60L, 34L, 56L, 28L, 39L, 72L), 6), 10, 6, dimnames = list(sprintf("%s", seq(1:10)), c(LETTERS[1:6])))))
  
  dat <- reactive({
    tibble(group = factor(rep(LETTERS[1:6],10)), 
           card = factor(sort(rep(1:10, 6))),
           est_age = c(as.integer(input$mat)),
           true_age = c(rv$truthmat),
           error = c(as.integer(input$mat) - rv$truthmat))
  })
  
  output$data <- renderTable({
    dat() |> as.data.frame()
  }, rownames = FALSE, colnames = TRUE)
  
  output$variation <- renderTable({
    dat() |> 
      group_by(.data$group) |>
      summarise(mean_abs_err = mean(abs(.data$error))) |>
      as.data.frame()
  }, rownames = FALSE, colnames = TRUE)
  
  output$bias <- renderPlot({
    dat() |>
      ggplot() + 
      geom_boxplot(aes(y = .data$error, x = .data$card)) + 
      labs(title = "Bias in age estimates (error < 0 then 'look younger', error > 0 then 'look older')",
           y = "Error (est_age - true_age)",
           x = "Person (card)") +
      theme_classic(base_size = 18)
  })
}

# Create Shiny app ----
shinyApp(ui, server)
