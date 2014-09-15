library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Count data models for zero-inflated data (generated from normal and binomial processes)"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    sliderInput("n", "Sample size (exponentiated):",
                min=4, max=9, value=4, step=1),
    sliderInput("th", "High number means less zero",
                min=-4, max=4, value=0, step=2)
      ),
      mainPanel(plotOutput("plot1")
                )
      ))
