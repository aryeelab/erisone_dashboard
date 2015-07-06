library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("bigDirs", height = 250)),
      box(plotOutput("history", height = 250))
    )
  )
)
