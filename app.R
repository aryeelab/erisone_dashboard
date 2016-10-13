## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(parsedate)
library(stringr)
library(plyr)
library(reshape2)

ui <- dashboardPage(
  dashboardHeader(title = "ERISone stats"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Storage", tabName = "storage", icon = icon("bar-chart"))
      #menuItem("CID", tabName = "cid", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "storage",
        h2(textOutput("heading")),    
        h3(textOutput("timestamp")),
        fluidRow(
          box(title="Biggest directories", plotOutput("bigDirs", width=700, height = 600))
        ),
        fluidRow(
          box(title="Directory size history", plotOutput("history", width=500, height = 250))
        )
      ),
      tabItem(tabName = "cid",
              fluidRow(
              )
      )
    )
  )
)



#system("cp /data/aryee/du.txt du_example.txt")

dat <- read.delim("du.txt", header=FALSE, stringsAsFactors = FALSE)
colnames(dat) <- c("date", "dir", "sizeKB")
dat$sizeTB <- dat$sizeKB * 1024 / 2^40
dat$date <- parse_date(dat$date)


server <- function(input, output) { 
  
  # Current biggest directories
  x <- subset(dat, dat$date==max(dat$date))
  x <- x[order(x$sizeKB, decreasing = TRUE),]
  # Drop top-level dir
  baseIdx <- which.min(str_count(x$dir, "/"))
  baseDir <- x[baseIdx,"dir"]
  baseDirTB <- round(x[baseIdx, "sizeTB"], 1)
  x <- x[-baseIdx,]
  x$dir <- sub(paste0(baseDir, "/"), "", x$dir)
  x <- x[1:40,]
  #curBiggestDirs <- x$dir
  x$dir <- factor(x$dir, levels=rev(x$dir))
  pBigDirs <- ggplot(x, aes(dir, sizeTB)) + geom_bar(stat="identity") + xlab("") + ylab("Size (TB)") + coord_flip() + theme_bw()
  
  
  
  # Directory size history
  ## Get directories to plot (Current biggest top-level dirs)
  x <- subset(dat, dir==baseDir)
  pHistory <- ggplot(x, aes(date, sizeTB)) + geom_area(fill="blue") + theme_bw()

  output$heading <- renderText({paste0(baseDir, "  (", baseDirTB, "TB)")})
  
  output$timestamp <- renderText({paste(as.character(max(dat$date)), "UTC")})
  
  output$bigDirs <- renderPlot({
    print(pBigDirs)
  })
  
  output$history <- renderPlot({
    print(pHistory)
  })

}

shinyApp(ui, server)
