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
        )
        #,
        #fluidRow(
        #  box(title="Directory size history", plotOutput("history", width=500, height = 250))
        #)
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
  x <- subset(dat, dat$date==max(dat$date))
  x <- x[order(x$sizeKB, decreasing = TRUE),]
  # Get top-level dir
  level <- str_count(x$dir, "/")
  baseDir <- x[which(level == min(level)), "dir"]
  # Drop top level prefix
  baseDir
  x$dir <- sub(paste0(baseDir, "/"), "", x$dir)
  dat$dir <- sub(paste0(baseDir, "/"), "", dat$dir)
  # Get level 1 dirs
  xx <- x[which(level == min(level)+1), ]
  xx <- subset(xx, sizeTB>=1)
  otherKB <- x[x$dir==baseDir, "sizeKB"] - sum(xx$sizeKB)
  otherTB <- otherKB/1e9
  topDirs <- c(xx$dir[1:min(10, nrow(xx))], "Other")
  topTB <- c(xx$sizeTB, otherTB)
  
  # Get sizes of top-level dirs and 'other' for all datetimes
  topDat <- ddply(dat, .(date), function(x) {
    rownames(x) <- x$dir
    xx <- subset(x, dir %in% topDirs)
    head(xx)
    otherKB <- x[baseDir,"sizeKB"] - sum(xx$sizeKB)
    otherTB <- otherKB/1e9
    xx <- rbind(xx, 
                data.frame(date=xx$date[1], dir="Other", sizeKB=otherKB, sizeTB=otherTB, stringsAsFactors = FALSE)
    )
    xx
  })
  
  if (FALSE) {
    # Replace NAs with zeros. (Also converts dates to factors unfortunately)
    m <- melt(topDat, id.vars = c("date", "dir"), measure.vars = "sizeTB")
    w <- acast(m, dir ~ date)
    w[is.na(w)] <- 0
    topDat <- melt(w,varnames = c("dir", "date"), value.name = "sizeTB")
  }
  
  topDat$dir <- factor(topDat$dir, levels=topDirs)
  xmin <- min(topDat$date)
  xmax <- min(topDat$date) + 1.2 * (max(topDat$date) - min(topDat$date))
  #p <- ggplot(topDat, aes(date, sizeTB, fill=dir, group=dir)) + geom_area() +  aes(order = as.numeric(-sizeTB)) + xlim(xmin, xmax) + theme_bw()
  p <- ggplot(topDat, aes(date, sizeTB, fill=dir, group=dir)) + geom_area() + xlim(xmin, xmax) + theme_bw()
  cs <- cumsum(topTB)
  y <- rowMeans(cbind(c(0, cs[-length(cs)]), cs))
  pHistory <- p + annotate("text", x = max(topDat$date), y=y, label=topDirs, hjust=-0.1) + guides(fill=FALSE)
  
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
