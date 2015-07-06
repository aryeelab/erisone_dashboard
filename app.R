## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(parsedate)
library(stringr)
library(plyr)
library(reshape2)

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

server <- function(input, output) { 
  
  dat <- read.delim("/data/aryee/du.txt", header=FALSE, stringsAsFactors = FALSE)
  colnames(dat) <- c("date", "dir", "sizeKB")
  dat$sizeTB <- dat$sizeKB / 1e9
  dat$date <- parse_date(dat$date)
  
  # Current biggest directories
  x <- subset(dat, dat$date==max(dat$date))
  x <- x[order(x$sizeKB, decreasing = TRUE),]
  # Drop top-level dir
  baseIdx <- which.min(str_count(x$dir, "/"))
  baseDir <- x[baseIdx,"dir"]
  x <- x[-baseIdx,]
  x$dir <- sub(paste0(baseDir, "/"), "", x$dir)
  x <- x[1:20,]
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
  
  
  output$bigDirs <- renderPlot({
    print(pBigDirs)
  })
  
  output$history <- renderPlot({
    print(pHistory)
  })

}

shinyApp(ui, server)
