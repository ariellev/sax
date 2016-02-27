require(shiny)
require(ggplot2)
require(knitr)
require(seewave)
require(Quandl)
require(rCharts)
require(TSclust)
require(stats)
require(dplyr)
require(wavelets)


apple <- read.table("apple.csv", stringsAsFactors = F)
apple$Date <- as.Date(apple$Date)
apple <- apple %>% arrange(Date)


# utility function to convert numeric vectors into data frames containing original dates
makeDF <- function(x, label, dates, type) {
  df <- as.data.frame(dates)
  names(df) <- c("Date")  
  df[label] <- x
  df$type <- type
  df
}

shinyServer(
  function(input,output, session) {  
    output$SAX <- renderText({

      # frame and alphabet size
      W <- input$window
      alpha <- input$alphabet

      print(alpha)
      data <- apple$Open
      normalize <- (data - mean(data)) / sd(data)

      # convert to SAX representation
      data_paa <- PAA(normalize, W)
      data_sax <- convert.to.SAX.symbol(data_paa, alpha)

      # converting to letters
      data_sax <- sapply(data_sax, function(x) LETTERS[x])
      data_sax
      })

    output$stockPlot <- renderChart2({

      # frame size
      W <- input$window

      data <- apple$Open

      apple_df <- apple[,1:2]
      apple_df$type <- "Stock"

      # calculating PAA over an unnormalized data for the graphical representation
      data_paa <- PAA(data, W)
      data_paa <- rep(data_paa, each=length(data)/W)

      # fixing length of p to align it with number of raw observations
      data_paa <- append(data_paa, rep(data_paa[length(data_paa)], length(data) - length(data_paa)))      

      paa_df <- makeDF(data_paa, "Open", apple$Date, "PAA")
      all <- rbind(apple_df, paa_df)

      # plotting    
      p <- nPlot(Open ~ Date, type = "lineChart", data = all, group = "type")
      p$xAxis(tickFormat =   "#!
              function(d) {return d3.time.format('%B')(new Date(d*1000*3600*24));}
            !#")

      p$chart(tooltipContent = "#! function(key, x, y){
          return '<h3>' + key + '</h3>' + 
          '<p>' + y + ' in ' + x + '</p>'
          } !#")

      p$xAxis(axisLabel = "Month")
      p$yAxis(axisLabel = "Price USD")


      p$chart(color = c('navy', '#e60000'), margin = list(left = 100, right = 100))
      p$addParams(width = 1600, pointSize = 0, lineWidth = 1)

      return(p)
    })
  }
)