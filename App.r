# --- Author: Anh Ha ----
# --- Email Address: aqh9586@rit.edu ---

# Load packages
library(shiny)
library(shinydashboard)
library(ggplot2)

# Load data
portfolio <- read.csv("Stock_Portfolio_Snapshot.csv", stringsAsFactors = F, header=T, na.strings=c(" ", "NA"))
monthly_performance <- read.csv("Monthly_Performance.csv", stringsAsFactors = F, header=T, na.strings=c("-", "NA"))
cumulative_gain <- read.csv("Portfolio_Cumulative_Gain.csv", stringsAsFactors = F, header=T, na.strings=c(" ", "NA"))

# Replace missing values in the data with 0
portfolio[is.na(portfolio)] <- 0
monthly_performance[is.na(monthly_performance)] <- 0
cumulative_gain[is.na(cumulative_gain)] <- 0

# DEFINE UI FOR APP

# Dashboard Header
header <- dashboardHeader(title = "Portfolio Dashboard")

# Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

# UI Components
# Create 4 information boxes: Position, Portfolio Cost, Cash on Hand and Current Market Value
frow1 <- fluidRow(
  valueBoxOutput("Position", width = 3),
  valueBoxOutput("PortfolioCost", width = 3),
  valueBoxOutput("CurrentMarketValue", width = 3),
  valueBoxOutput("CashOnHand", width = 3)
)

# Create Cumulative Gain Line Chart for Benchmark Comparison & Pie Chart for Asset Allocation
frow2 <- fluidRow(
          box(
            title = "Cumulative Gain",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            background = "light-blue",
            plotOutput("CumulativeGain")
          ),
          box(
           title = "Asset Allocation",
           solidHeader = TRUE,
           collapsible = TRUE,
           background = "light-blue",
           plotOutput("AssetAllocation")
         )
)

# Create Monthly Cumulative Gain Line Chart for Benchmark Comparison & Pie Chart for Equity Allocation
frow3 <- fluidRow(
         box(
           title = "Monthly Cumulative Gain",
           status = "primary",
           solidHeader = TRUE,
           collapsible = TRUE,
           background = "light-blue",
           plotOutput("MonthlyCumulativeGain")
         ),
         box(
           title = "Equity Allocation",
           solidHeader = TRUE,
           collapsible = TRUE,
           background = "light-blue",
           plotOutput("EquityAllocation")
         )
  )
  

# Combine three fluid row to make the dashboard body
body <- dashboardBody(frow1, frow2, frow3)

# Complete UI part of the dashboard
ui <- dashboardPage(title = 'RIT FMA Portfolio Dashboard 2017', header, sidebar, body, skin = "yellow")

# DEFINE SERVER 

# Define Server Logic ----
server <- function(input, output) {
  
  # Data manipulation to derive 4 ValueBoxOutPut
  # Calculate Position (How many Tickers we have in the portfolio?)
  total_position <- length(portfolio$TICKER) - 1
  
  # Calculate Portfolio Cost: SUM of TOTAL.COST column
  # Strip out $ sign and return numeric columns for Total Cost
  portfolio$TOTAL.COST <- as.numeric(gsub("[,$]", "", portfolio$TOTAL.COST))
  portfolio_cost <- sum(portfolio$TOTAL.COST)
  portfolio_cost <- paste("$", format(portfolio_cost, big.mark=",")) # Format it nicely again
  
  # Calculate Market Value: SUM if MKT..VALUE
  portfolio$MKT..VALUE <- as.numeric(gsub("[,$]", "", portfolio$MKT..VALUE))
  market_value = sum(portfolio$MKT..VALUE)
  market_value <- paste("$", format(market_value, big.mark=",")) # Format the number nicely again
  
  # Calculate Cash on Hand: Last Value of CASH+DIV col in Monthly Performance table
  cash_on_hand <- tail(monthly_performance$CASH.DIV, n =1 )
  
  
  # Create the valueBoxOutput content
  
  output$Position <- renderValueBox({
    
    valueBox(
       total_position,
       'Position',
       icon = icon("list",lib='glyphicon'),
       color = "purple")
  })
  
  
  output$PortfolioCost <- renderValueBox({
    
    valueBox(
      portfolio_cost,
      'Portfolio Cost',
      icon = icon("piggy-bank",lib='glyphicon'),
      color = "red")
    
  })
  
  
  output$CurrentMarketValue <- renderValueBox({
    
    valueBox(
      market_value,
      'Current Market Value',
      icon = icon("stats",lib='glyphicon'),
      color = "blue")
    
  })
  
  
  output$CashOnHand <- renderValueBox({
    
    valueBox(
      paste('$', format(cash_on_hand, big.mark=",")),
      'Cash on Hand',
      icon = icon("usd",lib='glyphicon'),
      color = "green")
    
  })
  
  
  # Create Cumulative Gain Line Chart
  
  # Clean data for the plot
  cumulative_gain$Portfolio <- as.numeric(gsub("%", "", cumulative_gain$Portfolio))
  cumulative_gain$S.P.500 <- as.numeric(gsub("%", "", cumulative_gain$S.P.500))
  
  # Create the plot
  output$CumulativeGain <- renderPlot({
    ggplot(cumulative_gain, aes(x=Date)) + 
      geom_line(aes(y=Portfolio, colour = "green", group=1)) + 
      geom_line(aes(y=S.P.500, colour = "yellow", group=1)) +
      labs(y = "Gain") +
      theme(
        axis.title.y=element_text(vjust=0.19),
        axis.title.x=element_text(vjust=0.19) ,
        legend.position = "bottom",
        legend.title=element_blank()) + 
      scale_color_discrete(labels = c("Portfolio", "S&P 500"))
    
  })
  
  # Create Monthly Cumulative Gain Line Chart
  cumulative_gain$Portfolio.Gain <- as.numeric(gsub("%", "", cumulative_gain$Portfolio.Gain))
  cumulative_gain$S.P.500.Gain <-  as.numeric(gsub("%", "", cumulative_gain$S.P.500.Gain))
  
  # Create line chart with those two variables
  output$MonthlyCumulativeGain <- renderPlot({ ggplot(cumulative_gain, aes(x=Date)) + 
    geom_line(aes(y=Portfolio.Gain, colour = "green", group=1)) + 
    geom_line(aes(y=S.P.500.Gain, colour = "yellow", group=1)) +
    labs(y = "Monthly Gain") +
    theme(
      axis.title.y=element_text(vjust=0.19),
      axis.title.x=element_text(vjust=0.19) ,
      legend.position = "bottom",
      legend.title=element_blank()) + 
    scale_color_discrete(labels = c("Portfolio", "S&P 500"))
})
  
  
  # Create 2 Pie Charts for Asset and Equity Allocation
  
  # Prep data for the Asset Allocation Pie Chart
  assets <- c("Equity", "Fixed Income", "Cash") 
  portfolio$MKT..VALUE <- as.numeric(gsub("[,$]", "", portfolio$MKT..VALUE))
  cash_on_hand <- as.numeric(gsub("[,$]", "", cash_on_hand))
  asset_data <- c(sum(portfolio$MKT..VALUE[1:37]),
                  sum(portfolio$MKT..VALUE[38:40]),
                  cash_on_hand)
  
  percentage <- round(asset_data/sum(asset_data)*100)
  assets <- paste(assets, percentage) # add percentage to the label
  assets <- paste(assets, "%", sep="") # add % to the label
  
  # Render the Asset Allocation Pie Chart
  output$AssetAllocation <- renderPlot({
    pie(asset_data, labels = assets, col=rainbow(length(sector_data)))
  })
  
  
  # Prep data for the Equity Allocation Pie Chart
  sectors <- c("Technology", "Defense", "Healthcare", "Financial", "Consumer", "Goods", "Service","REITS", "Energy")
  portfolio$MKT..VALUE <- as.numeric(gsub("[,$]", "", portfolio$MKT..VALUE))
  sector_data <- c(sum(portfolio$MKT..VALUE[1:7]),
                   portfolio$MKT..VALUE[8],
                   sum(portfolio$MKT..VALUE[9:16]),
                   sum(portfolio$MKT..VALUE[17:19]),
                   portfolio$MKT..VALUE[20],
                   sum(portfolio$MKT..VALUE[21:24]),
                   sum(portfolio$MKT..VALUE[25:28]),
                   sum(portfolio$MKT..VALUE[29:35]),
                   sum(portfolio$MKT..VALUE[36:37])
  )
  
  percentage <- round(sector_data/sum(sector_data)*100)
  sectors <- paste(sectors, percentage) # add percentage to the label
  sectors <- paste(sectors, "%", sep="") # add % to the label
  
  # Render the Equity Allocation Pie Chart
  output$EquityAllocation <- renderPlot({
    pie(sector_data, labels = sectors, col=rainbow(length(sector_data)))
  })
  
  
}

# Run the app
shinyApp(ui, server)