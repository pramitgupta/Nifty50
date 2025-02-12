# Load necessary libraries
library(shiny)
library(quantmod)


# Load Nifty 50 companies list
nifty50_companies <- data.frame(
  Company = c(
    # Financial Services
    "HDFC Bank", "ICICI Bank", "State Bank of India (SBI)", "Axis Bank", "Kotak Mahindra Bank", "Bajaj Finance", "Bajaj Finserv", "HDFC Life Insurance",
    # Information Technology
    "Tata Consultancy Services (TCS)", "Infosys", "HCL Technologies", "Wipro", "Tech Mahindra",
    # Consumer Goods
    "Hindustan Unilever", "ITC Limited", "Asian Paints", "Titan Company", "Britannia Industries", "Nestle India",
    # Energy
    "Reliance Industries", "Oil & Natural Gas Corporation (ONGC)", "Power Grid Corporation", "NTPC Limited", "Coal India",
    # Automotive
    "Mahindra & Mahindra", "Maruti Suzuki", "Tata Motors", "Bajaj Auto", "Hero MotoCorp", "Eicher Motors",
    # Metals & Mining
    "Tata Steel", "JSW Steel", "Hindalco Industries",
    # Healthcare
    "Sun Pharmaceutical", "Dr. Reddy's Laboratories", "Cipla", "Divi's Laboratories", "Apollo Hospitals",
    # Cement
    "UltraTech Cement", "Grasim Industries",
    # Infrastructure & Construction
    "Larsen & Toubro (L&T)", "Adani Ports & SEZ",
    # Telecommunications
    "Bharti Airtel",
    # Others
    "Adani Enterprises", "UPL Limited", "SBI Life Insurance", "BPCL (Bharat Petroleum Corporation Limited)", "IndusInd Bank", "Hindustan Aeronautics Limited (HAL)", "Tata Consumer Products", "LTIMindtree"
  ),
  Symbol = c(
    # Financial Services
    "HDFCBANK.NS", "ICICIBANK.NS", "SBIN.NS", "AXISBANK.NS", "KOTAKBANK.NS", "BAJFINANCE.NS", "BAJAJFINSV.NS", "HDFCLIFE.NS",
    # Information Technology
    "TCS.NS", "INFY.NS", "HCLTECH.NS", "WIPRO.NS", "TECHM.NS",
    # Consumer Goods
    "HINDUNILVR.NS", "ITC.NS", "ASIANPAINT.NS", "TITAN.NS", "BRITANNIA.NS", "NESTLEIND.NS",
    # Energy
    "RELIANCE.NS", "ONGC.NS", "POWERGRID.NS", "NTPC.NS", "COALINDIA.NS",
    # Automotive
    "M&M.NS", "MARUTI.NS", "TATAMOTORS.NS", "BAJAJ-AUTO.NS", "HEROMOTOCO.NS", "EICHERMOT.NS",
    # Metals & Mining
    "TATASTEEL.NS", "JSWSTEEL.NS", "HINDALCO.NS",
    # Healthcare
    "SUNPHARMA.NS", "DRREDDY.NS", "CIPLA.NS", "DIVISLAB.NS", "APOLLOHOSP.NS",
    # Cement
    "ULTRACEMCO.NS", "GRASIM.NS",
    # Infrastructure & Construction
    "LT.NS", "ADANIPORTS.NS",
    # Telecommunications
    "BHARTIARTL.NS",
    # Others
    "ADANIENT.NS", "UPL.NS", "SBILIFE.NS", "BPCL.NS", "INDUSINDBK.NS", "HAL.NS", "TATACONSUM.NS", "LTIM.NS"
  )
)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Nifty50 live Stock Price Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select a Company:", 
                  choices = setNames(nifty50_companies$Symbol, nifty50_companies$Company)),
      dateInput("start_date", "Select Start Date:", value = Sys.Date() - 365),
      dateInput("end_date", "Select End Date:", value = Sys.Date()),
    ),
    
    mainPanel(
      plotOutput("stockPlot"),
      tableOutput("stockTable"),
    
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  stockData <- reactive({
    req(input$company, input$start_date, input$end_date)
    
    tryCatch({
      getSymbols(input$company, src = "yahoo", 
                 from = input$start_date, to = input$end_date, auto.assign = FALSE)
    }, error = function(e) {
      showNotification("Error fetching stock data. Please check the company symbol or date range.", type = "error")
      return(NULL)
    })
  })
  
  output$stockPlot <- renderPlot({
    data <- stockData()
    if (!is.null(data)) {
      chartSeries(data, name = paste("Stock Price for", input$company), theme = chartTheme("white"))
    }
  })
  
  output$stockTable <- renderTable({
    data <- stockData()
    if (!is.null(data)) {
      data <- as.data.frame(data)
      colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      data
    }
  })
  
  

}

# Run the application
shinyApp(ui = ui, server = server)
