library(shiny)
if (interactive()) {
  
  # Define UI for application that takes into csv files and does Linear Regression
  ui <- fluidPage(
    
    # Application title
    titlePanel("Linear Regression Shiny Application"),
    
    # Sidebar with two file input button to input files
    #and an action button to do linear regression
    
    sidebarLayout(
      sidebarPanel(   fileInput("file1", "Choose CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
      ),
      
      
      fileInput("file2", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      actionButton("button", "Create Linear Regression")
      
      
      ),
      
      # Show a plot of
      mainPanel(
        tableOutput("contents"),
        plotOutput("contents2")
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFilex <- input$file1
      inFiley <- input$file2
      
      if (is.null(inFilex)||is.null(inFiley))
        return(NULL)
      
      tmpx <-    read.csv(inFilex$datapath,header=T)
      tmpy <-    read.csv(inFiley$datapath,header=T)
      
      Data <- data.frame(tmpx,tmpy)
      
      coefs <- coef(lm(age ~ D0, data = Data))
      res_data<- data.frame(coef1=coefs[1],coef2=coefs[2])
      
      regressionmod <- res_data
      
    })
    
    
    output$contents2 <- renderPlot({
      inFilex <- input$file1
      inFiley <- input$file2
      
      tmpx <-    read.csv(inFilex$datapath,header=T)
      tmpy <-    read.csv(inFiley$datapath,header=T)
      
      Data <- data.frame(tmpx,tmpy)
      
      plot(Data$age,Data$D0)
      abline(lm(Data$age~Data$D0,data=Data),col='red')
      
    })
    
  }
  
  shinyApp(ui, server)
}
