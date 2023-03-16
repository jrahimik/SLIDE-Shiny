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
      numericInput("col1", "Select a column (Make sure to stay in the correct index)", character(0)),
      numericInput("col2", "Select a column (Make sure to stay in correct index)" , character(0))
     ####New input to take in the column values for both x and y
      
      
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
      inFiley <- input$file2    #####Takes in the input files into a dataframe
      
      if (is.null(inFilex)||is.null(inFiley))
        return(NULL)
      
      tmpx <-    read.csv(inFilex$datapath,header = T) #### takes in datapath with crucial information
      tmpy <-    read.csv(inFiley$datapath,header=T)
      
      Data <- data.frame(tmpx,tmpy)
      y <- Data[input$col2]
      x <- Data[input$col1]  #### x and y takes in the data 
      
      coefs <- coef(lm(unlist(y)~ unlist(x) , data = Data))  ##### getting the coefficents of the data
      res_data<- data.frame(coef1=coefs[1],coef2=coefs[2])
      
      regressionmod <- paste0(res_data)   #### paste helps us the linear regression
     
    
      
    })
    
    ##### generating our plot
    output$contents2 <- renderPlot({
      inFilex <- input$file1
      inFiley <- input$file2       ####Takes in the input again
      
      tmpx <-    read.csv(inFilex$datapath,header = T)  ####
      tmpy <-    read.csv(inFiley$datapath,header=T)
      
      Data <- data.frame(tmpx,tmpy)   ####Generates the data
      
      y1 <- Data[input$col2]   ####Takes in input values
      x1 <- Data[input$col1]
      
      plot(unlist(y1), unlist(x1))     ####Plots them and do the regression line
      abline(lm(unlist(y1)~ unlist(x1),data=Data),col='red')
      
    })
    
  }
  
  shinyApp(ui, server)
}

