#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


  # Define UI for an application that calculates Linear Regression
  ui <- fluidPage(
    
    # Application title
    titlePanel("Let's see the Linear Regression Summary of your Data!"),
    
    # Allows User to take input of X, Y, and column files 
    sidebarLayout(
      sidebarPanel(  fileInput("X", "Input your X file (needs to have muliple columns)",
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
      ),
      
      fileInput("Y", "Input your Y file (needs to have only one column)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),
      ),
      
      fileInput("colNum", "Input your Column Numbers file( file needs to be file separated with a mimimum of two values and a max of four)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),       
                
      ),
      ) ,
      
      # Help show a summary of Linear Regression
      mainPanel( h1("Linear Regression"), h4("Lr: "),verbatimTextOutput("LinearRegressionSummary")))
                 
      )
    
  
  
  # Define server logic required to perform multiple Regression on 
  # columns of X and Y  and output the linear regression summary of the data
  ### 1. Takes in the reads the inputted files: x, y, and numcol 
  
  #### 2. combines x and y into a dataframe called Data 
  
  #### 3. Subsets the dataframe to find columns only contained in num column csv file
  #### 4. Does linear regression of the data 
  ####5. Prints linear regression 
  
  server <- function(input, output) {
   
    
    output$LinearRegressionSummary  <- renderPrint({


      Xfile  <- input$X
      Yfile <- input$Y
      Colnumfile <- input$colNum   ####These take in the inputted files
      
      allXColumns <-  read.csv(Xfile$datapath,header = T) #### takes in files with x columns
      YColumn <- data.frame(read.csv(Yfile$datapath,header = T)) ####Takes in files with the one column
      Data <- data.frame(allXColumns, YColumn)   ### Merges the muliple x columns and the one y column into one dataframe
      
      requestedColnum <-  data.frame(read.csv(Colnumfile$datapath,header = T))  ###Takes in the requested column file as a dataframe
      requestedColnumData <- Data[,requestedColnum$numcol]  #### obtains the x column that the user asked for by subsetting based on the index
     
      right_part_formula <- paste(colnames(requestedColnumData), collapse = " + ") #### Takes in the columns names separated by plus sign
      formula <- paste(colnames(YColumn)[1], " ~ ", right_part_formula) ### Combines the one y columns and the x columns
      print(formula)  #### Prints the name of the one the y column and the one x column the linear regresssion is being donw on
       
      LR.lm <- lm(formula = formula, data = Data) ###Generates the linear regression for the app
      print(summary(LR.lm)) ####prints the summary of our linear regresssion


# 
#        if(length(realcolData$numcol) == 2) {
#     LR.lm<-lm(Data[[length(Data)]]  ~ Data[[lst1[1]]]  + Data[[lst1[2]]], data= Data)
# 
#     print(colnames(subset))
#     print(summary(LR.lm))
# 
#       }
# 
#    if(length(realcolData$numcol) ==3) {
#        LR.lm<-lm(Data[[length(Data)]]  ~ Data[[lst1[1]]]  + Data[[lst1[2]]] + Data[[lst1[3]]], data= Data)
#      print(summary(LR.lm))
#      }
# 
#    if(length(realcolData$numcol) ==4) {
#       LR.lm<-  lm(Data[[length(Data)]]  ~ Data[[lst1[1]]]  + Data[[lst1[2]]] + Data[[lst1[3]]] + Data[[lst1[4]]], data= Data)
#        print(summary(LR.lm))
#           }
# 

    })
  }
      
      
      
      
      
      
      
      

    
    
    
  



# Run the application 
shinyApp(ui = ui, server = server)
