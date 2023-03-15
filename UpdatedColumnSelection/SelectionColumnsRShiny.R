






####Here is my user interface  with the various button displayed when the app is run the first time
ui = navbarPage(tabPanel("Linear Regression",
                         dataTableOutput('mytable'),
                         sidebarLayout(
                           sidebarPanel(width=3, fileInput("file1", "Please choose a CSV file",
                                                           multiple = T,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain",
                                                                      ".csv")),
                                        tags$hr(),
                                        checkboxInput("header", "Header", TRUE),
                                        radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ","),
                                        radioButtons("quote", "Quote",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"'),
                                        tags$hr(),
                                        radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")
                                        
                           ),    ###Various Radio Buttons with one break in the middle
                           
                           mainPanel(
                             tableOutput("contents"),
                             actionButton("choice", "Define Regression Variables"),
                             selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                             uiOutput("dependent1"),
                             
                             verbatimTextOutput("regressionTab"), plotOutput("plotit")
                           )
                         ),   ####Displays of the outputs
                         
))
####The communication point
server = function(input, output, session) {
  #### a reactive statement can now be used as a function anywhere in the code
  mydf <- reactive({
    
    
    req(input$file1)  #### Makes sure that there values in our data 
    
    df = read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,        ###Data path of the file
                  quote = input$quote) ### Assigned from the user inputs
    
    if(input$disp == "head") {
      return(head(df))
    }                         ####Either display the whole data or part of it
    else {
      return(df)
    }
    
  })
  ###Creates our table in the code
  output$contents = renderTable({
    req(mydf())       ###Makes sure there are values there
    mydf()   #### run the code below and then 
  })
  
  # Code for allowing the user to select the variables/columns of interest
  info <- eventReactive(input$choice, {
    req(mydf()) 
    f <- mydf()   
    f
  })
  
  observeEvent(input$choice, {  ## to update only when you click on the actionButton
    req(info())
    updateSelectInput(session,"independent", "Please Select independent Variable(s):", choices = names(info()) )
  })
  
  
   #### allows you to select the choices  
  
  output$dependent1 = renderUI({
    req(mydf(),input$independent)
    radioButtons("dependent1", "Select a dependent Variable:",choices=names(mydf())[!names(mydf()) %in% input$independent])
  })
  
 
  #### Main code that runs the regression
  runRegression <- reactive({
    req(mydf(),input$independent,input$dependent1)
    lm(reformulate(input$independent, input$dependent1), data=mydf())
   
  })
  
  output$regressionTab = renderPrint({
    req(runRegression())
    if(!is.null(input$independent)){      ####Generates the coeffiecents for linear regression
     
      coefs <- coef(runRegression(), data= mydf())
      res_data<- data.frame(coef1=coefs[1],coef2=coefs[2])
      
      regressionmod <- res_data
      print(regressionmod)    ####Prints the coefficients of it 
  
     
  
      
      
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }    ####If you do not wish to select them
    
  }
  )
  
  plottingtime <- reactive({ 
     
   x <- c(mydf()[input$independent])   
   y <- c(mydf()[input$dependent1])   ####Conversion of columns to vectors
   
   x_num <- as.numeric(unlist(x))     #### Coerced the list to type double to avoid the error
                                        #### of not coercing to type double
   
   y_num <-  as.numeric(unlist(y))
   
   plot(x_num,y_num)                     ####Plot the functions
   abline(lm(y_num~x_num),col='red')    #### draws the regression line y over x 
  })
  

   output$plotit = renderPlot({plottingtime()})  #### Repeats the code above

    
  
  
  
  
 
  
 
  
    
  
  
 
  
}

shinyApp(ui, server) 