
R version 4.2.3 (2023-03-15 ucrt) -- "Shortstop Beagle"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> 
> # Define server logic required to draw a histogram
> server <- function(input, output) {
+   
+   
+   
+   
+   
+   
+   ####Create a reactive statement that does the essential regression
+   ###
+   Er <- reactive({
+     inFilex <- input$X
+     inFiley <- input$Y  
+     yamule  <- input$yamul
+     
+     
+     if (is.null(inFilex)||is.null(inFiley)  )
+       return(NULL)
+     
+     tmpx <-    read.csv(inFilex$datapath,header=T)
+     tmpy <-    read.csv(inFiley$datapath,header=T)
+     Data <- data.frame(tmpx, tmpy) 
+     yamul <- data.frame(read.csv(yamule$datapath,header=T))
+     VectorOfInputs <- as.Vector(yamul$inputs)
+     print(vectorOfInputs)
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+   })
+   
+   
+ 
+     output$distPlot <- renderText({
+       
+      
+       Er()
+       
+      
+       
+       
+        
+       
+     })
+ }
> 
> # Run the application 
> shinyApp(ui = ui, server = server)
Error in shinyApp(ui = ui, server = server) : 
  could not find function "shinyApp"
> 
> 
> # Define server logic required to draw a histogram
> server <- function(input, output) {
+   
+   
+   
+   
+   
+   
+   ####Create a reactive statement that does the essential regression
+   ###
+   Er <- reactive({
+     inFilex <- input$X
+     inFiley <- input$Y  
+     yamule  <- input$yamul
+     
+     
+     if (is.null(inFilex)||is.null(inFiley)  )
+       return(NULL)
+     
+     tmpx <-    read.csv(inFilex$datapath,header=T)
+     tmpy <-    read.csv(inFiley$datapath,header=T)
+     Data <- data.frame(tmpx, tmpy) 
+     yamul <- data.frame(read.csv(yamule$datapath,header=T))
+     VectorOfInputs <- as.Vector(yamul$inputs)
+     print(vectorOfInputs)
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+   })
+   
+   
+ 
+     output$distPlot <- renderText({
+       
+      
+       Er()
+       
+      
+       
+       
+        
+       
+     })
+ }
> 
> # Run the application 
> shlibrary(shiny)inyApp(ui = ui, server = server)
Error: unexpected symbol in "shlibrary(shiny)inyApp"
> install.packages("gtools", quiet= TRUE)
--- Please select a CRAN mirror for use in this session ---
package ‘gtools’ successfully unpacked and MD5 sums checked
Warning: cannot remove prior installation of package ‘gtools’
Warning: restored ‘gtools’
Warning message:
In file.copy(savedcopy, lib, recursive = TRUE) :
  problem copying C:\Users\DellUser\AppData\Local\R\win-library\4.2\00LOCK\gtools\libs\x64\gtools.dll to C:\Users\DellUser\AppData\Local\R\win-library\4.2\gtools\libs\x64\gtools.dll: Permission denied
> source("Tuning.R")
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'Tuning.R': No such file or directory
> #source("EstNonpure.R")
> #source("EstPure.R")
> #source("Utilities.R")
> #source("EstOmega.R")
> #source("ER-inference.R")
> #source("Est_beta_dz.R")
> #library(MASS)
> #library(shiny)
> 
> # Define UI for application that draws a histogram
> ui <- fluidPage(
+ 
+     # Application title
+     titlePanel("Essential Regression"),
+ 
+     # Sidebar with a slider input for number of bins 
+     sidebarLayout(
+         sidebarPanel(
+         
+         fileInput("Y", "Input your Y file (needs to have only one column)",
+                   accept = c(
+                     "text/csv",
+                     "text/comma-separated-values,text/plain",
+                     ".csv")
+         ),
+         
+         fileInput("X", "Input your X file (needs to have muliple columns)",
+                  accept = c(
+                    "text/csv",
+                    "text/comma-separated-values,text/plain",
+                    ".csv")
+         ),
+         
+         
+         numericInput(
+           "lbf",
+              " Enter your  lbf value",
+           0,
+           min = 0,
+           max = 1),
+         numericInput(
+           "delta",
+           " Enter your delta value",
+           0,
+           min = 0,
+           max = 1
+   
+         
+         ),
+         
+         fileInput("yaml", "Input your Yamul File",
+                   accept = c(
+                     "text/csv",
+                     "text/comma-separated-values,text/plain",
+                     ".csv")
+         
+         
+         ),
+ 
+         # Show a plot of the generated distribution
+         mainPanel( textOutput("ER")
+            
+         )
+         )
+     )
+ )
Error in fluidPage(titlePanel("Essential Regression"), sidebarLayout(sidebarPanel(fileInput("Y",  : 
  could not find function "fluidPage"
>     
>     
>   
> 
> 
> # Define server logic required to draw a histogram
> server <- function(input, output) {
+   
+   
+   
+   
+   
+   
+   ####Create a reactive statement that does the essential regression
+   ###
+   Er <- reactive({
+     inFilex <- input$X
+     inFiley <- input$Y  
+     yamule  <- input$yamul
+     
+     
+     if (is.null(inFilex)||is.null(inFiley)  )
+       return(NULL)
+     
+     tmpx <-    read.csv(inFilex$datapath,header=T)
+     tmpy <-    read.csv(inFiley$datapath,header=T)
+     Data <- data.frame(tmpx, tmpy) 
+     yamul <- data.frame(read.csv(yamule$datapath,header=T))
+     VectorOfInputs <- as.Vector(yamul$inputs)
+     print(vectorOfInputs)
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+   })
+   
+   
+ 
+     output$distPlot <- renderText({
+       
+      
+       Er()
+       
+      
+       
+       
+        
+       
+     })
+ }
> 
> # Run the application 
> shinyApp(ui = ui, server = server)
Error in shinyApp(ui = ui, server = server) : 
  could not find function "shinyApp"
> source("C:\\Users\\DellUser\\Documents\\EsssentailRegressionIliyanCode\\app.R")

Attaching package: ‘shiny’

The following object is masked _by_ ‘.GlobalEnv’:

    singleton

package ‘gtools’ successfully unpacked and MD5 sums checked
Warning: cannot remove prior installation of package ‘gtools’
Warning: restored ‘gtools’
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning messages:
1: In file.copy(savedcopy, lib, recursive = TRUE) :
  problem copying C:\Users\DellUser\AppData\Local\R\win-library\4.2\00LOCK\gtools\libs\x64\gtools.dll to C:\Users\DellUser\AppData\Local\R\win-library\4.2\gtools\libs\x64\gtools.dll: Permission denied
2: In file(filename, "r", encoding = encoding) :
  cannot open file 'Tuning.R': No such file or directory
> 
> 
> local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
+ if(nchar(pkg)) library(pkg, character.only=TRUE)})
> local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
+ if(nchar(pkg)) library(pkg, character.only=TRUE)})
> library(shiny)
> install.packages("gtools", quiet= TRUE)
package ‘gtools’ successfully unpacked and MD5 sums checked
Warning: cannot remove prior installation of package ‘gtools’
Warning: restored ‘gtools’
Warning message:
In file.copy(savedcopy, lib, recursive = TRUE) :
  problem copying C:\Users\DellUser\AppData\Local\R\win-library\4.2\00LOCK\gtools\libs\x64\gtools.dll to C:\Users\DellUser\AppData\Local\R\win-library\4.2\gtools\libs\x64\gtools.dll: Permission denied
> source("Tuning.R")
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'Tuning.R': No such file or directory
> #source("EstNonpure.R")
> #source("EstPure.R")
> #source("Utilities.R")
> #source("EstOmega.R")
> #source("ER-inference.R")
> #source("Est_beta_dz.R")
> #library(MASS)
> #library(shiny)
> 
> # Define UI for application that draws a histogram
> ui <- fluidPage(
+ 
+     # Application title
+     titlePanel("Essential Regression"),
+ 
+     # Sidebar with a slider input for number of bins 
+     sidebarLayout(
+         sidebarPanel(
+         
+         fileInput("Y", "Input your Y file (needs to have only one column)",
+                   accept = c(
+                     "text/csv",
+                     "text/comma-separated-values,text/plain",
+                     ".csv")
+         ),
+         
+         fileInput("X", "Input your X file (needs to have muliple columns)",
+                  accept = c(
+                    "text/csv",
+                    "text/comma-separated-values,text/plain",
+                    ".csv")
+         ),
+         
+         
+         numericInput(
+           "lbf",
+              " Enter your  lbf value",
+           0,
+           min = 0,
+           max = 1),
+         numericInput(
+           "delta",
+           " Enter your delta value",
+           0,
+           min = 0,
+           max = 1
+   
+         
+         ),
+         
+         fileInput("yaml", "Input your Yamul File",
+                   accept = c(
+                     "text/csv",
+                     "text/comma-separated-values,text/plain",
+                     ".csv")
+         
+         
+         ),
+ 
+         # Show a plot of the generated distribution
+         mainPanel( textOutput("ER")
+            
+         )
+         )
+     )
+ )
Error in sidebarLayout(sidebarPanel(fileInput("Y", "Input your Y file (needs to have only one column)",  : 
  argument "mainPanel" is missing, with no default
>     
>     
>   
> 
> 
> # Define server logic required to draw a histogram
> server <- function(input, output) {
+   
+   
+   
+   
+   
+   
+   ####Create a reactive statement that does the essential regression
+   ###
+   Er <- reactive({
+     inFilex <- input$X
+     inFiley <- input$Y  
+     yamule  <- input$yamul
+     
+     
+     if (is.null(inFilex)||is.null(inFiley)  )
+       return(NULL)
+     
+     tmpx <-    read.csv(inFilex$datapath,header=T)
+     tmpy <-    read.csv(inFiley$datapath,header=T)
+     Data <- data.frame(tmpx, tmpy) 
+     yamul <- data.frame(read.csv(yamule$datapath,header=T))
+     VectorOfInputs <- as.Vector(yamul$inputs)
+     print(vectorOfInputs)
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+     
+   })
+   
+   
+ 
+     output$distPlot <- renderText({
+       
+      
+       Er()
+       
+      
+       
+       
+        
+       
+     })
+ }
> 
> # Run the application 
> shinyApp(ui = ui, server = server)

Listening on http://127.0.0.1:5746

