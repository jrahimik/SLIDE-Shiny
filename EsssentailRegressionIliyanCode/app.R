#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shiny)

source("Tuning.R")
source("EstNonpure.R")
source("EstPure.R")
source("Utilities.R")
source("EstOmega.R")
source("ER-inference.R")
source("Est_beta_dz.R")
source("DataInOrder.R")
library("MASS")
library(MASS)
library(shiny)
library("zip")

            


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Essential Regression"),

    # File Inputs Y, X, Ya, lbf, delta, rep_CV,  alpha level, CI,Pred, diagonal and action button
    sidebarLayout(
        sidebarPanel(
        
        fileInput("Y", "Input your Y file (needs to have only one column)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        
        fileInput("X", "Input your X file (needs to have muliple columns)",
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
        ),                                                
        
        fileInput("Ya", "Input your Yamul File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
                  
                  
        ),
        
        
        numericInput(
          "lbf",
             " Enter your  lbf value",
          0,
          min = 1,
          max = 100),
       
        numericInput(
          "rep_CV",
          " Enter your rep_CV value",
          0,
          min = 1,
          max = 100
          
          
        ),
        numericInput(
          "alpha_level",
          " Enter your alpha value",
          0,
          min = 1,
          max = 100
          
          
        ),
        numericInput(
          "delta_level",
          " Enter your delta value",
          .01,
          min = 0.01,
          max = 100
          
             
        ),
        
        
        checkboxInput("CI", "CI", FALSE),
        checkboxInput("pred", "Pred", FALSE),
        checkboxInput("Diagonal", "Diagonal", FALSE),
        checkboxInput("Merge", "Merge", FALSE),
        checkboxInput("equal_var", "equal_var", FALSE),
        checkboxInput("Verbose", "verbose", FALSE),
        actionButton("ER", "Execute Essential Regression"),
        
        
      
        
        ),

        # Show a plot of the generated distribution
        mainPanel( h1("Essential Regression"), h4("ES: "),verbatimTextOutput("EssentialRegressionSummary")
           
        ),
        )
    )

    
    
  

####When Press Action Button runs original code plus trying to store the code into a zip file 

server <- function(input, output,session) {
    
     observeEvent(input$ER,{ 
     inFiley <- input$Y  
     
     
     
     inFilex <- input$X
     
     yamule  <- input$Ya   
     
     
     X1 <-     data.frame(suppressWarnings(read.csv(inFilex$datapath,header=T)))
     X <-  as.matrix(X1) 
     tmpy <-   suppressWarnings(read.csv(inFiley$datapath,header=T)) 
     
     
     Y1  <- data.frame(tmpy)
     Y   <-   as.matrix(Y1)
     
     yamul <- data.frame(read.csv(yamule$datapath,header=T))
     otherInputs <-  unlist(yamul)
     
     
     
     
     
     
     ####Checking the parameters of the our equation to see if they are 
     
     #### Indices are 
     ###Like 1. CI 2. Pred 3.diagonal 4. merge 5. equal_var
     ### 6. correction 7.verbose   ###
     
     
     ER <- function(Y,X,delta, beta_est, CI,  pred, lbd,
                    rep_CV,  diagonal,  merge, equal_var,
                    alpha_level, support, correction,
                    verbose) {
       
      
   Y <- Y[,c("Y")]       
       View(Y)
       n <- nrow(X)
       p <- ncol(X)  
              ###The  number of columns of Hurricane.csv is 13 columns so we have an 8 by 13 matrix essential
       X <- as.matrix(X)
       if (equal_var) {  #### If this equal_var is true 
           se_est <- rep(1, p)  ####Then it will store the 1 repeated 13 time  via the rep() function
       }else{
          se_est <- apply(X, 2, sd)  
         
         }   #### Margin=2, so we are storing the standard deviation of the columns into se_est
         deltaGrids <- delta * sqrt(log(max(p, n)) / n)
         
        
         
        optDelta <- ifelse(length(deltaGrids) > 1,
                           median(replicate(rep_CV, CV_Delta(X, deltaGrids, diagonal, se_est, merge))),
                           deltaGrids)  
         
    
         
         if (verbose){
           cat("Selecting the delta =", delta[min(which(deltaGrids >= optDelta))], "\n")
         }
      
     
       print(class(X))
        
        
       
        
        Sigma <- crossprod(X)  / n
      
      View(Sigma)
     print( ncol(Sigma))   ####Since it is a p by p convariance matrix it should 1722 by 1722 
     print(nrow(Sigma)) 
     
      
      resultAI <- EstAI(Sigma, optDelta, se_est, merge)
    
      
         
        pure_numb <- sapply(resultAI$pureSignInd, FUN = function(x) {length(c(x$pos, x$neg))})
        
       
         if (sum(pure_numb == 1) > 0) {
           
          cat("Changing ``merge'' to ``union'' and reselect delta ... \n")
          optDelta <- ifelse(length(deltaGrids) > 1,
                            median(replicate(rep_CV, CV_Delta(X, deltaGrids, diagonal, se_est, merge = F))),
                           deltaGrids)
          resultAI <- EstAI(Sigma, optDelta, se_est, merge = F)
         }
         
         A_hat <- resultAI$AI
         I_hat <- resultAI$pureVec
         I_hat_ind <- resultAI$pureSignInd
        
       if (is.null(I_hat)) {
         
          cat("Algorithm fails due to non-existence of pure variable.\n")
           stop()
        }
         
         C_hat <- EstC(Sigma, A_hat,diagonal)
         Gamma_hat <- rep(0, p)
         Gamma_hat[I_hat] <- diag(Sigma[I_hat, I_hat]) - diag(A_hat[I_hat,] %*% C_hat %*% t(A_hat[I_hat,]))
         Gamma_hat[Gamma_hat < 0] <- 1e-2
         
        
         if (pred) {
           pred_result <- Prediction(Y, X, A_hat, Gamma_hat, I_hat)
           
           # the matrix to predict Z
          Theta_hat <- pred_result$Theta
          Q <- try(Theta_hat %*% solve(crossprod(X %*% Theta_hat) / n, crossprod(Theta_hat)), silent = T)
           if (class(Q)[1] == "try-error")
             Q <- Theta_hat %*% ginv(crossprod(X %*% Theta_hat) / n) %*% crossprod(Theta_hat)
         } else
           pred_result <- Q <- NULL
         
   
         
         if (beta_est == "NULL" || beta_est == "Dantzig") {

           beta_hat <- beta_CIs <- beta_var <- NULL
           if (beta_est == "Dantzig") {
             beta_hat <- Est_beta_dz(Y, X, A_hat, C_hat, I_hat, optDelta, 0.5, 0.5)
           }
         } else {
          
          if (CI) {
           if (length(resultAI$pureVec) != nrow(Sigma)) {
              Y_hat <- EstY(Sigma, A_hat, resultAI$pureVec)
              if (lbd > 0) {
                 AI_hat <- abs(A_hat[I_hat, ])
                 sigma_bar_sup <- max(solve(crossprod(AI_hat), t(AI_hat)) %*% se_est[I_hat])
                 AJ <- EstAJDant(C_hat, Y_hat, lbd * optDelta * sigma_bar_sup, sigma_bar_sup + se_est[-I_hat])
              } else 
                 AJ <- t(solve(C_hat, Y_hat))
               A_hat[-resultAI$pureVec, ] <- AJ  
             }
             
             Gamma_hat[-I_hat] <- diag(Sigma[-I_hat, -I_hat]) - diag(A_hat[-I_hat,] %*% C_hat %*% t(A_hat[-I_hat,]))
             Gamma_hat[Gamma_hat < 0] <- 1e2
          }
           
           res_beta <- Est_beta(Y, X, A_hat, C_hat, Gamma_hat, I_hat, I_hat_ind, CI = CI,
                                alpha_level = alpha_level, correction = correction)
           beta_hat <- res_beta$beta
           beta_CIs <- res_beta$CIs
           beta_var <- res_beta$beta_var
         } 
         return(list(K = ncol(A_hat), A = A_hat, C = C_hat, I = I_hat, I_ind = I_hat_ind, Gamma = Gamma_hat,
                    beta = beta_hat, beta_CIs = beta_CIs, pred = pred_result,
                     optDelta = delta[min(which(deltaGrids >= optDelta))],
                     Q = Q, beta_var = beta_var))
         
         
      
     }
list <-   ER(Y,X, input$delta_level ,  otherInputs[1] , input$CI, input$pred, input$lbd,
        input$rep_CV,   input$Diagonal,  input$Merge,  input$equal_var,
        input$alpha_level,otherInputs[3],  otherInputs[4],
        input$Verbose)

sink("ageDataset.csv")
print(list)
sink()

setwd("C:/Users/DellUser/Documents/EsssentailRegressionIliyanCode")
zipr("ERofAgeDataset.zip", "ageDataset.csv")


 






  
 
 

   
 
 
 
    
     
     
     })
}
   
    
    
       
      
    
         
      
        
       
      
        
          
         
   
          
        
       
      
  
   
      

# Run the application 
shinyApp(ui = ui, server = server)  ###Runs the app
