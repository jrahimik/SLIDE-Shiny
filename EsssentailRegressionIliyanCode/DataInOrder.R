library("dplyr")
                  
library("data.table")

 x <-  read.csv("C:/Users/DellUser/Downloads/raw_age_data_all_days.csv", sep= " ")  

 
 dim(x) 
colnames(x)
if("id" %in% colnames(x)) {
  x<- subset(x, select = -c(id) ) 
}
Y <-x[,1]

X_new<- x[,-1]     ####




Y <- as.vector(Y)
 
 
                                       ####Remove columns that have  

          #####Can see the X values 
 
  


Y<- cbind(Y, rep(1,length(Y)))


X_new<- setnafill(X_new, fill = 0)
 #View(X_new)
 #View(Y)
 




#icantsee <- read.table("C:/Users/DellUser/Desktop/Input Files for Code/x.csv", fill = TRUE)

write.table(Y,file= "C:/Users/DellUser/Desktop/Input Files for Code/y_new.csv", quote = TRUE, row.names = FALSE, col.names= TRUE, sep= "," )
write.table(X_new,file= "C:/Users/DellUser/Desktop/Input Files for Code/x_new.csv", quote = TRUE, row.names = FALSE, col.names =  TRUE, sep= ",")


 