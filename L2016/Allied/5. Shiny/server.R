library(shiny)
library(psych)
#setwd("C:/Yashwanth/Canada/5. Canada/5. Shiny/")

shinyServer(
  function(input,output,session){
    Canada_Data_Shiny <- read.csv("data/Canada_Data_Sample.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, 
                         comment.char="", as.is=TRUE)
    
    Canada_Data_Shiny$ID_unit <- Canada_Data_Shiny$Prob <- Canada_Data_Shiny$ID_unit <- Canada_Data_Shiny$Stratum <- NULL
    cols <- c(1,3,4,6,8,26)
    Canada_Data_Shiny[cols] <- lapply(Canada_Data_Shiny[cols],as.factor)
    Canada_Data_Shiny$Margin_Per[which(!is.finite(Canada_Data_Shiny$Margin_Per))] <- 0
    
    # Data View
    output$table <- renderDataTable({
      data = subset(Canada_Data_Shiny,Quarter==input$Quarter_DV)
      data
    })
    
    # Correlation
    output$corr <- renderTable({
      cor(subset(Canada_Data_Shiny,Quarter==input$Quarter_Cor,names(Canada_Data_Shiny))[,sapply(Canada_Data_Shiny,is.numeric)],use = "na.or.complete")
    })
    
    # Graphs
    output$plots <- renderPlot({
      
      if(input$Quarter_Gra=="1"){
        data1 = Canada_Data_Shiny[Canada_Data_Shiny$Quarter=="1",input$Var]
        hist(data1,breaks = seq(-5000,max(data1,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data1,na.rm = T),sd=sd(data1,na.rm = T)),add=TRUE)
      }
      else if(input$Quarter_Gra=="2"){
        data2 = Canada_Data_Shiny[Canada_Data_Shiny$Quarter=="2",input$Var]
        hist(data2,breaks = seq(-5000,max(data2,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data2,na.rm = T),sd=sd(data2,na.rm = T)),add=TRUE)
      }
      else if(input$Quarter_Gra=="3"){
        data3 = Canada_Data_Shiny[Canada_Data_Shiny$Quarter=="3",input$Var]
        hist(data3,breaks = seq(-5000,max(data3,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data3,na.rm = T),sd=sd(data3,na.rm = T)),add=TRUE)
      }
      else {
      data4 = Canada_Data_Shiny[Canada_Data_Shiny$Quarter=="4",input$Var]
      hist(data4,breaks = seq(-5000,max(data4,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
      curve(dnorm(x,mean=mean(data4,na.rm = T),sd=sd(data4,na.rm = T)),add=TRUE)
      }

    })
})



# attach(Canada_Data_Sample)
# ggplot(Canada_Data_Sample,aes(x=Avg_Selling_Price)) +
#   facet_wrap(~ Stock_Status) +
#   geom_histogram(aes(y=..density..),col = "blue2") +
#   stat_function(fun = dnorm,args = list(mean=mean(Avg_Selling_Price),sd=sd(Avg_Selling_Price)),colour = "red") +
#   labs(title = "Avg_Selling_Price Distribution by Store")
# detach(Canada_Data_Sample)