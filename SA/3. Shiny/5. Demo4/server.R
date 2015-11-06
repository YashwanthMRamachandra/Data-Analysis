library(shiny)
library(psych)
#setwd("C:/Yashwanth/Sports Authority/5. Shiny/5. Demo4/")

shinyServer(
  function(input,output,session){
    SA_Input <- read.csv("data/3. Model_Input_by_Cat_All_Reviews.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, 
                         comment.char="", as.is=TRUE)
    
    SA_Input <- SA_Input[SA_Input$Category=="Activity Monitors",]
    SA_Input <- with(SA_Input,SA_Input[order(Sales_Units,decreasing = TRUE),])
    SA_Input$Sales <- c(rep(1,10),rep(0,length(11:nrow(SA_Input))))
    SA_Input <- with(SA_Input,SA_Input[order(Product,decreasing = TRUE),])
    SA_Input$Sales <- as.factor(SA_Input$Sales)
    SA_Input$Product_ID <- as.factor(SA_Input$Product_ID)
    SA_Input$Product <- NULL
    
    # Data View
    output$table <- renderDataTable({
      data = subset(SA_Input,Category==input$category_DV)
      data
    })
    
    # Correlation
    output$corr <- renderTable({
      cor(subset(SA_Input,Category==input$category_Cor,names(SA_Input))[,sapply(SA_Input,is.numeric)],use = "na.or.complete")
    })
    
    # Graphs
    output$plots <- renderPlot({
      if(input$category_Gra=="Activity Monitors"){
        data1 = SA_Input[SA_Input$Category=="Activity Monitors",input$Var]
        hist(data1,breaks = seq(0,max(data1,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data1,na.rm = T),sd=sd(data1,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="Baseball Bats"){
        data2 = SA_Input[SA_Input$Category=="Baseball Bats",input$Var]
        hist(data2,breaks = seq(0,max(data2,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data2,na.rm = T),sd=sd(data2,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="Cleats"){
        data3 = SA_Input[SA_Input$Category=="Cleats",input$Var]
        hist(data3,breaks = seq(0,max(data3,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data3,na.rm = T),sd=sd(data3,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="Golf Complete Sets"){
        data4 = SA_Input[SA_Input$Category=="Golf Complete Sets",input$Var]
        hist(data4,breaks = seq(0,max(data4,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data4,na.rm = T),sd=sd(data4,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="NFL Apparel"){
        data5 = SA_Input[SA_Input$Category=="NFL Apparel",input$Var]
        hist(data5,breaks = seq(0,max(data5,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data5,na.rm = T),sd=sd(data5,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="Training Aid"){
        data6 = SA_Input[SA_Input$Category=="Training Aid",input$Var]
        hist(data6,breaks = seq(0,max(data6,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data6,na.rm = T),sd=sd(data6,na.rm = T)),add=TRUE)}
      else if(input$category_Gra=="Treadmill"){
        data7 = SA_Input[SA_Input$Category=="Treadmill",input$Var]
        hist(data7,breaks = seq(0,max(data7,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data7,na.rm = T),sd=sd(data7,na.rm = T)),add=TRUE)}
      else {
        data8 = SA_Input[SA_Input$Category=="Women's Running Shoes",input$Var]
        hist(data8,breaks = seq(0,max(data8,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
        curve(dnorm(x,mean=mean(data8,na.rm = T),sd=sd(data8,na.rm = T)),add=TRUE)}
    })
})
