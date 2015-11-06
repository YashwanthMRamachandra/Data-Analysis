library(shiny)
library(psych)
#setwd("C:/Yashwanth/Sports Authority/5. Shiny/4. Demo3/")
row.names(mtcars) <- NULL

shinyServer(
  function(input,output,session){
    dataset <- reactive({
        mtcars[,input$var]   #    mtcars[mtcars$input$cat,input$var] ; subset(mtcars,input$cat,input$var)
    })
    
    output$Graphs <- renderPlot({
      hist(log(dataset()),col = input$color, main = "Distribution", xlab = input$var)
      curve(dnorm(x,mean=mean(log(dataset())),sd=sd(log(dataset()))),add=TRUE)
    })
})



# ggplot(dataset(),aes(x=input$var)) +
#   facet_wrap(~ input$category_Gra) +
#   geom_histogram(aes(y=..density..),col = "blue2") +
#   stat_function(fun = dnorm,args = list(mean=mean(dataset()),sd=sd(dataset())),colour = "red") +
#   labs(title = "Distribution")
