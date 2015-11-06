# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                   Shiny
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                     Best Seller Prediction Model : server.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(shiny)
library(psych)
#setwd("C:/Yashwanth/Sports Authority/5. Shiny/1. SA_Apps/")

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
    
    # Models
         output$models <- renderPrint({
          if(input$model=="Baseline"){
            # Data Sampling
            Review_Aggr_Act_Mon_Model_Data <- SA_Input[SA_Input$Category=="Cleats",]
            set.seed(100)
            SA_IND  <- sample(nrow(Review_Aggr_Act_Mon_Model_Data),size=round(((nrow(Review_Aggr_Act_Mon_Model_Data)/100)*70)+1,0))
            SA_TRAIN <- Review_Aggr_Act_Mon_Model_Data[SA_IND,]
            SA_TEST <- Review_Aggr_Act_Mon_Model_Data[-SA_IND,]
            
            SA_LR <- glm(Sales ~ Reviews_Q1+Reviews_Q2+Reviews_Q3+Reviews_Q4,data=SA_TRAIN,family=binomial(link="logit"))
            print(summary(SA_LR));

            # logLikelihood & BIC
            library(lmtest)
            print(logLik(SA_LR))
            print(BIC(logLik(step(SA_LR,direction='forward'))))
            print(BIC(logLik(step(SA_LR,direction='backward'))))
            
            # Wald's test : Overall variance explained
            library(aod)
            print(wald.test(Sigma = vcov(SA_LR),b = coef(SA_LR), Terms = 2:length(coef(SA_LR))))
            
            # Prediction & Odds ratio
            SA_TEST$Prob <-  predict.glm(SA_LR,SA_TEST,type="response")
            SA_TEST$Pred <- ifelse(SA_TEST$Prob>0.5,1,0)
            SA_TEST$Odds <- round(SA_TEST$Prob/(1-SA_TEST$Prob),2)
            print(exp(cbind(OR = coef(SA_LR), confint(SA_LR))))
            
            # Classification Accuracy/ Confusion Matrix
            library(xtable)
            library(reshape)
            library(data.table)
            print(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred))
            DT <- cbind(melt(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                        Decision=c("False Positive","True Negative","False Negative","True Positive"))
            DT <- data.table(DT)
            setorderv(DT,"Decision",order = -1);DT
            
            Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred))
            print(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                  Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100))))
            print(sum(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                      Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))[3]));
          }
          else {
            # Standardised Residuals
            library(MASS)
            SA_SR <- cbind(SA_TRAIN,Std_Res=stdres(SA_LR))
            SA_SR <- cbind(SA_SR,Std_Res_Dec = ifelse(SA_SR$Std_Res>3 | SA_SR$Std_Res < -3,"YES","NO"))
            
            # Cook's distance : Cook's distance or Cook's D is a commonly used estimate of the influence of a data point when
            #                   performing least squares regression analysis.
            
            infl <- influence(SA_LR, do.coef = FALSE)
            SA_SR_CD <- cbind(SA_SR, Cook_D=cooks.distance(SA_LR, infl = influence(SA_LR, do.coef = FALSE),
                                                           res = infl$pear.res,
                                                           dispersion = summary(SA_LR)$dispersion,
                                                           hat = infl$hat))
            
            SA_SR_CD <- subset(SA_SR_CD,Std_Res_Dec=="NO" & Cook_D<1,names(SA_SR_CD))
            
            # Revised Logistic Regression
            SA_LR_Rev <- glm(Sales ~ Reviews_Q1+Reviews_Q2+Reviews_Q3+Reviews_Q4,
                             data=SA_SR_CD,family=binomial(link="logit"))
            
            print(summary(SA_LR_Rev));
            print(exp(cbind(OR = coef(SA_LR_Rev), confint(SA_LR_Rev))));
            
            # Wald's test : Overall variance explained
            print(wald.test(Sigma = vcov(SA_LR_Rev),b = coef(SA_LR_Rev), Terms = 2:length(coef(SA_LR_Rev))));
            
            # Prediction & Odds
            SA_TEST$Prob <-  predict.glm(SA_LR_Rev,SA_TEST,type="response")
            SA_TEST$Pred <- ifelse(SA_TEST$Prob>0.5,1,0)
            SA_TEST$Odds <- round(SA_TEST$Prob/(1-SA_TEST$Prob),2)
            
            # Classification Accuracy/ Confusion Matrix
            print(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred));
            DT <- cbind(melt(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                        Decision=c("False Negative","True Negative","False Positive","True Positive"))
            DT <- data.table(DT)
            setorderv(DT,"Decision",order = -1);print(DT);
            
            Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred))
            print(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                  Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100))));
            print(sum(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
                      Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))[3]))
          }
        })
    
})

# # Note : Should "terminateApp" in order to redeploy "deployApp".
# # Reference : http://stackoverflow.com/questions/26460585/r-shiny-selectinput-doesnt-reflect-the-variable-selection-when-multiple-true
# 
# # --------------------------------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------------------------------
# # --------------- END ---------------------------- Shiny ------------------------- END -------------------------
# # --------------------------------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------------------------------
# library(ggplot2)
#       ggplot(dataset,aes(x=input$var)) +
# facet_wrap(~ input$category_Gra) +
#   geom_histogram(aes(y=..density..),col = "blue2") +
#   stat_function(fun = dnorm,args = list(mean=mean(dataset),sd=sd(dataset)),colour = "red") +
#   labs(title = "Distribution")

# data <- switch(input$Var,
#                "Reviews_Q1" = data.frame(Reviews_Q1 = SA_Input$Reviews_Q1),
#                "Revenue" = data.frame(Revenue = SA_Input$Revenue))
# 
# # Review_Q1 Distribution
# p1 <-  ggplot(data,aes(x=Reviews_Q1),environment = environment) +
#   geom_histogram(aes(y=..density..), col = "blue2") +
#   stat_function(fun = dnorm,args = list(mean=mean(Reviews_Q1),sd=sd(Reviews_Q1)),colour = "red") +
#   labs(title = "Reviews Distribution",x="Reviews Q1")
# print(p1)
# 
# dataset <- reactive({
#   SA_Input[,input$Var]
# })
# output$plots <- renderPlot({
#   #     if(is.null(input$category_Gra)){return(NULL)}
#   #     else if(input$category_Gra){
#   #     hist(dataset,breaks = seq(0,max(dataset,na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
#   #     curve(dnorm(x,mean=mean(dataset,na.rm = T),sd=sd(dataset,na.rm = T)),add=TRUE)
#   hist(log(dataset),breaks = seq(0,max(log(dataset),na.rm = T),l=input$bins+1),col = input$color, main = "Distribution", xlab = input$Var)
#   curve(dnorm(x,mean=mean(log(dataset),na.rm = T),sd=sd(log(dataset),na.rm = T)),add=TRUE)
# })          
