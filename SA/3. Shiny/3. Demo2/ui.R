# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                       Shiny
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                            Subset : ui.R : reactive : Done
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Reference : http://stackoverflow.com/questions/27699991/subset-data-and-plot-this-subsetted-data-with-shiny

 library(devtools)
# install_github('rstudio/shinyapps')
 library(shinyapps)
# shinyapps::deployApp("C:/Users/yashwanth.r/Documents/Data Analysis/Data Analysis/Shiny")
library(shiny);library(shinythemes)
setwd("C:/Yashwanth/Sports Authority/5. Shiny/3. Demo2/")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel(title = h4("Sports Authority : Best Seller Prediction Model")),
  
  sidebarPanel(  
    selectInput("var","Select Variable : ",choices = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")),
    br(),
    sliderInput('bins',"select bins :", min = 5,max = 25, value = 15),
    br(),
    radioButtons('color',"select the color",choices = c("Green","Red","Yellow"))
    ),
  
  mainPanel(
    plotOutput("plots")
    )
))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                            Summary : ui.R : reactive : Done
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# 
# Iris <- iris
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Best Seller Prediction Model"),
#   
#   sidebarPanel(  
#     selectInput("Species","Species : ",as.character(unique(Iris$Species)))
#   ),
#   
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Summary",verbatimTextOutput("summary"))
#   )
# )
# ))
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                            Describe : ui.R : reactive : 
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Iris <- iris
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Best Seller Prediction Model"),
#   
#   sidebarPanel(  
#     selectInput("Species","Species : ",as.character(unique(Iris$Species)))
#   ),
#   
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Describe",verbatimTextOutput("describe"))
#   )
# )
# ))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                            Correlation : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# 
# Iris <- iris
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Best Seller Prediction Model"),
#   
#   sidebarPanel(  
#     selectInput("Species","Species : ",as.character(unique(Iris$Species)))
#   ),
#   
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Correlation",verbatimTextOutput("cor"))
#     )
#   )
# ))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Summary & Correlation : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Iris <- iris
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Best Seller Prediction Model"),
#   
#   sidebarPanel(  
#     selectInput("Species","Species : ",as.character(unique(Iris$Species)))
#   ),
#   
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Summary",verbatimTextOutput("describe")),
#       tabPanel("Correlation",verbatimTextOutput("cor"))
#     )
#   )
# ))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------- END ---------------------------- Shiny ------------------------- END -------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
