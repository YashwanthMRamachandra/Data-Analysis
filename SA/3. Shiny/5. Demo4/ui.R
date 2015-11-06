library(devtools)
# install_github('rstudio/shinyapps')
library(shinyapps)
library(shiny)
library(shinythemes)

Category <- c("Activity Monitors","Baseball Bats","Cleats","Golf Complete Sets",
              "NFL Apparel","Training Aid","Treadmill","Women's Running Shoes")
Variable <- c("Reviews_Q1","Reviews_Q2","Reviews_Q3","Reviews_Q4","Sales_Units")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel(title = "Sports Authority : Shoppers Interest Index"), # h4("",align = center)
  
  sidebarPanel(# About Sports Authority : help(condtionalPanel)
    conditionalPanel(condition = "$('li.active a').first().html()==='Sports Authority'",
                     h2("About Sports Authority"),
                     p("Sports Authority, Inc. is one of the largest sporting goods retailers in the United States. It is headquartered in Englewood, Colorado, and operates more than 460 stores in 45 U.S. states and Puerto Rico under the Sports Authority name."),
                     p("The company's website is on the GSI Commerce platform and supports the retail stores as well as other multi-channel programs. In addition, a joint venture with ÆON Co., Ltd. operates Sports Authority stores in Japan under a licensing agreement."),
                     p("Gart Sports began in 1928, when Denver Post newspaper carrier Nathan Gart started the company with $50 in fishing rod samples."),
                     br(),
                     tags$a(href = "https://en.wikipedia.org/wiki/Sports_Authority", "More details on Sports Authority"),
                     br(),br(),
                     tags$a(href = "https://twitter.com/SportsAuthority?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", "Follow @SportsAuthority"),
                     br(),br(),
                     tags$a(href = "http://www.sportsauthority.com/home/index.jsp", "Shop @SportsAuthority"),
                     br(),
                     br()),
    
    # Data View
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
                     selectInput('category_DV',"Select category :",choices = Category)),
    
    # Correlation
    conditionalPanel(condition = "$('li.active a').first().html()==='Correlation'",
                     selectInput('category_Cor',"Select category :",choices = Category)),
    
    # Plots
    conditionalPanel(condition = "$('li.active a').first().html()==='Graphs'",
                     selectInput('category_Gra',"Select Category :",choices = Category),
                     br(),
                     selectInput('Var',"Select Variable :", choices = Variable),
                     br(),
                     sliderInput('bins',"Select the bins", min = 5,max = 25,value =15),
                     br(),
                     radioButtons('color',"Select color of the Histogram", 
                                  choices = c("Green","Red","Yellow"), selected = "Green"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Sports Authority',
               img(src = "SA_1.png", height = 172, width = 250),img(src = "SA_2.png", height = 172, width = 250),
               img(src = "SA_3.png", height = 172, width = 250),img(src = "SA_4.png", height = 172, width = 250),
               img(src = "SA_5.png", height = 172, width = 250),img(src = "SA_6.png", height = 172, width = 250),
               img(src = "SA_7.png", height = 172, width = 250),img(src = "SA_8.png", height = 172, width = 250),
               img(src = "SA_9.png", height = 172, width = 250)),
      tabPanel('Data View',column(12,offset = 1,h2('Data Input'),dataTableOutput('table'))),
      tabPanel('Correlation',tableOutput("corr")),
      tabPanel('Graphs',plotOutput("plots"))
    ))
))
