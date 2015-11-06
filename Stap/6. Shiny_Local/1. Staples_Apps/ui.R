# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                   Shiny
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                Demand Loyalty versus Price Competitiveness : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

 library(devtools)
 #install_github('rstudio/shinyapps')
 library(shinyapps)
 #shinyapps::deployApp("C:/Yashwanth/Sports Authority/5. Shiny/Apps")

# library(shiny)
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Staples PE"),
#   
#   sidebarPanel(  
#     checkboxGroupInput("Loyalty_Cat","Loyalty : ",as.character(unique(Staples_IP$Loyalty_Cat)))
#   ),
#   
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Summary",verbatimTextOutput("describe")),
#       tabPanel("Correlation",verbatimTextOutput("cor"))
#     )
#   )
# ))
# 
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Weekly : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Staples PE"),
#   
#   sidebarPanel(  
#     checkboxGroupInput("Variable","Select a Variable : ",names(data.frame(Staples_Aggr_Weekly)[,sapply(data.frame(Staples_Aggr_Weekly),is.numeric)])),
#     checkboxGroupInput("Loyalty_Cat","Loyalty : ",as.character(unique(Staples_Aggr_Weekly$Loyalty_Cat)))
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
#                                   Monthly : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Loyalty_Cat <- c("HIGH","MID","LOW")
# Product_ID <- c("36619","37927","51168","117044","125369","196147","202184","205112","205138","205146","209908","257444","263236","272153","277582","301754","306647","324013","356338","358136","359265","365060","372623","374619","377179","377180","379168","391431","392941","404145","414392","415489","418668","424576","428331","429174","443685","452823","461374","462846","469080","476883","478887","479057","489574","492757","494224","502336","502351","504308","508581","511947","512427","514510","517011","517988","519668","538249","559215","563125","576947","580823","585006","597444","611549","611556","611562","616044","617433","618374","627963","638571","642736","648102","648103","648186","648370","648850","649692","653417","654684","656499","656500","658923","665698","665699","679088","683795","701218","712994","713993","721455","727093","727095","731565","735273","735277","735291","735305","738002","739213","739229","739318","741186","743478","752370","754639","757288","757289","760047","760476","762757","763216","771980","775751","775753","779330","782192","782504","783415","787361","790113","792257","800776","804574","806123","806124","806126","807747","812006","812010","812048","812327","853300","859520","859605","863061","863619","869097","886404","889138","889863","890785","891404","891405","896998","905412","917860","917881","917883","917917","931359","954076","956112")
# 
# 
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Staples PE"),
#   
#   sidebarPanel(  
#     checkboxGroupInput("Loyalty_Cat","Loyalty : ",Loyalty_Cat),
#     selectInput("Product_ID","Select the Product : ",Product_ID)
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
#                                   Monthly : ui.R : reactive : t.test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# Reference : https://interestingyhu.wordpress.com/2015/03/06/create-a-t-test-shiny-app/

library(shinythemes)
Loyalty_Cat <- c("HIGH","MID","LOW")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("Hypothesis Test (T-test)"),
  br(),
  br(),
  br(),
  
  sidebarPanel(
    # SlidebarPanel for t-test
    conditionalPanel(condition = "$('li.active a').first().html()==='About T-test'",
                     h2("About One Sample T-test:"),
                     p("A t-test is any statistical hypothesis test in which the test statistic follows a Student's t distribution if the null hypothesis is supported."),
                     p("it is most commonly applied when the test statistic would follow a normal distribution if the value of a scaling term in the test statistic were known."),
                     p("When the scaling term is unknown and is replaced by an estimate based on the data, the test statistic (under certain conditions) follows a Student's t distribution."),
                     br(),
                     tags$a(href = "http://en.wikipedia.org/wiki/Student's_t-test", "More Detail About t-test."),
                     br(),
                     br()
    ),
    
    # SlidebarPanel for file upload tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"')
    ),
    
    # SliderbarPanel for t-test tab
    conditionalPanel(condition = "$('li.active a').first().html()==='T-test'",
                     sliderInput("bins",
                                 "Numer of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 2
                     ),
                     radioButtons("sample",
                                  "Please choose one sample t test or two sample t test:",
                                  choices = c("One sample" = "oneSamp", 
                                              "Two sample" = "twoSamp")),
                     selectInput("var1", 
                                 label = "Please Select a Numerical Variable",
                                 ""
                     ),
                     conditionalPanel(condition = "input.sample == 'twoSamp'",
                                      selectInput("var2", 
                                                  label = "Please Select a Numerical Variable",
                                                  ""
                                      ),
                                      radioButtons("varequal",
                                                   "Are the two samples have equal variance:",
                                                   choices = c("Yes" = "y",
                                                               "No" = "n"))
                     ),
                     selectInput("tail",
                                 label = "Please Select a relationship you want to test:",
                                 choices = c("Equal" = "two.sided", 
                                             "Less" = "less",
                                             "Greater" = "greater")),
                     conditionalPanel(condition = "input.sample == 'oneSamp'",
                                      numericInput("test",
                                                   "Mean value You Want to Test",
                                                   value = 0
                                                   
                                      )
                     ),
                     numericInput("conf",
                                  label = "Please Select a confidence level:",
                                  value = 0.95,
                                  min = 0.8,
                                  max = 0.99),
                     helpText("Note: Please assign a number between 0 and 1 in the numeric Input")
                     
    )
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel('About T-test',
               plotOutput('tplot')),
      tabPanel('Data View', 
               fluidRow(column(10, offset = 1,
                               h2("Data Table"),
                               verbatimTextOutput('disc'))),
               
               fluidRow(column(10, offset = 1,
                               h2("Data Summary"),
                               verbatimTextOutput('str'))),
               fluidRow(column(10, offset = 1,
                               h2("Data Structure"),
                               tableOutput('contents')))      
      ),           
      tabPanel('T-test',
               fluidRow(column(10, offset = 1,
                               plotOutput('graph'))),
               fluidRow(column(8, offset = 1,
                               h2("Key summary statistics"),
                               p("The observed sample statistics were:"),
                               tableOutput('parametric'),
                               h2("Hypothesis of the t-test"),
                               p("We are testing the null hypothesis that the mean of population equals to the value you set"),
                               p("The observed t test statistic :"),
                               textOutput('tvalue'),
                               p("A low P value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population."),
                               textOutput('pvalue')))
      )
      
      
    )
  )
))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------- END ---------------------------- Shiny ------------------------- END -------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
