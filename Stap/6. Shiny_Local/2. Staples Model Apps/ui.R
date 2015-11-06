# Shiny UI Template
library(shiny)
library(shinythemes)
setwd("C:/Yashwanth/Staples PE/6. Shiny_Local/2. Staples Model Apps/")

Product_ID <- c("36619","37927","51168","117044","125369","196147","202184","205112","205138","205146","209908","257444","263236","272153","277582","301754","306647","324013","356338","358136","359265","365060","372623","374619","377179","377180","379168","391431","392941","404145","414392","415489","418668","424576","428331","429174","443685","452823","461374","462846","469080","476883","478887","479057","489574","492757","494224","502336","502351","504308","508581","511947","512427","514510","517011","517988","519668","538249","559215","563125","576947","580823","585006","597444","611549","611556","611562","616044","617433","618374","627963","638571","642736","648102","648103","648186","648370","648850","649692","653417","654684","656499","656500","658923","665698","665699","679088","683795","701218","712994","713993","721455","727093","727095","731565","735273","735277","735291","735305","738002","739213","739229","739318","741186","743478","752370","754639","757288","757289","760047","760476","762757","763216","771980","775751","775753","779330","782192","782504","783415","787361","790113","792257","800776","804574","806123","806124","806126","807747","812006","812010","812048","812327","853300","859520","859605","863061","863619","869097","886404","889138","889863","890785","891404","891405","896998","905412","917860","917881","917883","917917","931359","954076","956112")
Loyalty_Cat <- c("HIGH","MID","LOW")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("Staples : Price Sensitivity Model"),
  
  sidebarPanel(# About Staples : help(condtionalPanel)
    conditionalPanel(condition = "$('li.active a').first().html()==='Staples'",
                     h2("About Staples"),
                     p("Staples, Inc. is a large United States-based office supply chain store with over 2,000 stores worldwide in 26 countries. Headquartered in Framingham, Massachusetts, the company has retail stores serving customers under its original name in Australia, Austria, Brazil, China, Finland, France, Germany, India, Italy, Norway, Portugal, the United Kingdom, and the United States, while operating subsidiaries in Argentina as Officenet-Staples,"),
                     p("In 1991, Staples founded its Canadian subsidiary, The Business Depot and began opening stores under that name,[citation needed] though over a decade later, all stores were renamed as Staples. The first store opened in Vaughan, Ontario, Canada north of Toronto. The following year, Staples began expanding into Europe, and opened its first store in Swansea, United Kingdom."),
                     p("Throughout most of the company's history, Staples employed, in its American commercials and advertising promotions, the slogan Yeah, we've got that., signifying their wide selection of products."),
                     br(),
                     tags$a(href = "https://en.wikipedia.org/wiki/Staples_Inc.", "More details on Staples"),
                     br(),br(),
                     tags$a(href = "https://twitter.com/Staples?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", "Follow @Staples"),
                     br(),
                     br()),
    # Data View
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
        selectInput('latency_DV',"Select cycle :",choices = c("Monthly","Daily","Weekly","By Product"))),
    
    # Correlation
    conditionalPanel(condition = "$('li.active a').first().html()==='Correlation'",
        selectInput('latency_Cor',"Select cycle :",choices = c("Monthly","Daily","Weekly","By Product")),
        selectInput('Demand_Loyalty',' Select Loyalty : ',Loyalty_Cat)),
    
    # Plots
    conditionalPanel(condition  = "$('li.active a').first().html()==='Plots'")
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Staples',
               img(src = "Staples_1.png", height = 172, width = 250),img(src = "Staples_2.png", height = 172, width = 250),
               img(src = "Staples_3.png", height = 172, width = 250),img(src = "Staples_4.png", height = 172, width = 250),
               img(src = "Staples_5.png", height = 172, width = 250),img(src = "Staples_6.png", height = 172, width = 250),
               img(src = "Staples_7.png", height = 172, width = 250),img(src = "Staples_8.png", height = 172, width = 250),
               img(src = "Staples_9.png", height = 172, width = 250)),
      tabPanel('Data View',column(12,offset = 1,h2('Data Input'),dataTableOutput('table'))),
      tabPanel('Correlation',tableOutput("cor")),
      tabPanel('Plots',tableOutput("graphs"))
    ))
))