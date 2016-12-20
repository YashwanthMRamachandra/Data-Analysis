# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                   Shiny
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                     Canada data EDA : ui.R : reactive
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(devtools)
#install_github('rstudio/shinyapps')
library(shinyapps)
library(shiny)
library(shinythemes)

Quarter <- c("1","2","3","4")
Variable <- c("CAD_Sales_Dollar","CAD_Sales_Dollar_Comp","Comp_Weekly_Sales_Units","Sales_Units","Cost","Inventory_On_Hand_Dollar","Inventory_On_Hand_Units","List_Price","Invoice_Count","Margin_Correction_Comp","Margin_Correction","Comp_Cost","Margin_Dollar","Margin_Dollar_Comp","Margin_Per","Avg_Selling_Price")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel(title = "Lowes Canada"), # h4("",align = center)
  
  sidebarPanel(# About Lowes Canada : help(condtionalPanel)
    conditionalPanel(condition = "$('li.active a').first().html()==='Lowes Canada'",
                     h2("About Lowes Canada"),
                     p("Lowe's Companies, Inc. is an American company that operates a chain of retail home improvement and appliance stores in the United States, Canada, and Mexico. Founded in 1946 in North Wilkesboro, North Carolina, the chain has 1,840 stores in the United States, Canada, and Mexico."),
                     p("The first Lowe's store, Lowe's North Wilkesboro Hardware, was first opened in North Wilkesboro, North Carolina in 1921 by Lucius Smith Lowe (1879 - 1940), who had Baptist background.When Lowe died in 1940, the business was inherited by his daughter Ruth, who sold the company to her brother Jim that same year. Jim took on Carl Buchan as a partner in 1943."),
                     p("The Lowe's corporate headquarters is located in Mooresville, North Carolina. The Mooresville facility, the newest Lowe's corporate campus, is 25 miles (40 km) north of Charlotte and contains a five story, 400,000-square-foot (37,000 m2) building along with two newer seven story buildings. The building has a central atrium and two office wings; the atrium houses a food court, a five story spiral staircase, and meeting and reception rooms. A 7-acre (2.8 ha) lake flows underneath the headquarters building."),
                     br(),
                     tags$a(href = "https://en.wikipedia.org/wiki/Lowe%27s", "More details on Lowes Canada"),
                     br(),br(),
                     tags$a(href = "https://twitter.com/Lowes?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", "Follow @Lowes"),
                     br(),br(),
                     tags$a(href = "https://www.lowes.ca/", "Shop @Lowesca"),
                     br(),
                     br()),
    
    # Data View
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
                     selectInput('Quarter_DV',"Select Quarter :",choices = Quarter)),
    
    # Correlation
    conditionalPanel(condition = "$('li.active a').first().html()==='Correlation'",
                     selectInput('Quarter_Cor',"Select Quarter :",choices = Quarter)),
    
    # Plots
    conditionalPanel(condition = "$('li.active a').first().html()==='Graphs'",
                     selectInput('Quarter_Gra',"Select Quarter :",choices = Quarter),
                     br(),
                     selectInput('Var',"Select Variable :", choices = Variable),
                     br(),
                     sliderInput('bins',"Select the bins", min = -5,max = 25,value =15),
                     br(),
                     radioButtons('color',"Select color of the Histogram", 
                                  choices = c("Green","Red","Yellow"), selected = "Green"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Lowes Canada',
               img(src = "LC_1.jpg", height = 172, width = 250),img(src = "LC_2.jpg", height = 172, width = 250),
               img(src = "LC_3.jpg", height = 172, width = 250),img(src = "LC_4.jpg", height = 172, width = 250),
               img(src = "LC_5.jpg", height = 172, width = 250),img(src = "LC_6.jpg", height = 172, width = 250),
               img(src = "LC_7.jpg", height = 172, width = 250),img(src = "LC_8.jpg", height = 172, width = 250),
               img(src = "LC_9.jpg", height = 172, width = 250)),
      tabPanel('Data View',column(12,offset = 1,h2('Data Input'),dataTableOutput('table'))),
      tabPanel('Correlation',tableOutput("corr")),
      tabPanel('Graphs',plotOutput("plots"))
    ))
))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------- END ---------------------------- Shiny ------------------------- END -------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------