library(devtools)
library(shinyapps)
library(shiny)
library(shinythemes)

Atrributes <- c("mpg","disp","hp","drat","wt","qsec","vs","am","gear","carb")
CYL <- c("4","6","8")

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel(title = h4("MTCars Demo")),
  
    sidebarPanel(
    selectInput('cat', "Select CYL category", choices = CYL),
    selectInput('var',"Select the Variable",choices = Atrributes),
    radioButtons('color',"Select color of the Histogram",choices = c("Green","Red","Yellow"), selected = "Green")
  ),

  mainPanel(
    plotOutput('Graphs')
  )
))

