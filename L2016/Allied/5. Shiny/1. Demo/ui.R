
shinyUI(pageWithSidebar(
  # title
  headerPanel("Select Options"),
  
  #input
  sidebarPanel(
    # About Lowes Canada : help(condtionalPanel)
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
    conditionalPanel(selectInput("dataset","Data:", 
              list(Sample = "Sample", Population = "Population"))),
              
    # Correlation
    conditionalPanel(condition = "$('li.active a').first().html()==='Correlation'",
             selectInput('Quarter_Cor',"Select Quarter :",choices = Quarter)),
    
    # Plot
    conditionalPanel(selectInput("plot.type","Plot Type:", 
                list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
    )),
    
  uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
  uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
  
  checkboxInput("show.points", "show points", TRUE)
  ),	
  
  
  # output				
  mainPanel(
    h3(textOutput("caption")),
    #h3(htmlOutput("caption")),
    uiOutput("plot") # depends on input 
  )
))