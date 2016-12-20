
#initialize
library(datasets)
library(ggplot2) 

#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

# shiny server side code for each call
shinyServer(function(input, output, session){
  #update variable and group based on dataset
  output$variable <- renderUI({ 
    obj<-switch(input$dataset,
                "iris" = iris,
                "mtcars" = mtcars)	 
    var.opts<-namel(colnames(obj))
    selectInput("variable","Variable:", var.opts) # uddate UI 				 
  }) 
  
  output$group <- renderUI({ 
    obj<-switch(input$dataset,
                "iris" = iris,
                "mtcars" = mtcars)	 
    var.opts<-namel(colnames(obj))
    selectInput("group","Groups:", var.opts) # uddate UI 				 
  }) 
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    plot.obj<<-list() # not sure why input$X can not be used directly?
    plot.obj$data<<-get(input$dataset) 
    plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
    plot.obj$group<<-with(plot.obj$data,get(input$group)) 
    
    #dynamic plotting options
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= 	geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge")
    )
    
    require(ggplot2)
    #plotting theme
    .theme<- theme(
      axis.line = element_line(colour = 'gray', size = .75), 
      panel.background = element_blank(),  
      plot.background = element_blank()
    )	 
    if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$group, 
                  y 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group)
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      { 
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data, 
                aes(
                  x 		= plot.obj$variable,
                  fill 	= as.factor(plot.obj$group),
                  group 	= as.factor(plot.obj$group),
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      .theme
    print(p)
  })	
})