
if(input$latency=="Monthly" && input$Demand_Loyalty=="HIGH")
	{data=subset(Staples_Aggr_Monthly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else if(input$latency=="Monthly" && input$Demand_Loyalty=="MID")
	{data=subset(Staples_Aggr_Monthly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else 
	{data=subset(Staples_Aggr_Monthly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	
if(input$latency=="Weekly" && input$Demand_Loyalty=="HIGH")
	{data=subset(Staples_Aggr_Weekly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else if(input$latency=="Weekly" && input$Demand_Loyalty=="MID")
	{data=subset(Staples_Aggr_Weekly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else 
	{data=subset(Staples_Aggr_Weekly,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	
if(input$latency=="Daily" && input$Demand_Loyalty=="HIGH")
	{data=subset(Staples_Daily,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else if(input$latency=="Daily" && input$Demand_Loyalty=="MID")
	{data=subset(Staples_Daily,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else 
	{data=subset(Staples_Daily,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}

if(input$latency=="By Product" && input$Demand_Loyalty=="HIGH")
	{data=subset(Staples_Aggr_Monthly_by_Prod,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else if(input$latency=="By Product" && input$Demand_Loyalty=="MID")
	{data=subset(Staples_Aggr_Monthly_by_Prod,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}
	else 
	{data=subset(Staples_Aggr_Monthly_by_Prod,Loyalty_Cat==input$Demand_Loyalty)
	cor(data[,c("Conversion","Price_Comp")])
	}