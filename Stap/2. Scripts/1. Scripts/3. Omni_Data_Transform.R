# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Staples : API Authentication
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
# install_github("RSiteCatalyst","randyzwitch")
library(RSiteCatalyst)
library(devtools)

# Call SCAuth to set credentials before usage  ##
SCAuth("Skerch:Staples","60f8ce25dde9213bad36e04f3646ac2b")

# # Check credentials are working properly ##
# GetTokenCount()

# Get ReportSuites to find out what report suites are available to your username
report_suites <- GetReportSuites()
reportsuite.id <- "staplescomprod"

# # GetAvailableElements - Useful for understanding what breakdown available for use in Reporting API
# elements <- GetAvailableElements(report_suites$rsid)

# Extract Data
date.from <- "2014-01-01"
date.to <- "2015-07-09"

elements_list <- GetElements(reportsuite.id,metrics = "Orders",elements = c("referringdomainoriginal","product"),
                             date.granularity = "day")
elements <- c("referringdomainoriginal","product")  # "referringdomainoriginal" apparently not working..
elements <- c("product")

segments_list <- GetSegments(reportsuite.id)
segment.id <- "54496010e4b08a22507de424"

metrics_list <- GetMetrics(reportsuite.id)
metrics <- c("Orders","Orders")
date.granularity <- "day"

classification_list <- GetClassifications("staplescomprod",elements)
classification <- "product"


system.time(report.data_QO <- QueueOvertime(reportsuite.id, date.from, date.to, metrics,date.granularity,segment.id))

system.time(report.data_QR <- QueueRanked(reportsuite.id, date.from, date.to, metrics,elements,segment.id = segment.id,
                                          search = "google"))

# source("C:/Users/yashwanth.r/Downloads/RSiteCatalyst-master/R/QueueTrended.R")
system.time(report.data_QT <- QueueTrended(reportsuite.id, date.from, date.to, metrics,
                                elements = c("product"),
                                segment.id = segment.id,date.granularity = date.granularity))

# Reference : https://github.com/randyzwitch/RSiteCatalyst

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Staples : Data Transformation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(reshape)
library(data.table)
Orders <- read.table("C:/Yashwanth/Staples PE/1. Input/Input by domain/3. Direct/Input/Cart_Views.csv",
                         header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Orders <- melt(Orders,id="Product")
Orders <- data.table(Orders)
Orders$Product <- as.factor(Orders$Product)
Orders <- Orders[with(Orders,order(Product)),]
Orders <- data.frame(Orders)
colnames(Orders) <- c("Product","Dates","Orders")
#Orders$Orders <- round(Orders$Orders,4)
View(head(Orders))

write.csv(Orders,"C:/Yashwanth/Staples PE/1. Input/Input by domain/3. Direct/Output/Cart_Views.csv",row.names = FALSE)


# write.xlsx(Orders,"C:/Yashwanth/Staples PE/1. Input/Input by domain/Output/Orders.xlsx",
#            row.names=FALSE,sheetName="Orders",append=TRUE)


# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
# library(rJava)
# library(XLConnect)               # load XLConnect package 
# library(XLConnectJars)
# library(xlsx)
# options(java.parameters = "-Xmx4g" )
# options(java.parameters = "-Xmx1024m")
# 
# file <- system.file("demoFiles", "Direct_Vis_Rev_Orders.xlsx", package = "XLConnect")
# file_load <- loadWorkbook(file)
# 
# library(reshape)
# library(data.table)
# 
# Orders <- readWorksheet(file_load , sheet="Orders")

