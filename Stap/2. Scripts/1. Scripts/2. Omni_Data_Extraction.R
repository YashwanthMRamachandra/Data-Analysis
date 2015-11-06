# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Staples : API Authentication
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
install_github("RSiteCatalyst","randyzwitch")
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

# Define time period/duration
date.from <- "2014-01-01"
date.to <- "2015-07-09"

# Get the element list
elements_list <- GetElements(reportsuite.id,metrics = "visits",elements = c("referringdomainoriginal","product"),
                             date.granularity = "day")
elements <- c("referringdomainoriginal","product")  # "referringdomainoriginal" apparently not working..
elements <- c("product")

# Get segment list
segments_list <- GetSegments(reportsuite.id)
segment.id <- "54496010e4b08a22507de424"

# Get metric lists
metrics_list <- GetMetrics(reportsuite.id)
metrics <- c("visits","revenue")
date.granularity <- "day"

# Get Classification list
classification_list <- GetClassifications("staplescomprod",elements)
classification <- "product"


# Queue data: "QueueTrended" is most recommended
system.time(report.data_QO <- QueueOvertime(reportsuite.id, date.from, date.to, metrics,date.granularity,segment.id))

system.time(report.data_QR <- QueueRanked(reportsuite.id, date.from, date.to, metrics,elements,segment.id = segment.id,
                                          search = "google"))

system.time(report.data_QT <- QueueTrended(reportsuite.id, date.from, date.to, metrics,
                                elements = c("product"),
                                segment.id = segment.id,date.granularity = date.granularity))

# Reference : https://github.com/randyzwitch/RSiteCatalyst

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------- END -------------------------- Staples : API Authentication ------------- END --------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
