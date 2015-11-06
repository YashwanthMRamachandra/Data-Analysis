# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                            Review Aggregation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list = ls())
SA_Input_data <- read.table("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/9. All Category files/SA_Input.csv",
                            header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
SA_Input_data$Review_Creation_Date <- as.Date(SA_Input_data$Review_Creation_Date)
SA_Input_data$Review_Count <- as.numeric(SA_Input_data$Review_Count)
SA_Input_data$Products <- as.factor(SA_Input_data$Products)
SA_Input <- SA_Input_data

#---------------------------------------------------------------------------------------------------------------
#                                           Activity Monitor
#---------------------------------------------------------------------------------------------------------------

library(data.table)
SA_Input <- data.table(SA_Input)

# Subset data for Activity Monitors
Activity_Monitors <- SA_Input[Category_Name=="Activity_Monitors"]

# Monthly Aggregation
library(lubridate)
Activity_Monitors <- data.table(Activity_Monitors)
Activity_Monitors$Monthly <- floor_date(Activity_Monitors$Review_Creation_Date,"month")
Activity_Monitors_Monthly <- Activity_Monitors[,sum(Review_Count),by=.(Monthly,Products)]
Activity_Monitors_Monthly <- Activity_Monitors_Monthly[with(Activity_Monitors_Monthly,order(Activity_Monitors_Monthly$Monthly)),]
names(Activity_Monitors_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Activity_Monitors_Quarterly <- Activity_Monitors_Monthly
Activity_Monitors_Quarterly$Dates <- as.character(Activity_Monitors_Quarterly$Dates)
Activity_Monitors_Quarterly$Quarters <-  with(Activity_Monitors_Quarterly,
                                              ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                     ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                            ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Activity_Monitors_Quarterly$Dates <- as.Date(Activity_Monitors_Quarterly$Dates)

names(Activity_Monitors_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Activity_Monitors_Quarterly <- Activity_Monitors_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Activity_Monitors_Monthly,Activity_Monitors)
Activity_Monitors_Quarterly$Category <- "Activity_Monitors"

#---------------------------------------------------------------------------------------------------------------
#                                            Baseball Bat
#---------------------------------------------------------------------------------------------------------------

# Subset data for Baseball Bats
Baseball_bats <- SA_Input[Category_Name=="Baseball_Bats"]

# Monthly Aggregation
Baseball_bats <- data.table(Baseball_bats)
Baseball_bats$Monthly <- floor_date(Baseball_bats$Review_Creation_Date,"month")
Baseball_bats_Monthly <- Baseball_bats[,sum(Review_Count),by=.(Monthly,Products)]
Baseball_bats_Monthly <- Baseball_bats_Monthly[with(Baseball_bats_Monthly,order(Baseball_bats_Monthly$Monthly)),]
names(Baseball_bats_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Baseball_bats_Quarterly <- Baseball_bats_Monthly
Baseball_bats_Quarterly$Dates <- as.character(Baseball_bats_Quarterly$Dates)
Baseball_bats_Quarterly$Quarters <-  with(Baseball_bats_Quarterly,
                                          ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                 ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                        ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Baseball_bats_Quarterly$Dates <- as.Date(Baseball_bats_Quarterly$Dates)

names(Baseball_bats_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Baseball_bats_Quarterly <- Baseball_bats_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Baseball_bats_Monthly,Baseball_bats)
Baseball_bats_Quarterly$Category <- "Baseball_bats"

#---------------------------------------------------------------------------------------------------------------
#                                               Cleats 
#---------------------------------------------------------------------------------------------------------------

# Subset data for Activity Monitors
Cleats <- SA_Input[Category_Name=="Cleats"]

# Monthly Aggregation
Cleats <- data.table(Cleats)
Cleats$Monthly <- floor_date(Cleats$Review_Creation_Date,"month")
Cleats_Monthly <- Cleats[,sum(Review_Count),by=.(Monthly,Products)]
Cleats_Monthly <- Cleats_Monthly[with(Cleats_Monthly,order(Cleats_Monthly$Monthly)),]
names(Cleats_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Cleats_Quarterly <- Cleats_Monthly
Cleats_Quarterly$Dates <- as.character(Cleats_Quarterly$Dates)
Cleats_Quarterly$Quarters <-  with(Cleats_Quarterly,
                                   ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                          ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                 ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Cleats_Quarterly$Dates <- as.Date(Cleats_Quarterly$Dates)

names(Cleats_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Cleats_Quarterly <- Cleats_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Cleats_Monthly,Cleats)
Cleats_Quarterly$Category <- "Cleats"

#---------------------------------------------------------------------------------------------------------------
#                                           Golf Complete Sets
#---------------------------------------------------------------------------------------------------------------

# Subset data for Golf Complete Sets
Golf_Complete_Sets <- SA_Input[Category_Name=="Golf_Complete_Sets"]

# Monthly Aggregation
Golf_Complete_Sets <- data.table(Golf_Complete_Sets)
Golf_Complete_Sets$Monthly <- floor_date(Golf_Complete_Sets$Review_Creation_Date,"month")
Golf_Complete_Sets_Monthly <- Golf_Complete_Sets[,sum(Review_Count),by=.(Monthly,Products)]
Golf_Complete_Sets_Monthly <- Golf_Complete_Sets_Monthly[with(Golf_Complete_Sets_Monthly,order(Golf_Complete_Sets_Monthly$Monthly)),]
names(Golf_Complete_Sets_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Golf_Complete_Sets_Quarterly <- Golf_Complete_Sets_Monthly
Golf_Complete_Sets_Quarterly$Dates <- as.character(Golf_Complete_Sets_Quarterly$Dates)
Golf_Complete_Sets_Quarterly$Quarters <-  with(Golf_Complete_Sets_Quarterly,
                                               ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                      ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                             ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Golf_Complete_Sets_Quarterly$Dates <- as.Date(Golf_Complete_Sets_Quarterly$Dates)

names(Golf_Complete_Sets_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Golf_Complete_Sets_Quarterly <- Golf_Complete_Sets_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Golf_Complete_Sets_Monthly,Golf_Complete_Sets)
Golf_Complete_Sets_Quarterly$Category <- "Golf_Complete_Sets"

#---------------------------------------------------------------------------------------------------------------
#                                           NFL Apparel
#---------------------------------------------------------------------------------------------------------------

# Subset data for NFL Apparels
NFL_Apparels <- SA_Input[Category_Name=="NFL_Apparels"]

# Monthly Aggregation
NFL_Apparels <- data.table(NFL_Apparels)
NFL_Apparels$Monthly <- floor_date(NFL_Apparels$Review_Creation_Date,"month")
NFL_Apparels_Monthly <- NFL_Apparels[,sum(Review_Count,na.rm = T),by=.(Monthly,Products)]
NFL_Apparels_Monthly <- NFL_Apparels_Monthly[with(NFL_Apparels_Monthly,order(NFL_Apparels_Monthly$Monthly)),]
names(NFL_Apparels_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
NFL_Apparels_Quarterly <- NFL_Apparels_Monthly
NFL_Apparels_Quarterly$Dates <- as.character(NFL_Apparels_Quarterly$Dates)
NFL_Apparels_Quarterly$Quarters <-  with(NFL_Apparels_Quarterly,
                                         ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                       ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
NFL_Apparels_Quarterly$Dates <- as.Date(NFL_Apparels_Quarterly$Dates)

names(NFL_Apparels_Quarterly) <- c("Dates","Products","Reviews","Quarters")
NFL_Apparels_Quarterly <- NFL_Apparels_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(NFL_Apparels_Monthly,NFL_Apparels)
NFL_Apparels_Quarterly$Category <- "NFL_Apparels"

#---------------------------------------------------------------------------------------------------------------
#                                           Training Aid
#---------------------------------------------------------------------------------------------------------------

# Subset data for Training Aid
Training_Aid <- SA_Input[Category_Name=="Training_Aid"]

# Monthly Aggregation
Training_Aid <- data.table(Training_Aid)
Training_Aid$Monthly <- floor_date(Training_Aid$Review_Creation_Date,"month")
Training_Aid_Monthly <- Training_Aid[,sum(Review_Count),by=.(Monthly,Products)]
Training_Aid_Monthly <- Training_Aid_Monthly[with(Training_Aid_Monthly,order(Training_Aid_Monthly$Monthly)),]
names(Training_Aid_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Training_Aid_Quarterly <- Training_Aid_Monthly
Training_Aid_Quarterly$Dates <- as.character(Training_Aid_Quarterly$Dates)
Training_Aid_Quarterly$Quarters <-  with(Training_Aid_Quarterly,
                                         ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                       ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Training_Aid_Quarterly$Dates <- as.Date(Training_Aid_Quarterly$Dates)

names(Training_Aid_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Training_Aid_Quarterly <- Training_Aid_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Training_Aid_Monthly,Training_Aid)
Training_Aid_Quarterly$Category <- "Training_Aid"

#---------------------------------------------------------------------------------------------------------------
#                                           Treadmill
#---------------------------------------------------------------------------------------------------------------

# Subset data for Treadmills
Treadmills <- SA_Input[Category_Name=="Treadmills"]

# Monthly Aggregation
Treadmills <- data.table(Treadmills)
Treadmills$Monthly <- floor_date(Treadmills$Review_Creation_Date,"month")
Treadmills_Monthly <- Treadmills[,sum(Review_Count),by=.(Monthly,Products)]
Treadmills_Monthly <- Treadmills_Monthly[with(Treadmills_Monthly,order(Treadmills_Monthly$Monthly)),]
names(Treadmills_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Treadmills_Quarterly <- Treadmills_Monthly
Treadmills_Quarterly$Dates <- as.character(Treadmills_Quarterly$Dates)
Treadmills_Quarterly$Quarters <-  with(Treadmills_Quarterly,
                                       ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                              ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                     ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Treadmills_Quarterly$Dates <- as.Date(Treadmills_Quarterly$Dates)

names(Treadmills_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Treadmills_Quarterly <- Treadmills_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Treadmills_Monthly,Treadmills)
Treadmills_Quarterly$Category <- "Treadmills"

#---------------------------------------------------------------------------------------------------------------
#                                           Women's Running Shoes
#---------------------------------------------------------------------------------------------------------------

# Subset data for Womens Running Shoes
Womens_Running_Shoes <- SA_Input[Category_Name=="Womens_running_shoes"]

# Monthly Aggregation
Womens_Running_Shoes <- data.table(Womens_Running_Shoes)
Womens_Running_Shoes$Monthly <- floor_date(Womens_Running_Shoes$Review_Creation_Date,"month")
Womens_Running_Shoes_Monthly <- Womens_Running_Shoes[,sum(Review_Count,na.rm = T),by=.(Monthly,Products)]
Womens_Running_Shoes_Monthly <- Womens_Running_Shoes_Monthly[with(Womens_Running_Shoes_Monthly,order(Womens_Running_Shoes_Monthly$Monthly)),]
names(Womens_Running_Shoes_Monthly) <- c("Dates","Product","Reviews")

# Quaterly Aggregation : Dates -  From : 2014-04-01  to  2015-03-31
Womens_Running_Shoes_Quarterly <- Womens_Running_Shoes_Monthly
Womens_Running_Shoes_Quarterly$Dates <- as.character(Womens_Running_Shoes_Quarterly$Dates)
Womens_Running_Shoes_Quarterly$Quarters <-  with(Womens_Running_Shoes_Quarterly,
                                                 ifelse(Dates >= "2014-04-01"  & Dates <= "2014-06-31","Q1",
                                                        ifelse(Dates >= "2014-07-01" & Dates <= "2014-09-31","Q2",
                                                               ifelse(Dates >= "2014-10-01" & Dates <= "2014-12-31","Q3","Q4"))))
Womens_Running_Shoes_Quarterly$Dates <- as.Date(Womens_Running_Shoes_Quarterly$Dates)

names(Womens_Running_Shoes_Quarterly) <- c("Dates","Products","Reviews","Quarters")
Womens_Running_Shoes_Quarterly <- Womens_Running_Shoes_Quarterly[,.(Reviews=sum(Reviews)),by = .(Products,Quarters)]
rm(Womens_Running_Shoes_Monthly,Womens_Running_Shoes)
Womens_Running_Shoes_Quarterly$Category <- "Womens_Running_Shoes"

#-----------------------------------------------------------------------------------------------------------------------------------------------
#                                           Export Output
#-----------------------------------------------------------------------------------------------------------------------------------------------

library(openxlsx)
setwd("C:/Yashwanth/Sports Authority/3. Output/")
write.xlsx(Activity_Monitors_Quarterly,"Reviews_Ouput_201404_201503.xlsx",row.names=FALSE,sheetName="Activity Monitor")

write.xlsx(Baseball_bats_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Baseball Bat",append=TRUE)

write.xlsx(Cleats_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Cleats",append=TRUE)

write.xlsx(Golf_Complete_Sets_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Golf Complete Sets",append=TRUE)

write.xlsx(NFL_Apparels_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="NFL Apparel",append=TRUE)

write.xlsx(Training_Aid_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Training Aids",append=TRUE)

write.xlsx(Treadmills_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Treadmill",append=TRUE)

write.xlsx(Womens_Running_Shoes_Quarterly,"Reviews_Ouput_201404_201503.xlsx", row.names=FALSE,sheetName="Women's Running Shoes",append=TRUE)

Review_Aggr_Data <- rbind(Activity_Monitors_Quarterly,Baseball_bats_Quarterly,Cleats_Quarterly,
                          Golf_Complete_Sets_Quarterly,NFL_Apparels_Quarterly,Training_Aid_Quarterly,
                          Treadmills_Quarterly,Womens_Running_Shoes_Quarterly)

write.xlsx(Review_Aggr_Data,"C:/Yashwanth/Sports Authority/3. Output/All_Cat_Reviews_Ouput_201404_201503_v2.xlsx", row.names=FALSE,sheetName="All Categories")

# # Rbind all Categories
# All_Files <- list(ls(pattern = "Mapped"))
# Review_Aggr_Data <- data.frame()
# for(i in seq(along = All_Files)){
#   Review_In_Data <- data.frame(i)
#   if(is.null(Review_In_Data)){Review_Aggr_Data = Review_In_Data}
#   else{Review_Aggr_Data = rbind(Review_Aggr_Data , Review_In_Data)}
# }

# ---------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------- END ------------------------ Review Aggregation -------------------------- END -------------------                                   
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# SA_Input$Retailer <- as.factor(SA_Input$Retailer)
# levels(SA_Input$Retailer)[levels(SA_Input$Retailer)=="Dicks Sporting Goods" | levels(SA_Input$Retailer)=="Dick's Sporting Goods"] <- "DSG"
# levels(SA_Input$Retailer)[levels(SA_Input$Retailer)=="Roadrunnersports" | levels(SA_Input$Retailer)=="Road Runner Sports"] <- "RoadrunnerSports"
# levels(SA_Input$Retailer)[levels(SA_Input$Retailer)=="Sportsauthority" | levels(SA_Input$Retailer)=="Sports Authority"] <- "SportsAuthority"
# levels(SA_Input$Retailer)[levels(SA_Input$Retailer)=="Golf Galaxy"] <- "GolfGalaxy"
