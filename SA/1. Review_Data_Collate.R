#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                         Concatenation of files
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(openxlsx)
#---------------------------------------------------------------------------------------------------------------
#                                         Activity Monitors
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/1. activity monitors/")

All_Files <- list.files(pattern = "*.xlsx")
Activity_Monitors <- NULL
for(i in All_Files){
  AM_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(AM_In_data)){Activity_Monitors = AM_In_data}
  else{Activity_Monitors = rbind(Activity_Monitors,AM_In_data)}
} ; rm(AM_In_data)

Activity_Monitors$Review_Creation_Date <- as.Date(as.numeric(Activity_Monitors$Review_Creation_Date),origin = "1899-12-30")
Activity_Monitors$Category_Name <- "Activity_Monitors"
write.csv(Activity_Monitors,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/1. Activity Monitors/Activity_Monitors.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                         Baseball Bats
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/2. baseball bats/")

All_Files <- list.files(pattern = "*.xlsx")
Baseball_Bats <- NULL
for(i in All_Files){
  BB_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(BB_In_data)){Baseball_Bats = BB_In_data}
  else{Baseball_Bats = rbind(Baseball_Bats,BB_In_data)}
} ; rm(BB_In_data)

Baseball_Bats$Review_Creation_Date <- as.Date(as.numeric(Baseball_Bats$Review_Creation_Date),origin = "1899-12-30")
Baseball_Bats$Category_Name <- "Baseball_Bats"
write.csv(Baseball_Bats,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/2. Baseball Bat/Baseball_Bats.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                             Cleats
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/3. cleats/")

All_Files <- list.files(pattern = "*.xlsx")
Cleats <- NULL
for(i in All_Files){
  Cleats_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(Cleats_In_data)){Cleats = Cleats_In_data}
  else{Cleats = rbind(Cleats,Cleats_In_data)}
} ; rm(Cleats_In_data)

Cleats$Review_Creation_Date <- as.Date(as.numeric(Cleats$Review_Creation_Date),origin = "1899-12-30")
Cleats$Category_Name <- "Cleats"
write.csv(Cleats,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/3. Cleats/Cleats.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                               Golf Complete Sets
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/4. golf complete sets/")

All_Files <- list.files(pattern = "*.xlsx")
Golf_Complete_Sets <- NULL
for(i in All_Files){
  GCS_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(GCS_In_data)){Golf_Complete_Sets = GCS_In_data}
  else{Golf_Complete_Sets = rbind(Golf_Complete_Sets,GCS_In_data)}
} ; rm(GCS_In_data)

Golf_Complete_Sets$Review_Creation_Date <- as.Date(as.numeric(Golf_Complete_Sets$Review_Creation_Date),origin = "1899-12-30")
Golf_Complete_Sets$Category_Name <- "Golf_Complete_Sets"
write.csv(Golf_Complete_Sets,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/4. Golf Complete Sets/Golf_Complete_Sets.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                                 NFL Apparels
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/5. nfl apparel/")

All_Files <- list.files(pattern = "*.xlsx")
NFL_Apparels <- NULL
for(i in All_Files){
  NFL_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(NFL_In_data)){NFL_Apparels = NFL_In_data}
  else{NFL_Apparels = rbind(NFL_Apparels,NFL_In_data)}
} ; rm(NFL_In_data)

NFL_Apparels$Review_Creation_Date <- as.Date(as.numeric(NFL_Apparels$Review_Creation_Date),origin = "1899-12-30")
NFL_Apparels$Category_Name <- "NFL_Apparels"
write.csv(NFL_Apparels,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/5. NFL Apparel/NFL_Apparels.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                             Training Aids
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/6. training aids/")

All_Files <- list.files(pattern = "*.xlsx")
Training_Aids <- NULL
for(i in All_Files){
  TA_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(TA_In_data)){Training_Aids = TA_In_data}
  else{Training_Aids = rbind(Training_Aids,TA_In_data)}
} ; rm(TA_In_data)

Training_Aids$Review_Creation_Date <- as.Date(as.numeric(Training_Aids$Review_Creation_Date),origin = "1899-12-30")
Training_Aids$Category_Name <- "Training_Aids"
write.csv(Training_Aids,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/6. Training Aid/Training_Aids.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                             Treadmills
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/7. treadmills/")

All_Files <- list.files(pattern = "*.xlsx")
Treadmills <- NULL
for(i in All_Files){
  TM_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(TM_In_data)){Treadmills = TM_In_data}
  else{Treadmills = rbind(Treadmills,TM_In_data)}
} ; rm(TM_In_data)

Treadmills$Review_Creation_Date <- as.Date(as.numeric(Treadmills$Review_Creation_Date),origin = "1899-12-30")
Treadmills$Category_Name <- "Treadmills"
write.csv(Treadmills,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/7. Treadmill/Treadmills.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                         Women's Running Shoes
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/8. women running shoes/")

All_Files <- list.files(pattern = "*.xlsx")
Womens_Running_Shoes <- NULL
for(i in All_Files){
  WRS_In_data <- read.xlsx(i,sheet = "data")
  if(is.null(WRS_In_data)){Womens_Running_Shoes = WRS_In_data}
  else{Womens_Running_Shoes = rbind(Womens_Running_Shoes,WRS_In_data)}
} ; rm(WRS_In_data)

Womens_Running_Shoes$Review_Creation_Date <- as.Date(as.numeric(Womens_Running_Shoes$Review_Creation_Date),origin = "1899-12-30")
Womens_Running_Shoes$Category_Name <- "Womens_Running_Shoes"
write.csv(Womens_Running_Shoes,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/8. Women's Running Shoes/Womens_Running_Shoes.csv",
          row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                         Rbind files
#---------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/9. All Category files/")

All_Files <- list.files(pattern = "*.csv")
SA_Input <- NULL
for(i in All_Files){
  SA_Input_In_data <- read.csv(i,stringsAsFactors = FALSE,header = TRUE, sep = ",", quote = "\"", fill=TRUE, 
                               comment.char="", as.is=TRUE)
  if(is.null(SA_Input_In_data)){SA_Input = SA_Input_In_data}
  else{SA_Input = rbind(SA_Input,SA_Input_In_data)}
} ; rm(SA_Input_In_data)

#write.csv(data,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/8. Training Aid/Training_Aid.csv",row.names = F)

#---------------------------------------------------------------------------------------------------------------
#                                             Filter data files
#---------------------------------------------------------------------------------------------------------------

Activity_Monitors <- SA_Input[Category_Name=="Activity_Monitors"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Baseball_Bats <- SA_Input[Category_Name=="Baseball_Bats"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Cleats <- SA_Input[Category_Name=="Cleats"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Golf_Complete_Sets <- SA_Input[Category_Name=="Golf_Complete_Sets"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
NFL_Apparels <- SA_Input[Category_Name=="NFL_Apparels"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Training_Aids <- SA_Input[Category_Name=="Training_Aids"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Treadmills <- SA_Input[Category_Name=="Treadmills"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]
Womens_Running_Shoes <- SA_Input[Category_Name=="Womens_Running_Shoes"][Review_Creation_Date>="2014-04-01" & Review_Creation_Date<="2015-03-31"]

write.csv(Activity_Monitors,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/1. Activity Monitors/Activity_Monitors.csv",row.names = FALSE)
write.csv(Baseball_Bats,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/2. Baseball Bat/Baseball_Bats.csv",row.names = FALSE)
write.csv(Cleats,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/3. Cleats/Cleats.csv",row.names = FALSE)
write.csv(Golf_Complete_Sets,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/4. Golf Complete Sets/Golf_Complete_Sets.csv",row.names = FALSE)
write.csv(NFL_Apparels,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/5. NFL Apparel/NFL_Apparels.csv",row.names = FALSE)
write.csv(Training_Aids,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/6. Training Aid/Training_Aids.csv",row.names = FALSE)
write.csv(Treadmills,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/7. Treadmill/Treadmills.csv",row.names = FALSE)
write.csv(Womens_Running_Shoes,"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/8. Women's Running Shoes/Womens_Running_Shoes.csv",row.names = FALSE)

# ------------------------------------------------------------------------------------------------------------------------------------
# ------------------ END ----------------------------- Concatenation of files ------------------- END --------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------

list <- ls()
Names <- NULL
for(i in list){
  Names_data <- names(i)
  if(is.null(Names_data)){Names = Names_data}
  else{Names = cbind(Names,Names_data)}
}

Names <- data.frame(names(Activity_Monitors),names(Baseball_Bats),names(Cleats),names(Golf_Complete_Sets),names(NFL_Apparels),
                    names(Training_Aids),names(Treadmills),names(Womens_Running_Shoes))

# rm(list=ls())
# NFL_Apparels <- list.files(pattern = "*.xlsx")
# for (i in 1:length(NFL_Apparels)) {
#   assign(NFL_Apparels[i], read.xlsx(NFL_Apparels[i], sheet="data"))
# }
# gsub("\\.xlsx","",ls())

# data1 <- read.xlsx("Amazon_Bestseller_Review_Training_Aid.xlsx", sheet="data");
# names(data1) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# 
# data2 <- read.xlsx("Amazon_Training_Aids_Review.xlsx", sheet="data");data2$ReviewID <- NULL
# names(data2) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# 
# # data3 <- read.xlsx("Bestseller_Training_Aids.xlsx", sheet=1)
# # names(data3) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# 
# data4 <- read.xlsx("DSG_TrainingAids_Review.xlsx", sheet="data")
# names(data4) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# 
# data5 <- read.xlsx("Sports_Authority_Training_Aids_Review.xlsx", sheet="data")
# names(data5) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# 
# # data6 <- read.xlsx("Footlocker_Review.xlsx", sheet=1)
# # names(data6) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data7 <- read.xlsx("Roadrunnersports_Review.xlsx", sheet="Roadrunnersports_Review")
# # names(data7) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data8 <- read.xlsx("SA_Adult_cleats_Review.xlsx", sheet="Sports_Authority_Review")
# # names(data8) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # #data9 <- read.xlsx("SA_AdultCleat_Review_BS.xlsx", sheet=1)
# # #names(data9) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # #data10 <- read.xlsx("SA_Amazon_Adult_Cleats_Best_Seller_Review.xlsx", sheet=1)
# # names(data10) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data11 <- read.xlsx("SA_Amazon_Adult_cleats_Review.xlsx", sheet="data");data11$ReviewID <- NULL
# # names(data11) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # 
# # data12 <- read.xlsx("SA_Amazon_Kids_Cleats_Best_Seller_Review.xlsx", sheet="data"); data12$ReviewID <- NULL
# # names(data12) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # #data13 <- read.xlsx("SA_Amazon_Kids_cleats_Review.xlsx", sheet="data")
# # names(data13) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data14 <- read.xlsx("SA_DSG_bestseller_cleats_review_final.xlsx", sheet="amazone_bestseller_adult_cleats")
# # names(data14) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data15 <- read.xlsx("SA_Eastbay_Adult_Cleats_Best_Seller_Review.xlsx", sheet=1)
# # names(data15) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data16 <- read.xlsx("SA_Eastbay_Kids_Cleats_Best_Seller_Review.xlsx", sheet="data")
# # names(data16) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data17 <- read.xlsx("SA_Footlocker_AdultCleat_Review_BS.xlsx", sheet=1)
# # names(data17) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data18 <- read.xlsx("SA_Footlocker_Kids_Cleats_Best_Seller_Review.xlsx", sheet="SA_Footlocker_KidsCleat_Review_")
# # names(data18) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data19 <- read.xlsx("SA_Kids_Cleats_Best_Seller_Review.xlsx", sheet="Sports_Authority_Review")
# # names(data19) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data20 <- read.xlsx("SA_Zappos_Adult_cleats_Review.xlsx", sheet="Zappos_Adult_cleats_Review")
# # names(data20) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data21 <- read.xlsx("SA_Zappos_AdultCleat_Review_BS.xlsx", sheet="Zappos_Review_Loop_adults")
# # names(data21) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data22 <- read.xlsx("SA_Zappos_Kids_Cleats_Best_Seller_Review.xlsx", sheet="Zappos_KidsCleats_Review")
# # names(data22) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data23 <- read.xlsx("SportsAuthority_Review.xlsx", sheet="data")
# # names(data23) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
# # 
# # data24 <- read.xlsx("Zappos_Review.xlsx", sheet="data")
# # names(data24) <- c("UgamID","Category_Path","Retailer","pt_division","pt_category","pt_department","pt_class","pt_subclass","pt_product_url","Brand","Product_Name","Unique_Product_Identifier_Mainproduct","Unique_Product_Identifier_Variant","Final_Price_after_Discount","Extraction_Date","Review:Link","Review:Header","Review:Creation_Date","Review:From","Review:By","Review:Helpful_Votes","Review:Total_Votes","Review:Rating_Score","Review:Rating_Scale","Review:Reviewers_Badges","Review_Text","Review:Count_Of_Comments","Review:Verified_Purchase","Reviewer_Age","Reviewer_Gender","Review_Count","Product_reviews")
