# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Inventory EDA
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Inventory_IP <- read.table("C:/Yashwanth/2. Inventory/In transit week 48 MSD.csv",
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

str(Inventory_IP)

Inventory_IP$Business_Area <- as.factor(Inventory_IP$Business_Area)
Inventory_IP$Merchandise_Division <- as.factor(Inventory_IP$Merchandise_Division)
Inventory_IP$Merchandise_Sub_Division <- as.factor(Inventory_IP$Merchandise_Sub_Division)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
View(describe(Inventory_IP[,sapply(Inventory_IP,is.numeric)]))
#summary(Inventory_IP)
#summary(Inventory_IP[,sapply(Inventory_IP,is.numeric)])

# Correlation Matrix : All
cor(Inventory_IP[,sapply(Inventory_IP,is.numeric)])

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/
library(data.table)
# data.table 1.9.6  For help type ?data.table or https://github.com/Rdatatable/data.table/wiki
# The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way

# Distribution DC Landed
attach(Inventory_IP)
ggplot(Inventory_IP,aes(x=DC_landed)) +
  facet_wrap(~ Business_Area) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(DC_landed),sd=sd(DC_landed)),colour = "red") +
  labs(title = "DC_landed Distribution by Business Area")

ggplot(Inventory_IP,aes(x=DC_landed)) +
  facet_wrap(~ Merchandise_Division) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(DC_landed),sd=sd(DC_landed)),colour = "red") +
  labs(title = "DC_landed Distribution by Merchandise Division")


# Distribution Import In Transit
ggplot(Inventory_IP,aes(x=Import_In_Transit)) +
  facet_wrap(~ Business_Area) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Import_In_Transit),sd=sd(Import_In_Transit)),colour = "red") +
  labs(title = "Import_In_Transit Distribution by Business Area")

ggplot(Inventory_IP,aes(x=Import_In_Transit)) +
  facet_wrap(~ Merchandise_Division) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Import_In_Transit),sd=sd(Import_In_Transit)),colour = "red") +
  labs(title = "DC_landed Distribution by Business Area")
detach(Inventory_IP)


# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Boxplot
# --------------------------------------------------------------------------------------------------------------

# Distribution by Business Area
attach(Inventory_IP)
ggplot(Inventory_IP,aes(Business_Area,DC_landed,fill=Business_Area)) +
   geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "DC Landed Distribution",x="Business Areas", y="DC Landed") +
  theme_bw(base_size = 12, base_family = "")

ggplot(Inventory_IP,aes(Business_Area,Import_In_Transit,fill=Business_Area)) +
   geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Import In Transit Distribution",x="Business Areas", y="DC Landed") +
  theme_bw(base_size = 12, base_family = "")


# Distribution by Merchandise Division
ggplot(Inventory_IP,aes(Merchandise_Division,DC_landed,fill=Business_Area)) +
   geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "DC Landed Distribution",x="Merchandise Division", y="DC Landed") +
  theme_bw(base_size = 12, base_family = "")

ggplot(Inventory_IP,aes(Merchandise_Division,Import_In_Transit,fill=Business_Area)) +
   geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Import In Transit Distribution",x="Merchandise Division", y="DC Landed") +
  theme_bw(base_size = 12, base_family = "")
detach(Inventory_IP)


# --------------------------------------------------------------------------------------------------------------
#                                    Relationship b/n Variables : Scatterplot
# --------------------------------------------------------------------------------------------------------------

ggplot(Inventory_IP,aes(y=Import_In_Transit,x=DC_landed)) +
  facet_wrap(~ Business_Area) +
  geom_point(aes(colour = Import_In_Transit)) +
  scale_colour_gradient(high = "red2")+
  labs(title = "Relationship between DC Landed & Import In Transit", x = "DC Landed",y = "Import In Transit") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = 5393000,slope = 0.09884))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ------------------- END -------------------- Inventory EDA ------------------ END ----------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
