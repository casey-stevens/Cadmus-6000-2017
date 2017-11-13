#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

##  Create "Not In" operator
"%notin%" <- Negate("%in%")

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) 

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))
length(unique(buildings.dat$CK_Building_ID))
buildings.dat.clean <- buildings.dat[which(!duplicated(buildings.dat$CK_Building_ID)),]



#############################################################################################
# Item 223: PERCENTAGE OF BUILDINGS WITH NON-RESIDENTIAL USES BY BUILDING SIZE (MF table 15)
#############################################################################################
item223.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item223.dat) <- c("Commercial.Area"
                           ,"Nonres.Grocery.SQFT"
                           ,"Nonres.Office.SQFT"
                           ,"Nonres.Other.SQFT"
                           ,"Nonres.Retail.SQFT"
                           ,"Nonres.Vacant.SQFT"
                           ,"CK_Building_ID"
                           )

for (i in 1:6){
  item223.dat[,i] <- as.numeric(as.character(item223.dat[,i]))
}
item223.dat[is.na(item223.dat)] <- 0


item223.dat$CommercialAreaFlag <- ifelse(item223.dat$Nonres.Grocery.SQFT + 
                                           item223.dat$Nonres.Office.SQFT +
                                           item223.dat$Nonres.Other.SQFT +
                                           item223.dat$Nonres.Retail.SQFT +
                                           item223.dat$Nonres.Vacant.SQFT +
                                           item223.dat$Commercial.Area > 0,1,0)
item223.dat$CK_Building_ID <- as.character(item223.dat$CK_Building_ID)
item223.dat1 <- left_join(rbsa.dat, item223.dat)

item223.dat2 <- item223.dat1[grep("Multifamily", item223.dat1$BuildingType),]
item223.dat2 <- item223.dat2[which(!is.na(item223.dat2$CommercialAreaFlag)),]


################################################
# Adding pop and sample sizes for weights
################################################
item223.data <- weightedData(item223.dat2[which(colnames(item223.dat2) %notin% c("Commercial.Area"
                                                                                 ,"Nonres.Grocery.SQFT"
                                                                                 ,"Nonres.Office.SQFT"
                                                                                 ,"Nonres.Other.SQFT"
                                                                                 ,"Nonres.Retail.SQFT"
                                                                                 ,"Nonres.Vacant.SQFT" 
                                                                                 ,"CommercialAreaFlag"))])
item223.data <- left_join(item223.data, item223.dat2[which(colnames(item223.dat2) %in% c("CK_Building_ID"
                                                                                         ,"Commercial.Area"
                                                                                         ,"Nonres.Grocery.SQFT"
                                                                                         ,"Nonres.Office.SQFT"
                                                                                         ,"Nonres.Other.SQFT"
                                                                                         ,"Nonres.Retail.SQFT"
                                                                                         ,"Nonres.Vacant.SQFT" 
                                                                                         ,"CommercialAreaFlag"))])
item223.data$Count <- 1
#######################
# Weighted Analysis
#######################
item223.final <- proportions_one_group_MF(CustomerLevelData = item223.data
                                       ,valueVariable    = 'CommercialAreaFlag'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = TRUE)
item223.final <- item223.final[which(item223.final != "Total"),]

exportTable(item223.final, "MF", "Table 11", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item223.final <- proportions_one_group(CustomerLevelData = item223.data
                                       ,valueVariable    = 'Ind'
                                       ,groupingVariable = 'HomeType'
                                       ,total.name       = "All Sizes"
                                       ,weighted         = FALSE)
exportTable(item223.final, "MF", "Table 11", weighted = FALSE)



# item223.summary1 <-summarise(group_by(item223.dat2, BuildingTypeXX)
#                              ,SampleSize = length(unique(CK_Cadmus_ID))
#                              ,TotalWithCommercArea = sum(CommercialAreaFlag))
# 
# item223.summary2  <- summarise(group_by(item223.dat2)
#                                ,BuildingTypeXX = "All Sizes"
#                                ,SampleSize = length(unique(CK_Cadmus_ID))
#                                ,TotalWithCommercArea = sum(CommercialAreaFlag)) 
# 
# 
# item223.final <- rbind.data.frame(item223.summary1,item223.summary2)
# 
# item223.final$Percent <- item223.final$TotalWithCommercArea / item223.final$SampleSize
# item223.final$SE <- sqrt(item223.final$Percent * (1 - item223.final$Percent) / item223.final$SampleSize)
# 
# item223.sub <- item223.final[,which(!(colnames(item223.final) %in% c("TotalWithCommercArea","SampleSize")))]
# item223.table <- data.frame(item223.sub,"SampleSize" = item223.final$SampleSize,stringsAsFactors = F)

#############################################################################################
# Item 224: DISTRIBUTION OF NON-RESIDENTIAL FLOOR AREA (IN BUILDINGS WITH NON-RESIDENTIAL) BY USE TYPE AND BUILDING SIZE (MF table 16)
#############################################################################################
item224.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                  # ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_AreaOfCommercialSpaceInBuilding_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Grocery_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Office_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Other_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Retail_SqFt"
                                                                  ,"SITES_MFB_cfg_MFB_NONRESIDENTIAL_Vacant_SqFt"
                                                                  ,""))]

colnames(item224.dat) <- c("Grocery"
                           ,"Office"
                           ,"Other"
                           ,"Retail"
                           ,"Vacant"
                           ,"CK_Building_ID")


for (i in 1:5){
  item224.dat[,i] <- as.numeric(as.character(item224.dat[,i]))
}
item224.dat[is.na(item224.dat)] <- 0

item224.dat$Total.Area <- item224.dat$Grocery + item224.dat$Office + item224.dat$Other + item224.dat$Retail + item224.dat$Vacant

item224.melt <- melt(item224.dat, id = c("CK_Building_ID","Total.Area"))
colnames(item224.melt) <- c("CK_Building_ID", "Total.Area", "Nonres.Type", "Area")
item224.melt$CK_Building_ID <- as.character(item224.melt$CK_Building_ID)

item224.merge <- left_join(rbsa.dat, item224.melt)

#subset to only MF sites
item224.dat1 <- item224.merge[which(item224.merge$BuildingTypeXX %in% c("Apartment Building (3 or fewer floors)"
                                                                        ,"Apartment Building (4 to 6 floors)"
                                                                        ,"Apartment Building (More than 6 floors)")),]

#remove any zeros 
item224.dat2 <- data.frame(item224.dat1[which(item224.dat1$Total.Area != 0),], stringsAsFactors = F)
item224.dat2$Ind <- 0
item224.dat2$Ind[which(item224.dat2$Area > 0)] <- 1

item224.data <- weightedData(item224.dat2[which(colnames(item224.dat2) %notin% c("Total.Area"
                                                                                 ,"Nonres.Type"
                                                                                 ,"Area"
                                                                                 ,"Ind"))])
item224.data <- left_join(item224.data, item224.dat2[which(colnames(item224.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Total.Area"
                                                                                            ,"Nonres.Type"
                                                                                            ,"Area"
                                                                                            ,"Ind"))])
####################
#Weighted analysis
####################
item224.summary <- proportionRowsAndColumns1(CustomerLevelData = item224.data
                                             ,valueVariable = 'Area'
                                             ,columnVariable = 'HomeType'
                                             ,rowVariable = 'Nonres.Type'
                                             ,aggregateColumnName = 'All Sizes')
item224.summary <- item224.summary[which(item224.summary$Nonres.Type != "Total"),]


item224.cast <- dcast(setDT(item224.summary)
                      ,formula = Nonres.Type ~ HomeType
                      ,value.var = c("w.percent","w.SE", "count", "n", "N"))

item224.table <- data.frame("Nonresidential_Use_Type"  = item224.cast$Nonres.Type
                            ,"Low_Rise_1.3_Percent"    = item224.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item224.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_n"              = item224.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent"    = item224.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item224.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_n"              = item224.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = item224.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = item224.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = item224.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Percent"       = item224.cast$`w.percent_All Sizes`
                            ,"All_Sizes_SE"            = item224.cast$`w.SE_All Sizes`
                            ,"All_Sizes_n"             = item224.cast$`n_All Sizes`
                              )
exportTable(item224.table, "MF", "Table 16", weighted = TRUE)

####################
#Weighted analysis
####################
item224.summary <- proportions_two_groups_unweighted(CustomerLevelData = item224.data
                                             ,valueVariable = 'Area'
                                             ,columnVariable = 'HomeType'
                                             ,rowVariable = 'Nonres.Type'
                                             ,aggregateColumnName = 'All Sizes')
item224.summary <- item224.summary[which(item224.summary$Nonres.Type != "Total"),]


item224.cast <- dcast(setDT(item224.summary)
                      ,formula = Nonres.Type ~ HomeType
                      ,value.var = c("Percent","SE", "Count", "SampleSize"))

item224.table <- data.frame("Nonresidential_Use_Type"  = item224.cast$Nonres.Type
                            ,"Low_Rise_1.3_Percent"    = item224.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item224.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_n"              = item224.cast$`SampleSize_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent"    = item224.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item224.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_n"              = item224.cast$`SampleSize_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = item224.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = item224.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = item224.cast$`SampleSize_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Percent"       = item224.cast$`Percent_All Sizes`
                            ,"All_Sizes_SE"            = item224.cast$`SE_All Sizes`
                            ,"All_Sizes_n"             = item224.cast$`SampleSize_All Sizes`
)
exportTable(item224.table, "MF", "Table 16", weighted = FALSE)
