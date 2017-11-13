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

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Building_ID <- trimws(toupper(buildings.interview.dat$CK_BuildingID))



#############################################################################################
# Item 225: DISTRIBUTION OF OWNERSHIP TYPE BY BUILDING SIZE (MF table 17)
#############################################################################################
item225.dat <- buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Building_ID"
                                                                  ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataOwnership"))]

colnames(item225.dat) <- c("Ownership", "CK_Building_ID")

item225.dat0 <- item225.dat[which(!(is.na(item225.dat$Ownership))),]
item225.dat1 <- item225.dat0[which(item225.dat0$Ownership != "Unknown"),]


item225.dat2 <- left_join(rbsa.dat,item225.dat1)

#subset to only MF sites
item225.dat3 <- item225.dat2[which(item225.dat2$BuildingType %in% "Multifamily"),]

item225.merge <- item225.dat3[which(!is.na(item225.dat3$Ownership)),]
item225.merge <- item225.merge[which(item225.merge$Ownership != "N/A"),]

################################################
# Adding pop and sample sizes for weights
################################################
item225.data <- weightedData(item225.merge[-which(colnames(item225.merge) %in% c("Ownership"))])
item225.data <- left_join(item225.data, item225.merge[which(colnames(item225.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ownership"))])

item225.data$count <- 1
#######################
# Weighted Analysis
#######################
item225.final <- proportionRowsAndColumns1(CustomerLevelData = item225.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Ownership'
                                           ,aggregateColumnName = "Remove")
item225.final <- item225.final[which(item225.final$HomeType != "Remove"),]
item225.final <- item225.final[which(item225.final$Ownership != "Total"),]


item225.all.sizes <- proportions_one_group_MF(CustomerLevelData = item225.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Ownership'
                                              ,total.name = "All Sizes"
                                              ,columnName = "HomeType"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item225.all.sizes <- item225.all.sizes[which(item225.all.sizes$Ownership != "Total"),]

item225.final <- rbind.data.frame(item225.final, item225.all.sizes, stringsAsFactors = F)

item225.cast <- dcast(setDT(item225.final)
                      ,formula = Ownership ~ HomeType
                      ,value.var = c("w.percent","w.SE", "count","n","N"))

item225.table <- data.frame("Ownership"                = item225.cast$Ownership
                            ,"Low_Rise_1.3_Percent"    = item225.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item225.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"High_Rise_count"         = item225.cast$`count_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent"    = item225.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item225.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_count"         = item225.cast$`count_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = NA#item225.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = NA#item225.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High_Rise_count"         = NA#item225.cast$
                            ,"All_Sizes_Percent"       = item225.cast$`w.percent_All Sizes`
                            ,"All_Sizes_SE"            = item225.cast$`w.SE_All Sizes`
                            ,"All_Sizes_count"         = item225.cast$`count_All Sizes`)

exportTable(item225.table, "MF", "Table 17", weighted = TRUE)
#######################
# unweighted Analysis
#######################
item225.final <- proportions_two_groups_unweighted(CustomerLevelData = item225.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Ownership'
                                           ,aggregateColumnName = "Remove")
item225.final <- item225.final[which(item225.final$HomeType != "Remove"),]
item225.final <- item225.final[which(item225.final$Ownership != "Total"),]


item225.all.sizes <- proportions_one_group_MF(CustomerLevelData = item225.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Ownership'
                                              ,total.name = "All Sizes"
                                              ,columnName = "HomeType"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item225.all.sizes <- item225.all.sizes[which(item225.all.sizes$Ownership != "Total"),]

item225.final <- rbind.data.frame(item225.final, item225.all.sizes, stringsAsFactors = F)

item225.cast <- dcast(setDT(item225.final)
                      ,formula = Ownership ~ HomeType
                      ,value.var = c("Percent","SE", "Count","SampleSize"))

item225.table <- data.frame("Ownership"                = item225.cast$Ownership
                            ,"Low_Rise_1.3_Percent"    = item225.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE"             = item225.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_count"          = item225.cast$`Count_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Percent"    = item225.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE"             = item225.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_count"          = item225.cast$`Count_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Percent" = NA#item225.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = NA#item225.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High_Rise_count"         = NA#item225.cast$`Count_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Percent"       = item225.cast$`Percent_All Sizes`
                            ,"All_Sizes_SE"            = item225.cast$`SE_All Sizes`
                            ,"All_Sizes_count"         = item225.cast$`Count_All Sizes`)


exportTable(item225.table, "MF", "Table 17", weighted = FALSE)
