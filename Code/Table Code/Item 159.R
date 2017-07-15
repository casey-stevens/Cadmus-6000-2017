#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 159: DISTRIBUTION OF ELECTRICALLY HEATED HOMES BY GROUND CONTACT TYPE AND STATE (SF table B-2)
#############################################################################################

#############################################################################################
#
#For Envelope information
#
#############################################################################################

#subset to columns need in table
env.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]
env.dat1 <- env.dat[which(!(is.na(env.dat$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]

env.dat2 <- env.dat1[which(!(env.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt %in% c("0", "Unknown"))),]
env.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt <- as.numeric(as.character(env.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

env.sum <- summarise(group_by(env.dat2, CK_Cadmus_ID)
                     ,Site.SQFT = sum(ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

item159.envelope <- env.sum



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item159.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item159.dat.11 <- item159.dat.1[which(item159.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item159.dat.12 <- item159.dat.11[which(item159.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item159.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item159.dat.12$Primary.Heating.System)
item159.dat.12$count <- 1

item159.dat.13 <- unique(item159.dat.12[which(item159.dat.12$Heating.Fuel == "Electric"),])

item159.sum1 <- summarise(group_by(item159.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item159.sum1$Count <- 1
which(duplicated(item159.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item159.sum1$Heating.Fuel)

item159.mechanical <- item159.sum1






#############################################################################################
#
#Merge mechanical and envelope data together
#
#############################################################################################

item159.merge <- left_join(item159.mechanical, item159.envelope, by = c("CK_Cadmus_ID"))

item159.merge1 <- item159.merge[which(!(is.na(item159.merge$Heating.Fuel))),]

item159.merge2 <- left_join(item159.merge1, rbsa.dat, by = c("CK_Cadmus_ID"))

#summarise by state
#by ground contact types
item159.state1 <- summarise(group_by(item159.merge2, BuildingType, HomeYearBuilt_bins, State)
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#across ground contact types
item159.state2 <- summarise(group_by(item159.merge2, BuildingType, State)
                            ,HomeYearBuilt_bins = "All Vintages"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Count = sum(Count))
#summarise across states
#by ground contact types
item159.region1 <- summarise(group_by(item159.merge2, BuildingType, HomeYearBuilt_bins)
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count = sum(Count))
#across ground contact types
item159.region2 <- summarise(group_by(item159.merge2, BuildingType)
                             ,HomeYearBuilt_bins = "All Vintages"
                             ,State = "Region"
                             ,SampleSize = length(unique(CK_Cadmus_ID))
                             ,Count = sum(Count))
#row bind state information together
item159.merge3 <- rbind.data.frame(item159.state1, item159.state2, item159.region1, item159.region2, stringsAsFactors = F)

item159.total.counts <-  rbind.data.frame(item159.state2, item159.region2, stringsAsFactors = F)
item159.total.counts <- item159.total.counts[which(colnames(item159.total.counts) %in% c("BuildingType", "State", "Count"))]

#merge on total counts
item159.final <- left_join(item159.merge3, item159.total.counts, by = c("BuildingType", "State"))
colnames(item159.final) <- c("BuildingType", "Housing.Vintage", "State", "SampleSize", "Count", "Total.Count")

#calculate percent and SE
item159.final$Percent <- item159.final$Count / item159.final$Total.Count
item159.final$SE <- sqrt(item159.final$Percent * (1 - item159.final$Percent) / item159.final$SampleSize)

#cast out by state
library(data.table)
item159.cast <- dcast(setDT(item159.final)
                      ,formula = BuildingType + Housing.Vintage ~ State
                      ,value.var = c("Percent", "SE", "SampleSize"))

#put in table format
item159.table <- data.frame("BuildingType" = item159.cast$BuildingType
                            ,"Housing.Vintage" = item159.cast$Housing.Vintage
                            ,"Percent_MT" = item159.cast$Percent_MT
                            ,"SE_MT" = item159.cast$SE_MT
                            # ,"Percent_OR" = item159.cast$Percent_OR
                            # ,"SE_OR" = item159.cast$SE_OR
                            ,"Percent_WA" = item159.cast$Percent_WA
                            ,"SE_WA" = item159.cast$SE_WA
                            ,"Percent_Region" = item159.cast$Percent_Region
                            ,"SE_Region" = item159.cast$SE_Region
                            ,"SampleSize" = item159.cast$SampleSize_Region)

item159.table.final <- item159.table[which(item159.table$BuildingType %in% c("Single Family")),]
item159.table.final1 <- item159.table.final[which(!(is.na(item159.table.final$Housing.Vintage))),]



