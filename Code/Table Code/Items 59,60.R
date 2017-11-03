#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:          11/3/2017                                   
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list = ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen = 999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))







#############################################################################################
#Item 59: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY STATE (SF table 66)
#############################################################################################
#subset to columns needed for analysis
item59.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"))]
#remove any repeat header rows 
item59.dat00 <- item59.dat[which(item59.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item59.dat$count <- 1
#check unique system types
unique(item59.dat00$Generic)

item59.dat0 <- left_join(rbsa.dat, item59.dat00, by = "CK_Cadmus_ID")

#subset to only Generic = Ducting
item59.dat1 <- unique(item59.dat0[which(item59.dat0$Generic == "Ducting"),])
length(unique(item59.dat1$CK_Cadmus_ID))

# Add count var
item59.dat1$count <- 1


# Weighting function
item59.data <- weightedData(item59.dat1[-which(colnames(item59.dat1) %in% c("Generic"
                                                                            ,"System.Type"
                                                                            ,"count"))])
item59.data <- left_join(item59.data, item59.dat1[which(colnames(item59.dat1) %in% c("CK_Cadmus_ID"
                                                                                     ,"Generic"
                                                                                     ,"System.Type"
                                                                                     ,"count"))])

# Apply analysis
item59.final <- proportions_one_group(CustomerLevelData  = item59.data
                                      , valueVariable    = 'count'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Homes with Ducts")

# SF = Table 66
# Export table
item59.final.SF <- item59.final[which(item59.final$BuildingType == "Single Family"),-1]

exportTable(item59.final.SF, "SF", "Table 66")


# OLD CODE #
# 
# #summarise FULL data by state
# item59.sum1 <- summarise(group_by(item59.dat0, BuildingType, State)
#                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# #summarise only ducting data by state
# item59.sum2 <- summarise(group_by(item59.dat1, BuildingType, State)
#                          ,DuctCount = length(unique(CK_Cadmus_ID)))
# 
# #summarise FULL data for the region
# item59.sum3 <- summarise(group_by(item59.dat0, BuildingType)
#                          ,State = "Region"
#                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# #summarise only ducting for the region
# item59.sum4 <- summarise(group_by(item59.dat1, BuildingType)
#                          ,State = "Region"
#                          ,DuctCount = length(unique(CK_Cadmus_ID)))
# 
# #row bind DUCT data counts
# item59.duct <- rbind.data.frame(item59.sum2, item59.sum4, stringsAsFactors = F)
# #row bind FULL data counts
# item59.full <- rbind.data.frame(item59.sum1, item59.sum3, stringsAsFactors = F)
# 
# 
# #merge duct and full info
# item59.final <- left_join(item59.duct, item59.full, by = c("BuildingType","State"))
# 
# 
# #calculate Ducting percent
# item59.final$Percent <- item59.final$DuctCount / item59.final$SampleSize
# #calculate SE for proportion (percent)
# item59.final$SE <- sqrt(item59.final$Percent * (1 - item59.final$Percent) / item59.final$SampleSize)
# 
# #subset to only columns needed for table
# item59.table <- data.frame("BuildingType" = item59.final$BuildingType
#                            ,"State" = item59.final$State
#                            ,"Percent" = item59.final$Percent
#                            ,"SE" = item59.final$SE
#                            ,"SampleSize" = item59.final$SampleSize) 
# #subset to only building types needed for table
# item59.table.final <- item59.table[which(item59.table$BuildingType %in% c("Single Family")),]
# 
# 
# 





#############################################################################################
#Item 60: PERCENTAGE OF HOMES WITH DUCT SYSTEMS BY STATE (SF table 66)
#############################################################################################
#subset to columns needed for analysis
item60.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"System.Type"
                                                                   ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"))]
#check unique values for conditioned space
unique(item60.dat$Percentage.of.Supply.Ducts.in.Conditioned.Space)

item60.dat0 <- left_join(rbsa.dat, item60.dat, by = "CK_Cadmus_ID")

#remove coditioned space = datapoint not asked for
item60.dat1 <- item60.dat0[which(item60.dat0$Percentage.of.Supply.Ducts.in.Conditioned.Space != "-- Datapoint not asked for --"),]
#remove conditioned space = NA
item60.dat2 <- item60.dat1[which(!(is.na(item60.dat1$Percentage.of.Supply.Ducts.in.Conditioned.Space))),]

#Make conditioned space numeric
item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space <- as.numeric(as.character(item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space))

#Create Unconditioned Space Percent
item60.dat2$PercentDuctsUnconditionedSpace <- 100 - item60.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space
#double check these make sense
item60.dat2$PercentDuctsUnconditionedSpace


#create percent ducts unconditioned space bins
item60.dat2$UnconditionedBins <- ""
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace == 0)] <- "None"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 1 & item60.dat2$PercentDuctsUnconditionedSpace < 51)] <- "1-50%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace >= 51 & item60.dat2$PercentDuctsUnconditionedSpace < 100)] <- "51-99%"
item60.dat2$UnconditionedBins[which(item60.dat2$PercentDuctsUnconditionedSpace == 100)] <- "100%"
unique(item60.dat2$UnconditionedBins)
item60.dat2$count <- 1

item60.dat3 <- item60.dat2[which(item60.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item60.dat4 <- item60.dat3[which(item60.dat3$BuildingType %in% c("Single Family")),]








# For State
# Across Unconditioned Bins
item60.state1 <- summarise(group_by(item60.dat4, BuildingType, State)
                           ,UnconditionedBins = "Total"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count)) 
# By Unconditioned Bins
item60.state2 <- summarise(group_by(item60.dat4, BuildingType, State, UnconditionedBins)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item60.state <- rbind.data.frame(item60.state1, item60.state2, stringsAsFactors = F)

# For Region
# Across Unconditioned Bins
item60.region1 <- summarise(group_by(item60.dat4, BuildingType)
                           ,UnconditionedBins = "Total"
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count)) 
# By Unconditioned Bins
item60.region2 <- summarise(group_by(item60.dat4, BuildingType, UnconditionedBins)
                           ,State = "Region"
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Count = sum(count))
item60.region <- rbind.data.frame(item60.region1, item60.region2, stringsAsFactors = F)


# merge state and region information
item60.merge <- rbind.data.frame(item60.state, item60.region, stringsAsFactors = F)

#merge total counts onto previous data
item60.totalcount <- rbind.data.frame(item60.state1, item60.region1, stringsAsFactors = F)
item60.final <- left_join(item60.merge, item60.totalcount, by = c("BuildingType", "State"))

#rename columns to how they should appear in table
colnames(item60.final) <- c("BuildingType"
                            ,"State"
                            ,"Percentage of Ducts in Unconditioned Space"
                            ,"SampleSize"
                            ,"Count"
                            ,"Remove"
                            ,"Remove"
                            ,"TotalCount")
#calculate percent
item60.final$Percent <- item60.final$SampleSize / item60.final$TotalCount
item60.final$SE <- sqrt(item60.final$Percent * (1 - item60.final$Percent) / item60.final$SampleSize)

item60.cast <- dcast(setDT(item60.final)
                      , formula = BuildingType + `Percentage of Ducts in Unconditioned Space` ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

#Subset to only columns for table
item60.table <- data.frame("BuildingType" = item60.cast$BuildingType
                           ,"Percentage of Ducts in Unconditioned Space" = item60.cast$`Percentage of Ducts in Unconditioned Space`
                           ,"Percent_MT" = item60.cast$Percent_MT
                           ,"SE_MT" = item60.cast$SE_MT
                           ,"Percent_WA" = item60.cast$Percent_WA
                           ,"SE_WA" = item60.cast$SE_WA
                           ,"Percent_Region" = item60.cast$Percent_Region
                           ,"SE_Region" = item60.cast$SE_Region
                           ,"SampleSize" = item60.cast$SampleSize_Region)
