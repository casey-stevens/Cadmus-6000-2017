#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################
##  Clear variables
rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601


#Read in data for analysis
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))








#############################################################################################
#Item 32: DISTRIBUTION OF DOOR TYPES
#############################################################################################

item32.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item32.dat1 <- left_join(rbsa.dat, item32.dat, by = "CK_Cadmus_ID")
length(unique(item32.dat1$CK_Cadmus_ID)) #601 yay!

#subset to only doors
item32.dat2 <- item32.dat1[which(item32.dat1$Type == "Door"),]

#clean up frame/body type
item32.dat2$Frame.Type <- trimws(item32.dat2$`Frame./.Body.Type`)
item32.dat2$Frame.Type[grep("Wood", item32.dat2$Frame.Type)] <- "Wood"

#clean up glazing types
item32.dat2$Glazing <- trimws(item32.dat2$Glazing.Type)
item32.dat2$Glazing[which(item32.dat2$Glazing %in% c("Decorative window (arch, etc.)"
                                                     ,"Half window"
                                                     ,"Double"
                                                     ,"French door"
                                                     ,"Single"))] <- "Glazed"
item32.dat2$Framing.Categories <- paste(item32.dat2$Frame.Type, item32.dat2$Glazing, sep = " ")

item32.dat2$count <- 1

item32.data <- weightedData(item32.dat2[-which(colnames(item32.dat2) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Frame.Type"
                                                                            ,"Glazing"
                                                                            ,"Framing.Categories"
                                                                            ,"count"))])
item32.data <- left_join(item32.data, item32.dat2[which(colnames(item32.dat2) %in% c("CK_Cadmus_ID"
                                                                                     ,"Type"
                                                                                      ,"Sub-Type"
                                                                                      ,"Area"
                                                                                      ,"Quantity"
                                                                                      ,"Frame./.Body.Type"
                                                                                      ,"Glazing.Type"
                                                                                      ,"Frame.Type"
                                                                                      ,"Glazing"
                                                                                      ,"Framing.Categories"
                                                                                      ,"count"))])

item32.data$Quantity <- as.numeric(as.character(item32.data$Quantity))
item32.final <- proportions_one_group(CustomerLevelData = item32.data
                                        , valueVariable   = 'Quantity'
                                        , groupingVariable= 'Framing.Categories'
                                        , total.name      = "Total"
                                        , columnName      = "Door.Type")

item32.final.SF <- item32.final[which(item32.final$BuildingType == "Single Family"),-1]

exportTable(item3.table.SF, "SF", "Table 39")







# #Total count across door types
# item32.tmp1 <- summarise(group_by(item32.dat2, BuildingType)
#                           , Framing.Categories = "Total"
#                           , SampleSize = length(unique(CK_Cadmus_ID))
#                           , Count = sum(count))
# #door count by framing type categories
# item32.tmp2 <- summarise(group_by(item32.dat2, BuildingType, Framing.Categories)
#                          , SampleSize = length(unique(CK_Cadmus_ID))
#                          , Count = sum(count))
# item32.tmp3 <- summarise(group_by(item32.dat2, BuildingType)
#                          , TotalCount = sum(count))
# 
# item32.merge <- rbind.data.frame(item32.tmp1, item32.tmp2, stringsAsFactors = F)
# item32.final <- left_join(item32.merge, item32.tmp3, by = "BuildingType")
# 
# #percent
# item32.final$Percent <- item32.final$Count / item32.final$TotalCount
# item32.final$SE      <- sqrt(item32.final$Percent * (1 - item32.final$Percent) / item32.final$SampleSize)
# 
# item32.table <- data.frame("BuildingType" = item32.final$BuildingType
#                            ,"Framing.Categories" = item32.final$Framing.Categories
#                            ,"Percent" = item32.final$Percent
#                            ,"SE" = item32.final$SE
#                            ,"SampleSize" = item32.final$SampleSize)
# 
# ###Subset to only single family
# item32.final.SF <- item32.table[which(item32.table$BuildingType == 'Single Family'),
#                                 -which(colnames(item32.table) == 'BuildingType')]
# 
# ###Package xlsx working only now for some reason here is a template
# workbook.SF <- xlsx::loadWorkbook(file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", sep = "/"))
# tableSheets <- xlsx::getSheets(workbook.SF)
# tableSheet  <- tableSheets$`Table 39`
# addDataFrame(x = item32.final.SF,
#              sheet = tableSheet,
#              startRow = 20)
# 
# xlsx::saveWorkbook(wb = workbook.SF,
#                    file = paste(outputFolder, "Tables in Excel - SF - COPY.xlsx", 
#                                 sep = "/"))
# 
# ##################################### Table Format #####################################
# 
# item32.table <- data.frame("BuildingType" = item32.final.SF$BuildingType
#                            ,"Framing.Categories" = item32.final.SF$Framing.Categories
#                            ,"Percent" = item32.final.SF$Percent
#                            ,"SE" = item32.final.SF$SE
#                            ,"SampleSize" = item32.final.SF$SampleSize)









#############################################################################################
#Item 33: DISTRIBUTION OF WINDOW TYPES BY STATE
#############################################################################################
item33.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item33.dat1 <- left_join(rbsa.dat, item33.dat, by = "CK_Cadmus_ID")
length(unique(item33.dat1$CK_Cadmus_ID)) #565 yay!

#subset to only windows
item33.dat2 <- item33.dat1[which(item33.dat1$Type == "Window"),]

#clean up frame/body type
unique(item33.dat2$`Frame./.Body.Type`)
item33.dat2$Frame.Type <- trimws(item33.dat2$`Frame./.Body.Type`)
item33.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass", item33.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass"
item33.dat2$Frame.Type[grep("Metal|Aluminum", item33.dat2$Frame.Type)] <- "Metal"
item33.dat2$Frame.Type[grep("N/A", item33.dat2$Frame.Type)] <- "Unknown"
unique(item33.dat2$Frame.Type)

#clean up glazing types
item33.dat2$Glazing <- trimws(item33.dat2$Glazing.Type)
item33.dat2$Glazing[grep("Single", item33.dat2$Glazing)] <- "Single Glazed"
item33.dat2$Glazing[grep("Double", item33.dat2$Glazing)] <- "Double Glazed"
item33.dat2$Glazing[grep("Triple", item33.dat2$Glazing)] <- "Triple Glazed"
item33.dat2$Glazing[which(!(item33.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(item33.dat2$Glazing)

item33.dat2$Framing.Categories <- paste(item33.dat2$Frame.Type, item33.dat2$Glazing, sep = " ")

item33.dat2$count <- 1


#insert weights

#weighting application

#export table









# #####################################Analysis - needs cleaning & work ###################################33
# # summarise by state and framing categories
# item33.tmp1 <- summarise(group_by(item33.dat2, BuildingType, State, Framing.Categories)
#                          , SampleSize = length(unique(CK_Cadmus_ID))
#                          , Count = sum(count))
# # summarise by state across framing categories
# item33.tmp2 <- summarise(group_by(item33.dat2, BuildingType, State)
#                          , Framing.Categories = "All Window Types"
#                          , SampleSize = length(unique(CK_Cadmus_ID))
#                          , Count = sum(count))
# #summarise by framing categories across states
# item33.tmp3 <- summarise(group_by(item33.dat2, BuildingType, Framing.Categories)
#                          , State = "Region"
#                          , SampleSize = length(unique(CK_Cadmus_ID))
#                          , Count = sum(count))
# item33.tmp4 <- summarise(group_by(item33.dat2, BuildingType)
#                          , State = "Region"
#                          , Framing.Categories = "All Window Types"
#                          , SampleSize = length(unique(CK_Cadmus_ID))
#                          , Count = sum(count))
# 
# item33.merge1 <- rbind.data.frame(item33.tmp1, item33.tmp2, item33.tmp3, item33.tmp4, stringsAsFactors = F)
# 
# #obtain total counts
# item33.tot.counts <- rbind.data.frame(item33.tmp2, item33.tmp4, stringsAsFactors = F)
#   
# item33.final <- left_join(item33.merge1, item33.tot.counts, by = c("BuildingType","State"))
# colnames(item33.final) <- c("BuildingType"
#                              ,"State"
#                              ,"Framing.Categories"
#                              ,"SampleSize"
#                              ,"Count"
#                              ,"Remove"
#                              ,"Remove"
#                              ,"TotalCount")
# 
# item33.final$Percent <- item33.final$Count / item33.final$TotalCount
# item33.final$SE      <- sqrt(item33.final$Percent * (1 - item33.final$Percent) / item33.final$SampleSize)
# 
##################################### Table Format ###################################33
detach(package:reshape2)
library(data.table)
item33.table <- dcast(setDT(item33.final)
                      , formula = BuildingType + Framing.Categories ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

item33.table2 <- data.frame("BuildingType"        = item33.table$BuildingType
                            ,"Framing.Categories" = item33.table$Framing.Categories
                            ,"Percent_MT"         = item33.table$Percent_MT
                            ,"SE_MT"              = item33.table$SE_MT
                            ,"Percent_WA"         = item33.table$Percent_WA
                            ,"SE_WA"              = item33.table$SE_WA
                            ,"Percent_Region"     = item33.table$Percent_Region
                            ,"SE_Region"          = item33.table$SE_Region
                            ,"SampleSize"         = item33.table$SampleSize_Region)

item33.table3 <- item33.table2[which(item33.table2$BuildingType %in% c("Single Family")),]







#############################################################################################
#Item 34: PERCENTAGE OF HOMES WITH STORM WINDOWS BY STATE
#############################################################################################
item34.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item34.dat1 <- left_join(rbsa.dat, item34.dat, by = "CK_Cadmus_ID")
length(unique(item34.dat1$CK_Cadmus_ID)) #565 yay!

#subset to only SF
item34.dat.SF <- item34.dat1[which(item34.dat1$BuildingType == "Single Family"),]

#subset to only windows
unique(item34.dat.SF$Type)
item34.dat2 <- item34.dat.SF[which(item34.dat.SF$Type == "Storm Window"),]

#obtain total sample size counts
item34.tmp1 <- summarise(group_by(item34.dat.SF, BuildingType, State)
                         ,TotalCount = length(unique(CK_Cadmus_ID))
                         ,SampleSize = length(unique(CK_Cadmus_ID)))
#obtain counts of site with storms windows
item34.tmp2 <- summarise(group_by(item34.dat2, BuildingType, State)
                         ,StormWindCount = length(unique(CK_Cadmus_ID)))

#merge
item34.final <- left_join(item34.tmp2, item34.tmp1, by = c("BuildingType", "State"))
item34.final$Percent <- item34.final$StormWindCount / item34.final$TotalCount
item34.final$SE      <- sqrt(item34.final$Percent * (1 - item34.final$Percent) / item34.final$SampleSize)


##################################### Table Format #####################################

item34.table <- data.frame("BuildingType" = item34.final$BuildingType
                           ,"State" = item34.final$State
                           ,"Percent" = item34.final$Percent
                           ,"SE" = item34.final$SE
                           ,"SampleSize" = item34.final$SampleSize)
