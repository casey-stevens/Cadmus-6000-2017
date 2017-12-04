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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))


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
length(unique(item32.dat1$CK_Cadmus_ID)) 

#subset to only doors
item32.dat2 <- item32.dat1[which(item32.dat1$Type == "Door"),]

#clean up frame/body type
item32.dat2$Frame.Type <- trimws(item32.dat2$`Frame./.Body.Type`)
item32.dat2$Frame.Type[grep("wood|fiberglass", item32.dat2$Frame.Type, ignore.case = T)] <- "Wood/Fiberglass"
item32.dat2$Frame.Type[grep("metal|aluminum", item32.dat2$Frame.Type, ignore.case = T)] <- "Metal"
item32.dat2$Frame.Type[grep("plastic|other|vinyl", item32.dat2$Frame.Type, ignore.case = T)] <- "Other"
unique(item32.dat2$Frame.Type)

#clean up glazing types
item32.dat2$Glazing <- trimws(item32.dat2$Glazing.Type)
unique(item32.dat2$Glazing)
item32.dat2$Glazing[which(item32.dat2$Glazing %in% c("Decorative window (arch, etc.)"
                                                     ,"Half window"
                                                     ,"Double"
                                                     ,"Single"
                                                     ,"French door"
                                                     ,"Type unknown"))] <- "with Glazing"
item32.dat2$Framing.Categories <- paste(item32.dat2$Frame.Type, item32.dat2$Glazing, sep = " ")
unique(item32.dat2$Framing.Categories)

item32.dat2$count <- 1
item32.dat3 <- item32.dat2[-grep("Unknown|NA", item32.dat2$Framing.Categories, ignore.case = T),]
item32.dat3$Framing.Categories <- gsub(" None", "", item32.dat3$Framing.Categories)
unique(item32.dat3$Framing.Categories)


item32.data <- weightedData(item32.dat3[-which(colnames(item32.dat3) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Frame.Type"
                                                                            ,"Glazing"
                                                                            ,"Framing.Categories"
                                                                            ,"count"))])
item32.data <- left_join(item32.data, item32.dat3[which(colnames(item32.dat3) %in% c("CK_Cadmus_ID"
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



#################################
# Weighted Analysis
#################################
item32.final <- proportions_one_group(CustomerLevelData    = item32.data
                                      , valueVariable    = 'Quantity'
                                      , groupingVariable = 'Framing.Categories'
                                      , total.name       = "Total"
                                      , columnName       = "Door.Type"
                                      , weighted = TRUE)

item32.final.SF <- item32.final[which(item32.final$BuildingType == "Single Family"),-1]

exportTable(item32.final.SF, "SF", "Table 39", weighted = TRUE)



#################################
# Unweighted Analysis
#################################
item32.final <- proportions_one_group(CustomerLevelData    = item32.data
                                      , valueVariable    = 'Quantity'
                                      , groupingVariable = 'Framing.Categories'
                                      , total.name       = "Total"
                                      , columnName       = "Door.Type"
                                      , weighted = FALSE)

item32.final.SF <- item32.final[which(item32.final$BuildingType == "Single Family")
                                ,-which(colnames(item32.final) %in% c("Total.Count"
                                                                      ,"Door.Type"
                                                                      ,"BuildingType"))]

exportTable(item32.final.SF, "SF", "Table 39", weighted = FALSE)






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
length(unique(item33.dat1$CK_Cadmus_ID))

#subset to only windows
item33.dat2 <- item33.dat1[which(item33.dat1$Type == "Window"),]

#clean up frame/body type
unique(item33.dat2$`Frame./.Body.Type`)
item33.dat2$Frame.Type <- trimws(item33.dat2$`Frame./.Body.Type`)
item33.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass|wood|vinyl|fiberglass|tile|Garage", item33.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass/Tile"
item33.dat2$Frame.Type[grep("Metal|Aluminum|metal|aluminum", item33.dat2$Frame.Type)] <- "Metal"
item33.dat2$Frame.Type[grep("N/A", item33.dat2$Frame.Type)] <- "Unknown"
item33.dat2$Frame.Type[which(is.na(item33.dat2$Frame.Type))] <- "Unknown"
unique(item33.dat2$Frame.Type)

item33.dat2 <- item33.dat2[which(item33.dat2$Frame.Type != "Unknown"),]

#clean up glazing types
item33.dat2$Glazing <- trimws(item33.dat2$Glazing.Type)
item33.dat2$Glazing[grep("Single", item33.dat2$Glazing)] <- "Single Glazed"
item33.dat2$Glazing[grep("Double", item33.dat2$Glazing)] <- "Double Glazed"
item33.dat2$Glazing[grep("Triple", item33.dat2$Glazing)] <- "Triple Glazed"
item33.dat2$Glazing[which(!(item33.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(item33.dat2$Glazing)

item33.dat2 <- item33.dat2[which(item33.dat2$Glazing != "Unknown"),]

item33.dat2$Framing.Categories <- paste(item33.dat2$Frame.Type, item33.dat2$Glazing, sep = " ")

item33.dat2$count <- 1
item33.dat3 <- item33.dat2[which(!is.na(as.numeric(as.character(item33.dat2$Quantity)))),]


#insert weights
item33.data <- weightedData(item33.dat3[-which(colnames(item33.dat3) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Frame.Type"
                                                                            ,"Glazing"
                                                                            ,"Framing.Categories"
                                                                            ,"count"))])
item33.data <- left_join(item33.data, item33.dat3[which(colnames(item33.dat3) %in% c("CK_Cadmus_ID"
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

item33.data$Quantity <- as.numeric(as.character(item33.data$Quantity))

#################################
# Weighted Analysis
#################################
item33.final <- proportionRowsAndColumns1(CustomerLevelData     = item33.data
                                          , valueVariable       = 'Quantity'
                                          , columnVariable      = 'State'
                                          , rowVariable         = 'Framing.Categories'
                                          , aggregateColumnName = 'Region')
item33.final <- item33.final[which(item33.final$Framing.Categories != "Total"),]

item33.data$State.x <- item33.data$State
item33.all.framing.categories <- proportions_one_group(CustomerLevelData = item33.data
                                                       ,valueVariable    = 'Quantity'
                                                       ,groupingVariable = 'State.x'
                                                       ,total.name       = "All Framing Categories"
                                                       ,columnName       = "Framing.Categories"
                                                       ,weighted         = TRUE)
colnames(item33.all.framing.categories)[which(colnames(item33.all.framing.categories) == "State.x")] <- "State"

item33.final <- rbind.data.frame(item33.final, item33.all.framing.categories)

item33.cast <- dcast(setDT(item33.final)
                     , formula = BuildingType + Framing.Categories ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n"))

item33.table <- data.frame("BuildingType"        = item33.cast$BuildingType
                           ,"Framing.Categories" = item33.cast$Framing.Categories
                           ,"Percent_ID"         = item33.cast$w.percent_ID
                           ,"SE_ID"              = item33.cast$w.SE_ID
                           ,"Count_ID"           = item33.cast$count_ID
                           ,"Percent_MT"         = item33.cast$w.percent_MT
                           ,"SE_MT"              = item33.cast$w.SE_MT
                           ,"Count_MT"           = item33.cast$count_MT
                           ,"Percent_OR"         = item33.cast$w.percent_OR
                           ,"SE_OR"              = item33.cast$w.SE_OR
                           ,"Count_OR"           = item33.cast$count_OR
                           ,"Percent_WA"         = item33.cast$w.percent_WA
                           ,"SE_WA"              = item33.cast$w.SE_WA
                           ,"Count_WA"           = item33.cast$count_WA
                           ,"Percent_Region"     = item33.cast$w.percent_Region
                           ,"SE_Region"          = item33.cast$w.SE_Region
                           ,"Count_Region"       = item33.cast$count_Region
                           # ,"SampleSize"         = item33.cast$SampleSize_Region
)

item33.final.SF <- item33.table[which(item33.table$BuildingType == "Single Family"),-which(colnames(item33.table) %in% c("BuildingType"))]

exportTable(item33.final.SF, "SF", "Table 40", weighted = TRUE)


#################################
# Unweighted Analysis
#################################
item33.final <- proportions_two_groups_unweighted(CustomerLevelData     = item33.data
                                                  , valueVariable       = 'Quantity'
                                                  , columnVariable      = 'State'
                                                  , rowVariable         = 'Framing.Categories'
                                                  , aggregateColumnName = 'Region')
item33.final <- item33.final[which(item33.final$Framing.Categories != "Total"),]

item33.data$State.x <- item33.data$State
item33.all.framing.categories <- proportions_one_group(CustomerLevelData = item33.data
                                                       ,valueVariable    = 'Quantity'
                                                       ,groupingVariable = 'State.x'
                                                       ,total.name       = "All Framing Categories"
                                                       ,columnName       = "Framing.Categories"
                                                       ,weighted         = FALSE)
colnames(item33.all.framing.categories)[which(colnames(item33.all.framing.categories) == "State.x")] <- "State"

item33.final <- rbind.data.frame(item33.final, item33.all.framing.categories)
item33.cast <- dcast(setDT(item33.final)
                     , formula = BuildingType + Framing.Categories ~ State
                     , value.var = c("Percent", "SE", "Count", "SampleSize"))

item33.table <- data.frame("BuildingType"        = item33.cast$BuildingType
                           ,"Framing.Categories" = item33.cast$Framing.Categories
                           ,"Percent_ID"         = item33.cast$Percent_ID
                           ,"SE_ID"              = item33.cast$SE_ID
                           ,"Count_ID"           = item33.cast$Count_ID
                           ,"Percent_MT"         = item33.cast$Percent_MT
                           ,"SE_MT"              = item33.cast$SE_MT
                           ,"Count_MT"           = item33.cast$Count_MT
                           ,"Percent_OR"         = item33.cast$Percent_OR
                           ,"SE_OR"              = item33.cast$SE_OR
                           ,"Count_OR"           = item33.cast$Count_OR
                           ,"Percent_WA"         = item33.cast$Percent_WA
                           ,"SE_WA"              = item33.cast$SE_WA
                           ,"Count_WA"           = item33.cast$Count_WA
                           ,"Percent_Region"     = item33.cast$Percent_Region
                           ,"SE_Region"          = item33.cast$SE_Region
                           ,"Count_Region"       = item33.cast$Count_Region
                           # ,"SampleSize"         = item33.cast$SampleSize_Region
)

item33.final.SF <- item33.table[which(item33.table$BuildingType == "Single Family")
                                ,-which(colnames(item33.table) %in% c("BuildingType"
                                                                      ,"Total.Count"))]

exportTable(item33.final.SF, "SF", "Table 40", weighted = FALSE)






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
length(unique(item34.dat1$CK_Cadmus_ID))

item34.dat1$Indicator <- 0
item34.dat1$Indicator[which(item34.dat1$Type == 'Storm Window')] <- 1

item34.summary <- summarise(group_by(item34.dat1, CK_Cadmus_ID)
                            ,Ind = sum(unique(Indicator)))

item34.merge <- left_join(rbsa.dat, item34.summary)
item34.merge <- item34.merge[which(!is.na(item34.merge$Ind)),]


#insert weights
item34.data <- weightedData(item34.merge[-which(colnames(item34.merge) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Ind"))])
item34.data <- left_join(item34.data, item34.merge[which(colnames(item34.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"Type"
                                                                                     ,"Sub-Type"
                                                                                     ,"Area"
                                                                                     ,"Quantity"
                                                                                     ,"Frame./.Body.Type"
                                                                                     ,"Glazing.Type"
                                                                                     ,"Ind"))])
item34.data$Count <- 1

###############################
# Weighted Analysis
###############################
item34.final <- proportions_one_group(CustomerLevelData  = item34.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Remove"
                                      , weighted = TRUE)

item34.final.SF <- item34.final[which(item34.final$BuildingType == "Single Family"),-1]

exportTable(item34.final.SF, "SF", "Table 41", weighted = TRUE)

###############################
# Weighted Analysis
###############################
item34.final <- proportions_one_group(CustomerLevelData  = item34.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'State'
                                      , total.name       = "Region"
                                      , columnName       = "Remove"
                                      , weighted = FALSE)

item34.final.SF <- item34.final[which(item34.final$BuildingType == "Single Family")
                                ,-which(colnames(item34.final) %in% c("BuildingType"
                                                                      ,"Total.Count"
                                                                      ,"Remove"))]

exportTable(item34.final.SF, "SF", "Table 41", weighted = FALSE)
