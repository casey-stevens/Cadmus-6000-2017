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
                                                       ,weighted         = TRUE
                                                       ,two.prop.total   = TRUE)
item33.all.framing.categories$State.x[which(item33.all.framing.categories$State.x == "Total")] <- "Region"
colnames(item33.all.framing.categories)[which(colnames(item33.all.framing.categories) == "State.x")] <- "State"

item33.final <- rbind.data.frame(item33.final, item33.all.framing.categories)

item33.cast <- dcast(setDT(item33.final)
                     , formula = BuildingType + Framing.Categories ~ State
                     , value.var = c("w.percent", "w.SE", "count", "n", "EB"))

item33.table <- data.frame("BuildingType"        = item33.cast$BuildingType
                           ,"Framing.Categories" = item33.cast$Framing.Categories
                           ,"Percent_ID"         = item33.cast$w.percent_ID
                           ,"SE_ID"              = item33.cast$w.SE_ID
                           ,"n_ID"               = item33.cast$n_ID
                           ,"Percent_MT"         = item33.cast$w.percent_MT
                           ,"SE_MT"              = item33.cast$w.SE_MT
                           ,"n_MT"               = item33.cast$n_MT
                           ,"Percent_OR"         = item33.cast$w.percent_OR
                           ,"SE_OR"              = item33.cast$w.SE_OR
                           ,"n_OR"               = item33.cast$n_OR
                           ,"Percent_WA"         = item33.cast$w.percent_WA
                           ,"SE_WA"              = item33.cast$w.SE_WA
                           ,"n_WA"               = item33.cast$n_WA
                           ,"Percent_Region"     = item33.cast$w.percent_Region
                           ,"SE_Region"          = item33.cast$w.SE_Region
                           ,"n_Region"           = item33.cast$n_Region
                           ,"EB_ID"              = item33.cast$EB_ID
                           ,"EB_MT"              = item33.cast$EB_MT
                           ,"EB_OR"              = item33.cast$EB_OR
                           ,"EB_WA"              = item33.cast$EB_WA
                           ,"EB_Region"          = item33.cast$EB_Region
)


levels(item33.table$Framing.Categories)
rowOrder <- c("Metal Single Glazed"
              ,"Metal Double Glazed"
              ,"Metal Triple Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Single Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Double Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Triple Glazed"
              ,"Other Double Glazed" 
              ,"All Framing Categories"
)
item33.table <- item33.table %>% mutate(Framing.Categories = factor(Framing.Categories, levels = rowOrder)) %>% arrange(Framing.Categories)  
item33.table <- data.frame(item33.table)


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
                                                       ,weighted         = FALSE
                                                       ,two.prop.total   = TRUE)
item33.all.framing.categories$State.x[which(item33.all.framing.categories$State.x == "Total")] <- "Region"
colnames(item33.all.framing.categories)[which(colnames(item33.all.framing.categories) == "State.x")] <- "State"

item33.final <- rbind.data.frame(item33.final, item33.all.framing.categories)
item33.cast <- dcast(setDT(item33.final)
                     , formula = BuildingType + Framing.Categories ~ State
                     , value.var = c("Percent", "SE", "Count", "n"))

item33.table <- data.frame("BuildingType"        = item33.cast$BuildingType
                           ,"Framing.Categories" = item33.cast$Framing.Categories
                           ,"Percent_ID"         = item33.cast$Percent_ID
                           ,"SE_ID"              = item33.cast$SE_ID
                           ,"n_ID"               = item33.cast$n_ID
                           ,"Percent_MT"         = item33.cast$Percent_MT
                           ,"SE_MT"              = item33.cast$SE_MT
                           ,"n_MT"               = item33.cast$n_MT
                           ,"Percent_OR"         = item33.cast$Percent_OR
                           ,"SE_OR"              = item33.cast$SE_OR
                           ,"n_OR"               = item33.cast$n_OR
                           ,"Percent_WA"         = item33.cast$Percent_WA
                           ,"SE_WA"              = item33.cast$SE_WA
                           ,"n_WA"               = item33.cast$n_WA
                           ,"Percent_Region"     = item33.cast$Percent_Region
                           ,"SE_Region"          = item33.cast$SE_Region
                           ,"n_Region"           = item33.cast$n_Region
)

levels(item33.table$Framing.Categories)
rowOrder <- c("Metal Single Glazed"
              ,"Metal Double Glazed"
              ,"Metal Triple Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Single Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Double Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Triple Glazed"
              ,"Other Double Glazed" 
              ,"All Framing Categories"
)
item33.table <- item33.table %>% mutate(Framing.Categories = factor(Framing.Categories, levels = rowOrder)) %>% arrange(Framing.Categories)  
item33.table <- data.frame(item33.table)


item33.final.SF <- item33.table[which(item33.table$BuildingType == "Single Family")
                                ,-which(colnames(item33.table) %in% c("BuildingType"))]

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
item34.dat1$Indicator[grep("storm",item34.dat1$Type, ignore.case = T)] <- 1

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
                                      , weighted = FALSE)

item34.final.SF <- item34.final[which(item34.final$BuildingType == "Single Family")
                                ,-which(colnames(item34.final) %in% c("BuildingType"))]

exportTable(item34.final.SF, "SF", "Table 41", weighted = FALSE)
























############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#############################################################################################
#Item 32: DISTRIBUTION OF DOOR TYPES
#############################################################################################

item32.os.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item32.os.dat1 <- left_join(scl.dat, item32.os.dat, by = "CK_Cadmus_ID")
length(unique(item32.os.dat1$CK_Cadmus_ID)) 

#subset to only doors
item32.os.dat2 <- item32.os.dat1[which(item32.os.dat1$Type == "Door"),]

#clean up frame/body type
item32.os.dat2$Frame.Type <- trimws(item32.os.dat2$`Frame./.Body.Type`)
item32.os.dat2$Frame.Type[grep("wood|fiberglass", item32.os.dat2$Frame.Type, ignore.case = T)] <- "Wood/Fiberglass"
item32.os.dat2$Frame.Type[grep("metal|aluminum", item32.os.dat2$Frame.Type, ignore.case = T)] <- "Metal"
item32.os.dat2$Frame.Type[grep("plastic|other|vinyl", item32.os.dat2$Frame.Type, ignore.case = T)] <- "Other"
unique(item32.os.dat2$Frame.Type)

#clean up glazing types
item32.os.dat2$Glazing <- trimws(item32.os.dat2$Glazing.Type)
unique(item32.os.dat2$Glazing)
item32.os.dat2$Glazing[which(item32.os.dat2$Glazing %in% c("Decorative window (arch, etc.)"
                                                     ,"Half window"
                                                     ,"Double"
                                                     ,"Single"
                                                     ,"French door"
                                                     ,"Type unknown"))] <- "with Glazing"
item32.os.dat2$Framing.Categories <- paste(item32.os.dat2$Frame.Type, item32.os.dat2$Glazing, sep = " ")
unique(item32.os.dat2$Framing.Categories)

item32.os.dat2$count <- 1
item32.os.dat3 <- item32.os.dat2[-grep("Unknown|NA", item32.os.dat2$Framing.Categories, ignore.case = T),]
item32.os.dat3$Framing.Categories <- gsub(" None", "", item32.os.dat3$Framing.Categories)
unique(item32.os.dat3$Framing.Categories)


item32.os.data <- weightedData(item32.os.dat3[-which(colnames(item32.os.dat3) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Frame.Type"
                                                                            ,"Glazing"
                                                                            ,"Framing.Categories"
                                                                            ,"count"))])
item32.os.data <- left_join(item32.os.data, unique(item32.os.dat3[which(colnames(item32.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Type"
                                                                                     ,"Sub-Type"
                                                                                     ,"Area"
                                                                                     ,"Quantity"
                                                                                     ,"Frame./.Body.Type"
                                                                                     ,"Glazing.Type"
                                                                                     ,"Frame.Type"
                                                                                     ,"Glazing"
                                                                                     ,"Framing.Categories"
                                                                                     ,"count"))]))

item32.os.data$Quantity <- as.numeric(as.character(item32.os.data$Quantity))



#################################
# Weighted Analysis
#################################
item32.os.final <- proportionRowsAndColumns1(CustomerLevelData = item32.os.data
                                             ,valueVariable = 'Quantity'
                                             ,columnVariable = 'CK_Building_ID'
                                             ,rowVariable = 'Framing.Categories'
                                             ,aggregateColumnName = "Remove")
item32.os.final <- item32.os.final[which(item32.os.final$CK_Building_ID != "Remove"),]

item32.os.cast <- dcast(setDT(item32.os.final)
                        ,formula = Framing.Categories ~ CK_Building_ID
                        ,value.var = c("w.percent","w.SE","n","EB"))

item32.os.table <- data.frame("Door.Type"             = item32.os.cast$Framing.Categories
                              ,"Percent_SCL.GenPop"   = item32.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item32.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item32.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item32.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = item32.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = item32.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item32.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = item32.os.cast$`w.SE_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item32.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item32.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item32.os.cast$`n_2017 RBSA PS`
                              ,"n_SCL.EH"             = item32.os.cast$`n_SCL EH`
                              ,"EB_SCL.GenPop"        = item32.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item32.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item32.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item32.os.cast$`EB_2017 RBSA PS`)

# row ordering example code
levels(item32.os.table$Door.Type)
rowOrder <- c("Metal"
              ,"Metal with Glazing"
              ,"Other"
              ,"Other with Glazing"
              ,"Wood/Fiberglass"
              ,"Wood/Fiberglass with Glazing"
              ,"Total")
item32.os.table <- item32.os.table %>% mutate(Door.Type = factor(Door.Type, levels = rowOrder)) %>% arrange(Door.Type)  
item32.os.table <- data.frame(item32.os.table)


exportTable(item32.os.table, "SF", "Table 39", weighted = TRUE, osIndicator = "SCL", OS = T)



#################################
# Unweighted Analysis
#################################
item32.os.final <- proportions_two_groups_unweighted(CustomerLevelData = item32.os.data
                                             ,valueVariable = 'Quantity'
                                             ,columnVariable = 'CK_Building_ID'
                                             ,rowVariable = 'Framing.Categories'
                                             ,aggregateColumnName = "Remove")
item32.os.final <- item32.os.final[which(item32.os.final$CK_Building_ID != "Remove"),]

item32.os.cast <- dcast(setDT(item32.os.final)
                         ,formula = Framing.Categories ~ CK_Building_ID
                         ,value.var = c("Percent","SE","n"))

item32.os.table <- data.frame("Door.Type"             = item32.os.cast$Framing.Categories
                              ,"Percent_SCL.GenPop"   = item32.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item32.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item32.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item32.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item32.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item32.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item32.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item32.os.cast$`SE_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item32.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item32.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item32.os.cast$`n_2017 RBSA PS`
                              ,"n_SCL.EH"             = item32.os.cast$`n_SCL EH`)

# row ordering example code
levels(item32.os.table$Door.Type)
rowOrder <- c("Metal"
              ,"Metal with Glazing"
              ,"Other"
              ,"Other with Glazing"
              ,"Wood/Fiberglass"
              ,"Wood/Fiberglass with Glazing"
              ,"Total")
item32.os.table <- item32.os.table %>% mutate(Door.Type = factor(Door.Type, levels = rowOrder)) %>% arrange(Door.Type)  
item32.os.table <- data.frame(item32.os.table)

exportTable(item32.os.table, "SF", "Table 39", weighted = FALSE, osIndicator = "SCL", OS = T)






#############################################################################################
#Item 33: DISTRIBUTION OF WINDOW TYPES BY CK_Building_ID
#############################################################################################
item33.os.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item33.os.dat1 <- left_join(scl.dat, item33.os.dat, by = "CK_Cadmus_ID")
length(unique(item33.os.dat1$CK_Cadmus_ID))

#subset to only windows
item33.os.dat2 <- item33.os.dat1[which(item33.os.dat1$Type == "Window"),]

#clean up frame/body type
unique(item33.os.dat2$`Frame./.Body.Type`)
item33.os.dat2$Frame.Type <- trimws(item33.os.dat2$`Frame./.Body.Type`)
item33.os.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass|wood|vinyl|fiberglass|tile|Garage", item33.os.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass/Tile"
item33.os.dat2$Frame.Type[grep("Metal|Aluminum|metal|aluminum", item33.os.dat2$Frame.Type)] <- "Metal"
item33.os.dat2$Frame.Type[grep("N/A", item33.os.dat2$Frame.Type)] <- "Unknown"
item33.os.dat2$Frame.Type[which(is.na(item33.os.dat2$Frame.Type))] <- "Unknown"
unique(item33.os.dat2$Frame.Type)

item33.os.dat2 <- item33.os.dat2[which(item33.os.dat2$Frame.Type != "Unknown"),]

#clean up glazing types
item33.os.dat2$Glazing <- trimws(item33.os.dat2$Glazing.Type)
item33.os.dat2$Glazing[grep("Single", item33.os.dat2$Glazing)] <- "Single Glazed"
item33.os.dat2$Glazing[grep("Double", item33.os.dat2$Glazing)] <- "Double Glazed"
item33.os.dat2$Glazing[grep("Triple", item33.os.dat2$Glazing)] <- "Triple Glazed"
item33.os.dat2$Glazing[which(!(item33.os.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(item33.os.dat2$Glazing)

item33.os.dat2 <- item33.os.dat2[which(item33.os.dat2$Glazing != "Unknown"),]

item33.os.dat2$Framing.Categories <- paste(item33.os.dat2$Frame.Type, item33.os.dat2$Glazing, sep = " ")

item33.os.dat2$count <- 1
item33.os.dat3 <- item33.os.dat2[which(!is.na(as.numeric(as.character(item33.os.dat2$Quantity)))),]


#insert weights
item33.os.data <- weightedData(item33.os.dat3[-which(colnames(item33.os.dat3) %in% c("Type"
                                                                            ,"Sub-Type"
                                                                            ,"Area"
                                                                            ,"Quantity"
                                                                            ,"Frame./.Body.Type"
                                                                            ,"Glazing.Type"
                                                                            ,"Frame.Type"
                                                                            ,"Glazing"
                                                                            ,"Framing.Categories"
                                                                            ,"count"))])
item33.os.data <- left_join(item33.os.data, unique(item33.os.dat3[which(colnames(item33.os.dat3) %in% c("CK_Cadmus_ID"
                                                                                     ,"Type"
                                                                                     ,"Sub-Type"
                                                                                     ,"Area"
                                                                                     ,"Quantity"
                                                                                     ,"Frame./.Body.Type"
                                                                                     ,"Glazing.Type"
                                                                                     ,"Frame.Type"
                                                                                     ,"Glazing"
                                                                                     ,"Framing.Categories"
                                                                                     ,"count"))]))

item33.os.data$Quantity <- as.numeric(as.character(item33.os.data$Quantity))

#################################
# Weighted Analysis
#################################
item33.os.final <- proportionRowsAndColumns1(CustomerLevelData     = item33.os.data
                                          , valueVariable       = 'Quantity'
                                          , columnVariable      = 'CK_Building_ID'
                                          , rowVariable         = 'Framing.Categories'
                                          , aggregateColumnName = 'Remove')
# item33.os.final <- item33.os.final[which(item33.os.final$Framing.Categories != "Total"),]
item33.os.final <- item33.os.final[which(item33.os.final$CK_Building_ID != "Remove"),]

item33.os.cast <- dcast(setDT(item33.os.final)
                     , formula = BuildingType + Framing.Categories ~ CK_Building_ID
                     , value.var = c("w.percent", "w.SE", "count", "n", "EB"))

item33.os.table <- data.frame("BuildingType"          = item33.os.cast$BuildingType
                              ,"Framing.Categories"   = item33.os.cast$Framing.Categories
                              ,"Percent_SCL.GenPop"   = item33.os.cast$`w.percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item33.os.cast$`w.SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item33.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item33.os.cast$`w.percent_SCL LI`
                              ,"SE_SCL.LI"            = item33.os.cast$`w.SE_SCL LI`
                              ,"n_SCL.LI"             = item33.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item33.os.cast$`w.percent_SCL EH`
                              ,"SE_SCL.EH"            = item33.os.cast$`w.SE_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item33.os.cast$`w.percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item33.os.cast$`w.SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item33.os.cast$`n_2017 RBSA PS`
                              ,"n_SCL.EH"             = item33.os.cast$`n_SCL EH`
                              ,"EB_SCL.GenPop"        = item33.os.cast$`EB_SCL GenPop`
                              ,"EB_SCL.LI"            = item33.os.cast$`EB_SCL LI`
                              ,"EB_SCL.EH"            = item33.os.cast$`EB_SCL EH`
                              ,"EB_2017.RBSA.PS"      = item33.os.cast$`EB_2017 RBSA PS`)


levels(item33.os.table$Framing.Categories)
rowOrder <- c("Metal Single Glazed"
              ,"Metal Double Glazed"
              ,"Metal Triple Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Single Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Double Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Triple Glazed"
              ,"Other Double Glazed" 
              ,"Total"
)
item33.os.table <- item33.os.table %>% mutate(Framing.Categories = factor(Framing.Categories, levels = rowOrder)) %>% arrange(Framing.Categories)  
item33.os.table <- data.frame(item33.os.table)


item33.os.final.SF <- item33.os.table[which(item33.os.table$BuildingType == "Single Family"),-which(colnames(item33.os.table) %in% c("BuildingType"))]

exportTable(item33.os.final.SF, "SF", "Table 40", weighted = TRUE, osIndicator = "SCL", OS = T)


#################################
# Unweighted Analysis
#################################
item33.os.final <- proportions_two_groups_unweighted(CustomerLevelData     = item33.os.data
                                                  , valueVariable       = 'Quantity'
                                                  , columnVariable      = 'CK_Building_ID'
                                                  , rowVariable         = 'Framing.Categories'
                                                  , aggregateColumnName = 'Remove')
item33.os.final <- item33.os.final[which(item33.os.final$CK_Building_ID != "Remove"),]

item33.os.cast <- dcast(setDT(item33.os.final)
                     , formula = BuildingType + Framing.Categories ~ CK_Building_ID
                     , value.var = c("Percent", "SE", "Count", "n"))

item33.os.table <- data.frame("BuildingType"          = item33.os.cast$BuildingType
                              ,"Framing.Categories"   = item33.os.cast$Framing.Categories
                              ,"Percent_SCL.GenPop"   = item33.os.cast$`Percent_SCL GenPop`
                              ,"SE_SCL.GenPop"        = item33.os.cast$`SE_SCL GenPop`
                              ,"n_SCL.GenPop"         = item33.os.cast$`n_SCL GenPop`
                              ,"Percent_SCL.LI"       = item33.os.cast$`Percent_SCL LI`
                              ,"SE_SCL.LI"            = item33.os.cast$`SE_SCL LI`
                              ,"n_SCL.LI"             = item33.os.cast$`n_SCL LI`
                              ,"Percent_SCL.EH"       = item33.os.cast$`Percent_SCL EH`
                              ,"SE_SCL.EH"            = item33.os.cast$`SE_SCL EH`
                              ,"n_SCL.EH"             = item33.os.cast$`n_SCL EH`
                              ,"Percent_2017.RBSA.PS" = item33.os.cast$`Percent_2017 RBSA PS`
                              ,"SE_2017.RBSA.PS"      = item33.os.cast$`SE_2017 RBSA PS`
                              ,"n_2017.RBSA.PS"       = item33.os.cast$`n_2017 RBSA PS`)


levels(item33.os.table$Framing.Categories)
rowOrder <- c("Metal Single Glazed"
              ,"Metal Double Glazed"
              ,"Metal Triple Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Single Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Double Glazed"
              ,"Wood/Vinyl/Fiberglass/Tile Triple Glazed"
              ,"Other Double Glazed" 
              ,"All Framing Categories"
)
item33.os.table <- item33.os.table %>% mutate(Framing.Categories = factor(Framing.Categories, levels = rowOrder)) %>% arrange(Framing.Categories)  
item33.os.table <- data.frame(item33.os.table)


item33.os.final.SF <- item33.os.table[which(item33.os.table$BuildingType == "Single Family")
                                ,-which(colnames(item33.os.table) %in% c("BuildingType"))]

exportTable(item33.os.final.SF, "SF", "Table 40", weighted = FALSE, osIndicator = "SCL", OS = T)






#############################################################################################
#Item 34: PERCENTAGE OF HOMES WITH STORM WINDOWS BY CK_Building_ID
#############################################################################################
item34.os.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
item34.os.dat1 <- left_join(scl.dat, item34.os.dat, by = "CK_Cadmus_ID")
length(unique(item34.os.dat1$CK_Cadmus_ID))

item34.os.dat1$Indicator <- 0
item34.os.dat1$Indicator[grep("storm",item34.os.dat1$Type, ignore.case = T)] <- 1

item34.os.summary <- summarise(group_by(item34.os.dat1, CK_Cadmus_ID)
                            ,Ind = sum(unique(Indicator)))

item34.os.merge <- left_join(scl.dat, item34.os.summary)
item34.os.merge <- item34.os.merge[which(!is.na(item34.os.merge$Ind)),]


#insert weights
item34.os.data <- weightedData(item34.os.merge[-which(colnames(item34.os.merge) %in% c("Type"
                                                                              ,"Sub-Type"
                                                                              ,"Area"
                                                                              ,"Quantity"
                                                                              ,"Frame./.Body.Type"
                                                                              ,"Glazing.Type"
                                                                              ,"Ind"))])
item34.os.data <- left_join(item34.os.data, unique(item34.os.merge[which(colnames(item34.os.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Type"
                                                                                       ,"Sub-Type"
                                                                                       ,"Area"
                                                                                       ,"Quantity"
                                                                                       ,"Frame./.Body.Type"
                                                                                       ,"Glazing.Type"
                                                                                       ,"Ind"))]))
item34.os.data$Count <- 1

###############################
# Weighted Analysis
###############################
item34.os.final <- proportions_one_group(CustomerLevelData  = item34.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Remove"
                                      , weighted = TRUE)
item34.os.final <- item34.os.final[which(item34.os.final$CK_Building_ID != "Total"),]
item34.os.final.SF <- item34.os.final[which(item34.os.final$BuildingType == "Single Family"),-1]

exportTable(item34.os.final.SF, "SF", "Table 41", weighted = TRUE, osIndicator = "SCL", OS = T)

###############################
# Weighted Analysis
###############################
item34.os.final <- proportions_one_group(CustomerLevelData  = item34.os.data
                                      , valueVariable    = 'Ind'
                                      , groupingVariable = 'CK_Building_ID'
                                      , total.name       = "Region"
                                      , weighted = FALSE)
item34.os.final <- item34.os.final[which(item34.os.final$CK_Building_ID != "Total"),]

item34.os.final.SF <- item34.os.final[which(item34.os.final$BuildingType == "Single Family")
                                ,-which(colnames(item34.os.final) %in% c("BuildingType"))]

exportTable(item34.os.final.SF, "SF", "Table 41", weighted = FALSE, osIndicator = "SCL", OS = T)

