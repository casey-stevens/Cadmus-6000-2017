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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#Read in data for analysis
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item166.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item166.dat.11 <- item166.dat.1[which(item166.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item166.dat.12 <- item166.dat.11[which(item166.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item166.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item166.dat.12$Primary.Heating.System)
item166.dat.12$count <- 1

item166.dat.13 <- unique(item166.dat.12[which(item166.dat.12$Heating.Fuel == "Electric"),])

item166.sum <- summarise(group_by(item166.dat.13, CK_Cadmus_ID, Heating.Fuel)
                         ,Count = sum(count))
item166.sum$Count <- 1
which(duplicated(item166.sum$CK_Cadmus_ID)) #none are duplicated!
unique(item166.sum$Heating.Fuel)

item166.merge <- left_join(rbsa.dat, item166.sum)
item166.merge <- item166.merge[which(!is.na(item166.merge$Heating.Fuel)),]

item166.mechanical <- item166.merge





#############################################################################################
# 
#Windows Data
# 
#############################################################################################
item166.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]

item166.dat0 <- left_join(item166.dat, item166.mechanical, by = "CK_Cadmus_ID")
item166.dat0.1 <- item166.dat0[which(item166.dat0$Heating.Fuel == "Electric"),]


item166.dat1 <- left_join(item166.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")
length(unique(item166.dat1$CK_Cadmus_ID)) #316

#subset to only windows
item166.dat2 <- item166.dat1[which(item166.dat1$Type == "Window"),]

#clean up frame/body type
unique(item166.dat2$`Frame./.Body.Type`)
item166.dat2$Frame.Type <- trimws(item166.dat2$`Frame./.Body.Type`)
item166.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass|wood|vinyl|fiberglass|tile|Garage", item166.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass/Tile"
item166.dat2$Frame.Type[grep("Metal|Aluminum|metal|aluminum", item166.dat2$Frame.Type)] <- "Metal"
item166.dat2$Frame.Type[grep("N/A", item166.dat2$Frame.Type)] <- "Unknown"
item166.dat2$Frame.Type[which(is.na(item166.dat2$Frame.Type))] <- "Unknown"
unique(item166.dat2$Frame.Type)

item166.dat2 <- item166.dat2[which(item166.dat2$Frame.Type != "Unknown"),]

#clean up glazing types
item166.dat2$Glazing <- trimws(item166.dat2$Glazing.Type)
item166.dat2$Glazing[grep("Single", item166.dat2$Glazing)] <- "Single Glazed"
item166.dat2$Glazing[grep("Double", item166.dat2$Glazing)] <- "Double Glazed"
item166.dat2$Glazing[grep("Triple", item166.dat2$Glazing)] <- "Triple Glazed"
item166.dat2$Glazing[which(!(item166.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(item166.dat2$Glazing)

item166.dat2 <- item166.dat2[which(item166.dat2$Glazing != "Unknown"),]

item166.dat2$Framing.Categories <- paste(item166.dat2$Frame.Type, item166.dat2$Glazing, sep = " ")

item166.dat2$count <- 1
unique(item166.dat2$Quantity)
item166.dat2$Quantity <- as.numeric(as.character(item166.dat2$Quantity))
item166.dat3 <- item166.dat2[which(!is.na(as.numeric(as.character(item166.dat2$Quantity)))),]

item166.sum <- summarise(group_by(item166.dat3, CK_Cadmus_ID, Framing.Categories)
                         ,Quantity = sum(Quantity))

#############################################################################################
#
# Merge mechanical and window data together
#
#############################################################################################

item166.merge <- left_join(item166.sum, item166.mechanical)

item166.merge1 <- item166.merge[which(!(is.na(item166.merge$Heating.Fuel))),]

item166.merge2 <- left_join(rbsa.dat, item166.merge1)
item166.merge <- item166.merge2[which(!is.na(item166.merge2$Quantity)),]
item166.merge <- item166.merge[which(!is.na(item166.merge$Framing.Categories)),]



################################################
# Adding pop and sample sizes for weights
################################################
item166.data <- weightedData(item166.merge[-which(colnames(item166.merge) %in% c("Heating.Fuel"
                                                                                 ,"Count"
                                                                                 ,"Framing.Categories"
                                                                                 ,"Quantity"))])
item166.data <- left_join(item166.data, item166.merge[which(colnames(item166.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating.Fuel"
                                                                                           ,"Count"
                                                                                           ,"Framing.Categories"
                                                                                           ,"Quantity"))])


#######################
# Weighted Analysis
#######################
item166.final <- proportionRowsAndColumns1(CustomerLevelData = item166.data
                                           ,valueVariable    = 'Quantity'
                                           ,columnVariable   = 'State'
                                           ,rowVariable      = 'Framing.Categories'
                                           ,aggregateColumnName = "Region")
item166.final$Framing.Categories[which(item166.final$Framing.Categories == "Total")] <- "All Framing Types"

item166.cast <- dcast(setDT(item166.final)
                      , formula = BuildingType + Framing.Categories ~ State
                      , value.var = c("w.percent", "w.SE", "count", "n", "N"))

item166.table <- data.frame("BuildingType"    = item166.cast$BuildingType
                            ,"Framing.Categories"= item166.cast$Framing.Categories
                            ,"Percent_ID"     = item166.cast$w.percent_ID
                            ,"SE_ID"          = item166.cast$w.SE_ID
                            # ,"Count_ID"       = item166.cast$count_ID
                            ,"n_ID"           = item166.cast$n_ID
                            ,"Percent_MT"     = item166.cast$w.percent_MT
                            ,"SE_MT"          = item166.cast$w.SE_MT
                            # ,"Count_MT"       = item166.cast$count_MT
                            ,"n_MT"           = item166.cast$n_MT
                            ,"Percent_OR"     = item166.cast$w.percent_OR
                            ,"SE_OR"          = item166.cast$w.SE_OR
                            # ,"Count_OR"       = item166.cast$count_OR
                            ,"n_OR"           = item166.cast$n_OR
                            ,"Percent_WA"     = item166.cast$w.percent_WA
                            ,"SE_WA"          = item166.cast$w.SE_WA
                            # ,"Count_WA"       = item166.cast$count_WA
                            ,"n_WA"           = item166.cast$n_WA
                            ,"Percent_Region" = item166.cast$w.percent_Region
                            ,"SE_Region"      = item166.cast$w.SE_Region
                            # ,"Count_Region"   = item166.cast$count_Region
                            ,"n_Region"       = item166.cast$n_Region
)


item166.final.SF <- item166.table[which(item166.table$BuildingType == "Single Family")
                                  ,-which(colnames(item166.table) %in% c("BuildingType"))]

exportTable(item166.final.SF, "SF", "Table B-11", weighted = TRUE)


#######################
# Unweighted Analysis
#######################
item166.final <- proportions_two_groups_unweighted(CustomerLevelData = item166.data
                                                   ,valueVariable    = 'Quantity'
                                                   ,columnVariable   = 'State'
                                                   ,rowVariable      = 'Framing.Categories'
                                                   ,aggregateColumnName = "Region")
item166.final$Framing.Categories[which(item166.final$Framing.Categories == "Total")] <- "All Framing Types"


item166.cast <- dcast(setDT(item166.final)
                      , formula = BuildingType + Framing.Categories ~ State
                      , value.var = c("Percent", "SE", "Count", "n"))


item166.table <- data.frame("BuildingType"    = item166.cast$BuildingType
                            ,"Framing.Categories"= item166.cast$Framing.Categories
                            ,"Percent_ID"     = item166.cast$Percent_ID
                            ,"SE_ID"          = item166.cast$SE_ID
                            ,"n_ID"       = item166.cast$n_ID
                            ,"Percent_MT"     = item166.cast$Percent_MT
                            ,"SE_MT"          = item166.cast$SE_MT
                            ,"n_MT"       = item166.cast$n_MT
                            ,"Percent_OR"     = item166.cast$Percent_OR
                            ,"SE_OR"          = item166.cast$SE_OR
                            ,"n_OR"       = item166.cast$n_OR
                            ,"Percent_WA"     = item166.cast$Percent_WA
                            ,"SE_WA"          = item166.cast$SE_WA
                            ,"n_WA"       = item166.cast$n_WA
                            ,"Percent_Region" = item166.cast$Percent_Region
                            ,"SE_Region"      = item166.cast$SE_Region
                            ,"n_Region"   = item166.cast$n_Region
)


item166.final.SF <- item166.table[which(item166.table$BuildingType == "Single Family")
                                  ,-which(colnames(item166.table) %in% c("BuildingType"))]

exportTable(item166.final.SF, "SF", "Table B-11", weighted = FALSE)

