#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))

#Read in data for analysis
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))


#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))




#############################################################################################
#Item 293: AVERAGE NUMBER OF APPLIANCES PER HOME BY TYPE (MF table 86)
#############################################################################################
item293.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),which(names(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                                                             ,"Generic"))]
item293.mech$Water.Heater <- 1
item293.mech1 <- left_join(rbsa.dat, item293.mech, by = "CK_Cadmus_ID")
item293.mech2 <- unique(item293.mech1[grep("Multifamily", item293.mech1$BuildingType),])
item293.mech2 <- unique(item293.mech1[grep("SITE", item293.mech1$CK_Building_ID),])
which(duplicated(item293.mech2$CK_Cadmus_ID))

item293.mech2$Water.Heater[which(is.na(item293.mech2$Water.Heater))] <- 0
item293.mech2$count <- 1

#summarise by home
item293.site <- summarise(group_by(item293.mech2, CK_Cadmus_ID, Category)
                          ,Water.Heater = sum(Water.Heater))
unique(item293.site$Water.Heater)


#subset to columns needed for analysis
item293.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"CK_SiteID"
                                                                    ,"Type"
                                                                    ,"Large.Unusual.Load.Quantity"))]

item293.dat00 <- item293.dat[grep("SITE",item293.dat$CK_SiteID),]

item293.dat0 <- item293.dat00[which(item293.dat00$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item293.dat1 <- left_join(rbsa.dat, item293.dat0)

item293.dat2 <- item293.dat1[grep("Multifamily", item293.dat1$BuildingType),]

item293.dat2$Large.Unusual.Load.Quantity[which(is.na(item293.dat2$Large.Unusual.Load.Quantity))] <- 1
item293.dat2$Large.Unusual.Load.Quantity[which(item293.dat2$Large.Unusual.Load.Quantity %in% c("Datapoint not asked for", "N/A"))] <- 1
unique(item293.dat2$Large.Unusual.Load.Quantity)

item293.dat2$Large.Unusual.Load.Quantity <- as.numeric(as.character(item293.dat2$Large.Unusual.Load.Quantity))
item293.dat2$Large.Unusual.Load.Quantity[which(item293.dat2$Large.Unusual.Load.Quantity == 0)] <- 1
item293.dat2$count <- 1

item293.dat2$TotalQty <- item293.dat2$Large.Unusual.Load.Quantity * item293.dat2$count
item293.dat2[is.na(item293.dat2)] <- 0
unique(item293.dat2$TotalQty)

item293.dat3 <- item293.dat2[grep("SITE",item293.dat2$CK_Building_ID),]

item293.sum <- summarise(group_by(item293.dat3, CK_Cadmus_ID, Category, Type)
                        ,SiteCount = sum(TotalQty,na.rm = T))
unique(item293.sum$Type)
item293.cast <- dcast(setDT(item293.sum)
                      ,formula = CK_Cadmus_ID + Category ~ Type
                      ,value.var = "SiteCount")
item293.cast <- data.frame(item293.cast, stringsAsFactors = F)
item293.cast[is.na(item293.cast)] <- 0

item293.join <- left_join(item293.cast, item293.site)

item293.melt <- melt(item293.join, id = c("CK_Cadmus_ID","Category"))
colnames(item293.melt) <- c("CK_Cadmus_ID", "Category", "Type", "Count")


item293.merge <- left_join(rbsa.dat, item293.melt)
item293.merge <- item293.merge[grep("site", item293.merge$CK_Building_ID, ignore.case = T),]
item293.merge <- item293.merge[which(!is.na(item293.merge$Count)),]
length(unique(item293.merge$CK_Cadmus_ID))
######################################
#Pop and Sample Sizes for weights
######################################
item293.data <- weightedData(item293.merge[which(colnames(item293.merge) %notin% c("Type"
                                                                                   ,"Count"
                                                                                   ,"Category"))])

item293.data <- left_join(item293.data, item293.merge[which(colnames(item293.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Count"
                                                                                           ,"Category"))])
item293.data$count <- 1

#########################
# weighted analysis
#########################
item293.cast <- mean_two_groups(CustomerLevelData = item293.data
                                ,valueVariable = "Count"
                                ,byVariableRow = "Type"
                                ,byVariableColumn = "Category"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "Remove")
item293.cast <- item293.cast[which(item293.cast$Type != "Remove"),]


item293.final <- data.frame("Type"                 = item293.cast$Type
                            ,"PSE.Mean"                 = item293.cast$Mean_PSE
                            ,"PSE.SE"                   = item293.cast$SE_PSE
                            ,"PSE.n"                    = item293.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item293.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item293.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item293.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item293.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item293.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item293.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item293.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item293.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item293.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                   = item293.cast$EB_PSE
                            ,"PSE.King.County_EB"       = item293.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"   = item293.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"          = item293.cast$`EB_2017 RBSA PS`
)

exportTable(item293.final, "MF", "Table 86", weighted = TRUE,OS = T, osIndicator = "PSE")


#########################
# weighted analysis
#########################
item293.cast <- mean_two_groups_unweighted(CustomerLevelData = item293.data
                                           ,valueVariable = "Count"
                                           ,byVariableRow = "Type"
                                           ,byVariableColumn = "Category"
                                           ,columnAggregate = "Remove"
                                           ,rowAggregate = "Remove")
item293.cast <- item293.cast[which(item293.cast$Type != "Remove"),]


item293.final <- data.frame("Type"                 = item293.cast$Type
                            ,"PSE.Mean"                 = item293.cast$Mean_PSE
                            ,"PSE.SE"                   = item293.cast$SE_PSE
                            ,"PSE.n"                    = item293.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item293.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item293.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item293.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item293.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item293.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item293.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item293.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item293.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item293.cast$`n_2017 RBSA PS`
)
exportTable(item293.final, "MF", "Table 86", weighted = FALSE,OS = T, osIndicator = "PSE")





#############################################################################################
#Item 294: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 88)
#############################################################################################
#subset to columns needed for analysis
fridge.type.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Type"
                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                   ,"Refrigerator/Freezer.Size"))]


fridge.type.dat0 <- fridge.type.dat[which(!is.na(fridge.type.dat$APPLIANCE_FRIDGE_FREEZER_Type)),]
fridge.type.dat1 <- left_join(rbsa.dat, fridge.type.dat0)

#clean type to match detailed type
fridge.type.dat1$Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, chest")] <- "Freezer"
fridge.type.dat1$Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Freezer")] <- "Freezer"
fridge.type.dat1$Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Freezer, upright")] <- "Freezer"

#clean detailed type to match previous RBSA table
unique(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type)
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Top Freezer")] <- "Refrigerator with Top Freezer"
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "R/F Bottom Freezer")] <- "Refrigerator with Bottom Freezer"
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side w/ Bottom Freezer")] <- "Side-by-Side Refrigerator with Bottom Freezer"
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Side by Side Refrigerator/Freezer")] <- "Refrigerator with Side-by-Side Freezer"
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Full Size Single Refrigerator Only")] <- "Full Size Refrigerator Only"
fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type == "Mini-Fridge")] <- "Mini Refrigerator"

fridge.type.dat2 <- fridge.type.dat1[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type != "N/A"),]

fridge.type.dat3 <- fridge.type.dat2[which(fridge.type.dat2$Type == "Refrigerator"),]

item294.dat <- fridge.type.dat3[grep("SITE",fridge.type.dat3$CK_Building_ID),]
item294.dat$count <- 1

item294.merge <- left_join(rbsa.dat,item294.dat)
item294.merge <- item294.merge[grep("SITE",item294.merge$CK_Building_ID),]
item294.merge$APPLIANCE_FRIDGE_FREEZER_Type[which(is.na(item294.merge$count))] <- "Unknown"

item294.merge <- item294.merge[which(item294.merge$BuildingType == "Multifamily"),]
item294.merge <- item294.merge[which(item294.merge$APPLIANCE_FRIDGE_FREEZER_Type != "Unknown"),]
######################################
#Pop and Sample Sizes for weights
######################################
item294.data <- weightedData(item294.merge[which(colnames(item294.merge) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"
                                                                              ,"count"
                                                                              ,"Category"))])

item294.data <- left_join(item294.data, item294.merge[which(colnames(item294.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"
                                                                                     ,"count"
                                                                                     ,"Category"))])
item294.data$count <- 1


######################
# weighted analysis
######################
item294.summary <- proportionRowsAndColumns1(CustomerLevelData = item294.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item294.summary <- item294.summary[which(item294.summary$Category != "Remove"),]

item294.cast <- dcast(setDT(item294.summary)
                      ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ Category
                      ,value.var = c("w.percent","w.SE", "n","EB"))

item294.final <- data.frame("BuildingType" = item294.cast$BuildingType
                            ,"Type"                    = item294.cast$APPLIANCE_FRIDGE_FREEZER_Type
                            ,"PSE.Percent"                 = item294.cast$w.percent_PSE
                            ,"PSE.SE"                      = item294.cast$w.SE_PSE
                            ,"PSE.n"                       = item294.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item294.cast$`w.percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item294.cast$`w.SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item294.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item294.cast$`w.percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item294.cast$`w.SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item294.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item294.cast$`w.percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item294.cast$`w.SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item294.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                      = item294.cast$EB_PSE
                            ,"PSE.King.County_EB"          = item294.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"      = item294.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"             = item294.cast$`EB_2017 RBSA PS`
)

item294.final.MF <- item294.final[which(item294.final$BuildingType == "Multifamily")
                                ,which(colnames(item294.final) %notin% c("BuildingType"))]
exportTable(item294.final.MF, "MF", "Table 88", weighted = TRUE,OS = T, osIndicator = "PSE")

######################
# unweighted analysis
######################
item294.summary <- proportions_two_groups_unweighted(CustomerLevelData = item294.data
                                             ,valueVariable = "count"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "APPLIANCE_FRIDGE_FREEZER_Type"
                                             ,aggregateColumnName = "Remove")
item294.summary <- item294.summary[which(item294.summary$Category != "Remove"),]

item294.cast <- dcast(setDT(item294.summary)
                      ,formula = BuildingType + APPLIANCE_FRIDGE_FREEZER_Type ~ Category
                      ,value.var = c("Percent","SE", "n"))

item294.final <- data.frame("BuildingType" = item294.cast$BuildingType
                            ,"Type"                    = item294.cast$APPLIANCE_FRIDGE_FREEZER_Type
                            ,"PSE.Percent"                 = item294.cast$Percent_PSE
                            ,"PSE.SE"                      = item294.cast$SE_PSE
                            ,"PSE.n"                       = item294.cast$n_PSE
                            ,"PSE.King.County.Percent"     = item294.cast$`Percent_PSE KING COUNTY`
                            ,"PSE.King.County.SE"          = item294.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"           = item294.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Percent" = item294.cast$`Percent_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"      = item294.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"       = item294.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Percent"        = item294.cast$`Percent_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"             = item294.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"              = item294.cast$`n_2017 RBSA PS`
)
item294.final.MF <- item294.final[which(item294.final$BuildingType == "Multifamily")
                                  ,which(colnames(item294.final) %notin% c("BuildingType"))]
exportTable(item294.final.MF, "MF", "Table 88", weighted = FALSE,OS = T, osIndicator = "PSE")








#############################################################################################
#Item 295: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 90)
#############################################################################################
#subset to columns needed for analysis
item295.dat <- item294.merge#[-grep("unknown|No Refrigerator",item294.merge$APPLIANCE_FRIDGE_FREEZER_Type, ignore.case = T),]
item295.dat <- item295.dat[-grep("unknown",item295.dat$`Refrigerator/Freezer.Size`, ignore.case = T),]

######################################
#Pop and Sample Sizes for weights
######################################
item295.data <- weightedData(item295.dat[which(colnames(item295.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                            ,"Type"
                                                                            ,"Refrigerator/Freezer.Size"
                                                                            ,"count"
                                                                            ,"Category"))])

item295.data <- left_join(item295.data, item295.dat[which(colnames(item295.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                   ,"Type"
                                                                                   ,"Refrigerator/Freezer.Size"
                                                                                   ,"count"
                                                                                   ,"Category"))])
item295.data$count <- 1

item295.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item295.data$`Refrigerator/Freezer.Size`)
unique(item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item295.data$`Refrigerator/Freezer.Size`))

#########################
# weighted analysis
#########################
item295.cast <- mean_two_groups(CustomerLevelData = item295.data
                                ,valueVariable = "Refrigerator/Freezer.Size"
                                ,byVariableRow = "APPLIANCE_FRIDGE_FREEZER_Type"
                                ,byVariableColumn = "Category"
                                ,columnAggregate = "Remove"
                                ,rowAggregate = "Remove")
item295.cast <- item295.cast[which(item295.cast$APPLIANCE_FRIDGE_FREEZER_Type != "Remove")]


item295.final <- data.frame("Lamp.Category"                 = item295.cast$Lamp.Category
                            ,"PSE.Mean"                 = item295.cast$Mean_PSE
                            ,"PSE.SE"                   = item295.cast$SE_PSE
                            ,"PSE.n"                    = item295.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item295.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item295.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item295.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item295.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item295.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item295.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item295.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item295.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item295.cast$`n_2017 RBSA PS`
                            ,"PSE_EB"                   = item295.cast$EB_PSE
                            ,"PSE.King.County_EB"       = item295.cast$`EB_PSE KING COUNTY`
                            ,"PSE.Non.King.County_EB"   = item295.cast$`EB_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS_EB"          = item295.cast$`EB_2017 RBSA PS`
)

exportTable(item295.final, "MF", "Table 89", weighted = TRUE,OS = T, osIndicator = "PSE")

#########################
# weighted analysis
#########################
item295.cast <- mean_two_groups_unweighted(CustomerLevelData = item295.data
                                           ,valueVariable = "Refrigerator/Freezer.Size"
                                           ,byVariableRow = "APPLIANCE_FRIDGE_FREEZER_Type"
                                           ,byVariableColumn = "Category"
                                           ,columnAggregate = "Remove"
                                           ,rowAggregate = "Remove")
item295.cast <- item295.cast[which(item295.cast$APPLIANCE_FRIDGE_FREEZER_Type != "Remove")]


item295.final <- data.frame("Lamp.Category"                 = item295.cast$Lamp.Category
                            ,"PSE.Mean"                 = item295.cast$Mean_PSE
                            ,"PSE.SE"                   = item295.cast$SE_PSE
                            ,"PSE.n"                    = item295.cast$n_PSE
                            ,"PSE.King.County.Mean"     = item295.cast$`Mean_PSE KING COUNTY`
                            ,"PSE.King.County.SE"       = item295.cast$`SE_PSE KING COUNTY`
                            ,"PSE.King.County.n"        = item295.cast$`n_PSE KING COUNTY`
                            ,"PSE.Non.King.County.Mean" = item295.cast$`Mean_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.SE"   = item295.cast$`SE_PSE NON-KING COUNTY`
                            ,"PSE.Non.King.County.n"    = item295.cast$`n_PSE NON-KING COUNTY`
                            ,"2017.RBSA.PS.Mean"        = item295.cast$`Mean_2017 RBSA PS`
                            ,"2017.RBSA.PS.SE"          = item295.cast$`SE_2017 RBSA PS`
                            ,"2017.RBSA.PS.n"           = item295.cast$`n_2017 RBSA PS`
)
exportTable(item295.final, "MF", "Table 89", weighted = FALSE,OS = T, osIndicator = "PSE")

