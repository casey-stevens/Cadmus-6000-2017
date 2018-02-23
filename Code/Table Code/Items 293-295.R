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
item293.site <- summarise(group_by(item293.mech2, CK_Cadmus_ID)
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

item293.sum <- summarise(group_by(item293.dat3, CK_Cadmus_ID, Type)
                        ,SiteCount = sum(TotalQty,na.rm = T))
unique(item293.sum$Type)
item293.cast <- dcast(setDT(item293.sum)
                      ,formula = CK_Cadmus_ID ~ Type
                      ,value.var = "SiteCount")
item293.cast <- data.frame(item293.cast, stringsAsFactors = F)
item293.cast[is.na(item293.cast)] <- 0

item293.join <- left_join(item293.cast, item293.site)

item293.melt <- melt(item293.join, id.var = "CK_Cadmus_ID")
colnames(item293.melt) <- c("CK_Cadmus_ID", "Type", "Count")


item293.merge <- left_join(rbsa.dat, item293.melt)
item293.merge <- item293.merge[grep("site", item293.merge$CK_Building_ID, ignore.case = T),]
item293.merge <- item293.merge[which(!is.na(item293.merge$Count)),]
length(unique(item293.merge$CK_Cadmus_ID))
######################################
#Pop and Sample Sizes for weights
######################################
item293.data <- weightedData(item293.merge[which(colnames(item293.merge) %notin% c("Type"
                                                                                   ,"Count"))])

item293.data <- left_join(item293.data, item293.merge[which(colnames(item293.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Count"))])
item293.data$count <- 1


#########################
# weighted analysis
#########################
item293.final <- mean_one_group(CustomerLevelData = item293.data
                                ,valueVariable = 'Count'
                                ,byVariable = 'Type'
                                ,aggregateRow = "Remove")
item293.final <- item293.final[which(item293.final$Type != "Remove"),]

item293.final <- item293.final[which(item293.final$Type %in% c("Washer"
                                                               ,"Dishwasher"
                                                               ,"Dryer"
                                                               ,"Freezer"
                                                               ,"Refrigerator"
                                                               ,"Water.Heater")),]
item293.final.MF <- item293.final[which(colnames(item293.final) %notin% c("BuildingType"))]
exportTable(item293.final.MF, "MF","Table 86",weighted =TRUE)


#########################
# unweighted analysis
#########################
item293.final <- mean_one_group_unweighted(CustomerLevelData = item293.data
                                ,valueVariable = 'Count'
                                ,byVariable = 'Type'
                                ,aggregateRow = "Remove")
item293.final <- item293.final[which(item293.final$Type != "Remove"),]

item293.final <- item293.final[which(item293.final$Type %in% c("Washer"
                                                               ,"Dishwasher"
                                                               ,"Dryer"
                                                               ,"Freezer"
                                                               ,"Refrigerator"
                                                               ,"Water.Heater")),]
item293.final.MF <- item293.final[which(colnames(item293.final) %notin% c("BuildingType"))]
exportTable(item293.final.MF, "MF","Table 86",weighted = FALSE)





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

# fridge.type.dat2 <- fridge.type.dat1[which(fridge.type.dat1$APPLIANCE_FRIDGE_FREEZER_Type != "Unknown"),]

fridge.type.dat3 <- fridge.type.dat1[which(fridge.type.dat1$Type == "Refrigerator"),]

item294.dat <- fridge.type.dat3[grep("SITE",fridge.type.dat3$CK_Building_ID),]
item294.dat$count <- 1

item294.merge <- left_join(rbsa.dat,item294.dat)
item294.merge <- item294.merge[grep("SITE",item294.merge$CK_Building_ID),]
item294.merge$APPLIANCE_FRIDGE_FREEZER_Type[which(is.na(item294.merge$count))] <- "No Refrigerator"

item294.merge <- item294.merge[which(item294.merge$BuildingType == "Multifamily"),]

######################################
#Pop and Sample Sizes for weights
######################################
item294.data <- weightedData(item294.merge[which(colnames(item294.merge) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                              ,"Type"
                                                                              ,"Refrigerator/Freezer.Size"
                                                                              ,"count"))])

item294.data <- left_join(item294.data, item294.merge[which(colnames(item294.merge) %in% c("CK_Cadmus_ID"
                                                                                     ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                     ,"Type"
                                                                                     ,"Refrigerator/Freezer.Size"
                                                                                     ,"count"))])
item294.data$count <- 1


######################
# weighted analysis
######################
item294.final <- proportions_one_group(CustomerLevelData = item294.data
                                      ,valueVariable = 'count'
                                      ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                      ,total.name = 'Total')
item294.final.MF <- item294.final[which(item294.final$BuildingType == "Multifamily")
                                ,which(colnames(item294.final) %notin% c("BuildingType"))]
exportTable(item294.final.MF, "MF", "Table 88", weighted = TRUE)

######################
# unweighted analysis
######################
item294.final <- proportions_one_group(CustomerLevelData = item294.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                       ,total.name = 'Total'
                                       ,weighted = FALSE)
item294.final.MF <- item294.final[which(item294.final$BuildingType == "Multifamily")
                                  ,which(colnames(item294.final) %notin% c("BuildingType"))]
exportTable(item294.final.MF, "MF", "Table 88", weighted = FALSE)








#############################################################################################
#Item 295: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 90)
#############################################################################################
#subset to columns needed for analysis
item295.dat <- item294.merge[-grep("unknown|No Refrigerator",item294.merge$APPLIANCE_FRIDGE_FREEZER_Type, ignore.case = T),]
item295.dat <- item295.dat[-grep("unknown",item295.dat$`Refrigerator/Freezer.Size`, ignore.case = T),]

######################################
#Pop and Sample Sizes for weights
######################################
item295.data <- weightedData(item295.dat[which(colnames(item295.dat) %notin% c("APPLIANCE_FRIDGE_FREEZER_Type"
                                                                            ,"Type"
                                                                            ,"Refrigerator/Freezer.Size"
                                                                            ,"count"))])

item295.data <- left_join(item295.data, item295.dat[which(colnames(item295.dat) %in% c("CK_Cadmus_ID"
                                                                                   ,"APPLIANCE_FRIDGE_FREEZER_Type"
                                                                                   ,"Type"
                                                                                   ,"Refrigerator/Freezer.Size"
                                                                                   ,"count"))])
item295.data$count <- 1

item295.data$`Refrigerator/Freezer.Size` <- gsub(" cu ft", "", item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- gsub("cu ft", "", item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- gsub(" ", "", item295.data$`Refrigerator/Freezer.Size`)
unique(item295.data$`Refrigerator/Freezer.Size`)
item295.data$`Refrigerator/Freezer.Size` <- as.numeric(as.character(item295.data$`Refrigerator/Freezer.Size`))

######################
# weighted analysis
######################
item295.final <- mean_one_group(CustomerLevelData = item295.data
                               ,valueVariable = 'Refrigerator/Freezer.Size'
                               ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                               ,aggregateRow = "All Refrigerator Types")

item295.final.MF <- item295.final[which(item295.final$BuildingType == "Multifamily")
                                ,which(colnames(item295.final) %notin% c("BuildingType"))]
exportTable(item295.final.MF, "MF", "Table 89", weighted = TRUE)

######################
# unweighted analysis
######################
item295.final <- mean_one_group_unweighted(CustomerLevelData = item295.data
                                          ,valueVariable = 'Refrigerator/Freezer.Size'
                                          ,byVariable = 'APPLIANCE_FRIDGE_FREEZER_Type'
                                          ,aggregateRow = "All Refrigerator Types")

item295.final.MF <- item295.final[which(item295.final$BuildingType == "Multifamily")
                                ,which(colnames(item295.final) %notin% c("BuildingType"))]
exportTable(item295.final.MF, "MF", "Table 89", weighted = FALSE)
