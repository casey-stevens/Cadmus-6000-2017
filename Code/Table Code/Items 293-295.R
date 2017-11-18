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
item293.mech <- mechanical.dat[grep("Water Heat", mechanical.dat$Generic),]
item293.mech$Water.Heater <- 1
item293.mech1 <- left_join(rbsa.dat, item293.mech, by = "CK_Cadmus_ID")
item293.mech2 <- unique(item293.mech1[grep("Multifamily", item293.mech1$BuildingType),])
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
item293.dat2$Large.Unusual.Load.Quantity[which(item293.dat2$Large.Unusual.Load.Quantity == "-- Datapoint not asked for --")] <- 1
unique(item293.dat2$Large.Unusual.Load.Quantity)

item293.dat2$Large.Unusual.Load.Quantity <- as.numeric(as.character(item293.dat2$Large.Unusual.Load.Quantity))
item293.dat2$count <- 1

item293.dat2$TotalQty <- item293.dat2$Large.Unusual.Load.Quantity * item293.dat2$count

item293.sum <- summarise(group_by(item293.dat2, CK_Cadmus_ID, Type)
                        ,SiteCount = sum(TotalQty))

item293.cast <- dcast(setDT(item293.sum)
                      ,formula = CK_Cadmus_ID ~ Type
                      ,value.var = "SiteCount")
item293.cast <- data.frame(item293.cast, stringsAsFactors = F)
item293.cast[is.na(item293.cast)] <- 0


item293.join <- left_join(item293.cast, item293.site)

item293.melt <- melt(item293.join, id.var = "CK_Cadmus_ID")
colnames(item293.melt) <- c("CK_Cadmus_ID", "Type", "Count")


item293.merge <- left_join(rbsa.dat, item293.melt)
item293.merge <- item293.merge[which(!is.na(item293.merge$Count)),]

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
item293.final.MF <- item293.final[which(colnames(item293.final) %notin% c("BuildingType","n"))]
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
exportTable(item293.final.MF, "MF","Table 86",weighted =FALSE)





#############################################################################################
#Item 294: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 88)
#############################################################################################
item294.dat <- item293.sum

item294.dat1 <- item294.dat[which(item294.dat$Type == "Refrigerator"),]













#############################################################################################
#Item 294: DISTRIBUTION OF IN-UNIT REFRIGERATORS BY TYPE (MF table 90)
#############################################################################################
item295.dat <- item293.sum

item295.dat1 <- item295.dat[which(item295.dat$Type == "Refrigerator"),]
