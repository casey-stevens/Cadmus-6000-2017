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
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID,ignore.case = T),]
rbsa.dat.MF   <- rbsa.dat.bldg[grep("multifamily",rbsa.dat.bldg$BuildingType, ignore.case = T),]

#Read in data for analysis
one.line.bldg.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), startRow = 2)
#clean cadmus IDs
one.line.bldg.dat$CK_Building_ID <- trimws(toupper(one.line.bldg.dat$CK_BuildingID))

#Read in data for analysis
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
#clean cadmus IDs
buildings.dat$CK_Building_ID <- trimws(toupper(buildings.dat$PK_BuildingID))



#############################################################################################
#Item 214: MF table 6
#############################################################################################
item214.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"))]
item214.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item214.dat$Qty.Buildings.in.Complex))

item214.dat1 <- item214.dat[which(!is.na(item214.dat$Qty.Buildings.in.Complex)),]

item214.dat1$Ind <- 0
item214.dat1$Ind[which(item214.dat1$Qty.Buildings.in.Complex > 1)] <- 1

item214.merge <- left_join(rbsa.dat.MF, item214.dat1)
item214.merge <- item214.merge[which(!is.na(item214.merge$Ind)),]


######################################
#Pop and Sample Sizes for weights
######################################
item214.data <- weightedData(item214.merge[which(colnames(item214.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Ind"))])

item214.data <- left_join(item214.data, item214.merge[which(colnames(item214.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Ind"))])
item214.data$count <- 1
item214.data$Count <- 1

#########################
# weighted analysis
#########################
item214.final <- proportions_one_group(CustomerLevelData = item214.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes")
item214.final.MF <- item214.final[which(names(item214.final) != "BuildingType")]

exportTable(item214.final.MF, "MF","Table 6",weighted = TRUE)

#########################
# unweighted analysis
#########################
item214.final <- proportions_one_group(CustomerLevelData = item214.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item214.final.MF <- item214.final[which(names(item214.final) != "BuildingType")]

exportTable(item214.final.MF, "MF","Table 6",weighted = FALSE)



#############################################################################################
#Item 215: MF table 7
#############################################################################################
item215.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Qty.Buildings.in.Complex"
                                                                          ,"Total.Units.in.Building"))]
item215.dat$Qty.Buildings.in.Complex <- as.numeric(as.character(item215.dat$Qty.Buildings.in.Complex))
item215.dat$Total.Units.in.Building <- as.numeric(as.character(item215.dat$Total.Units.in.Building))

item215.dat1 <- item215.dat[which(!is.na(item215.dat$Qty.Buildings.in.Complex)),]
item215.dat1 <- item215.dat1[which(!is.na(item215.dat1$Total.Units.in.Building)),]

item215.dat1$Ind <- 0
item215.dat1$Ind[which(item215.dat1$Qty.Buildings.in.Complex > 1)] <- item215.dat1$Total.Units.in.Building[which(item215.dat1$Qty.Buildings.in.Complex > 1)]

item215.merge <- left_join(rbsa.dat.MF, item215.dat1)
item215.merge <- item215.merge[which(!is.na(item215.merge$Ind)),]


######################################
#Pop and Sample Sizes for weights
######################################
item215.data <- weightedData(item215.merge[which(colnames(item215.merge) %notin% c("Qty.Buildings.in.Complex"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Ind"))])

item215.data <- left_join(item215.data, item215.merge[which(colnames(item215.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Qty.Buildings.in.Complex"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Ind"))])
item215.data$count <- 1
item215.data$Count <- item215.data$Total.Units.in.Building

#########################
# weighted analysis
#########################
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes")
item215.final.MF <- item215.final[which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = TRUE)

#########################
# unweighted analysis
#########################
item215.final <- proportions_one_group(CustomerLevelData = item215.data
                                       ,valueVariable = 'Ind'
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
item215.final.MF <- item215.final[which(names(item215.final) != "BuildingType")]

exportTable(item215.final.MF, "MF","Table 7",weighted = FALSE)




#############################################################################################
#Item 222: MF table 14
#############################################################################################
item222.one.line.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                                   ,"Total.Units.in.Building"))]
item222.building.dat <- buildings.dat[which(colnames(buildings.dat) %in% c("CK_Building_ID"
                                                                           ,"SITES_MFB_cfg_MFB_PARKING_ParkingStalls_Number"
                                                                           # ,"SITES_MFB_cfg_MFB_PARKING_CoveredLot"
                                                                           # ,"SITES_MFB_cfg_MFB_PARKING_EnclosedGarage"
                                                                           # ,"SITES_MFB_cfg_MFB_PARKING_OpenGarage"
                                                                           # ,"SITES_MFB_cfg_MFB_PARKING_ParkingLot"
                                                                           ))]
names(item222.building.dat) <- c(#"Covered.Parking.Lot"
#                                  ,"Enclosed.Parking.Garage"
#                                  ,"Open.Parking.Garage"
#                                  ,"Open.Parkng.Lot"
                                 "Number.of.Stalls"
                                 ,"CK_Building_ID")

item222.dat <- left_join(item222.one.line.dat, item222.building.dat)
item222.dat$Number.of.Stalls <- as.numeric(as.character(item222.dat$Number.of.Stalls))
item222.dat$Total.Units.in.Building <- as.numeric(as.character(item222.dat$Total.Units.in.Building))

item222.dat1 <- item222.dat[which(!is.na(item222.dat$Number.of.Stalls)),]
item222.dat1 <- item222.dat1[which(!is.na(item222.dat1$Total.Units.in.Building)),]
item222.dat1 <- item222.dat1[which(item222.dat1$Total.Units.in.Building > 0),]

item222.dat1$Stalls.per.unit <- item222.dat1$Number.of.Stalls / item222.dat1$Total.Units.in.Building

# item222.sub <- item222.dat1[which(names(item222.dat1) %in% c("CK_Building_ID"
#                                                              ,"Covered.Parking.Lot"
#                                                              ,"Enclosed.Parking.Garage"
#                                                              ,"Open.Parking.Garage"
#                                                              ,"Open.Parkng.Lot"))]
# item222.melt <- melt(item222.sub, id.vars = "CK_Building_ID")
# item222.melt <- item222.melt[which(item222.melt$value == "Yes"),]
# names(item222.melt) <- c("CK_Building_ID","Parking.Type","Parking.Ind")
# item222.melt <- item222.melt[which(names(item222.melt) %in% c("CK_Building_ID","Parking.Type"))]
# 
# item222.dat2 <- item222.dat1[which(names(item222.dat1) %in% c("CK_Building_ID"
#                                                               ,"Number.of.Stalls"
#                                                               ,"Total.Units.in.Building"
#                                                               ,"Stalls.per.unit"))]
# 
# item222.dat2 <- left_join(item222.melt, item222.dat2)

item222.merge <- left_join(rbsa.dat.MF, item222.dat1)
item222.merge <- item222.merge[which(!is.na(item222.merge$Stalls.per.unit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item222.data <- weightedData(item222.merge[which(colnames(item222.merge) %notin% c("Number.of.Stalls"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"Stalls.per.unit"))])

item222.data <- left_join(item222.data, item222.merge[which(colnames(item222.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Number.of.Stalls"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"Stalls.per.unit"))])
item222.data$count <- 1

#########################
# weighted analysis
#########################
item222.final <- mean_one_group(CustomerLevelData = item222.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222.final.MF <- item222.final[which(names(item222.final) != "BuildingType")]

exportTable(item222.final.MF, "MF", "Table 14", weighted = TRUE)

#########################
# unweighted analysis
#########################
item222.final <- mean_one_group_unweighted(CustomerLevelData = item222.data
                                ,valueVariable = "Stalls.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item222.final.MF <- item222.final[which(names(item222.final) != "BuildingType")]

exportTable(item222.final.MF, "MF", "Table 14", weighted = FALSE)




# #############################################################################################
# #Item 231: MF Table 23
# #############################################################################################
# item231.dat <- one.line.bldg.dat[c(grep("%",names(one.line.bldg.dat)),
#                                    which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID","Total.Window.Area")))]
# for (i in 1:13){
#   item231.dat[,i] <- as.numeric(as.character(item231.dat[,i]))
# }
# item231.dat[is.na(item231.dat)] <- 0
# 
# i = 2
# for (i in 1:12){
#   item231.dat[,i] <- item231.dat[,i] * item231.dat[,13]
# }
# 
# #######################################################
# # Updated up to here
# #######################################################
# 
# item231.merge <- left_join(rbsa.dat.MF, item231.dat1)
# item231.merge <- item231.merge[which(!is.na(item231.merge$Ind)),]
# 
# 
# ######################################
# #Pop and Sample Sizes for weights
# ######################################
# item231.data <- weightedData(item231.merge[which(colnames(item231.merge) %notin% c("Qty.Buildings.in.Complex"
#                                                                                    ,"Total.Units.in.Building"
#                                                                                    ,"Ind"))])
# 
# item231.data <- left_join(item231.data, item231.merge[which(colnames(item231.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Qty.Buildings.in.Complex"
#                                                                                            ,"Total.Units.in.Building"
#                                                                                            ,"Ind"))])
# item231.data$count <- 1
# item231.data$Count <- item231.data$Total.Units.in.Building
# 
# #########################
# # weighted analysis
# #########################




#############################################################################################
#Item 240: MF table 32
#############################################################################################
item240.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"
                                                                          ,"Total.Units.in.Building"))]

item240.dat$Whole.House.UA <- as.numeric(as.character(item240.dat$Whole.House.UA))
item240.dat$Total.Units.in.Building <- as.numeric(as.character(item240.dat$Total.Units.in.Building))

item240.dat1 <- item240.dat[which(!is.na(item240.dat$Whole.House.UA)),]
item240.dat1 <- item240.dat1[which(!is.na(item240.dat1$Total.Units.in.Building)),]
item240.dat1 <- item240.dat1[which(item240.dat1$Total.Units.in.Building > 0),]

item240.dat1$UA.per.unit <- item240.dat1$Whole.House.UA / item240.dat1$Total.Units.in.Building

item240.merge <- left_join(rbsa.dat.MF, item240.dat1)
item240.merge <- item240.merge[which(!is.na(item240.merge$UA.per.unit)),]


######################################
#Pop and Sample Sizes for weights
######################################
item240.data <- weightedData(item240.merge[which(colnames(item240.merge) %notin% c("Whole.House.UA"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"UA.per.unit"))])

item240.data <- left_join(item240.data, item240.merge[which(colnames(item240.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"UA.per.unit"))])
item240.data$count <- 1

#########################
# weighted analysis
#########################
item240.final <- mean_one_group(CustomerLevelData = item240.data
                                ,valueVariable = "Whole.House.UA"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]

exportTable(item240.final.MF, "MF", "Table 32", weighted = TRUE)

#########################
# unweighted analysis
#########################
item240.final <- mean_one_group_unweighted(CustomerLevelData = item240.data
                                           ,valueVariable = "Whole.House.UA"
                                           ,byVariable = "HomeType"
                                           ,aggregateRow = "All Sizes")
item240.final.MF <- item240.final[which(names(item240.final) != "BuildingType")]

exportTable(item240.final.MF, "MF", "Table 32", weighted = FALSE)



#############################################################################################
#Item 241: MF table 33
#############################################################################################
item241.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"
                                                                          ,"Total.Units.in.Building"))]

item241.dat$Whole.House.UA <- as.numeric(as.character(item241.dat$Whole.House.UA))
item241.dat$Total.Units.in.Building <- as.numeric(as.character(item241.dat$Total.Units.in.Building))

item241.dat1 <- item241.dat[which(!is.na(item241.dat$Whole.House.UA)),]
item241.dat1 <- item241.dat1[which(!is.na(item241.dat1$Total.Units.in.Building)),]
item241.dat1 <- item241.dat1[which(item241.dat1$Total.Units.in.Building > 0),]

item241.dat1$UA.per.unit <- item241.dat1$Whole.House.UA / item241.dat1$Total.Units.in.Building

item241.merge <- left_join(rbsa.dat.MF, item241.dat1)
item241.merge <- item241.merge[which(!is.na(item241.merge$UA.per.unit)),]
item241.merge <- item241.merge[which(!is.na(item241.merge$HomeYearBuilt)),]

######################################
#Pop and Sample Sizes for weights
######################################
item241.data <- weightedData(item241.merge[which(colnames(item241.merge) %notin% c("Whole.House.UA"
                                                                                   ,"Total.Units.in.Building"
                                                                                   ,"UA.per.unit"))])

item241.data <- left_join(item241.data, item241.merge[which(colnames(item241.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"Total.Units.in.Building"
                                                                                           ,"UA.per.unit"))])
item241.data$count <- 1

#########################
# weighted analysis
#########################
item241.final <- mean_one_group(CustomerLevelData = item241.data
                                ,valueVariable = "UA.per.unit"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = TRUE)

#########################
# unweighted analysis
#########################
item241.final <- mean_one_group_unweighted(CustomerLevelData = item241.data
                                           ,valueVariable = "UA.per.unit"
                                           ,byVariable = "HomeType"
                                           ,aggregateRow = "All Sizes")
item241.final.MF <- item241.final[which(names(item241.final) != "BuildingType")]

exportTable(item241.final.MF, "MF", "Table 33", weighted = FALSE)



#############################################################################################
#Item 242: MF table 34
#############################################################################################
item242.dat <- one.line.bldg.dat[which(colnames(one.line.bldg.dat) %in% c("CK_Building_ID"
                                                                          ,"Whole.House.UA"))]

item242.dat$Whole.House.UA <- as.numeric(as.character(item242.dat$Whole.House.UA))
item242.dat$Total.Units.in.Building <- as.numeric(as.character(item242.dat$Total.Units.in.Building))

item242.dat1 <- item242.dat[which(!is.na(item242.dat$Whole.House.UA)),]

item242.dat1$UA.per.area <- item242.dat1$Whole.House.UA / item242.dat1$Conditioned.Area

item242.merge <- left_join(rbsa.dat.MF, item242.dat1)
item242.merge <- item242.merge[which(!is.na(item242.merge$UA.per.area)),]


######################################
#Pop and Sample Sizes for weights
######################################
item242.data <- weightedData(item242.merge[which(colnames(item242.merge) %notin% c("Whole.House.UA"
                                                                                   ,"UA.per.area"))])

item242.data <- left_join(item242.data, item242.merge[which(colnames(item242.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Whole.House.UA"
                                                                                           ,"UA.per.area"))])
item242.data$count <- 1

#########################
# weighted analysis
#########################
item242.final <- mean_one_group(CustomerLevelData = item242.data
                                ,valueVariable = "UA.per.area"
                                ,byVariable = "HomeType"
                                ,aggregateRow = "All Sizes")
item242.final.MF <- item242.final[which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = TRUE)

#########################
# unweighted analysis
#########################
item242.final <- mean_one_group_unweighted(CustomerLevelData = item242.data
                                           ,valueVariable = "UA.per.area"
                                           ,byVariable = "HomeType"
                                           ,aggregateRow = "All Sizes")
item242.final.MF <- item242.final[which(names(item242.final) != "BuildingType")]

exportTable(item242.final.MF, "MF", "Table 34", weighted = FALSE)
