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
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis -- Item 292
rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))


#############################################################################################
#Item 290: LIGHTING CHARACTERISTICS (MF Table 82)
#############################################################################################
item290.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Clean.Wattage"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]

item290.dat$Total.Lamps <- as.numeric(as.character(item290.dat$Fixture.Qty)) * as.numeric(as.character(item290.dat$LIGHTING_BulbsPerFixture))
unique(item290.dat$Total.Lamps)

item290.dat1 <- item290.dat[which(!(item290.dat$Total.Lamps %in% c(NA))),]

item290.dat2 <- left_join(item290.dat1, rbsa.dat, by = "CK_Cadmus_ID")

item290.dat3 <- item290.dat2[which(item290.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item290.dat4 <- item290.dat3[grep("SITE", item290.dat3$CK_SiteID),]

item290.dat5 <- item290.dat4[grep("Multifamily", item290.dat4$BuildingType),]

item290.dat5$Lamp.Category[which(item290.dat5$Lamp.Category %in% c("Unknown", "Incandescent / Halogen"))] <- "Other"
item290.dat5 <- item290.dat5[which(item290.dat5$Clean.Room != "Storage")]# <- item290.dat5$Clean.Room[which(item290.dat5$Clean.Room == "Storage")]
  
item290.site <- summarise(group_by(item290.dat5, CK_Cadmus_ID)
                          ,Total.Unit.Fixtures = sum(Fixture.Qty)
                          ,Total.Unit.Lamps = sum(Total.Lamps))

item290.cast <- dcast(setDT(item290.dat5)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category, sum
                      ,value.var = "Total.Lamps")
item290.join <- left_join(item290.site, item290.cast)

item290.melt <- melt(item290.join, id.vars = "CK_Cadmus_ID")
names(item290.melt) <- c("CK_Cadmus_ID", "Lamp.Category", "Lamp.Count")


item290.merge <- left_join(rbsa.dat, item290.melt)
item290.merge <- item290.merge[which(!is.na(item290.merge$Lamp.Count)),]

######################################
#Pop and Sample Sizes for weights
######################################
item290.data <- weightedData(item290.merge[which(colnames(item290.merge) %notin% c("Lamp.Category"
                                                                                   ,"Lamp.Count"))])

item290.data <- left_join(item290.data, item290.merge[which(colnames(item290.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamp.Category"
                                                                                           ,"Lamp.Count"))])
item290.data$count <- 1


#########################
# weighted analysis
#########################
item290.final <- mean_one_group(CustomerLevelData = item290.data
                                ,valueVariable = 'Lamp.Count'
                                ,byVariable = 'Lamp.Category'
                                ,aggregateRow = "Remove")
item290.final <- item290.final[which(item290.final$Lamp.Category != "Remove"),]

exportTable(item290.final, "MF", "Table 82", weighted = TRUE)


#########################
# weighted analysis
#########################
item290.final <- mean_one_group_unweighted(CustomerLevelData = item290.data
                                ,valueVariable = 'Lamp.Count'
                                ,byVariable = 'Lamp.Category'
                                ,aggregateRow = "Remove")
item290.final <- item290.final[which(item290.final$Lamp.Category != "Remove"),]

exportTable(item290.final, "MF", "Table 82", weighted = FALSE)







#############################################################################################
#Item 291: DISTRIBUTION OF LAMPS BY TYPE (MF Table 83)
#############################################################################################
item291.dat <- item290.dat5
item291.dat1 <- item291.dat[grep("Multifamily", item291.dat$BuildingType),]

item291.dat1$Lamp.Category[which(item291.dat1$Lamp.Category %in% c("Incandescent / Halogen"))] <- "Other"


item291.dat$count <- 1

item291.sum1 <- summarise(group_by(item291.dat, Lamp.Category)
                         ,Count = sum(count)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item291.sum2 <- summarise(group_by(item291.dat)
                         ,Total.Count = sum(count)
                         ,Denom.SampleSize = length(unique(CK_Cadmus_ID)))

item291.final <- cbind.data.frame(item291.sum1, item291.sum2)
item291.final$Percent <- item291.final$Count / item291.final$Total.Count
item291.final$SE <- sqrt(item291.final$Percent * (1 - item291.final$Percent) / item291.final$Denom.SampleSize)

item291.table <- data.frame("Lamp.Category" = item291.final$Lamp.Category
                            ,"Percent" = item291.final$Percent
                            ,"SE" = item291.final$SE
                            ,"SampleSize" = item291.final$SampleSize)











#############################################################################################
#Item 292: AVERAGE LIGHTING POWER DENSITY (LPD) BY ROOM TYPE AND OVERALL (MF Table 84)
#############################################################################################
############
# For Rooms
############
item292.rooms <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID","Clean.Type","Area"))]
colnames(item292.rooms) <- c("CK_Cadmus_ID","Clean.Room","Area")

item292.area <- item292.rooms[which(!(item292.rooms$Area %in% c("0", "Unknown", NA, "-- Datapoint not asked for --"))),]
unique(item292.area$Area)
item292.area$Area <- as.numeric(as.character(item292.area$Area))

item292.area1 <- summarise(group_by(item292.area, CK_Cadmus_ID, Clean.Room)
                          ,SiteArea = sum(Area))


############
# For Lighting
############
item292.dat1 <- item290.dat5
item292.dat1$Total.Wattage <- item292.dat1$Total.Lamps * as.numeric(as.character(item292.dat1$Clean.Wattage))

item292.dat2 <- item292.dat1[which(!(is.na(item292.dat1$Total.Wattage))),]

item292.dat3 <- summarise(group_by(item292.dat2, CK_Cadmus_ID, Clean.Room)
                         ,Total.Wattage = sum(Total.Wattage))


item292.dat4 <- left_join(item292.dat3, item292.area1, by = c("CK_Cadmus_ID", "Clean.Room"))

item292.dat4$LPD <- item292.dat4$Total.Wattage / item292.dat4$SiteArea

item292.merge <- left_join(rbsa.dat, item292.dat4)
item292.merge <- item292.merge[which(!(is.na(item292.merge$LPD))),]


######################################
#Pop and Sample Sizes for weights
######################################
item292.data <- weightedData(item292.merge[which(colnames(item292.merge) %notin% c("Clean.Room"
                                                                                   ,"Total.Wattage"
                                                                                   ,"SiteArea"
                                                                                   ,"LPD"))])

item292.data <- left_join(item292.data, item292.merge[which(colnames(item292.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Clean.Room"
                                                                                           ,"Total.Wattage"
                                                                                           ,"SiteArea"
                                                                                           ,"LPD"))])
item292.data$count <- 1


######################
# weighted analysis
######################
item292.final <- mean_one_group(CustomerLevelData = item292.data
                                ,valueVariable = "LPD"
                                ,byVariable = 'Clean.Room'
                                ,aggregateRow = "Unit LPD")
item292.final <- item292.final[which(colnames(item292.final) %notin% c("BuildingType"))]
  
exportTable(item292.final, "MF", "Table 85", weighted = TRUE)

######################
# weighted analysis
######################
item292.final <- mean_one_group_unweighted(CustomerLevelData = item292.data
                                ,valueVariable = "LPD"
                                ,byVariable = 'Clean.Room'
                                ,aggregateRow = "Unit LPD")
item292.final <- item292.final[which(colnames(item292.final) %notin% c("BuildingType"))]

exportTable(item292.final, "MF", "Table 85", weighted = FALSE)





#############################################################################################
#Table 82A: STORAGE LIGHTING CHARACTERISTICS (MF Table 82)
#############################################################################################
table82A.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Clean.Wattage"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]

table82A.dat$Total.Lamps <- as.numeric(as.character(table82A.dat$Fixture.Qty)) * as.numeric(as.character(table82A.dat$LIGHTING_BulbsPerFixture))
unique(table82A.dat$Total.Lamps)

table82A.dat1 <- table82A.dat[which(!(table82A.dat$Total.Lamps %in% c(NA))),]

table82A.dat2 <- left_join(table82A.dat1, rbsa.dat, by = "CK_Cadmus_ID")

table82A.dat3 <- table82A.dat2[which(table82A.dat2$CK_Cadmus_ID != "CK_CADMUS_ID"),]

table82A.dat4 <- table82A.dat3[grep("SITE", table82A.dat3$CK_SiteID),]

table82A.dat5 <- table82A.dat4[grep("Multifamily", table82A.dat4$BuildingType),]

table82A.dat5$Lamp.Category[which(table82A.dat5$Lamp.Category %in% c("Unknown", "Incandescent / Halogen"))] <- "Other"
table82A.dat5 <- table82A.dat5[which(table82A.dat5$Clean.Room == "Storage"),]# <- table82A.dat5$Clean.Room[which(table82A.dat5$Clean.Room == "Storage")]

table82A.site <- summarise(group_by(table82A.dat5, CK_Cadmus_ID)
                          ,Total.Unit.Fixtures = sum(Fixture.Qty)
                          ,Total.Unit.Lamps = sum(Total.Lamps))

table82A.cast <- dcast(setDT(table82A.dat5)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category, sum
                      ,value.var = "Total.Lamps")
table82A.join <- left_join(table82A.site, table82A.cast)

table82A.melt <- melt(table82A.join, id.vars = "CK_Cadmus_ID")
names(table82A.melt) <- c("CK_Cadmus_ID", "Lamp.Category", "Lamp.Count")


table82A.merge <- left_join(rbsa.dat, table82A.melt)
table82A.merge <- table82A.merge[which(!is.na(table82A.merge$Lamp.Count)),]

######################################
#Pop and Sample Sizes for weights
######################################
table82A.data <- weightedData(table82A.merge[which(colnames(table82A.merge) %notin% c("Lamp.Category"
                                                                                   ,"Lamp.Count"))])

table82A.data <- left_join(table82A.data, table82A.merge[which(colnames(table82A.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Lamp.Category"
                                                                                           ,"Lamp.Count"))])
table82A.data$count <- 1


#########################
# weighted analysis
#########################
table82A.final <- mean_one_group(CustomerLevelData = table82A.data
                                ,valueVariable = 'Lamp.Count'
                                ,byVariable = 'Lamp.Category'
                                ,aggregateRow = "Remove")
table82A.final <- table82A.final[which(table82A.final$Lamp.Category != "Remove"),]

exportTable(table82A.final, "MF", "Table 82A", weighted = TRUE)


#########################
# weighted analysis
#########################
table82A.final <- mean_one_group_unweighted(CustomerLevelData = table82A.data
                                           ,valueVariable = 'Lamp.Count'
                                           ,byVariable = 'Lamp.Category'
                                           ,aggregateRow = "Remove")
table82A.final <- table82A.final[which(table82A.final$Lamp.Category != "Remove"),]

exportTable(table82A.final, "MF", "Table 82A", weighted = FALSE)


