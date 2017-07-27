#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export))
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))

#Read in data for analysis
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))
#clean cadmus IDs
buildings.interview.dat$CK_Cadmus_ID <- trimws(toupper(buildings.interview.dat$CK_Cadmus_ID))


#############################################################################################
#Item 256: AVERAGE NUMBER OF COMMON AREA LAMPS PER UNIT BY BUILDING SIZE (MF Table 48)
#############################################################################################
item256.buildings.int <- unique(buildings.interview.dat[which(colnames(buildings.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                                ,"INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding"))])

which(duplicated(item256.buildings.int$CK_Cadmus_ID))

#subset to columns needed for analysis
item256.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"CK_SiteID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"))]
item256.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item256.dat1 <- left_join(item256.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove building info
item256.dat2 <- item256.dat1[grep("BLDG", item256.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item256.dat2$Fixture.Qty <- as.numeric(as.character(item256.dat2$Fixture.Qty))
item256.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item256.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item256.dat2$Lamps <- item256.dat2$Fixture.Qty * item256.dat2$LIGHTING_BulbsPerFixture
unique(item256.dat2$Lamps)

#remove missing lamp quantities
item256.dat3 <- item256.dat2[which(!(is.na(item256.dat2$Lamps))),]

#Subset to Multifamily
item256.dat4 <- item256.dat3[grep("Multifamily", item256.dat3$BuildingType),]

#summarise up to the site level
item256.dat5 <- summarise(group_by(item256.dat4, CK_Cadmus_ID, BuildingTypeXX)
                          ,SiteCount = sum(Lamps))


item256.dat6 <- left_join(item256.dat5, item256.buildings.int, by = "CK_Cadmus_ID")

item256.dat6$LampsPerUnit <- item256.dat6$SiteCount / item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding

#subset to remove any missing number of units, or unit size equal to 1 (doesn't make sense)
item256.dat7 <- item256.dat6[which(item256.dat6$INTRVW_MFB_MGR_BasicCustomerandBuildingDataTotalNumberOfUnitsInAuditedBuilding > 1),]

#summarise by building size
item256.sum1 <- summarise(group_by(item256.dat7, BuildingTypeXX)
                          ,Mean = mean(LampsPerUnit)
                          ,SE = sd(LampsPerUnit) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#summarise across building size
item256.sum2 <- summarise(group_by(item256.dat7)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(LampsPerUnit)
                          ,SE = sd(LampsPerUnit) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
#row bind
item256.final <- rbind.data.frame(item256.sum1, item256.sum2, stringsAsFactors = F)



















#############################################################################################
#Item 257: DISTRIBUTION OF COMMON AREA LAMPS BY LAMP TYPE AND BUILDING SIZE (MF Table 49)
#############################################################################################

#subset to columns needed for analysis
item257.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"))]
item257.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item257.dat1 <- left_join(item257.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove building info
item257.dat2 <- item257.dat1[grep("BLDG", item257.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item257.dat2$Fixture.Qty <- as.numeric(as.character(item257.dat2$Fixture.Qty))
item257.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item257.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item257.dat2$Lamps <- item257.dat2$Fixture.Qty * item257.dat2$LIGHTING_BulbsPerFixture
unique(item257.dat2$Lamps)

#remove missing lamp quantities
item257.dat3 <- item257.dat2[which(!(is.na(item257.dat2$Lamps))),]

#Subset to Multifamily
item257.dat4 <- item257.dat3[grep("Multifamily", item257.dat3$BuildingType),]

#summarise up to the site level
item257.dat5 <- summarise(group_by(item257.dat4, CK_Cadmus_ID, BuildingTypeXX, Lamp.Category)
                          ,SiteCount = sum(Lamps))
unique(item257.dat5$Lamp.Category)

item257.dat6 <- item257.dat5[which(item257.dat5$Lamp.Category != "Unknown"),]

#summarise by building size
#by lamp category
item257.sum1 <- summarise(group_by(item257.dat6, BuildingTypeXX, Lamp.Category)
                          ,Count = sum(SiteCount))

#summarise across building size
#by lamp category
item257.sum2 <- summarise(group_by(item257.dat6, Lamp.Category)
                          ,BuildingTypeXX = "All Sizes"
                          ,Count = sum(SiteCount))

item257.merge <- rbind.data.frame(item257.sum1, item257.sum2, stringsAsFactors = F)

#get total counts
item257.total.count <- summarise(group_by(item257.merge, BuildingTypeXX)
                                 ,Total.Count = sum(Count))#get total counts
item257.samplesizes <- summarise(group_by(item257.dat6, BuildingTypeXX)
                                 ,SampleSize = length(unique(CK_Cadmus_ID)))

item257.join <- left_join(item257.merge, item257.total.count, by = "BuildingTypeXX")

item257.final <- left_join(item257.join, item257.samplesizes, by = "BuildingTypeXX")
item257.final$SampleSize[which(is.na(item257.final$SampleSize))] <- sum(unique(item257.final$SampleSize[which(!(is.na(item257.final$SampleSize)))]))


item257.final$Percent <- item257.final$Count / item257.final$Total.Count
item257.final$SE <- sqrt(item257.final$Percent * (1 - item257.final$Percent) / item257.final$SampleSize)

library(data.table)
item257.cast <- dcast(setDT(item257.final)
                      ,formula = BuildingTypeXX ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "SampleSize"))

item257.table <- data.frame("Building.Size" = item257.cast$BuildingTypeXX
                            ,"Compact.Fluorescent" = item257.cast$`Percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE" = item257.cast$`SE_Compact Fluorescent`
                            ,"Halogen" = item257.cast$Percent_Halogen
                            ,"Halogen.SE" = item257.cast$SE_Halogen
                            ,"Incandescent" = item257.cast$Percent_Incandescent
                            ,"Incandescent.SE" = item257.cast$SE_Incandescent
                            ,"Incandescent.Halogen" = item257.cast$`Percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item257.cast$`SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent" = item257.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item257.cast$`SE_Linear Fluorescent`
                            ,"LED" = item257.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE" = item257.cast$`SE_Light Emitting Diode`
                            ,"Other" = item257.cast$Percent_Other
                            ,"Other.SE" = item257.cast$SE_Other
                            ,"Sample.Size" = item257.cast$`SampleSize_Compact Fluorescent`)















#############################################################################################
#Item 258: DISTRIBUTION OF COMMON AREA LAMPS BY LAMP TYPE AND BUILDING SIZE (MF Table 50)
#############################################################################################

#subset to columns needed for analysis
item258.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
item258.dat$count <- 1

#join clean rbsa data onto lighting analysis data
item258.dat1 <- left_join(item258.dat, rbsa.dat, by = "CK_Cadmus_ID")

#remove building info
item258.dat2 <- item258.dat1[grep("BLDG", item258.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item258.dat2$Fixture.Qty <- as.numeric(as.character(item258.dat2$Fixture.Qty))
item258.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item258.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item258.dat2$Lamps <- item258.dat2$Fixture.Qty * item258.dat2$LIGHTING_BulbsPerFixture
unique(item258.dat2$Lamps)

#remove missing lamp quantities
item258.dat3 <- item258.dat2[which(!(is.na(item258.dat2$Lamps))),]

#Subset to Multifamily
item258.dat4 <- item258.dat3[grep("Multifamily", item258.dat3$BuildingType),]

#summarise up to the site level
item258.dat5 <- summarise(group_by(item258.dat4, CK_Cadmus_ID, Clean.Room, Lamp.Category)
                          ,SiteCount = sum(Lamps))
unique(item258.dat5$Lamp.Category)

item258.dat6 <- item258.dat5[which(item258.dat5$Lamp.Category != "Unknown"),]



#summarise by room type
#by lamp category
item258.sum1 <- summarise(group_by(item258.dat6, Clean.Room, Lamp.Category)
                          ,Count = sum(SiteCount))

#summarise across room types
#by lamp category
item258.sum2 <- summarise(group_by(item258.dat6, Lamp.Category)
                          ,Clean.Room = "All Types"
                          ,Count = sum(SiteCount))

item258.merge <- rbind.data.frame(item258.sum1, item258.sum2, stringsAsFactors = F)

#get total counts
item258.total.count <- summarise(group_by(item258.merge, Clean.Room)
                                 ,Total.Count = sum(Count))#get total counts

#get sample sizes
item258.samplesizes1 <- summarise(group_by(item258.dat6, Clean.Room)
                                 ,SampleSize = length(unique(CK_Cadmus_ID)))
item258.samplesizes2 <- summarise(group_by(item258.dat6)
                                  ,Clean.Room = "All Types"
                                  ,SampleSize = length(unique(CK_Cadmus_ID)))
#merge sample sizes
item258.SampleSizes <- rbind.data.frame(item258.samplesizes1, item258.samplesizes2, stringsAsFactors = F)

item258.join <- left_join(item258.merge, item258.total.count, by = "Clean.Room")

item258.final <- left_join(item258.join, item258.samplesizes, by = "Clean.Room")

item258.final$Percent <- item258.final$Count / item258.final$Total.Count
item258.final$SE <- sqrt(item258.final$Percent * (1 - item258.final$Percent) / item258.final$SampleSize)

library(data.table)
item258.cast <- dcast(setDT(item258.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "SampleSize"))

item258.table <- data.frame("Clean.Room" = item258.cast$Clean.Room
                            ,"Compact.Fluorescent" = item258.cast$`Percent_Compact Fluorescent`
                            ,"Compact.Fluorescent.SE" = item258.cast$`SE_Compact Fluorescent`
                            ,"Halogen" = item258.cast$Percent_Halogen
                            ,"Halogen.SE" = item258.cast$SE_Halogen
                            ,"Incandescent" = item258.cast$Percent_Incandescent
                            ,"Incandescent.SE" = item258.cast$SE_Incandescent
                            ,"Incandescent.Halogen" = item258.cast$`Percent_Incandescent / Halogen`
                            ,"Incandescent.Halogen.SE" = item258.cast$`SE_Incandescent / Halogen`
                            ,"Linear.Fluorescent" = item258.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item258.cast$`SE_Linear Fluorescent`
                            ,"LED" = item258.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE" = item258.cast$`SE_Light Emitting Diode`
                            ,"Other" = item258.cast$Percent_Other
                            ,"Other.SE" = item258.cast$SE_Other)

item258.table1 <- left_join(item258.table, item258.SampleSizes, by = "Clean.Room")
