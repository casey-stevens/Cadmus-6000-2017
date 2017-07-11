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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
#Item 102: DISTRIBUTION OF TANK SIZE BY FUEL TYPE (SF table 109)
#############################################################################################
#subset to columns needed for analysis
item102.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                   ,"Generic"
                                                                   ,"DHW.Fuel"
                                                                   ,"DHW.Location"
                                                                   ,"DHW.Size.(Gallons)"
                                                                   ,""))]
item102.dat$count <- 1

item102.dat0 <- item102.dat[which(item102.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item102.dat1 <- left_join(item102.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item102.dat2 <- item102.dat1[grep("Water Heater",item102.dat1$Generic),]
item102.dat3 <- item102.dat2[which(!(is.na(item102.dat2$`DHW.Size.(Gallons)`))),]
item102.dat4 <- item102.dat3[which(!(item102.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item102.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item102.dat4$`DHW.Size.(Gallons)`))

item102.dat4$Gallon_bins <- item102.dat4$`DHW.Size.(Gallons)`
item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item102.dat4$Gallon_bins[which(item102.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item102.dat4$Gallon_bins)

unique(item102.dat4$DHW.Fuel)

#summarise by DHW.Fuel types
item102.sum1 <- summarise(group_by(item102.dat4, BuildingType, Gallon_bins, DHW.Fuel)
                           ,Count = sum(count))
item102.sum2 <- summarise(group_by(item102.dat4, BuildingType, DHW.Fuel)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Total.Count = sum(count))

item102.merge1 <- left_join(item102.sum1, item102.sum2, by = c("BuildingType", "DHW.Fuel"))

#summarise across DHW.Fuel types
item102.sum3 <- summarise(group_by(item102.dat4, BuildingType, Gallon_bins)
                          ,DHW.Fuel = "All Fuel Types"
                          ,Count = sum(count))
item102.sum4 <- summarise(group_by(item102.dat4, BuildingType)
                          ,DHW.Fuel = "All Fuel Types"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))

item102.merge2 <- left_join(item102.sum3, item102.sum4, by = c("BuildingType", "DHW.Fuel"))

item102.final <- rbind.data.frame(item102.merge1, item102.merge2, stringsAsFactors = F)
item102.final$Percent <- item102.final$Count / item102.final$Total.Count
item102.final$SE <- sqrt(item102.final$Percent * (1 - item102.final$Percent) / item102.final$SampleSize)

item102.cast <- dcast(setDT(item102.final)
                      ,formula = BuildingType + DHW.Fuel ~ Gallon_bins
                      ,value.var = c("SampleSize", "Percent","SE"))
item102.final <- data.frame("BuildingType"          = item102.cast$BuildingType
                            ,"DHW.Fuel"             = item102.cast$DHW.Fuel
                            ,"Percent_0_50_Gallons" = item102.cast$`Percent_0-55 Gallons`
                            ,"SE_0_50_Gallons"      = item102.cast$`SE_0-55 Gallons`
                            ,"Percent_GT50_Gallons"  = item102.cast$`Percent_>55 Gallons`
                            ,"SE_GT50_Gallons"       = item102.cast$`SE_>55 Gallons`
                            ,"SampleSize"           = item102.cast$`SampleSize_0-55 Gallons`)
item102.table <- item102.final[which(item102.final$BuildingType == "Single Family"),]














#############################################################################################
#Item 103: DISTRIBUTION OF ELECTRIC WATER HEATER TANK SIZE BY LOCATION (SF table 110)
#############################################################################################
#subset to columns needed for analysis
item103.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item103.dat$count <- 1

item103.dat0 <- item103.dat[which(item103.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item103.dat1 <- left_join(item103.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item103.dat2 <- item103.dat1[grep("Water Heater",item103.dat1$Generic),]
item103.dat3 <- item103.dat2[which(!(is.na(item103.dat2$`DHW.Size.(Gallons)`))),]
item103.dat4 <- item103.dat3[which(!(item103.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item103.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item103.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item103.dat4$Gallon_bins <- item103.dat4$`DHW.Size.(Gallons)`
item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item103.dat4$Gallon_bins[which(item103.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item103.dat4$Gallon_bins)

#clean location types
item103.dat4$DHW.Location[grep("storage|Storage",item103.dat4$DHW.Location)] <- "Storage"
item103.dat4$DHW.Location[grep("outside|Outside|exterior|Exterior",item103.dat4$DHW.Location)] <- "Outside"
item103.dat4$DHW.Location[grep("Other|2&3|Mechanical",item103.dat4$DHW.Location)] <- "Other"
unique(item103.dat4$DHW.Location)

item103.dat5 <- item103.dat4[which(item103.dat4$DHW.Location == "Electric"),]

#summarise by DHW.Location types
item103.sum1 <- summarise(group_by(item103.dat4, BuildingType, Gallon_bins, DHW.Location)
                          ,Count = sum(count))
item103.sum2 <- summarise(group_by(item103.dat4, BuildingType, DHW.Location)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))

item103.merge1 <- left_join(item103.sum1, item103.sum2, by = c("BuildingType", "DHW.Location"))

#summarise across DHW.Location types
item103.sum3 <- summarise(group_by(item103.dat4, BuildingType, Gallon_bins)
                          ,DHW.Location = "All Locations"
                          ,Count = sum(count))
item103.sum4 <- summarise(group_by(item103.dat4, BuildingType)
                          ,DHW.Location = "All Locations"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))

item103.merge2 <- left_join(item103.sum3, item103.sum4, by = c("BuildingType", "DHW.Location"))

item103.final <- rbind.data.frame(item103.merge1, item103.merge2, stringsAsFactors = F)
item103.final$Percent <- item103.final$Count / item103.final$Total.Count
item103.final$SE <- sqrt(item103.final$Percent * (1 - item103.final$Percent) / item103.final$SampleSize)

item103.cast <- dcast(setDT(item103.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("SampleSize", "Percent","SE"))
item103.final <- data.frame("BuildingType"          = item103.cast$BuildingType
                            ,"DHW.Location"         = item103.cast$DHW.Location
                            ,"Percent_0_50_Gallons" = item103.cast$`Percent_0-55 Gallons`
                            ,"SE_0_50_Gallons"      = item103.cast$`SE_0-55 Gallons`
                            ,"Percent_GT50_Gallons" = item103.cast$`Percent_>55 Gallons`
                            ,"SE_GT50_Gallons"      = item103.cast$`SE_>55 Gallons`
                            ,"SampleSize"           = item103.cast$`SampleSize_0-55 Gallons`)
item103.table <- item103.final[which(item103.final$BuildingType == "Single Family"),]













#############################################################################################
#Item 104: DISTRIBUTION OF GAS WATER HEATER TANK SIZE BY LOCATION (SF table 110)
#############################################################################################
#subset to columns needed for analysis
item104.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,""))]
item104.dat$count <- 1

item104.dat0 <- item104.dat[which(item104.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item104.dat1 <- left_join(item104.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item104.dat2 <- item104.dat1[grep("Water Heater",item104.dat1$Generic),]
item104.dat3 <- item104.dat2[which(!(is.na(item104.dat2$`DHW.Size.(Gallons)`))),]
item104.dat4 <- item104.dat3[which(!(item104.dat3$`DHW.Size.(Gallons)` %in% c(0, "Unknown"))),]

item104.dat4$`DHW.Size.(Gallons)` <- as.numeric(as.character(item104.dat4$`DHW.Size.(Gallons)`))

#clean gallon bins
item104.dat4$Gallon_bins <- item104.dat4$`DHW.Size.(Gallons)`
item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` <= 55)] <- "0-55 Gallons"
item104.dat4$Gallon_bins[which(item104.dat4$`DHW.Size.(Gallons)` >  55)] <- ">55 Gallons"
unique(item104.dat4$Gallon_bins)

#clean location types
item104.dat4$DHW.Location[grep("storage|Storage",item104.dat4$DHW.Location)] <- "Storage"
item104.dat4$DHW.Location[grep("outside|Outside|exterior|Exterior",item104.dat4$DHW.Location)] <- "Outside"
item104.dat4$DHW.Location[grep("Other|2&3|Mechanical",item104.dat4$DHW.Location)] <- "Other"
unique(item104.dat4$DHW.Location)

item104.dat5 <- item104.dat4[which(item104.dat4$DHW.Location == "Natural Gas"),]

#summarise by DHW.Location types
item104.sum1 <- summarise(group_by(item104.dat4, BuildingType, Gallon_bins, DHW.Location)
                          ,Count = sum(count))
item104.sum2 <- summarise(group_by(item104.dat4, BuildingType, DHW.Location)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))

item104.merge1 <- left_join(item104.sum1, item104.sum2, by = c("BuildingType", "DHW.Location"))

#summarise across DHW.Location types
item104.sum3 <- summarise(group_by(item104.dat4, BuildingType, Gallon_bins)
                          ,DHW.Location = "All Locations"
                          ,Count = sum(count))
item104.sum4 <- summarise(group_by(item104.dat4, BuildingType)
                          ,DHW.Location = "All Locations"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Total.Count = sum(count))

item104.merge2 <- left_join(item104.sum3, item104.sum4, by = c("BuildingType", "DHW.Location"))

item104.final <- rbind.data.frame(item104.merge1, item104.merge2, stringsAsFactors = F)
item104.final$Percent <- item104.final$Count / item104.final$Total.Count
item104.final$SE <- sqrt(item104.final$Percent * (1 - item104.final$Percent) / item104.final$SampleSize)

item104.cast <- dcast(setDT(item104.final)
                      ,formula = BuildingType + DHW.Location ~ Gallon_bins
                      ,value.var = c("SampleSize", "Percent","SE"))
item104.final <- data.frame("BuildingType"          = item104.cast$BuildingType
                            ,"DHW.Location"         = item104.cast$DHW.Location
                            ,"Percent_0_50_Gallons" = item104.cast$`Percent_0-55 Gallons`
                            ,"SE_0_50_Gallons"      = item104.cast$`SE_0-55 Gallons`
                            ,"Percent_GT50_Gallons" = item104.cast$`Percent_>55 Gallons`
                            ,"SE_GT50_Gallons"      = item104.cast$`SE_>55 Gallons`
                            ,"SampleSize"           = item104.cast$`SampleSize_0-55 Gallons`)
item104.table <- item104.final[which(item104.final$BuildingType == "Single Family"),]















#############################################################################################
#Item 105: DISTRIBUTION OF WATER HEATERS BY VINTAGE (SF table 112, MH table 87)
#############################################################################################
#subset to columns needed for analysis
item105.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"DHW.Fuel"
                                                                    ,"DHW.Location"
                                                                    ,"DHW.Size.(Gallons)"
                                                                    ,"DHW.Year.Manufactured"))]
item105.dat$count <- 1

item105.dat0 <- item105.dat[which(item105.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item105.dat1 <- left_join(item105.dat0, rbsa.dat, by = "CK_Cadmus_ID")

item105.dat2 <- item105.dat1[which(!(is.na(item105.dat1$DHW.Year.Manufactured))),]
item105.dat3 <- item105.dat2[which(!(item105.dat2$DHW.Year.Manufactured %in% c("-- Datapoint not asked for --", "Unknown"))),]
unique(item105.dat3$DHW.Year.Manufactured)

# Bin equipment vintages for items 50 and 52 (4 categories)
item105.dat3$EquipVintage_bins <- as.numeric(as.character(item105.dat3$DHW.Year.Manufactured))

item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured < 1990)] <- "Pre 1990"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 1990 & item105.dat3$DHW.Year.Manufactured < 1999)] <- "1990-1999"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2000 & item105.dat3$DHW.Year.Manufactured < 2005)] <- "2000-2004"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2005 & item105.dat3$DHW.Year.Manufactured < 2009)] <- "2005-2009"
item105.dat3$EquipVintage_bins[which(item105.dat3$DHW.Year.Manufactured >= 2009)] <- "Post 2009"
#check uniques
unique(item105.dat3$EquipVintage_bins)


#summarise by equipment vintage bins
item105.sum1 <- summarise(group_by(item105.dat3, BuildingType, EquipVintage_bins)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = sum(count))


#summarise across equipment vitnage bins
item105.sum2 <- summarise(group_by(item105.dat3, BuildingType)
                          ,EquipVintage_bins = "Total"
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          ,Count = sum(count))

#merge
item105.merge1 <- rbind.data.frame(item105.sum1, item105.sum2, stringsAsFactors = F)

item105.tot.counts <- item105.sum2[which(colnames(item105.sum2) %in% c("BuildingType", "Count"))]

#join
item105.final <- left_join(item105.merge1, item105.tot.counts, by = c("BuildingType"))
colnames(item105.final) <- c("BuildingType", "EquipmentVintages", "SampleSize", "Count", "TotalCount")

item105.final$Percent <- item105.final$Count / item105.final$TotalCount
item105.final$SE <- sqrt(item105.final$Percent * (1 - item105.final$Percent) / item105.final$SampleSize)

#keep only relevant columns
item105.table <- item105.final[which(colnames(item105.final) %in% c("BuildingType"
                                                                    ,"EquipmentVintages"
                                                                    ,"SampleSize"
                                                                    ,"Percent"
                                                                    ,"SE"))]


#subset to only relevant building types
item105.table1 <- item105.table[which(item105.table$BuildingType %in% c("Single Family", "Manufactured")),]
