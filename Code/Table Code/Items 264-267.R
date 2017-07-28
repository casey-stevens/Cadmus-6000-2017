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






#############################################################################################
#Item 264: DISTRIBUTION OF EXTERIOR LIGHTING POWER (WATTS) BY LAMP TYPE AND EXTERIOR CATEGORY (MF Table 56)
#############################################################################################
#subset to columns needed for analysis
item264.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"CK_SiteID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Wattage"
                                                                ,"Clean.Room"
                                                                ,"Switch.Type"))]
item264.dat$count <- 1

item264.dat00 <- item264.dat[which(item264.dat$Clean.Room %in% c("Outside", "Parking")),]

#join clean rbsa data onto lighting analysis data
item264.dat0 <- left_join(item264.dat00, rbsa.dat, by = "CK_Cadmus_ID")

#remove missing vintage info
item264.dat1 <- item264.dat0[which(!(is.na(item264.dat0$HomeYearBuilt_MF))),]

#remove building info
item264.dat2 <- item264.dat1[grep("BLDG", item264.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
item264.dat2$Fixture.Qty <- as.numeric(as.character(item264.dat2$Fixture.Qty))
item264.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(item264.dat2$LIGHTING_BulbsPerFixture))

#calculate total lamps as # fixtures * # bulbs per fixture
item264.dat2$Lamps <- item264.dat2$Fixture.Qty * item264.dat2$LIGHTING_BulbsPerFixture
unique(item264.dat2$Lamps)
#remove missing lamp quantities
item264.dat3 <- item264.dat2[which(!(is.na(item264.dat2$Lamps))),]

#calculate total wattage as wattage per bulb multiplied by total bulbs
item264.dat3$Total.Wattage <- item264.dat3$Lamps * as.numeric(as.character(item264.dat3$Clean.Wattage))
#remove missing wattage quantities
item264.dat4 <- item264.dat3[which(!(is.na(item264.dat3$Total.Wattage))),]
#make wattage numeric
item264.dat4$Total.Wattage <- as.numeric(as.character(item264.dat4$Total.Wattage))

#Subset to Multifamily
item264.dat5 <- item264.dat4[grep("Multifamily", item264.dat4$BuildingType),]

#summarise up to the site level
item264.dat6 <- summarise(group_by(item264.dat5, CK_Cadmus_ID, Clean.Room, Lamp.Category)
                          ,SiteCount = sum(Lamps)
                          ,SiteWattage = sum(Total.Wattage))


#summarise by lamp category
# by clean room
item264.sum1 <- summarise(group_by(item264.dat6, Clean.Room, Lamp.Category)
                          ,Wattage = sum(SiteWattage))

#summarise by lamp category
# across clean room
item264.sum2 <- summarise(group_by(item264.dat6, Lamp.Category)
                          ,Clean.Room = "All Categories"
                          ,Wattage = sum(SiteWattage))

item264.merge1 <- rbind.data.frame(item264.sum1, item264.sum2, stringsAsFactors = F)


#summarise by clean room
item264.sum3 <- summarise(group_by(item264.dat6, Clean.Room)
                          ,Total.Wattage = sum(SiteWattage)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# across clean room
item264.sum4 <- summarise(group_by(item264.dat6)
                          ,Clean.Room = "All Categories"
                          ,Total.Wattage = sum(SiteWattage)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item264.merge2 <- rbind.data.frame(item264.sum3, item264.sum4, stringsAsFactors = F)

item264.final <- left_join(item264.merge1, item264.merge2, by = "Clean.Room")
item264.final$Percent <- item264.final$Wattage / item264.final$Total.Wattage
item264.final$SE <- sqrt(item264.final$Percent * (1 - item264.final$Percent) / item264.final$SampleSize)

item264.cast <- dcast(setDT(item264.final)
                      ,formula = Clean.Room + SampleSize ~ Lamp.Category
                      ,value.var = c("Percent", "SE"))

item264.table <- data.frame("Exterior.Category" = item264.cast$Clean.Room
                            ,"CFL" = item264.cast$`Percent_Compact Fluorescent`
                            ,"CFL.SE" = item264.cast$`SE_Compact Fluorescent`
                            ,"Halogen"= item264.cast$Percent_Halogen
                            ,"Halogen.SE" = item264.cast$SE_Halogen
                            ,"Incandescent" = item264.cast$Percent_Incandescent
                            ,"Incandescent.SE" = item264.cast$SE_Incandescent
                            ,"Linear.Fluorescent" = item264.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item264.cast$`SE_Linear Fluorescent`
                            ,"LED" = item264.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE" = item264.cast$`SE_Light Emitting Diode`
                            ,"Other" = item264.cast$Percent_Other
                            ,"Other.SE" = item264.cast$SE_Other
                            ,"SampleSize" = item264.cast$SampleSize)




#############################################################################################
#Item 265: DISTRIBUTION OF EXTERIOR LAMPS BY LAMP TYPE AND EXTERIOR CATEGORY (MF Table 57)
#############################################################################################
item265.dat <- item264.dat6

#summarise by lamp category
# by clean room
item265.sum1 <- summarise(group_by(item265.dat, Clean.Room, Lamp.Category)
                          ,LampCount = sum(SiteCount))

#summarise by lamp category
# across clean room
item265.sum2 <- summarise(group_by(item265.dat, Lamp.Category)
                          ,Clean.Room = "All Categories"
                          ,LampCount = sum(SiteCount))

item265.merge1 <- rbind.data.frame(item265.sum1, item265.sum2, stringsAsFactors = F)


#summarise by clean room
item265.sum3 <- summarise(group_by(item265.dat, Clean.Room)
                          ,TotalLampCount = sum(SiteCount)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# across clean room
item265.sum4 <- summarise(group_by(item265.dat)
                          ,Clean.Room = "All Categories"
                          ,TotalLampCount = sum(SiteCount)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item265.merge2 <- rbind.data.frame(item265.sum3, item265.sum4, stringsAsFactors = F)

item265.final <- left_join(item265.merge1, item265.merge2, by = "Clean.Room")
item265.final$Percent <- item265.final$LampCount / item265.final$TotalLampCount
item265.final$SE <- sqrt(item265.final$Percent * (1 - item265.final$Percent) / item265.final$SampleSize)

item265.cast <- dcast(setDT(item265.final)
                      ,formula = Clean.Room + SampleSize ~ Lamp.Category
                      ,value.var = c("Percent", "SE"))

item265.table <- data.frame("Exterior.Category" = item265.cast$Clean.Room
                            ,"CFL" = item265.cast$`Percent_Compact Fluorescent`
                            ,"CFL.SE" = item265.cast$`SE_Compact Fluorescent`
                            ,"Halogen"= item265.cast$Percent_Halogen
                            ,"Halogen.SE" = item265.cast$SE_Halogen
                            ,"Incandescent" = item265.cast$Percent_Incandescent
                            ,"Incandescent.SE" = item265.cast$SE_Incandescent
                            ,"Linear.Fluorescent" = item265.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item265.cast$`SE_Linear Fluorescent`
                            ,"LED" = item265.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE" = item265.cast$`SE_Light Emitting Diode`
                            ,"Other" = item265.cast$Percent_Other
                            ,"Other.SE" = item265.cast$SE_Other
                            ,"SampleSize" = item265.cast$SampleSize)






#############################################################################################
#Item 266: AVERAGE EXTERIOR LIGHTING POWER (WATTS) BY EXTERIOR CATEGORY AND BUILDING SIZE (MF Table 58)
#############################################################################################
item266.dat <- item264.dat6

item266.dat1 <- left_join(item266.dat, rbsa.dat, by = "CK_Cadmus_ID")

#summarise by building size
# by clean room
item266.sum1 <- summarise(group_by(item266.dat1, Clean.Room, BuildingTypeXX)
                          ,Mean = mean(SiteWattage)
                          ,SE = sd(SiteWattage) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise by building size
# across clean room
item266.sum2 <- summarise(group_by(item266.dat1, BuildingTypeXX)
                          ,Clean.Room = "All Categories"
                          ,Mean = mean(SiteWattage)
                          ,SE = sd(SiteWattage) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building size
# by clean room
item266.sum3 <- summarise(group_by(item266.dat1, Clean.Room)
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(SiteWattage)
                          ,SE = sd(SiteWattage) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

#summarise across building size
# across clean room
item266.sum4 <- summarise(group_by(item266.dat1)
                          ,Clean.Room = "All Categories"
                          ,BuildingTypeXX = "All Sizes"
                          ,Mean = mean(SiteWattage)
                          ,SE = sd(SiteWattage) / sqrt(length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


item266.final <- rbind.data.frame(item266.sum1, item266.sum2, item266.sum3, item266.sum4, stringsAsFactors = F)

item266.cast <- dcast(setDT(item266.final)
                      ,formula = Clean.Room ~ BuildingTypeXX
                      ,value.var = c("Mean", "SE", "SampleSize"))

item266.table <- data.frame("Exterior.Category" = item266.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean" = item266.cast$`Mean_Apartment Building (3 or fewer floors)`
                            ,"Low_Rise_SE" = item266.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Mid_Rise_4.6_Mean" = item266.cast$`Mean_Apartment Building (4 to 6 floors)`
                            ,"Mid_Rise_SE" = item266.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"High_Rise_7Plus_Mean" = NA#item266.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE" = NA#item266.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean" = item266.cast$`Mean_All Sizes`
                            ,"All_Sizes_SE" = item266.cast$`SE_All Sizes`
                            ,"SampleSize" = item266.sum1$SampleSize)



#############################################################################################
#Item 267: DISTRIBUTION OF EXTERIOR LIGHTING POWER (WATTS) BY CONTROL TYPE AND EXTERIOR CATEGORY (MF Table 59)
#############################################################################################
item267.dat <- item264.dat5
item267.dat$Switch.Type[grep("On/off", item267.dat$Switch.Type)] <- "Manual Switch"
item267.dat$Switch.Type[grep("Other", item267.dat$Switch.Type)] <- "Other"

#summarise up to the site level
item267.dat1 <- summarise(group_by(item267.dat, CK_Cadmus_ID, Clean.Room, Switch.Type)
                          ,SiteCount = sum(Lamps)
                          ,SiteWattage = sum(Total.Wattage))

#summarise by Switch.Type
# by clean room
item267.sum1 <- summarise(group_by(item267.dat1, Clean.Room, Switch.Type)
                          ,LampCount = sum(SiteCount))

#summarise by Switch.Type
# across clean room
item267.sum2 <- summarise(group_by(item267.dat1, Switch.Type)
                          ,Clean.Room = "All Categories"
                          ,LampCount = sum(SiteCount))

item267.merge1 <- rbind.data.frame(item267.sum1, item267.sum2, stringsAsFactors = F)


#summarise by clean room
item267.sum3 <- summarise(group_by(item267.dat1, Clean.Room)
                          ,TotalLampCount = sum(SiteCount)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
# across clean room
item267.sum4 <- summarise(group_by(item267.dat1)
                          ,Clean.Room = "All Categories"
                          ,TotalLampCount = sum(SiteCount)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item267.merge2 <- rbind.data.frame(item267.sum3, item267.sum4, stringsAsFactors = F)

item267.final <- left_join(item267.merge1, item267.merge2, by = "Clean.Room")
item267.final$Percent <- item267.final$LampCount / item267.final$TotalLampCount
item267.final$SE <- sqrt(item267.final$Percent * (1 - item267.final$Percent) / item267.final$SampleSize)

item267.cast <- dcast(setDT(item267.final)
                      ,formula = Clean.Room + SampleSize ~ Switch.Type
                      ,value.var = c("Percent", "SE"))

item267.table <- data.frame("Exterior.Category" = item267.cast$Clean.Room
                            ,"Hour.24.Operation" = NA#
                            ,"Hour.24.Operation.SE" = NA#
                            ,"Manual.Switch" = item267.cast$`Percent_Manual Switch`
                            ,"Manual.Switch.SE" = item267.cast$`SE_Manual Switch`
                            ,"Motion.Sensor" = item267.cast$`Percent_Motion Sensor`
                            ,"Motion.Sensor.SE" = item267.cast$`SE_Motion Sensor`
                            ,"Light.Sensor" = item267.cast$`Percent_Light Sensor`
                            ,"Light.Sensor.SE" = item267.cast$`SE_Light Sensor`
                            ,"Light.and.Motion.Sensor" = NA#
                            ,"Light.and.Motion.Sensor.SE" = NA#
                            ,"Timer.Control" = item267.cast$Percent_Timer
                            ,"Timer.Control.SE" = item267.cast$SE_Timer
                            ,"Other" = item267.cast$Percent_Other
                            ,"Other.SE" = item267.cast$SE_Other
                            ,"Unknown" = item267.cast$Percent_Unknown
                            ,"Unknown.SE" = item267.cast$SE_Unknown
                            ,"SampleSize" = item267.cast$SampleSize)





