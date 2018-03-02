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
item264.dat0 <- left_join(rbsa.dat, item264.dat00, by = c("CK_Building_ID" = "CK_SiteID"))
colnames(item264.dat0)[which(colnames(item264.dat0) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#remove missing vintage info
item264.dat1 <- item264.dat0[which(!(is.na(item264.dat0$HomeYearBuilt_MF))),]

#remove building info
item264.dat2 <- item264.dat1[grep("BLDG", item264.dat1$CK_Building_ID),]

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


## Clean Switch Type
unique(item264.dat4$Switch.Type)
item264.dat4$Switch.Type[grep("On/off|3", item264.dat4$Switch.Type, ignore.case = T)] <- "Manual Switch"
item264.dat4$Switch.Type[grep("Other", item264.dat4$Switch.Type, ignore.case = T)] <- "Other"
item264.dat4$Switch.Type[grep("Timer", item264.dat4$Switch.Type, ignore.case = T)] <- "Timer Control"
item264.dat4$Switch.Type[grep("Dimmer", item264.dat4$Switch.Type, ignore.case = T)] <- "Dimmer Switch"
item264.dat4$Switch.Type[grep("Unknown", item264.dat4$Switch.Type, ignore.case = T)] <- "Unknown Switch"
item264.dat4$Switch.Type[grep("Always", item264.dat4$Switch.Type, ignore.case = T)] <- "24 Hour Operation"



#Subset to Multifamily
item264.dat5 <- item264.dat4[grep("Multifamily", item264.dat4$BuildingType),]

#summarise up to the site level
item264.dat6 <- summarise(group_by(item264.dat5, CK_Cadmus_ID, CK_Building_ID, Clean.Room, Lamp.Category)
                          ,SiteLampCount = sum(Lamps)
                          ,SiteWattage = sum(Total.Wattage))

item264.merge <- left_join(rbsa.dat, item264.dat6)
item264.merge <- item264.merge[which(!is.na(item264.merge$SiteWattage)),]

######################################
#Pop and Sample Sizes for weights
######################################
item264.data <- weightedData(item264.merge[which(colnames(item264.merge) %notin% c("SiteWattage"
                                                                                   ,"Clean.Room"
                                                                                   ,"Lamp.Category"
                                                                                   ,'SiteLampCount'))])

item264.data <- left_join(item264.data, item264.merge[which(colnames(item264.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"SiteWattage"
                                                                                           ,"Clean.Room"
                                                                                           ,"Lamp.Category"
                                                                                           ,'SiteLampCount'))])
item264.data$count <- 1


######################
# weighted analysis
######################
item264.summary <- proportionRowsAndColumns1(CustomerLevelData = item264.data
                                             ,valueVariable = 'SiteWattage'
                                             ,columnVariable = 'Clean.Room'
                                             ,rowVariable = 'Lamp.Category'
                                             ,aggregateColumnName = "Remove")
item264.summary <- item264.summary[which(item264.summary$Clean.Room != "Remove"),]
# item264.summary <- item264.summary[which(item264.summary$Lamp.Category != "Total"),]

item264.all.categories <- proportions_one_group(CustomerLevelData = item264.data
                                                   ,valueVariable = 'SiteWattage'
                                                   ,groupingVariable = 'Lamp.Category'
                                                   ,total.name = 'All Categories'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = TRUE
                                                   ,two.prop.total = TRUE)
# item264.all.categories <- item264.all.categories[which(item264.all.categories$Lamp.Category != "Total"),]

item264.final <- rbind.data.frame(item264.summary, item264.all.categories, stringsAsFactors = F)

item264.cast <- dcast(setDT(item264.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("w.percent", "w.SE", "count", "n","N","EB"))

item264.table <- data.frame("Exterior.Category"      = item264.cast$Clean.Room
                            ,"CFL"                   = item264.cast$`w.percent_Compact Fluorescent`
                            ,"CFL.SE"                = item264.cast$`w.SE_Compact Fluorescent`
                            ,"Halogen"               = item264.cast$w.percent_Halogen
                            ,"Halogen.SE"            = item264.cast$w.SE_Halogen
                            ,"Incandescent"          = item264.cast$w.percent_Incandescent
                            ,"Incandescent.SE"       = item264.cast$w.SE_Incandescent
                            ,"Linear.Fluorescent"    = item264.cast$`w.percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item264.cast$`w.SE_Linear Fluorescent`
                            ,"LED"                   = item264.cast$`w.percent_Light Emitting Diode`
                            ,"LED.SE"                = item264.cast$`w.SE_Light Emitting Diode`
                            ,"Other"                 = item264.cast$w.percent_Other
                            ,"Other.SE"              = item264.cast$w.SE_Other
                            ,"Unknown"               = item264.cast$w.percent_Unknown
                            ,"Unknown.SE"            = item264.cast$w.SE_Unknown
                            ,"n"                     = item264.cast$n_Total
                            ,"CFL.EB"                = item264.cast$`EB_Compact Fluorescent`
                            ,"Halogen.EB"            = item264.cast$EB_Halogen
                            ,"Incandescent.EB"       = item264.cast$EB_Incandescent
                            ,"Linear.Fluorescent.EB" = item264.cast$`EB_Linear Fluorescent`
                            ,"LED.EB"                = item264.cast$`EB_Light Emitting Diode`
                            ,"Other.EB"              = item264.cast$EB_Other
                            ,"Unknown.EB"            = item264.cast$EB_Unknown)

exportTable(item264.table, "MF", "Table 56", weighted = TRUE)


######################
# unweighted analysis
######################
item264.summary <- proportions_two_groups_unweighted(CustomerLevelData = item264.data
                                             ,valueVariable = 'SiteWattage'
                                             ,columnVariable = 'Clean.Room'
                                             ,rowVariable = 'Lamp.Category'
                                             ,aggregateColumnName = "Remove")
item264.summary <- item264.summary[which(item264.summary$Clean.Room != "Remove"),]
# item264.summary <- item264.summary[which(item264.summary$Lamp.Category != "Total"),]

item264.all.categories <- proportions_one_group(CustomerLevelData = item264.data
                                                   ,valueVariable = 'SiteWattage'
                                                   ,groupingVariable = 'Lamp.Category'
                                                   ,total.name = 'All Categories'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = FALSE
                                                   ,two.prop.total = TRUE)
# item264.all.categories <- item264.all.categories[which(item264.all.categories$Lamp.Category != "Total"),]

item264.final <- rbind.data.frame(item264.summary, item264.all.categories, stringsAsFactors = F)

item264.cast <- dcast(setDT(item264.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "Count", "n"))

item264.table <- data.frame("Exterior.Category"      = item264.cast$Clean.Room
                            ,"CFL"                   = item264.cast$`Percent_Compact Fluorescent`
                            ,"CFL.SE"                = item264.cast$`SE_Compact Fluorescent`
                            ,"Halogen"               = item264.cast$Percent_Halogen
                            ,"Halogen.SE"            = item264.cast$SE_Halogen
                            ,"Incandescent"          = item264.cast$Percent_Incandescent
                            ,"Incandescent.SE"       = item264.cast$SE_Incandescent
                            ,"Linear.Fluorescent"    = item264.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item264.cast$`SE_Linear Fluorescent`
                            ,"LED"                   = item264.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE"                = item264.cast$`SE_Light Emitting Diode`
                            ,"Other"                 = item264.cast$Percent_Other
                            ,"Other.SE"              = item264.cast$SE_Other
                            ,"Unknown"               = item264.cast$Percent_Unknown
                            ,"Unknown.SE"            = item264.cast$SE_Unknown
                            ,"n"                     = item264.cast$n_Total)

exportTable(item264.table, "MF", "Table 56", weighted = FALSE)




#############################################################################################
#Item 265: DISTRIBUTION OF EXTERIOR LAMPS BY LAMP TYPE AND EXTERIOR CATEGORY (MF Table 57)
#############################################################################################
item265.data <- item264.data

######################
# weighted analysis
######################
item265.summary <- proportionRowsAndColumns1(CustomerLevelData = item265.data
                                             ,valueVariable = 'SiteLampCount'
                                             ,columnVariable = 'Clean.Room'
                                             ,rowVariable = 'Lamp.Category'
                                             ,aggregateColumnName = "Remove")
item265.summary <- item265.summary[which(item265.summary$Clean.Room != "Remove"),]
# item265.summary <- item265.summary[which(item265.summary$Lamp.Category != "Total"),]

item265.all.categories <- proportions_one_group(CustomerLevelData = item265.data
                                                   ,valueVariable = 'SiteLampCount'
                                                   ,groupingVariable = 'Lamp.Category'
                                                   ,total.name = 'All Categories'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = TRUE
                                                   ,two.prop.total = TRUE)
# item265.all.categories <- item265.all.categories[which(item265.all.categories$Lamp.Category != "Total"),]

item265.final <- rbind.data.frame(item265.summary, item265.all.categories, stringsAsFactors = F)

item265.cast <- dcast(setDT(item265.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("w.percent", "w.SE", "count", "n","N","EB"))

item265.table <- data.frame("Exterior.Category"      = item265.cast$Clean.Room
                            ,"CFL"                   = item265.cast$`w.percent_Compact Fluorescent`
                            ,"CFL.SE"                = item265.cast$`w.SE_Compact Fluorescent`
                            ,"Halogen"               = item265.cast$w.percent_Halogen
                            ,"Halogen.SE"            = item265.cast$w.SE_Halogen
                            ,"Incandescent"          = item265.cast$w.percent_Incandescent
                            ,"Incandescent.SE"       = item265.cast$w.SE_Incandescent
                            ,"Linear.Fluorescent"    = item265.cast$`w.percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item265.cast$`w.SE_Linear Fluorescent`
                            ,"LED"                   = item265.cast$`w.percent_Light Emitting Diode`
                            ,"LED.SE"                = item265.cast$`w.SE_Light Emitting Diode`
                            ,"Other"                 = item265.cast$w.percent_Other
                            ,"Other.SE"              = item265.cast$w.SE_Other
                            ,"Unknown"               = item265.cast$w.percent_Unknown
                            ,"Unknown.SE"            = item265.cast$w.SE_Unknown
                            ,"n"                     = item265.cast$n_Total
                            ,"CFL.EB"                = item265.cast$`EB_Compact Fluorescent`
                            ,"Halogen.EB"            = item265.cast$EB_Halogen
                            ,"Incandescent.EB"       = item265.cast$EB_Incandescent
                            ,"Linear.Fluorescent.EB" = item265.cast$`EB_Linear Fluorescent`
                            ,"LED.EB"                = item265.cast$`EB_Light Emitting Diode`
                            ,"Other.EB"              = item265.cast$EB_Other
                            ,"Unknown.EB"            = item265.cast$EB_Unknown)

exportTable(item265.table, "MF", "Table 57", weighted = TRUE)


######################
# unweighted analysis
######################
item265.summary <- proportions_two_groups_unweighted(CustomerLevelData = item265.data
                                                     ,valueVariable = 'SiteLampCount'
                                                     ,columnVariable = 'Clean.Room'
                                                     ,rowVariable = 'Lamp.Category'
                                                     ,aggregateColumnName = "Remove")
item265.summary <- item265.summary[which(item265.summary$Clean.Room != "Remove"),]
# item265.summary <- item265.summary[which(item265.summary$Lamp.Category != "Total"),]

item265.all.categories <- proportions_one_group(CustomerLevelData = item265.data
                                                   ,valueVariable = 'SiteLampCount'
                                                   ,groupingVariable = 'Lamp.Category'
                                                   ,total.name = 'All Categories'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = FALSE
                                                   ,two.prop.total = TRUE)
# item265.all.categories <- item265.all.categories[which(item265.all.categories$Lamp.Category != "Total"),]

item265.final <- rbind.data.frame(item265.summary, item265.all.categories, stringsAsFactors = F)

item265.cast <- dcast(setDT(item265.final)
                      ,formula = Clean.Room ~ Lamp.Category
                      ,value.var = c("Percent", "SE", "Count", "n"))

item265.table <- data.frame("Exterior.Category"      = item265.cast$Clean.Room
                            ,"CFL"                   = item265.cast$`Percent_Compact Fluorescent`
                            ,"CFL.SE"                = item265.cast$`SE_Compact Fluorescent`
                            ,"Halogen"               = item265.cast$Percent_Halogen
                            ,"Halogen.SE"            = item265.cast$SE_Halogen
                            ,"Incandescent"          = item265.cast$Percent_Incandescent
                            ,"Incandescent.SE"       = item265.cast$SE_Incandescent
                            ,"Linear.Fluorescent"    = item265.cast$`Percent_Linear Fluorescent`
                            ,"Linear.Fluorescent.SE" = item265.cast$`SE_Linear Fluorescent`
                            ,"LED"                   = item265.cast$`Percent_Light Emitting Diode`
                            ,"LED.SE"                = item265.cast$`SE_Light Emitting Diode`
                            ,"Other"                 = item265.cast$Percent_Other
                            ,"Other.SE"              = item265.cast$SE_Other
                            ,"Unknown"               = item265.cast$Percent_Unknown
                            ,"Unknown.SE"            = item265.cast$SE_Unknown
                            ,"n"                     = item265.cast$n_Total)

exportTable(item265.table, "MF", "Table 57", weighted = FALSE)






#############################################################################################
#Item 266: AVERAGE EXTERIOR LIGHTING POWER (WATTS) BY EXTERIOR CATEGORY AND BUILDING SIZE (MF Table 58)
#############################################################################################
item266.data <- item264.data

######################
# weighted analysis
######################
item266.cast <- mean_two_groups(CustomerLevelData = item266.data
                                   ,valueVariable = 'SiteWattage'
                                   ,byVariableRow = 'Clean.Room'
                                   ,byVariableColumn = 'HomeType'
                                   ,columnAggregate = "All Sizes"
                                   ,rowAggregate = "All Categories")
item266.cast <- data.frame(item266.cast, stringsAsFactors = F)

item266.table <- data.frame("Exterior.Category"        = item266.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean"       = item266.cast$Mean_Apartment.Building..3.or.fewer.floors.
                            ,"Low_Rise_SE"             = item266.cast$SE_Apartment.Building..3.or.fewer.floors.
                            ,"Low_Rise_n"              = item266.cast$n_Apartment.Building..3.or.fewer.floors.
                            ,"Mid_Rise_4.6_Mean"       = item266.cast$Mean_Apartment.Building..4.to.6.floors.
                            ,"Mid_Rise_SE"             = item266.cast$SE_Apartment.Building..4.to.6.floors.
                            ,"Mid_Rise_n"              = item266.cast$n_Apartment.Building..4.to.6.floors.
                            ,"High_Rise_7Plus_Mean"    = NA#item266.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = NA#item266.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = NA#item266.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean"          = item266.cast$Mean_All.Sizes
                            ,"All_Sizes_SE"            = item266.cast$SE_All.Sizes
                            ,"All_Sizes_n"             = item266.cast$n_All.Sizes
                            ,"Low_Rise_EB"             = item266.cast$EB_Apartment.Building..3.or.fewer.floors.
                            ,"Mid_Rise_EB"             = item266.cast$EB_Apartment.Building..4.to.6.floors.
                            ,"High_Rise_EB"            = NA#item266.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_EB"            = item266.cast$EB_All.Sizes
                            
)

exportTable(item266.table, "MF", "Table 58", weighted = TRUE)


######################
# weighted analysis
######################
item266.cast <- mean_two_groups_unweighted(CustomerLevelData = item266.data
                                ,valueVariable = 'SiteWattage'
                                ,byVariableRow = 'Clean.Room'
                                ,byVariableColumn = 'HomeType'
                                ,columnAggregate = "All Sizes"
                                ,rowAggregate = "All Categories")
item266.cast <- data.frame(item266.cast, stringsAsFactors = F)

item266.table <- data.frame("Exterior.Category"        = item266.cast$Clean.Room
                            ,"Low_Rise_1.3_Mean"       = item266.cast$Mean_Apartment.Building..3.or.fewer.floors.
                            ,"Low_Rise_SE"             = item266.cast$SE_Apartment.Building..3.or.fewer.floors.
                            ,"Low_Rise_n"              = item266.cast$n_Apartment.Building..3.or.fewer.floors.
                            ,"Mid_Rise_4.6_Mean"       = item266.cast$Mean_Apartment.Building..4.to.6.floors.
                            ,"Mid_Rise_SE"             = item266.cast$SE_Apartment.Building..4.to.6.floors.
                            ,"Mid_Rise_n"              = item266.cast$n_Apartment.Building..4.to.6.floors.
                            ,"High_Rise_7Plus_Mean"    = NA#item266.cast$`Mean_Apartment Building (More than 6 floors)`
                            ,"High_Rise_SE"            = NA#item266.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"Hight_Rise_n"            = NA#item266.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All_Sizes_Mean"          = item266.cast$Mean_All.Sizes
                            ,"All_Sizes_SE"            = item266.cast$SE_All.Sizes
                            ,"All_Sizes_n"             = item266.cast$n_All.Sizes
)

exportTable(item266.table, "MF", "Table 58", weighted = FALSE)







#############################################################################################
#Item 267: DISTRIBUTION OF EXTERIOR LIGHTING POWER (WATTS) BY CONTROL TYPE AND EXTERIOR CATEGORY (MF Table 59)
#############################################################################################
#Subset to Multifamily
item267.dat5 <- item264.dat4[grep("Multifamily", item264.dat4$BuildingType),]

#summarise up to the site level
item267.dat6 <- summarise(group_by(item267.dat5, CK_Cadmus_ID, CK_Building_ID, Clean.Room, Lamp.Category, Switch.Type)
                          ,SiteLampCount = sum(Lamps)
                          ,SiteWattage = sum(Total.Wattage))

item267.merge <- left_join(rbsa.dat, item267.dat6)
item267.merge <- item267.merge[which(!is.na(item267.merge$SiteWattage)),]

######################################
#Pop and Sample Sizes for weights
######################################
item267.data <- weightedData(item267.merge[which(colnames(item267.merge) %notin% c("Clean.Room"
                                                                                   ,"Switch.Type"
                                                                                   ,"Lamp.Category"
                                                                                   ,"SiteLampCount"
                                                                                   ,"SiteWattage"))])

item267.data <- left_join(item267.data, item267.merge[which(colnames(item267.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Clean.Room"
                                                                                           ,"Switch.Type"
                                                                                           ,"Lamp.Category"
                                                                                           ,"SiteLampCount"
                                                                                           ,"SiteWattage"))])
item267.data$count <- 1


######################
# weighted analysis
######################
item267.summary <- proportionRowsAndColumns1(CustomerLevelData = item267.data
                                             ,valueVariable = 'SiteLampCount'
                                             ,columnVariable = 'Clean.Room'
                                             ,rowVariable = 'Switch.Type'
                                             ,aggregateColumnName = "Remove")
item267.summary <- item267.summary[which(item267.summary$Clean.Room != "Remove"),]
# item267.summary <- item267.summary[which(item267.summary$Switch.Type != "Total"),]

item267.all.categories <- proportions_one_group(CustomerLevelData = item267.data
                                                   ,valueVariable = 'SiteLampCount'
                                                   ,groupingVariable = 'Switch.Type'
                                                   ,total.name = 'All Types'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = TRUE
                                                   ,two.prop.total = TRUE)
# item267.all.categories <- item267.all.categories[which(item267.all.categories$Lamp.Category != "Total"),]

item267.final <- rbind.data.frame(item267.summary, item267.all.categories, stringsAsFactors = F)

item267.cast <- dcast(setDT(item267.final)
                      ,formula = Clean.Room ~ Switch.Type
                      ,value.var = c("w.percent", "w.SE", "count", "n","N","EB"))
names(item267.cast)
item267.table <- data.frame("Exterior.Category"           = item267.cast$Clean.Room
                            ,"24.Hour.Operation"          = item267.cast$`w.percent_24 Hour Operation`
                            ,"24.Hour.Operation.SE"       = item267.cast$`w.SE_24 Hour Operation`
                            ,"Manual.Switch"              = item267.cast$`w.percent_Manual Switch`
                            ,"Manual.Switch.SE"           = item267.cast$`w.SE_Manual Switch`
                            ,"Motion.Sensor"              = item267.cast$`w.percent_Motion Sensor`
                            ,"Motion.Sensor.SE"           = item267.cast$`w.SE_Motion Sensor`
                            ,"Photo.Sensor"               = item267.cast$`w.percent_Light Sensor`
                            ,"Photo.Sensor.SE"            = item267.cast$`w.SE_Light Sensor`
                            ,"Photo.and.Motion.Sensor"    = item267.cast$`w.percent_Motion & Light Sensor`
                            ,"Photo.and.Motion.Sensor.SE" = item267.cast$`w.SE_Motion & Light Sensor`
                            ,"Timer Control"              = item267.cast$`w.percent_Timer Control`
                            ,"Timer Control.SE"           = item267.cast$`w.SE_Timer Control`
                            ,"Other"                      = NA#item267.cast$w.percent
                            ,"Other.SE"                   = NA#item267.cast$w.SE_Other
                            ,"Unknown"                    = item267.cast$w.percent_Unknown
                            ,"Unknown.SE"                 = item267.cast$w.SE_Unknown
                            ,"n"                          = item267.cast$n_Total
                            ,"24.Hour.Operation.EB"       = item267.cast$`EB_24 Hour Operation`
                            ,"Manual.Switch.EB"           = item267.cast$`EB_Manual Switch`
                            ,"Motion.Sensor.EB"           = item267.cast$`EB_Motion Sensor`
                            ,"Photo.Sensor.EB"            = item267.cast$`EB_Light Sensor`
                            ,"Photo.and.Motion.Sensor.EB" = item267.cast$`EB_Motion & Light Sensor`
                            ,"Timer Control.EB"           = item267.cast$`EB_Timer Control`
                            ,"Other.EB"                   = NA#item267.cast$EB_Other
                            ,"Unknown.EB"                 = item267.cast$EB_Unknown
                            )

exportTable(item267.table, "MF", "Table 59", weighted = TRUE)

######################
# weighted analysis
######################
item267.summary <- proportions_two_groups_unweighted(CustomerLevelData = item267.data
                                             ,valueVariable = 'SiteLampCount'
                                             ,columnVariable = 'Clean.Room'
                                             ,rowVariable = 'Switch.Type'
                                             ,aggregateColumnName = "Remove")
item267.summary <- item267.summary[which(item267.summary$Clean.Room != "Remove"),]
# item267.summary <- item267.summary[which(item267.summary$Switch.Type != "Total"),]

item267.all.categories <- proportions_one_group(CustomerLevelData = item267.data
                                                   ,valueVariable = 'SiteLampCount'
                                                   ,groupingVariable = 'Switch.Type'
                                                   ,total.name = 'All Types'
                                                   ,columnName = 'Clean.Room'
                                                   ,weighted = FALSE
                                                   ,two.prop.total = TRUE)
# item267.all.categories <- item267.all.categories[which(item267.all.categories$Lamp.Category != "Total"),]

item267.final <- rbind.data.frame(item267.summary, item267.all.categories, stringsAsFactors = F)

item267.cast <- dcast(setDT(item267.final)
                      ,formula = Clean.Room ~ Switch.Type
                      ,value.var = c("Percent", "SE", "Count", "n"))

item267.table <- data.frame("Exterior.Category"           = item267.cast$Clean.Room
                            ,"24.Hour.Operation"          = item267.cast$`Percent_24 Hour Operation`
                            ,"24.Hour.Operation.SE"       = item267.cast$`SE_24 Hour Operation`
                            ,"Manual.Switch"              = item267.cast$`Percent_Manual Switch`
                            ,"Manual.Switch.SE"           = item267.cast$`SE_Manual Switch`
                            ,"Motion.Sensor"              = item267.cast$`Percent_Motion Sensor`
                            ,"Motion.Sensor.SE"           = item267.cast$`SE_Motion Sensor`
                            ,"Photo.Sensor"               = item267.cast$`Percent_Light Sensor`
                            ,"Photo.Sensor.SE"            = item267.cast$`SE_Light Sensor`
                            ,"Photo.and.Motion.Sensor"    = item267.cast$`Percent_Motion & Light Sensor`
                            ,"Photo.and.Motion.Sensor.SE" = item267.cast$`SE_Motion & Light Sensor`
                            ,"Timer Control"              = item267.cast$`Percent_Timer Control`
                            ,"Timer Control.SE"           = item267.cast$`SE_Timer Control`
                            ,"Other"                      = NA#item267.cast$Percent
                            ,"Other.SE"                   = NA#item267.cast$SE_Other
                            ,"Unknown"                    = item267.cast$Percent_Unknown
                            ,"Unknown.SE"                 = item267.cast$SE_Unknown
                            ,"n"                          = item267.cast$n_Total)

exportTable(item267.table, "MF", "Table 59", weighted = FALSE)

