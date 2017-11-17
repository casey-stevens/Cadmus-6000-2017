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
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))

mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"CK_SiteID"
                                                                        ,"System.Type"
                                                                        ,"Heating.Fuel",
                                                                        "Heat.Iteration",
                                                                        "Cool.Iteration",
                                                                        "Primary.Heating.System",
                                                                        "Primary.Cooling.System"))]

mechanical.dat2  <- left_join(rbsa.dat, mechanical.dat1, by = c("CK_Building_ID" = "CK_SiteID"))
length(unique(mechanical.dat2$CK_Cadmus_ID))
names(mechanical.dat2)[which(names(mechanical.dat2) == "CK_Cadmus_ID.x")] <- "CK_Cadmus_ID"

#Subset to MF
mechanical.dat.MF <- mechanical.dat2[grep("Multifamily", mechanical.dat2$BuildingType),]

#############################################################################################
#Item 243: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY FUEL / Table 35
#############################################################################################
item243.dat <- mechanical.dat.MF[which(!is.na(mechanical.dat.MF$Heat.Iteration)),]

#remove datapoint not asked for and repeated header lines
item243.dat1 <- item243.dat[grep("BLDG",item243.dat$CK_Building_ID),]
unique(item243.dat1$Primary.Heating.System)
item243.dat1$Primary.Heating.System[which(item243.dat1$Primary.Heating.System %in% c("-- Datapoint not asked for --", "N/A"))] <- "Yes"
item243.dat2 <- item243.dat1[which(item243.dat1$Primary.Heating.System %in% c("Yes", "No")),]

item243.dat2$Heating.System.Ind <- item243.dat2$Primary.Heating.System
item243.dat2$Heating.System.Ind[which(item243.dat2$Primary.Heating.System == "Yes")] <- "Primary Heating System"
item243.dat2$Heating.System.Ind[which(item243.dat2$Primary.Heating.System == "No")] <- "Secondary Heating System"

unique(item243.dat2$Heating.Fuel)
item243.dat2$Fuel <- item243.dat2$Heating.Fuel
item243.dat2$Fuel[grep("wood",item243.dat2$Fuel, ignore.case = T)] <- "Wood"

unique(item243.dat2$System.Type[which(is.na(item243.dat2$Fuel)|
                          item243.dat2$Fuel == "Other" |
                          item243.dat2$Fuel == "Unknown")])

item243.dat2$Fuel[grep("hp", item243.dat2$System.Type, ignore.case = T)] <- "Electric"
item243.dat2$Fuel[grep("natural gas|gas", item243.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item243.dat2$Fuel[grep("resistance", item243.dat2$Fuel, ignore.case = T)] <- "Electric"
item243.dat2$Fuel[is.na(item243.dat2$Fuel)] <- "Unknown"
item243.dat2$Fuel[grep("unknown", item243.dat2$Fuel, ignore.case = T)] <- "Electric"
unique(item243.dat2$Fuel)

item243.dat3 <- unique(data.frame("CK_Cadmus_ID"      = item243.dat2$CK_Cadmus_ID
                                 ,"Heating_System"    = item243.dat2$System.Type
                                 ,"Fuel"              = item243.dat2$Fuel
                                 ,"Primary_Secondary" = item243.dat2$Heating.System.Ind, stringsAsFactors = F))

item243.dat4 <- item243.dat3[which(item243.dat3$Primary_Secondary == "Primary Heating System"),]
length(unique(item243.dat4$CK_Cadmus_ID))

item243.dat5 <- item243.dat4[which(item243.dat4$Heating_System != "Packaged HP"),]


item243.merge <- left_join(rbsa.dat, item243.dat5)
item243.merge <- item243.merge[which(!is.na(item243.merge$Primary_Secondary)),]


################################################
# Adding pop and sample sizes for weights
################################################
item243.data <- weightedData(item243.merge[-which(colnames(item243.merge) %in% c("Heating_System"
                                                                                 ,"Fuel"
                                                                                 ,"Primary_Secondary"))])
item243.data <- left_join(item243.data, item243.merge[which(colnames(item243.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating_System"
                                                                                           ,"Fuel"
                                                                                           ,"Primary_Secondary"))])

item243.data$count <- 1

#######################
# Weighted Analysis
#######################
item243.final <- proportionRowsAndColumns1(CustomerLevelData = item243.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Heating_System'
                                           ,rowVariable      = 'Fuel'
                                           ,aggregateColumnName = "All Systems")
item243.final <- item243.final[which(item243.final$Fuel != "Total"),]


item243.all.types <- proportions_one_group_MF(CustomerLevelData = item243.data
                                                 ,valueVariable = 'count'
                                                 ,groupingVariable = 'Heating_System'
                                                 ,total.name = "All Types"
                                                 ,columnName = "Fuel"
                                                 ,weighted = TRUE
                                                 ,two.prop.total = TRUE)
item243.all.types <- item243.all.types[which(item243.all.types$Heating_System != "Total"),]

item243.final <- rbind.data.frame(item243.final, item243.all.types, stringsAsFactors = F)

item243.cast <- dcast(setDT(item243.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("w.percent","w.SE", "count","n","N"))


item243.table <- data.frame("Primary.Heating.System" = item243.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Air.Source.HP.n"       = 0
                            ,"Electric"              = item243.cast$w.percent_Electric
                            ,"Electric.SE"           = item243.cast$w.SE_Electric
                            ,"Electric.n"            = item243.cast$count_Electric
                            ,"Natural.Gas"           = item243.cast$`w.percent_Natural Gas`
                            ,"Natural.Gas.SE"        = item243.cast$`w.SE_Natural Gas`
                            ,"Natural.Gas.n"         = item243.cast$`count_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Oil.n"                 = 0
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"Purchased.Steam.n"     = 0
                            ,"All.Types"            = item243.cast$`w.percent_All Types`
                            ,"All.Types.SE"         = item243.cast$`w.SE_All Types`
                            ,"All.Types.n"          = item243.cast$`count_All Types`
                            )

exportTable(item243.table, "MF", "Table 35", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item243.final <- proportions_two_groups_unweighted(CustomerLevelData = item243.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Heating_System'
                                           ,rowVariable      = 'Fuel'
                                           ,aggregateColumnName = "All Systems")
item243.final <- item243.final[which(item243.final$Fuel != "Total"),]


item243.all.types <- proportions_one_group_MF(CustomerLevelData = item243.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Heating_System'
                                              ,total.name = "All Types"
                                              ,columnName = "Fuel"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item243.all.types <- item243.all.types[which(item243.all.types$Heating_System != "Total"),]

item243.final <- rbind.data.frame(item243.final, item243.all.types, stringsAsFactors = F)

item243.cast <- dcast(setDT(item243.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent","SE", "Count","SampleSize"))


item243.table <- data.frame("Primary.Heating.System" = item243.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Air.Source.HP.n"       = 0
                            ,"Electric"              = item243.cast$Percent_Electric
                            ,"Electric.SE"           = item243.cast$SE_Electric
                            ,"Electric.n"            = item243.cast$Count_Electric
                            ,"Natural.Gas"           = item243.cast$`Percent_Natural Gas`
                            ,"Natural.Gas.SE"        = item243.cast$`SE_Natural Gas`
                            ,"Natural.Gas.n"         = item243.cast$`Count_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Oil.n"                 = 0
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"Purchased.Steam.n"     = 0
                            ,"All.Types"            = item243.cast$`Percent_All Types`
                            ,"All.Types.SE"         = item243.cast$`SE_All Types`
                            ,"All.Types.n"          = item243.cast$`Count_All Types`
)

exportTable(item243.table, "MF", "Table 35", weighted = FALSE)








#############################################################################################
#Item 244: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 36
#############################################################################################
item244.dat <- item243.dat2
item244.dat1 <- unique(data.frame("CK_Cadmus_ID"       = item244.dat$CK_Cadmus_ID
                                  ,"Heating_System"    = item244.dat$System.Type
                                  ,"HomeType"          = item244.dat$BuildingTypeXX
                                  ,"Primary_Secondary" = item244.dat$Heating.System.Ind, stringsAsFactors = F))

item244.dat2 <- item244.dat1[which(item244.dat1$Primary_Secondary == "Primary Heating System"),]
length(unique(item244.dat2$CK_Cadmus_ID))


item244.dat3 <- item244.dat2[which(item244.dat2$Heating_System != "Packaged HP"),]


item244.merge <- left_join(rbsa.dat, item244.dat3)
item244.merge <- item244.merge[which(!is.na(item244.merge$Primary_Secondary)),]

################################################
# Adding pop and sample sizes for weights
################################################
item244.data <- weightedData(item244.merge[-which(colnames(item244.merge) %in% c("Heating_System"
                                                                                 ,"HomeType"
                                                                                 ,"Primary_Secondary"))])
item244.data <- left_join(item244.data, item244.merge[which(colnames(item244.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating_System"
                                                                                           ,"Primary_Secondary"))])

item244.data$count <- 1

#######################
# Weighted Analysis
#######################
item244.final <- proportionRowsAndColumns1(CustomerLevelData = item244.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Heating_System'
                                           ,aggregateColumnName = "All Sizes")
item244.final <- item244.final[which(item244.final$Heating_System != "Total"),]

item244.cast <- dcast(setDT(item244.final)
                      ,formula = Heating_System ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))


item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"Low.Rise.1.3"      = item244.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item244.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item244.cast$`count_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item244.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item244.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item244.cast$`count_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item244.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item244.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item244.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"      = item244.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"       = item244.cast$`count_All Sizes`) 
exportTable(item244.table, "MF", "Table 36", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item244.final <- proportions_two_groups_unweighted(CustomerLevelData = item244.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Heating_System'
                                           ,aggregateColumnName = "All Sizes")
item244.final <- item244.final[which(item244.final$Heating_System != "Total"),]

item244.cast <- dcast(setDT(item244.final)
                      ,formula = Heating_System ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "SampleSize"))

item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"Low.Rise.1.3"      = item244.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item244.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item244.cast$`Count_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item244.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item244.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item244.cast$`Count_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item244.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item244.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item244.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"      = item244.cast$`SE_All Sizes`
                            ,"All.Sizes.n"       = item244.cast$`Count_All Sizes`) 

exportTable(item244.table, "MF", "Table 36", weighted = FALSE)





#############################################################################################
#Item 245: DISTRIBUTION OF SECONDARY HEATING SYSTEMS BY FUEL/ Table 37
#############################################################################################
item245.dat3 <- item243.dat3
item245.dat4 <- item245.dat3[which(item245.dat3$Primary_Secondary == "Secondary Heating System"),]

item245.merge <- left_join(rbsa.dat, item245.dat4)
item245.merge <- item245.merge[which(!is.na(item245.merge$Primary_Secondary)),]


################################################
# Adding pop and sample sizes for weights
################################################
item245.data <- weightedData(item245.merge[-which(colnames(item245.merge) %in% c("Heating_System"
                                                                                 ,"Fuel"
                                                                                 ,"Primary_Secondary"))])
item245.data <- left_join(item245.data, item245.merge[which(colnames(item245.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating_System"
                                                                                           ,"Fuel"
                                                                                           ,"Primary_Secondary"))])

item245.data$count <- 1

#######################
# Weighted Analysis
#######################
item245.final <- proportionRowsAndColumns1(CustomerLevelData = item245.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'Heating_System'
                                           ,rowVariable      = 'Fuel'
                                           ,aggregateColumnName = "All Systems")
item245.final <- item245.final[which(item245.final$Fuel != "Total"),]


item245.all.types <- proportions_one_group_MF(CustomerLevelData = item245.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Heating_System'
                                              ,total.name = "All Types"
                                              ,columnName = "Fuel"
                                              ,weighted = TRUE
                                              ,two.prop.total = TRUE)
item245.all.types <- item245.all.types[which(item245.all.types$Heating_System != "Total"),]

item245.final <- rbind.data.frame(item245.final, item245.all.types, stringsAsFactors = F)

item245.cast <- dcast(setDT(item245.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("w.percent","w.SE", "count","n","N"))


item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Air.Source.HP.n"       = 0
                            ,"Electric"              = item245.cast$w.percent_Electric
                            ,"Electric.SE"           = item245.cast$w.SE_Electric
                            ,"Electric.n"            = item245.cast$count_Electric
                            ,"Natural.Gas"           = NA#item245.cast$`w.percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item245.cast$`w.SE_Natural Gas`
                            ,"Natural.Gas.n"         = NA#item245.cast$`count_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Oil.n"                 = 0
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"Purchased.Steam.n"     = 0
                            ,"All.Types"            = item245.cast$`w.percent_All Types`
                            ,"All.Types.SE"         = item245.cast$`w.SE_All Types`
                            ,"All.Types.n"          = item245.cast$`count_All Types`
)

exportTable(item245.table, "MF", "Table 37", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item245.final <- proportions_two_groups_unweighted(CustomerLevelData = item245.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'Heating_System'
                                                   ,rowVariable      = 'Fuel'
                                                   ,aggregateColumnName = "All Systems")
item245.final <- item245.final[which(item245.final$Fuel != "Total"),]


item245.all.types <- proportions_one_group_MF(CustomerLevelData = item245.data
                                              ,valueVariable = 'count'
                                              ,groupingVariable = 'Heating_System'
                                              ,total.name = "All Types"
                                              ,columnName = "Fuel"
                                              ,weighted = FALSE
                                              ,two.prop.total = TRUE)
item245.all.types <- item245.all.types[which(item245.all.types$Heating_System != "Total"),]

item245.final <- rbind.data.frame(item245.final, item245.all.types, stringsAsFactors = F)

item245.cast <- dcast(setDT(item245.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent","SE", "Count","SampleSize"))


item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Air.Source.HP.n"       = 0
                            ,"Electric"              = item245.cast$Percent_Electric
                            ,"Electric.SE"           = item245.cast$SE_Electric
                            ,"Electric.n"            = item245.cast$Count_Electric
                            ,"Natural.Gas"           = NA#item245.cast$`Percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item245.cast$`SE_Natural Gas`
                            ,"Natural.Gas.n"         = NA#item245.cast$`Count_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Oil.n"                 = 0
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"Purchased.Steam.n"     = 0
                            ,"All.Types"            = item245.cast$`Percent_All Types`
                            ,"All.Types.SE"         = item245.cast$`SE_All Types`
                            ,"All.Types.n"          = item245.cast$`Count_All Types`
)

exportTable(item245.table, "MF", "Table 37", weighted = FALSE)





#############################################################################################
#item246: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 38
#############################################################################################
item246.dat <- item243.dat2
item246.dat1 <- unique(data.frame("CK_Cadmus_ID"       = item246.dat$CK_Cadmus_ID
                                  ,"Heating_System"    = item246.dat$System.Type
                                  ,"HomeType"          = item246.dat$BuildingTypeXX
                                  ,"Primary_Secondary" = item246.dat$Heating.System.Ind, stringsAsFactors = F))

item246.dat2 <- item246.dat1[which(item246.dat1$Primary_Secondary == "Secondary Heating System"),]
length(unique(item246.dat2$CK_Cadmus_ID))


item246.dat3 <- item246.dat2[which(item246.dat2$Heating_System != "Packaged HP"),]


item246.merge <- left_join(rbsa.dat, item246.dat3)
item246.merge <- item246.merge[which(!is.na(item246.merge$Primary_Secondary)),]

################################################
# Adding pop and sample sizes for weights
################################################
item246.data <- weightedData(item246.merge[-which(colnames(item246.merge) %in% c("Heating_System"
                                                                                 ,"HomeType"
                                                                                 ,"Primary_Secondary"))])
item246.data <- left_join(item246.data, item246.merge[which(colnames(item246.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating_System"
                                                                                           ,"Primary_Secondary"))])

item246.data$count <- 1

#######################
# Weighted Analysis
#######################
item246.final <- proportionRowsAndColumns1(CustomerLevelData = item246.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Heating_System'
                                           ,aggregateColumnName = "All Sizes")
item246.final <- item246.final[which(item246.final$Heating_System != "Total"),]

item246.cast <- dcast(setDT(item246.final)
                      ,formula = Heating_System ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N"))


item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_System
                            ,"Low.Rise.1.3"      = item246.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item246.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item246.cast$`count_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = NA#item246.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = NA#item246.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = NA#item246.cast$`count_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item246.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item246.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item246.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"      = item246.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"       = item246.cast$`count_All Sizes`) 
exportTable(item246.table, "MF", "Table 38", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item246.final <- proportions_two_groups_unweighted(CustomerLevelData = item246.data
                                                   ,valueVariable    = 'count'
                                                   ,columnVariable   = 'HomeType'
                                                   ,rowVariable      = 'Heating_System'
                                                   ,aggregateColumnName = "All Sizes")
item246.final <- item246.final[which(item246.final$Heating_System != "Total"),]

item246.cast <- dcast(setDT(item246.final)
                      ,formula = Heating_System ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "SampleSize"))

item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_System
                            ,"Low.Rise.1.3"      = item246.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item246.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item246.cast$`Count_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = NA#item246.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = NA#item246.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = NA#item246.cast$`Count_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item246.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item246.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item246.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"      = item246.cast$`SE_All Sizes`
                            ,"All.Sizes.n"       = item246.cast$`Count_All Sizes`) 

exportTable(item246.table, "MF", "Table 38", weighted = FALSE)







#############################################################################################
#Item 248: DISTRIBUTION OF UNIT COOLING SYSTEMS / Table 40
#############################################################################################
item248.dat <- unique(mechanical.dat.MF[which(colnames(mechanical.dat.MF) %in% c("CK_Cadmus_ID",
                                                                                 "Cool.Iteration",
                                                                                 "CK_Building_ID",
                                                                                 "System.Type"))])

item248.dat$CoolingInd <- 0
item248.dat$CoolingInd[which(!is.na(item248.dat$Cool.Iteration))] <- 1

item248.dat1 <- data.frame(
  summarise(group_by(item248.dat,CK_Cadmus_ID,System.Type),
            CoolingInd = sum(CoolingInd)),stringsAsFactors = F )
item248.dat2 <- data.frame(
  summarise(group_by(item248.dat1,CK_Cadmus_ID),
            CoolingSum = sum(CoolingInd)),stringsAsFactors = F )

item248.dat1$SystemType[which(item248.dat1$CoolingInd == 0)] <- "No Cooling"
item248.dat1$SystemType[which(item248.dat1$CoolingInd > 0)] <- 
  item248.dat1$System.Type[which(item248.dat1$CoolingInd > 0)]

item248.dat3 <- unique(item248.dat1[,c("CK_Cadmus_ID","SystemType")])

item248.dat3 <- left_join(item248.dat3,item248.dat2, by = "CK_Cadmus_ID")

item248.dat4 <- item248.dat3[-which(item248.dat3$CoolingSum > 0 &
                                      item248.dat3$SystemType == "No Cooling"),]

item248.merge <- left_join(rbsa.dat, item248.dat4)
item248.merge <- item248.merge[which(!is.na(item248.merge$CoolingSum)),]


################################################
# Adding pop and sample sizes for weights
################################################
item248.data <- weightedData(item248.merge[-which(colnames(item248.merge) %in% c("SystemType"
                                                                                 ,"CoolingSum"))])
item248.data <- left_join(item248.data, item248.merge[which(colnames(item248.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"SystemType"
                                                                                           ,"CoolingSum"))])

item248.data$count <- 1

#######################
# Weighted Analysis
#######################
item248.final <- proportions_one_group_MF(CustomerLevelData = item248.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'SystemType'
                                          ,total.name = "All Systems"
                                          )
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("N", "n", "BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item248.final <- proportions_one_group_MF(CustomerLevelData = item248.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'SystemType'
                                          ,total.name = "All Systems"
                                          ,weighted = FALSE
)
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("SampleSize", "BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = FALSE)
