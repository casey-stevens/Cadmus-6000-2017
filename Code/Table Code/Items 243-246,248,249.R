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
# item243.dat2$Fuel[grep("wood",item243.dat2$Fuel, ignore.case = T)] <- "Wood"

unique(item243.dat2$System.Type[which(item243.dat2$Fuel %in% c("Other","Unknown","N/A",NA))])

item243.dat2$Fuel[grep("hp", item243.dat2$System.Type, ignore.case = T)] <- "Electric"
item243.dat2$Fuel[grep("natural gas|gas", item243.dat2$Fuel, ignore.case = T)] <- "Natural Gas"
item243.dat2$Fuel[grep("resistance", item243.dat2$Fuel, ignore.case = T)] <- "Electric"
item243.dat2$Fuel[grep("N/A", item243.dat2$Fuel, ignore.case = T)] <- "Unknown"
# item243.dat2$Fuel[is.na(item243.dat2$Fuel)] <- "Unknown"
# item243.dat2$Fuel[grep("unknown", item243.dat2$Fuel, ignore.case = T)] <- "Electric"

item243.dat2 <- item243.dat2[which(item243.dat2$Fuel != "Unknown"),]
unique(item243.dat2$Fuel)

item243.dat3 <- unique(data.frame("CK_Cadmus_ID"      = item243.dat2$CK_Cadmus_ID
                                 ,"Heating_System"    = item243.dat2$System.Type
                                 ,"Fuel"              = item243.dat2$Fuel
                                 ,"Primary_Secondary" = item243.dat2$Heating.System.Ind, stringsAsFactors = F))

item243.dat4 <- item243.dat3[which(item243.dat3$Primary_Secondary == "Primary Heating System"),]
length(unique(item243.dat4$CK_Cadmus_ID))

item243.dat5 <- item243.dat4#[which(item243.dat4$Heating_System != "Packaged HP"),]


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
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))

names(item243.cast)
item243.table <- data.frame("Primary.Heating.System" = item243.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Electric"              = item243.cast$w.percent_Electric
                            ,"Electric.SE"           = item243.cast$w.SE_Electric
                            ,"Natural.Gas"           = NA#item243.cast$`w.percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item243.cast$`w.SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"All.Types"             = item243.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item243.cast$`w.SE_All Types`
                            ,"n"                     = item243.cast$`n_All Types`
                            ,"Electric.EB"           = item243.cast$EB_Electric
                            ,"Natural.Gas.EB"        = NA#item243.cast$`EB_Natural Gas`
                            ,"Oil.EB"                = NA
                            ,"Purchased.Steam.EB"    = NA
                            ,"All.Types.EB"          = item243.cast$`EB_All Types`
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
                      ,value.var = c("Percent","SE", "Count","n"))


item243.table <- data.frame("Primary.Heating.System" = item243.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Electric"              = item243.cast$Percent_Electric
                            ,"Electric.SE"           = item243.cast$SE_Electric
                            ,"Natural.Gas"           = NA#item243.cast$`Percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item243.cast$`SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"All.Types"             = item243.cast$`Percent_All Types`
                            ,"All.Types.SE"          = item243.cast$`SE_All Types`
                            ,"n"                     = item243.cast$`n_All Types`
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
item244.merge <- item244.merge[which(item244.merge$Primary_Secondary %notin% c("N/A",NA)),]

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
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

names(item244.cast)
item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"Low.Rise.1.3"      = item244.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item244.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item244.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item244.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item244.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item244.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item244.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item244.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item244.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"      = item244.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"       = item244.cast$`n_All Sizes`
                            ,"Low.Rise.1.3.EB"   = item244.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6.EB"   = item244.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7.EB"  = NA#item244.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.EB"      = item244.cast$`EB_All Sizes`) 
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
                      ,value.var = c("Percent", "SE", "Count", "n"))

item244.table <- data.frame("Primary.Heating.System"= item244.cast$Heating_System
                            ,"Low.Rise.1.3"      = item244.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item244.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item244.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item244.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item244.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item244.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = NA#item244.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = NA#item244.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = NA
                            ,"All.Sizes"         = item244.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"      = item244.cast$`SE_All Sizes`
                            ,"All.Sizes.n"       = item244.cast$`n_All Sizes`) 

exportTable(item244.table, "MF", "Table 36", weighted = FALSE)





#############################################################################################
#Item 245: DISTRIBUTION OF SECONDARY HEATING SYSTEMS BY FUEL/ Table 37
#############################################################################################
item245.dat3 <- item243.dat3
item245.dat4 <- item245.dat3[which(item245.dat3$Primary_Secondary == "Secondary Heating System"),]

item245.merge <- left_join(rbsa.dat, item245.dat4)
item245.merge <- item245.merge[which(item245.merge$BuildingType == "Multifamily"),]
item245.merge <- item245.merge[grep("BLDG",item245.merge$CK_Building_ID),]
item245.merge$Heating_System[which(item245.merge$Primary_Secondary %in% c("N/A",NA))] <- "None"
item245.merge$Fuel[which(item245.merge$Primary_Secondary %in% c("N/A",NA))] <- "None"
unique(item245.merge$Heating_System)
unique(item245.merge$Fuel)

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
item245.final$Fuel[which(item245.final$Fuel == "Total")] <- "All Types"

item245.cast <- dcast(setDT(item245.final)
                      ,formula = BuildingType + Heating_System ~ Fuel
                      ,value.var = c("w.percent","w.SE", "count","n","N","EB"))


item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Electric"              = item245.cast$w.percent_Electric
                            ,"Electric.SE"           = item245.cast$w.SE_Electric
                            ,"Natural.Gas"           = NA#item245.cast$`w.percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item245.cast$`w.SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"None"                  = item245.cast$w.percent_None
                            ,"None.SE"               = item245.cast$w.SE_None
                            ,"All.Types"             = item245.cast$`w.percent_All Types`
                            ,"All.Types.SE"          = item245.cast$`w.SE_All Types`
                            ,"n"                     = item245.cast$`n_All Types`
                            ,"Air.Source.HP.EB"      = NA
                            ,"Electric.EB"           = item245.cast$EB_Electric
                            ,"Natural.Gas.EB"        = NA#item245.cast$`EB_Natural Gas`
                            ,"Oil.EB"                = NA
                            ,"Purchased.Steam.EB"    = NA
                            ,"None.EB"               = item245.cast$EB_None
                            ,"All.Types.EB"          = item245.cast$`EB_All Types`
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
item245.final$Fuel[which(item245.final$Fuel == "Total")] <- "All Types"

item245.cast <- dcast(setDT(item245.final)
                      ,formula = Heating_System ~ Fuel
                      ,value.var = c("Percent","SE", "Count","n"))


item245.table <- data.frame("Secondary.Heating.System" = item245.cast$Heating_System
                            ,"Air.Source.HP"         = NA
                            ,"Air.Source.HP.SE"      = NA
                            ,"Electric"              = item245.cast$Percent_Electric
                            ,"Electric.SE"           = item245.cast$SE_Electric
                            ,"Natural.Gas"           = NA#item245.cast$`Percent_Natural Gas`
                            ,"Natural.Gas.SE"        = NA#item245.cast$`SE_Natural Gas`
                            ,"Oil"                   = NA
                            ,"Oil.SE"                = NA
                            ,"Purchased.Steam"       = NA
                            ,"Purchased.Steam.SE"    = NA
                            ,"None"                  = item245.cast$Percent_None
                            ,"None.SE"               = item245.cast$SE_None
                            ,"All.Types"             = item245.cast$`Percent_All Types`
                            ,"All.Types.SE"          = item245.cast$`SE_All Types`
                            ,"n"                     = item245.cast$`n_All Types`
)

exportTable(item245.table, "MF", "Table 37", weighted = FALSE)





#############################################################################################
#item246: DISTRIBUTION OF PRIMARY HEATING SYSTEMS BY BUILDING/ Table 38
#############################################################################################
item246.dat <- item243.dat2
item246.dat1 <- unique(data.frame("CK_Cadmus_ID"       = item246.dat$CK_Cadmus_ID
                                  ,"Heating_Type"    = item246.dat$System.Type
                                  ,"HomeType"          = item246.dat$BuildingTypeXX
                                  ,"Primary_Secondary" = item246.dat$Heating.System.Ind, stringsAsFactors = F))

item246.dat2 <- item246.dat1[which(item246.dat1$Primary_Secondary == "Secondary Heating System"),]
length(unique(item246.dat2$CK_Cadmus_ID))


item246.dat3 <- item246.dat2[which(item246.dat2$Heating_Type != "Packaged HP"),]


item246.merge <- left_join(rbsa.dat, item246.dat3)
item246.merge <- item246.merge[which(item246.merge$BuildingType == "Multifamily"),]
item246.merge <- item246.merge[grep("BLDG",item246.merge$CK_Building_ID),]
item246.merge$Heating_Type[which(item246.merge$Primary_Secondary %in% c("N/A",NA))] <- "None"

################################################
# Adding pop and sample sizes for weights
################################################
item246.data <- weightedData(item246.merge[-which(colnames(item246.merge) %in% c("Heating_Type"
                                                                                 ,"HomeType"
                                                                                 ,"Primary_Secondary"))])
item246.data <- left_join(item246.data, item246.merge[which(colnames(item246.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Heating_Type"
                                                                                           ,"Primary_Secondary"))])

item246.data$count <- 1

#######################
# Weighted Analysis
#######################
item246.summary <- proportionRowsAndColumns1(CustomerLevelData = item246.data
                                           ,valueVariable    = 'count'
                                           ,columnVariable   = 'HomeType'
                                           ,rowVariable      = 'Heating_Type'
                                           ,aggregateColumnName = "All Sizes")
item246.summary <- item246.summary[which(item246.summary$HomeType != "All Sizes"),]

item246.all.sizes <- proportions_one_group(CustomerLevelData = item246.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Heating_Type"
                                           ,total.name = "All Sizes"
                                           ,columnName = "HomeType"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item246.final <- rbind.data.frame(item246.summary, item246.all.sizes, stringsAsFactors = F)

item246.cast <- dcast(setDT(item246.final)
                      ,formula = Heating_Type ~ HomeType
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))


item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_Type
                            ,"Low.Rise.1.3"      = item246.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item246.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item246.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item246.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item246.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item246.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = item246.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = item246.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = item246.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"         = item246.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"      = item246.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"       = item246.cast$`n_All Sizes`
                            ,"Low.Rise.1.3.EB"   = item246.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6.EB"   = item246.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7.EB"  = item246.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.EB"      = item246.cast$`EB_All Sizes`
                            ) 
exportTable(item246.table, "MF", "Table 38", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item246.summary <- proportions_two_groups_unweighted(CustomerLevelData = item246.data
                                             ,valueVariable    = 'count'
                                             ,columnVariable   = 'HomeType'
                                             ,rowVariable      = 'Heating_Type'
                                             ,aggregateColumnName = "All Sizes")
item246.summary <- item246.summary[which(item246.summary$HomeType != "All Sizes"),]

item246.all.sizes <- proportions_one_group(CustomerLevelData = item246.data
                                           ,valueVariable = "count"
                                           ,groupingVariable = "Heating_Type"
                                           ,total.name = "All Sizes"
                                           ,columnName = "HomeType"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)

item246.final <- rbind.data.frame(item246.summary, item246.all.sizes, stringsAsFactors = F)

item246.cast <- dcast(setDT(item246.final)
                      ,formula = Heating_Type ~ HomeType
                      ,value.var = c("Percent", "SE", "Count", "n"))

item246.table <- data.frame("Secondary.Heating.System"= item246.cast$Heating_Type
                            ,"Low.Rise.1.3"      = item246.cast$`Percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.SE"   = item246.cast$`SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.1.3.n"    = item246.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.4.6"      = item246.cast$`Percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.SE"   = item246.cast$`SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.4.6.n"    = item246.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.gt7"     = item246.cast$`Percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.SE"  = item246.cast$`SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.gt7.n"   = item246.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"         = item246.cast$`Percent_All Sizes`
                            ,"All.Sizes.SE"      = item246.cast$`SE_All Sizes`
                            ,"All.Sizes.n"       = item246.cast$`n_All Sizes`) 

exportTable(item246.table, "MF", "Table 38", weighted = FALSE)







#############################################################################################
#Item 248: DISTRIBUTION OF UNIT COOLING SYSTEMS / Table 40
#############################################################################################
item248.dat <- unique(mechanical.dat.MF[which(colnames(mechanical.dat.MF) %in% c("CK_Cadmus_ID",
                                                                                 "Cool.Iteration",
                                                                                 "CK_Building_ID",
                                                                                 "System.Type"
                                                                                 ,"Primary.Cooling.System"))])
item248.dat <- item248.dat[which(item248.dat$Primary.Cooling.System %in% c("Yes","No")),]

item248.dat$CoolingInd <- 0
item248.dat$CoolingInd[which(item248.dat$Primary.Cooling.System == "Yes")] <- 1

item248.dat1 <- data.frame(summarise(group_by(item248.dat,CK_Cadmus_ID,System.Type),
                                     CoolingInd = sum(unique(CoolingInd), na.rm = T)), stringsAsFactors = F)
# item248.dat2 <- data.frame(
#   summarise(group_by(item248.dat1,CK_Cadmus_ID),
#             CoolingSum = sum(CoolingInd)),stringsAsFactors = F )

item248.dat1 <- left_join(rbsa.dat, item248.dat1)

item248.dat1$CoolingInd[which(is.na(item248.dat1$CoolingInd))] <- 0
item248.dat1$System.Type[which(item248.dat1$CoolingInd == 0)] <- "No Cooling"
item248.dat1$System.Type[which(item248.dat1$CoolingInd > 0)] <- item248.dat1$System.Type[which(item248.dat1$CoolingInd > 0)]

item248.dat2 <- unique(item248.dat1[,c("CK_Cadmus_ID","System.Type")])

item248.dat3 <- left_join(item248.dat2,item248.dat1)
item248.dat3$CoolingSum <- as.numeric(as.character(item248.dat3$CoolingInd))
item248.dat4 <- item248.dat3[which(item248.dat3$System.Type %notin% c(NA,"No Cooling","-- Unassigned --","N/A")),]

item248.merge <- left_join(rbsa.dat, item248.dat4)
item248.merge <- item248.merge[which(item248.merge$BuildingType == "Multifamily"),]
item248.merge <- item248.merge[grep("SITE",item248.merge$CK_Building_ID),]
item248.merge$CoolingSum[which(item248.merge$CoolingSum %in% c("N/A",NA))] <- 1
item248.merge$System.Type[which(item248.merge$System.Type %in% c("N/A",NA))] <- "No Cooling"
unique(item248.merge$System.Type)

################################################
# Adding pop and sample sizes for weights
################################################
item248.data <- weightedData(item248.merge[-which(colnames(item248.merge) %in% c("System.Type"
                                                                                 ,"CoolingSum"
                                                                                 ,"CoolingInd"))])
item248.data <- left_join(item248.data, item248.merge[which(colnames(item248.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"System.Type"
                                                                                           ,"CoolingSum"
                                                                                           ,"CoolingInd"))])

item248.data$count <- 1

#######################
# Weighted Analysis
#######################
item248.final <- proportions_one_group(CustomerLevelData = item248.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'System.Type'
                                          ,total.name = "All Systems"
                                          )
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item248.final <- proportions_one_group(CustomerLevelData = item248.data
                                          ,valueVariable = 'count'
                                          ,groupingVariable = 'System.Type'
                                          ,total.name = "All Systems"
                                          ,weighted = FALSE
)
item248.final.MF <- item248.final[which(colnames(item248.final) %notin% c("BuildingType"))]

exportTable(item248.final.MF, "MF", "Table 40", weighted = FALSE)





#############################################################################################
#Item 249: DISTRIBUTION OF UNIT COOLING SYSTEMS / Table 40
#############################################################################################
item249.dat <- unique(mechanical.dat.MF[which(colnames(mechanical.dat.MF) %in% c("CK_Cadmus_ID",
                                                                                 "Cool.Iteration",
                                                                                 "CK_Building_ID",
                                                                                 "System.Type"
                                                                                 ,"Primary.Cooling.System"))])
item249.dat <- item249.dat[which(item249.dat$Primary.Cooling.System %in% c("Yes","No")),]

item249.dat$CoolingInd <- 0
item249.dat$CoolingInd[which(item249.dat$Primary.Cooling.System == "Yes")] <- 1

item249.dat1 <- data.frame(summarise(group_by(item249.dat,CK_Cadmus_ID,System.Type),
                                     CoolingInd = sum(unique(CoolingInd), na.rm = T)), stringsAsFactors = F)
# item249.dat2 <- data.frame(
#   summarise(group_by(item249.dat1,CK_Cadmus_ID),
#             CoolingSum = sum(CoolingInd)),stringsAsFactors = F )

item249.dat1 <- left_join(rbsa.dat, item249.dat1)

item249.dat1$CoolingInd[which(is.na(item249.dat1$CoolingInd))] <- 0
item249.dat1$System.Type[which(item249.dat1$CoolingInd == 0)] <- "No Cooling"
item249.dat1$System.Type[which(item249.dat1$CoolingInd > 0)] <- item249.dat1$System.Type[which(item249.dat1$CoolingInd > 0)]

item249.dat2 <- unique(item249.dat1[,c("CK_Cadmus_ID","System.Type")])

item249.dat3 <- left_join(item249.dat2,item249.dat1)
item249.dat3$CoolingSum <- as.numeric(as.character(item249.dat3$CoolingInd))
item249.dat4 <- item249.dat3[which(item249.dat3$System.Type %notin% c(NA,"No Cooling","-- Unassigned --","N/A")),]

item249.merge <- left_join(rbsa.dat, item249.dat4)
item249.merge <- item249.merge[which(item249.merge$BuildingType == "Multifamily"),]
item249.merge <- item249.merge[grep("BLDG",item249.merge$CK_Building_ID),]
item249.merge$CoolingSum[which(item249.merge$CoolingSum %in% c("N/A",NA))] <- 1
item249.merge$System.Type[which(item249.merge$System.Type %in% c("N/A",NA))] <- "No Cooling"
unique(item249.merge$System.Type)

################################################
# Adding pop and sample sizes for weights
################################################
item249.data <- weightedData(item249.merge[-which(colnames(item249.merge) %in% c("System.Type"
                                                                                 ,"CoolingSum"
                                                                                 ,"CoolingInd"))])
item249.data <- left_join(item249.data, item249.merge[which(colnames(item249.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"System.Type"
                                                                                           ,"CoolingSum"
                                                                                           ,"CoolingInd"))])

item249.data$count <- 1

#######################
# Weighted Analysis
#######################
item249.final <- proportions_one_group(CustomerLevelData = item249.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
)
item249.final.MF <- item249.final[which(colnames(item249.final) %notin% c("BuildingType"))]

exportTable(item249.final.MF, "MF", "Table 41", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item249.final <- proportions_one_group(CustomerLevelData = item249.data
                                       ,valueVariable = 'count'
                                       ,groupingVariable = 'System.Type'
                                       ,total.name = "All Systems"
                                       ,weighted = FALSE
)
item249.final.MF <- item249.final[which(colnames(item249.final) %notin% c("BuildingType"))]

exportTable(item249.final.MF, "MF", "Table 41", weighted = FALSE)
