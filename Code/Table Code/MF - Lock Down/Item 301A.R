#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list = ls())
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
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]
rbsa.dat.MF <- rbsa.dat.MF[grep("SITE",rbsa.dat.MF$CK_Building_ID,ignore.case = T),]

#Read in data for analysis
# appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
#clean cadmus IDs
appliances.dat$CK_Cadmus_ID <- trimws(toupper(appliances.dat$CK_Cadmus_ID))

###################################################################################################################
# ITEM 301B: IN-UNIT POWER STRIP CHARACTERISTICS (MF Table 94)
###################################################################################################################
#For everything else
colnames(appliances.dat)[grep("power",colnames(appliances.dat), ignore.case = T)]
item301B.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Power.Strip.Use"))]

item301B.dat0 <- item301B.dat[grep("power",item301B.dat$Type, ignore.case = T),]

item301B.merge <- left_join(rbsa.dat, item301B.dat0, by = "CK_Cadmus_ID")

item301B.merge <- item301B.merge[which((item301B.merge$Power.Strip.Use %notin% c("Unknown",NA))),]
item301B.merge$Power.Strip.Use <- trimws(item301B.merge$Power.Strip.Use)
unique(item301B.merge$Power.Strip.Use)

item301B.merge$Power.Strip.Use[grep("aquarium|charger", item301B.merge$Power.Strip.Use, ignore.case = T)] <- "Other"

item301B.merge <- left_join(rbsa.dat, item301B.merge)
item301B.merge <- item301B.merge[which(!is.na(item301B.merge$Power.Strip.Use)),]
length(unique(item301B.merge$CK_Cadmus_ID[which(item301B.merge$BuildingType == "Multifamily")]))
################################################
# Adding pop and sample sizes for weights
################################################
item301B.data <- weightedData(item301B.merge[-which(colnames(item301B.merge) %in% c("Type"
                                                                                 ,"Power.Strip.Use"))])
item301B.data <- left_join(item301B.data, unique(item301B.merge[which(colnames(item301B.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Type"
                                                                                           ,"Power.Strip.Use"))]))
item301B.data$Count <- 1
#######################
# Weighted Analysis
#######################
item301B.summary <- proportionRowsAndColumns1(CustomerLevelData = item301B.data
                                             ,valueVariable = "Count"
                                             ,columnVariable = "HomeType"
                                             ,rowVariable = "Power.Strip.Use"
                                             ,aggregateColumnName = "All Sizes")
item301B.summary <- item301B.summary[which(item301B.summary$HomeType != "All Sizes"),]

item301B.all.sizes <- proportions_one_group(CustomerLevelData = item301B.data
                                             ,valueVariable = "Count"
                                             ,groupingVariable = "Power.Strip.Use"
                                             ,total.name = "All Sizes"
                                             ,columnName = "HomeType"
                                             ,weighted = TRUE
                                             ,two.prop.total = TRUE)
item301B.final <- rbind.data.frame(item301B.summary, item301B.all.sizes, stringsAsFactors = F)

item301B.cast <- dcast(setDT(item301B.final)
                      ,formula = BuildingType + Power.Strip.Use ~ HomeType
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))

item301B.table <- data.frame("BuildingType"    = item301B.cast$BuildingType
                            ,"Power.Strip.Use" = item301B.cast$Power.Strip.Use
                            ,"Low.Rise"        = item301B.cast$`w.percent_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.SE"     = item301B.cast$`w.SE_Apartment Building (3 or fewer floors)`
                            ,"Low.Rise.n"      = item301B.cast$`n_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise"        = item301B.cast$`w.percent_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.SE"     = item301B.cast$`w.SE_Apartment Building (4 to 6 floors)`
                            ,"Mid.Rise.n"      = item301B.cast$`n_Apartment Building (4 to 6 floors)`
                            ,"High.Rise"       = item301B.cast$`w.percent_Apartment Building (More than 6 floors)`
                            ,"High.Rise.SE"    = item301B.cast$`w.SE_Apartment Building (More than 6 floors)`
                            ,"High.Rise.n"     = item301B.cast$`n_Apartment Building (More than 6 floors)`
                            ,"All.Sizes"       = item301B.cast$`w.percent_All Sizes`
                            ,"All.Sizes.SE"    = item301B.cast$`w.SE_All Sizes`
                            ,"All.Sizes.n"     = item301B.cast$`n_All Sizes`
                            ,"Low.Rise.EB"     = item301B.cast$`EB_Apartment Building (3 or fewer floors)`
                            ,"Mid.Rise.EB"     = item301B.cast$`EB_Apartment Building (4 to 6 floors)`
                            ,"High.Rise.EB"    = item301B.cast$`EB_Apartment Building (More than 6 floors)`
                            ,"All.Sizes.EB"    = item301B.cast$`EB_All Sizes`)

item301B.table.MF <- item301B.table[which(item301B.table$BuildingType == "Multifamily")
                                  ,which(colnames(item301B.table) %notin% c("BuildingType"))]

exportTable(item301B.table.MF, "MF", "Table II", weighted = TRUE)

#######################
# Weighted Analysis
#######################
item301B.summary <- proportions_two_groups_unweighted(CustomerLevelData = item301B.data
                                              ,valueVariable = "Count"
                                              ,columnVariable = "HomeType"
                                              ,rowVariable = "Power.Strip.Use"
                                              ,aggregateColumnName = "All Sizes")
item301B.summary <- item301B.summary[which(item301B.summary$HomeType != "All Sizes"),]

item301B.all.sizes <- proportions_one_group(CustomerLevelData = item301B.data
                                            ,valueVariable = "Count"
                                            ,groupingVariable = "Power.Strip.Use"
                                            ,total.name = "All Sizes"
                                            ,columnName = "HomeType"
                                            ,weighted = FALSE
                                            ,two.prop.total = TRUE)
item301B.final <- rbind.data.frame(item301B.summary, item301B.all.sizes, stringsAsFactors = F)

item301B.cast <- dcast(setDT(item301B.final)
                       ,formula = BuildingType + Power.Strip.Use ~ HomeType
                       ,value.var = c("Percent","SE","n"))

item301B.table <- data.frame("BuildingType"    = item301B.cast$BuildingType
                             ,"Power.Strip.Use" = item301B.cast$Power.Strip.Use
                             ,"Low.Rise"        = item301B.cast$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.SE"     = item301B.cast$`SE_Apartment Building (3 or fewer floors)`
                             ,"Low.Rise.n"      = item301B.cast$`n_Apartment Building (3 or fewer floors)`
                             ,"Mid.Rise"        = item301B.cast$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.SE"     = item301B.cast$`SE_Apartment Building (4 to 6 floors)`
                             ,"Mid.Rise.n"      = item301B.cast$`n_Apartment Building (4 to 6 floors)`
                             ,"High.Rise"       = item301B.cast$`Percent_Apartment Building (More than 6 floors)`
                             ,"High.Rise.SE"    = item301B.cast$`SE_Apartment Building (More than 6 floors)`
                             ,"High.Rise.n"     = item301B.cast$`n_Apartment Building (More than 6 floors)`
                             ,"All.Sizes"       = item301B.cast$`Percent_All Sizes`
                             ,"All.Sizes.SE"    = item301B.cast$`SE_All Sizes`
                             ,"All.Sizes.n"     = item301B.cast$`n_All Sizes`)

item301B.table.MF <- item301B.table[which(item301B.table$BuildingType == "Multifamily")
                                    ,which(colnames(item301B.table) %notin% c("BuildingType"))]

exportTable(item301B.table.MF, "MF", "Table II", weighted = FALSE)



#############################################################################################
# Table HH: Percent of homes with smart powerstrips by State
#############################################################################################
#For everything else
item301A.dat <- appliances.dat[which(colnames(appliances.dat) %in% c("CK_Cadmus_ID","Type","Smart.Power.Strip"))]

item301A.dat0 <- item301A.dat[grep("power",item301A.dat$Type, ignore.case = T),]
item301A.dat1 <- item301A.dat0[which(item301A.dat0$Smart.Power.Strip %in% c("Yes","No")),]

item301A.merge <- left_join(rbsa.dat, item301A.dat1, by = "CK_Cadmus_ID")

item301A.merge <- item301A.merge[which(!is.na(item301A.merge$Smart.Power.Strip)),]
item301A.merge$Ind <- 0
item301A.merge$Ind[which(item301A.merge$Smart.Power.Strip == "Yes")] <- 1

item301A.sum <- summarise(group_by(item301A.merge, CK_Cadmus_ID)
                         ,Ind = sum(Ind))

item301A.sum$Ind[which(item301A.sum$Ind > 0)] <- 1

item301A.merge <- left_join(rbsa.dat, item301A.sum, by = "CK_Cadmus_ID")
item301A.merge$Ind[which(is.na(item301A.merge$Ind))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
item301A.data <- weightedData(item301A.merge[-which(colnames(item301A.merge) %in% c("Ind"))])
item301A.data <- left_join(item301A.data, item301A.merge[which(colnames(item301A.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
item301A.data$Count <- 1
#######################
# Weighted Analysis
#######################
item301A.table <- proportions_one_group(CustomerLevelData = item301A.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = TRUE)

item301A.table.MF <- item301A.table[which(item301A.table$BuildingType == "Multifamily")
                                  ,which(colnames(item301A.table) %notin% c("BuildingType"))]

exportTable(item301A.table.MF, "MF", "Table HH", weighted = TRUE)

#######################
# Weighted Analysis
#######################
item301A.table <- proportions_one_group(CustomerLevelData = item301A.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)

item301A.table.MF <- item301A.table[which(item301A.table$BuildingType == "Multifamily")
                                    ,which(colnames(item301A.table) %notin% c("BuildingType"))]

exportTable(item301A.table.MF, "MF", "Table HH", weighted = FALSE)