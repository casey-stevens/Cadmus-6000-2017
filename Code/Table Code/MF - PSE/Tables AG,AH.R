#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())
rundate <-  format(Sys.time(), "%d%b%y")
options(scipen=999)


##  Create "Not In" operator
"%notin%" <- Negate("%in%")


# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")


# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.pse.data", rundate, ".xlsx", sep = "")))
rbsa.dat <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis -- Item 77 and table ZZ
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 3)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))


#Read in data for analysis -- Item 79
# rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
#clean cadmus IDs
rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))



#############################################################################################
#Table AG: Distribution of All stored lamps by lamp type and state
#############################################################################################
#subset to columns needed for analysis
tableAG.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                                ,"Fixture.Qty"
                                                                ,"LIGHTING_BulbsPerFixture"
                                                                ,"CK_SiteID"
                                                                ,"Lamp.Category"
                                                                ,"Clean.Room"))]
tableAG.dat$count <- 1

tableAG.dat0 <- left_join(rbsa.dat, tableAG.dat, by = "CK_Cadmus_ID")

tableAG.dat1 <- tableAG.dat0[which(!is.na(tableAG.dat0$Lamp.Category)),]

tableAG.dat2 <- tableAG.dat1[grep("SITE", tableAG.dat1$CK_SiteID),]

#clean fixture and bulbs per fixture
tableAG.dat2$Fixture.Qty <- as.numeric(as.character(tableAG.dat2$Fixture.Qty))
tableAG.dat2$LIGHTING_BulbsPerFixture <- as.numeric(as.character(tableAG.dat2$LIGHTING_BulbsPerFixture))

tableAG.dat2$Lamps <- tableAG.dat2$Fixture.Qty * tableAG.dat2$LIGHTING_BulbsPerFixture
unique(tableAG.dat2$Lamps)

tableAG.dat3 <- tableAG.dat2[which(!(is.na(tableAG.dat2$Lamps))),]

tableAG.led.sum <- summarise(group_by(tableAG.dat3, CK_Cadmus_ID, Lamp.Category)
                             ,TotalBulbs = sum(Lamps))

## subset to only storage bulbs
tableAG.storage <- tableAG.dat3[which(tableAG.dat3$Clean.Room == "Storage"),]
#summarise within site
tableAG.storage.sum <- summarise(group_by(tableAG.storage, CK_Cadmus_ID, Lamp.Category)
                                 ,StorageBulbs = sum(Lamps))
length(unique(tableAG.storage.sum$CK_Cadmus_ID))


tableAG.merge1 <- left_join(tableAG.led.sum, tableAG.storage.sum)

tableAG.cast1 <- dcast(setDT(tableAG.merge1)
                      ,formula = CK_Cadmus_ID ~ Lamp.Category
                      ,value.var = c("StorageBulbs"))
tableAG.cast1[is.na(tableAG.cast1),] <- 0

tableAG.melt1 <- melt(tableAG.cast1, id.vars = "CK_Cadmus_ID")
names(tableAG.melt1) <- c("CK_Cadmus_ID", "Lamp.Category", "StorageBulbs")

tableAG.cast2 <- dcast(setDT(tableAG.merge1)
                       ,formula = CK_Cadmus_ID ~ Lamp.Category
                       ,value.var = c("TotalBulbs"))
tableAG.cast2[is.na(tableAG.cast2),] <- 0

tableAG.melt2 <- melt(tableAG.cast2, id.vars = "CK_Cadmus_ID")
names(tableAG.melt2) <- c("CK_Cadmus_ID", "Lamp.Category", "TotalBulbs")

tableAG.merge2 <- left_join(tableAG.melt1, tableAG.melt2)
tableAG.merge  <- left_join(rbsa.dat, tableAG.merge2)
tableAG.merge$StorageBulbs[which(is.na(tableAG.merge$StorageBulbs))] <- 0

################################################
# Adding pop and sample sizes for weights
################################################
tableAG.data <- weightedData(tableAG.merge[-which(colnames(tableAG.merge) %in% c("StorageBulbs"
                                                                                 ,"TotalBulbs"
                                                                                 ,"Lamp.Category"
                                                                                 ,"Category"))])
tableAG.data <- left_join(tableAG.data, tableAG.merge[which(colnames(tableAG.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"StorageBulbs"
                                                                                           ,"TotalBulbs"
                                                                                           ,"Lamp.Category"
                                                                                           ,"Category"))])
tableAG.data$count <- 1
stopifnot(nrow(tableAG.data) == nrow(tableAG.merge))

#######################
# Weighted Analysis
#######################
tableAG.MF.data <- tableAG.data[which(tableAG.data$BuildingType == "Multifamily"),]
tableAG.MF.data <- tableAG.MF.data[grep("site",tableAG.MF.data$CK_Building_ID,ignore.case = T),]
tableAG.MF.data$Count <- 1

tableAG.MF.summary <- proportionRowsAndColumns1(CustomerLevelData = tableAG.MF.data
                                             ,valueVariable = "StorageBulbs"
                                             ,columnVariable = "Category"
                                             ,rowVariable = "Lamp.Category"
                                             ,aggregateColumnName = "All Sizes")
tableAG.MF.summary <- tableAG.MF.summary[which(tableAG.MF.summary$Category != "All Sizes"),]

tableAG.all.sizes <- proportions_one_group(CustomerLevelData = tableAG.MF.data
                                           ,valueVariable = "StorageBulbs"
                                           ,groupingVariable = "Lamp.Category"
                                           ,total.name = "All Sizes"
                                           ,columnName = "Category"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE
)

tableAG.MF.merge <- rbind.data.frame(tableAG.MF.summary, tableAG.all.sizes, stringsAsFactors = F)

tableAG.MF.cast <- dcast(setDT(tableAG.MF.merge)
                                    ,formula = BuildingType + Lamp.Category ~ Category
                                    ,value.var = c("w.percent","w.SE","count","n","N","EB"))
names(tableAG.MF.cast)
tableAG.MF.final <- data.frame("BuildingType"       = tableAG.MF.cast$BuildingType
                               ,"Lamp.Category"     = tableAG.MF.cast$Lamp.Category
                               ,"PSE.Percent"                 = tableAG.MF.cast$w.percent_PSE
                               ,"PSE.SE"                      = tableAG.MF.cast$w.SE_PSE
                               ,"PSE.n"                       = tableAG.MF.cast$n_PSE
                               ,"PSE.King.County.Percent"     = tableAG.MF.cast$`w.percent_PSE KING COUNTY`
                               ,"PSE.King.County.SE"          = tableAG.MF.cast$`w.SE_PSE KING COUNTY`
                               ,"PSE.King.County.n"           = tableAG.MF.cast$`n_PSE KING COUNTY`
                               ,"PSE.Non.King.County.Percent" = tableAG.MF.cast$`w.percent_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.SE"      = tableAG.MF.cast$`w.SE_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.n"       = tableAG.MF.cast$`n_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS.Percent"        = tableAG.MF.cast$`w.percent_2017 RBSA PS`
                               ,"2017.RBSA.PS.SE"             = tableAG.MF.cast$`w.SE_2017 RBSA PS`
                               ,"2017.RBSA.PS.n"              = tableAG.MF.cast$`n_2017 RBSA PS`
                               ,"PSE_EB"                      = tableAG.MF.cast$EB_PSE
                               ,"PSE.King.County_EB"          = tableAG.MF.cast$`EB_PSE KING COUNTY`
                               ,"PSE.Non.King.County_EB"      = tableAG.MF.cast$`EB_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS_EB"             = tableAG.MF.cast$`EB_2017 RBSA PS`
)

unique(tableAG.MF.final$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"Total")
tableAG.MF.final <- tableAG.MF.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAG.MF.final <- data.frame(tableAG.MF.final)

tableAG.MF.final.MF <- tableAG.MF.final[which(tableAG.MF.final$BuildingType == "Multifamily")
                                  ,-which(colnames(tableAG.MF.final) %in% c("BuildingType"))]
tableAG.MF.final.MF$PSE.n <- min(tableAG.MF.final.MF$PSE.n)
tableAG.MF.final.MF$PSE.King.County.n <- min(tableAG.MF.final.MF$PSE.King.County.n)
tableAG.MF.final.MF$PSE.Non.King.County.n <- min(tableAG.MF.final.MF$PSE.Non.King.County.n)
tableAG.MF.final.MF$X2017.RBSA.PS.n <- min(tableAG.MF.final.MF$X2017.RBSA.PS.n)

exportTable(tableAG.MF.final.MF, "MF", "Table AG", weighted = TRUE,OS = T, osIndicator = "PSE")


#######################
# Unweighted Analysis
#######################
tableAG.MF.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableAG.MF.data
                                                     ,valueVariable = "StorageBulbs"
                                                     ,columnVariable = "Category"
                                                     ,rowVariable = "Lamp.Category"
                                                     ,aggregateColumnName = "All Sizes")
# tableAG.MF.summary <- tableAG.MF.summary[which(tableAG.MF.summary$Lamp.Category != "Total"),]

tableAG.MF.cast <- data.frame(dcast(setDT(tableAG.MF.summary)
                                    ,formula = BuildingType + Lamp.Category ~ Category
                                    ,value.var = c("Percent","SE","Count","n")))

tableAG.MF.final <- data.frame("BuildingType"       = tableAG.MF.cast$BuildingType
                               ,"Lamp.Category"     = tableAG.MF.cast$Lamp.Category
                               ,"PSE.Percent"                 = tableAG.MF.cast$Percent_PSE
                               ,"PSE.SE"                      = tableAG.MF.cast$SE_PSE
                               ,"PSE.n"                       = tableAG.MF.cast$n_PSE
                               ,"PSE.King.County.Percent"     = tableAG.MF.cast$`Percent_PSE KING COUNTY`
                               ,"PSE.King.County.SE"          = tableAG.MF.cast$`SE_PSE KING COUNTY`
                               ,"PSE.King.County.n"           = tableAG.MF.cast$`n_PSE KING COUNTY`
                               ,"PSE.Non.King.County.Percent" = tableAG.MF.cast$`Percent_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.SE"      = tableAG.MF.cast$`SE_PSE NON-KING COUNTY`
                               ,"PSE.Non.King.County.n"       = tableAG.MF.cast$`n_PSE NON-KING COUNTY`
                               ,"2017.RBSA.PS.Percent"        = tableAG.MF.cast$`Percent_2017 RBSA PS`
                               ,"2017.RBSA.PS.SE"             = tableAG.MF.cast$`SE_2017 RBSA PS`
                               ,"2017.RBSA.PS.n"              = tableAG.MF.cast$`n_2017 RBSA PS`
)

unique(tableAG.MF.final$Lamp.Category)
rowOrder <- c("Compact Fluorescent"
              ,"Halogen"
              ,"Incandescent"
              ,"Incandescent / Halogen"
              ,"Light Emitting Diode"
              ,"Linear Fluorescent"
              ,"Other"
              ,"Unknown"
              ,"Total")
tableAG.MF.final <- tableAG.MF.final %>% mutate(Lamp.Category = factor(Lamp.Category, levels = rowOrder)) %>% arrange(Lamp.Category)  
tableAG.MF.final <- data.frame(tableAG.MF.final)

tableAG.MF.final.MF <- tableAG.MF.final[which(tableAG.MF.final$BuildingType == "Multifamily")
                                        ,-which(colnames(tableAG.MF.final) %in% c("BuildingType"))]

exportTable(tableAG.MF.final.MF, "MF", "Table AG", weighted = FALSE,OS = T, osIndicator = "PSE")
















#############################################################################################
#Table AH: AVERAGE household wattage per lamp BY STATE
#############################################################################################
#subset to columns needed for analysis
tableAH.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Clean.Wattage"
                                                               ,"Wattage.for.Calc"))]
tableAH.dat$count <- 1

tableAH.dat1 <- left_join(tableAH.dat, rbsa.dat, by = "CK_Cadmus_ID")

tableAH.dat2 <- tableAH.dat1[grep("SITE", tableAH.dat1$CK_SiteID),]

tableAH.dat3 <- tableAH.dat2[which(!(tableAH.dat2$Clean.Room %in% c("Storage"))),]
unique(tableAH.dat3$Wattage.for.Calc)
tableAH.dat4 <- tableAH.dat3[-grep("-|Unknown|unknown", tableAH.dat3$Wattage.for.Calc),]

tableAH.dat4$Total.Wattage <- as.numeric(as.character(tableAH.dat4$Fixture.Qty)) * 
  as.numeric(as.character(tableAH.dat4$LIGHTING_BulbsPerFixture)) * 
  as.numeric(as.character(tableAH.dat4$Wattage.for.Calc))

tableAH.dat5 <- tableAH.dat4[which(!(is.na(tableAH.dat4$Wattage.for.Calc))),]
tableAH.dat5$Wattage.for.Calc <- as.numeric(as.character(tableAH.dat5$Wattage.for.Calc))

tableAH.dat6 <- summarise(group_by(tableAH.dat5, CK_Cadmus_ID)
                         ,Wattage.per.bulb = mean(Wattage.for.Calc, na.rm = T))

tableAH.prep <- left_join(rbsa.dat, tableAH.dat6)
tableAH.prep1 <- tableAH.prep[which(!is.na(tableAH.prep$Wattage.per.bulb)),]
tableAH.prep1 <- tableAH.prep1[which(tableAH.prep1$Wattage.per.bulb != "Inf"),]

################################################
# Adding pop and sample sizes for weights
################################################
tableAH.data <- weightedData(tableAH.prep1[-which(colnames(tableAH.prep1) %in% c("Wattage.per.bulb"
                                                                                 ,"Category"))])
tableAH.data <- left_join(tableAH.data, tableAH.prep1[which(colnames(tableAH.prep1) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wattage.per.bulb"
                                                                                       ,"Category"))])
stopifnot(nrow(tableAH.data) == nrow(tableAH.data))
# #######################
# # Weighted Analysis
# #######################
tableAH.data$count <-1
# tableAH.final <- mean_one_group(CustomerLevelData = tableAH.data
#                                ,valueVariable    = 'Wattage.per.bulb'
#                                ,byVariable       = 'State'
#                                ,aggregateRow     = 'Region')
# 
# # Export table
# tableAH.final.SF <- tableAH.final[which(tableAH.final$BuildingType == "Single Family"),-1]
# tableAH.final.MH <- tableAH.final[which(tableAH.final$BuildingType == "Manufactured"),-1]
# 
# exportTable(tableAH.final.SF, "SF", "Table AH", weighted = TRUE)
# # exportTable(tableAH.final.MH, "MH", "Table AH", weighted = TRUE)

#######################
# MULTIFAMILY
#######################
tableAH.final.MF <- mean_one_group(CustomerLevelData = tableAH.data
                                ,valueVariable    = 'Wattage.per.bulb'
                                ,byVariable       = 'Category'
                                ,aggregateRow     = 'Remove')

# Export table
tableAH.final.MF <- tableAH.final.MF[which(tableAH.final.MF$BuildingType == "Multifamily"),-1]
exportTable(tableAH.final.MF, "MF", "Table AH", weighted = TRUE,OS = T, osIndicator = "PSE")





################################
# Unweighted Analysis
################################
# tableAH.final <- mean_one_group_unweighted(CustomerLevelData = tableAH.data
#                                           ,valueVariable    = 'Wattage.per.bulb'
#                                           ,byVariable       = 'State'
#                                           ,aggregateRow     = 'Region')
# 
# # Export table
# tableAH.final.SF <- tableAH.final[which(tableAH.final$BuildingType == "Single Family"),-1]
# tableAH.final.MH <- tableAH.final[which(tableAH.final$BuildingType == "Manufactured"),-1]
# 
# exportTable(tableAH.final.SF, "SF", "Table AH", weighted = FALSE)
# # exportTable(tableAH.final.MH, "MH", "Table AH", weighted = FALSE)

#######################
# MULTIFAMILY
#######################
tableAH.final.MF <- mean_one_group_unweighted(CustomerLevelData = tableAH.data
                                   ,valueVariable    = 'Wattage.per.bulb'
                                   ,byVariable       = 'Category'
                                   ,aggregateRow     = 'Remove')

# Export table
tableAH.final.MF <- tableAH.final.MF[which(tableAH.final.MF$BuildingType == "Multifamily"),-1]
exportTable(tableAH.final.MF, "MF", "Table AH", weighted = FALSE,OS = T, osIndicator = "PSE")
