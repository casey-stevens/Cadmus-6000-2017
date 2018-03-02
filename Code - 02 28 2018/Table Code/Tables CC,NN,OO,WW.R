#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
rm(list=ls())
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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
#clean cadmus IDs
lighting.dat$CK_Cadmus_ID <- trimws(toupper(lighting.dat$CK_Cadmus_ID))
lighting.dat <- lighting.dat[which(colnames(lighting.dat) %in% c("CK_Cadmus_ID"
                                                               ,"Fixture.Qty"
                                                               ,"LIGHTING_BulbsPerFixture"
                                                               ,"CK_SiteID"
                                                               ,"Lamp.Category"
                                                               ,"Clean.Room"
                                                               ,"Switch.Type"))]
lighting.dat.LED <- lighting.dat[which(lighting.dat$Lamp.Category == "Light Emitting Diode"),]
lighting.dat.CFL <- lighting.dat[which(lighting.dat$Lamp.Category == "Compact Fluorescent"),]

#Read in data for analysis
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
#clean cadmus IDs
survey.dat$CK_Cadmus_ID <- trimws(toupper(survey.dat$NEXID))
survey.dat <- survey.dat[which(colnames(survey.dat) %in% c("CK_Cadmus_ID", "Do.you.own.or.rent.your.home?"))]
colnames(survey.dat) <- c("Ownership.Type", "CK_Cadmus_ID")

#merge together analysis data with cleaned RBSA data
merge.dat1 <- left_join(rbsa.dat, survey.dat, by = "CK_Cadmus_ID")
unique(merge.dat1$Ownership.Type)

#remove NA from ownership type
merge.dat2 <- merge.dat1[which(merge.dat1$Ownership.Type %notin% c("N/A",NA)),]
length(unique(merge.dat2$CK_Cadmus_ID))


#############################################################################################
# Table CC: Percentage of homes with LEDs by State and Owndership Type
#############################################################################################
tableCC.dat <- left_join(merge.dat2, lighting.dat.LED)
tableCC.dat$Ind <- 0
tableCC.dat$Ind[which(tableCC.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableCC.sum <- summarise(group_by(tableCC.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))
unique(tableCC.sum$Ind)
tableCC.merge <- left_join(merge.dat2, tableCC.sum)
unique(tableCC.merge$Ind)

tableCC.merge <- tableCC.merge[which(tableCC.merge$Ownership.Type != "Prefer not to say"),]

################################################
# Adding pop and sample sizes for weights
################################################
tableCC.data <- weightedData(tableCC.merge[-which(colnames(tableCC.merge) %in% c("Ownership.Type"
                                                                                 ,"Ind"))])
tableCC.data <- left_join(tableCC.data, tableCC.merge[which(colnames(tableCC.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Ownership.Type"
                                                                                       ,"Ind"))])
tableCC.data$count <- 1
tableCC.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableCC.summary <- proportionRowsAndColumns1(CustomerLevelData = tableCC.data
                                             ,valueVariable = "Ind"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Ownership.Type"
                                             ,aggregateColumnName = "Region")
tableCC.summary <- tableCC.summary[which(tableCC.summary$Ownership.Type != "Total"),]

tableCC.all.types <- proportions_one_group(CustomerLevelData = tableCC.data
                                           ,valueVariable = "Ind"
                                           ,groupingVariable = "State"
                                           ,total.name = "All Types"
                                           ,columnName = "Ownership.Type"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
tableCC.all.types$State[which(tableCC.all.types$State == "Total")] <- "Region"

tableCC.final <- rbind.data.frame(tableCC.summary, tableCC.all.types, stringsAsFactors = F)

tableCC.cast <- dcast(setDT(tableCC.final)
                      ,formula = BuildingType + Ownership.Type ~ State
                      ,value.var = c("w.percent","w.SE","count","n", "N", "EB"))

tableCC.table <- data.frame("BuildingType"    = tableCC.cast$BuildingType
                            ,"Ownership.Type" = tableCC.cast$Ownership.Type
                            ,"ID"             = tableCC.cast$w.percent_ID
                            ,"ID.SE"          = tableCC.cast$w.SE_ID
                            ,"ID.n"           = tableCC.cast$n_ID
                            ,"MT"             = tableCC.cast$w.percent_MT
                            ,"MT.SE"          = tableCC.cast$w.SE_MT
                            ,"MT.n"           = tableCC.cast$n_MT
                            ,"OR"             = tableCC.cast$w.percent_OR
                            ,"OR.SE"          = tableCC.cast$w.SE_OR
                            ,"OR.n"           = tableCC.cast$n_OR
                            ,"WA"             = tableCC.cast$w.percent_WA
                            ,"WA.SE"          = tableCC.cast$w.SE_WA
                            ,"WA.n"           = tableCC.cast$n_WA
                            ,"Region"         = tableCC.cast$w.percent_Region
                            ,"Region.SE"      = tableCC.cast$w.SE_Region
                            ,"Region.n"       = tableCC.cast$n_Region
                            ,"EB_ID"          = tableCC.cast$EB_ID
                            ,"EB_MT"          = tableCC.cast$EB_MT
                            ,"EB_OR"          = tableCC.cast$EB_OR
                            ,"EB_WA"          = tableCC.cast$EB_WA
                            ,"EB_Region"      = tableCC.cast$EB_Region
                            )

levels(tableCC.table$Ownership.Type)
rowOrder <- c("Own / buying"
              ,"Rent"
              ,"Occupy without rent"
              ,"All Types")
tableCC.table <- tableCC.table %>% mutate(Ownership.Type = factor(Ownership.Type, levels = rowOrder)) %>% arrange(Ownership.Type)  
tableCC.table <- data.frame(tableCC.table)

tableCC.table.SF <- tableCC.table[which(tableCC.table$BuildingType == "Single Family")
                                  ,which(colnames(tableCC.table) %notin% c("BuildingType"))]
tableCC.table.MH <- tableCC.table[which(tableCC.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableCC.table) %notin% c("BuildingType"))]

# exportTable(tableCC.table.SF, "SF", "Table CC", weighted = TRUE)
exportTable(tableCC.table.MH, "MH", "Table CC", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableCC.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableCC.data
                                             ,valueVariable = "Ind"
                                             ,columnVariable = "State"
                                             ,rowVariable = "Ownership.Type"
                                             ,aggregateColumnName = "Region")
tableCC.summary <- tableCC.summary[which(tableCC.summary$Ownership.Type != "Total"),]

tableCC.all.types <- proportions_one_group(CustomerLevelData = tableCC.data
                                           ,valueVariable = "Ind"
                                           ,groupingVariable = "State"
                                           ,total.name = "All Types"
                                           ,columnName = "Ownership.Type"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
tableCC.all.types$State[which(tableCC.all.types$State == "Total")] <- "Region"

tableCC.final <- rbind.data.frame(tableCC.summary, tableCC.all.types, stringsAsFactors = F)

tableCC.cast <- dcast(setDT(tableCC.final)
                      ,formula = BuildingType + Ownership.Type ~ State
                      ,value.var = c("Percent","SE","Count","n"))

tableCC.table <- data.frame("BuildingType"    = tableCC.cast$BuildingType
                            ,"Ownership.Type" = tableCC.cast$Ownership.Type
                            ,"ID"             = tableCC.cast$Percent_ID
                            ,"ID.SE"          = tableCC.cast$SE_ID
                            ,"ID.n"           = tableCC.cast$n_ID
                            ,"MT"             = tableCC.cast$Percent_MT
                            ,"MT.SE"          = tableCC.cast$SE_MT
                            ,"MT.n"           = tableCC.cast$n_MT
                            ,"OR"             = tableCC.cast$Percent_OR
                            ,"OR.SE"          = tableCC.cast$SE_OR
                            ,"OR.n"           = tableCC.cast$n_OR
                            ,"WA"             = tableCC.cast$Percent_WA
                            ,"WA.SE"          = tableCC.cast$SE_WA
                            ,"WA.n"           = tableCC.cast$n_WA
                            ,"Region"         = tableCC.cast$Percent_Region
                            ,"Region.SE"      = tableCC.cast$SE_Region
                            ,"Region.n"       = tableCC.cast$n_Region
)

levels(tableCC.table$Ownership.Type)
rowOrder <- c("Own / buying"
              ,"Rent"
              ,"Occupy without rent"
              ,"All Types")
tableCC.table <- tableCC.table %>% mutate(Ownership.Type = factor(Ownership.Type, levels = rowOrder)) %>% arrange(Ownership.Type)  
tableCC.table <- data.frame(tableCC.table)

tableCC.table.SF <- tableCC.table[which(tableCC.table$BuildingType == "Single Family")
                                  ,which(colnames(tableCC.table) %notin% c("BuildingType"))]
tableCC.table.MH <- tableCC.table[which(tableCC.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableCC.table) %notin% c("BuildingType"))]

# exportTable(tableCC.table.SF, "SF", "Table CC", weighted = FALSE)
exportTable(tableCC.table.MH, "MH", "Table CC", weighted = FALSE)



#############################################################################################
# Table NN: Percentage of homes with CFLs by state
#############################################################################################
tableNN.dat <- left_join(rbsa.dat, lighting.dat.CFL)
tableNN.dat$Ind <- 0
tableNN.dat$Ind[which(tableNN.dat$Lamp.Category == "Compact Fluorescent")] <- 1

tableNN.sum <- summarise(group_by(tableNN.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableNN.merge <- left_join(rbsa.dat, tableNN.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableNN.data <- weightedData(tableNN.merge[-which(colnames(tableNN.merge) %in% c("Ind"))])
tableNN.data <- left_join(tableNN.data, tableNN.merge[which(colnames(tableNN.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
tableNN.data$count <- 1
tableNN.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableNN.table <- proportions_one_group(CustomerLevelData = tableNN.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableNN.table.SF <- tableNN.table[which(tableNN.table$BuildingType == "Single Family")
                                  ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
tableNN.table.MH <- tableNN.table[which(tableNN.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableNN.table) %notin% c("BuildingType"))]

# exportTable(tableNN.table.SF, "SF", "Table NN", weighted = TRUE)
exportTable(tableNN.table.MH, "MH", "Table NN", weighted = TRUE)


#######################
# unweighted Analysis
#######################
tableNN.table <- proportions_one_group(CustomerLevelData = tableNN.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableNN.table.SF <- tableNN.table[which(tableNN.table$BuildingType == "Single Family")
                                  ,which(colnames(tableNN.table) %notin% c("BuildingType"))]
tableNN.table.MH <- tableNN.table[which(tableNN.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableNN.table) %notin% c("BuildingType"))]

# exportTable(tableNN.table.SF, "SF", "Table NN", weighted = FALSE)
exportTable(tableNN.table.MH, "MH", "Table NN", weighted = FALSE)



#############################################################################################
# Table OO: Percentage of homes with LEDs by state
#############################################################################################
tableOO.dat <- left_join(rbsa.dat, lighting.dat.LED)
tableOO.dat$Ind <- 0
tableOO.dat$Ind[which(tableOO.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableOO.sum <- summarise(group_by(tableOO.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableOO.merge <- left_join(rbsa.dat, tableOO.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableOO.data <- weightedData(tableOO.merge[-which(colnames(tableOO.merge) %in% c("Ind"))])
tableOO.data <- left_join(tableOO.data, tableOO.merge[which(colnames(tableOO.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
tableOO.data$count <- 1
tableOO.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableOO.table <- proportions_one_group(CustomerLevelData = tableOO.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableOO.table.SF <- tableOO.table[which(tableOO.table$BuildingType == "Single Family")
                                  ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
tableOO.table.MH <- tableOO.table[which(tableOO.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableOO.table) %notin% c("BuildingType"))]

# exportTable(tableOO.table.SF, "SF", "Table OO", weighted = TRUE)
exportTable(tableOO.table.MH, "MH", "Table OO", weighted = TRUE)


#######################
# unweighted Analysis
#######################
tableOO.table <- proportions_one_group(CustomerLevelData = tableOO.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableOO.table.SF <- tableOO.table[which(tableOO.table$BuildingType == "Single Family")
                                  ,which(colnames(tableOO.table) %notin% c("BuildingType"))]
tableOO.table.MH <- tableOO.table[which(tableOO.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableOO.table) %notin% c("BuildingType"))]

# exportTable(tableOO.table.SF, "SF", "Table OO", weighted = FALSE)
exportTable(tableOO.table.MH, "MH", "Table OO", weighted = FALSE)




#############################################################################################
# Table RR: Percentage of homes with LEDs by state
#############################################################################################
tableWW.dat <- left_join(rbsa.dat, lighting.dat)
tableWW.dat$Ind <- 0
tableWW.dat$Ind[grep("grow",tableWW.dat$Clean.Room,ignore.case = T)] <- 1

tableWW.sum <- summarise(group_by(tableWW.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableWW.merge <- left_join(rbsa.dat, tableWW.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableWW.data <- weightedData(tableWW.merge[-which(colnames(tableWW.merge) %in% c("Ind"))])
tableWW.data <- left_join(tableWW.data, tableWW.merge[which(colnames(tableWW.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))])
tableWW.data$count <- 1
tableWW.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableWW.table <- proportions_one_group(CustomerLevelData = tableWW.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = TRUE)
tableWW.table.SF <- tableWW.table[which(tableWW.table$BuildingType == "Single Family")
                                  ,which(colnames(tableWW.table) %notin% c("BuildingType"))]
tableWW.table.MH <- tableWW.table[which(tableWW.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableWW.table) %notin% c("BuildingType"))]

# exportTable(tableWW.table.SF, "SF", "Table WW", weighted = TRUE)
exportTable(tableWW.table.MH, "MH", "Table WW", weighted = TRUE)


#######################
# unweighted Analysis
#######################
tableWW.table <- proportions_one_group(CustomerLevelData = tableWW.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "State"
                                       ,total.name = "Region"
                                       ,weighted = FALSE)
tableWW.table.SF <- tableWW.table[which(tableWW.table$BuildingType == "Single Family")
                                  ,which(colnames(tableWW.table) %notin% c("BuildingType"))]
tableWW.table.MH <- tableWW.table[which(tableWW.table$BuildingType == "Manufactured")
                                  ,which(colnames(tableWW.table) %notin% c("BuildingType"))]

# exportTable(tableWW.table.SF, "SF", "Table WW", weighted = FALSE)
exportTable(tableWW.table.MH, "MH", "Table WW", weighted = FALSE)




































############################################################################################################
#
#
# OVERSAMPLE ANALYSIS
#
#
############################################################################################################

# Read in clean scl data
scl.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.scl.data", rundate, ".xlsx", sep = "")))
length(unique(scl.dat$CK_Cadmus_ID))
scl.dat$CK_Building_ID <- scl.dat$Category
scl.dat <- scl.dat[which(names(scl.dat) != "Category")]

#merge together analysis data with cleaned scl data
merge.dat1 <- left_join(scl.dat, survey.dat, by = "CK_Cadmus_ID")
unique(merge.dat1$Ownership.Type)

#remove NA from ownership type
merge.dat2 <- merge.dat1[which(merge.dat1$Ownership.Type %notin% c("N/A",NA)),]
length(unique(merge.dat2$CK_Cadmus_ID))


#############################################################################################
# Table CC: Percentage of homes with LEDs by CK_Building_ID and Owndership Type
#############################################################################################
tableCC.os.dat <- left_join(merge.dat2, lighting.dat.LED)
tableCC.os.dat$Ind <- 0
tableCC.os.dat$Ind[which(tableCC.os.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableCC.os.sum <- summarise(group_by(tableCC.os.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))
unique(tableCC.os.sum$Ind)
tableCC.os.merge <- left_join(merge.dat2, tableCC.os.sum)
unique(tableCC.os.merge$Ind)

tableCC.os.merge <- tableCC.os.merge[which(tableCC.os.merge$Ownership.Type != "Prefer not to say"),]

################################################
# Adding pop and sample sizes for weights
################################################
tableCC.os.data <- weightedData(tableCC.os.merge[-which(colnames(tableCC.os.merge) %in% c("Ownership.Type"
                                                                                 ,"Ind"))])
tableCC.os.data <- left_join(tableCC.os.data, unique(tableCC.os.merge[which(colnames(tableCC.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ownership.Type"
                                                                                           ,"Ind"))]))
tableCC.os.data$count <- 1
tableCC.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableCC.os.summary <- proportionRowsAndColumns1(CustomerLevelData = tableCC.os.data
                                             ,valueVariable = "Ind"
                                             ,columnVariable = "CK_Building_ID"
                                             ,rowVariable = "Ownership.Type"
                                             ,aggregateColumnName = "Remove")
tableCC.os.summary <- tableCC.os.summary[which(tableCC.os.summary$Ownership.Type != "Total"),]
tableCC.os.summary <- tableCC.os.summary[which(tableCC.os.summary$CK_Building_ID != "Remove"),]

tableCC.os.all.types <- proportions_one_group(CustomerLevelData = tableCC.os.data
                                           ,valueVariable = "Ind"
                                           ,groupingVariable = "CK_Building_ID"
                                           ,total.name = "All Types"
                                           ,columnName = "Ownership.Type"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)
tableCC.os.all.types <- tableCC.os.all.types[which(tableCC.os.all.types$CK_Building_ID != "Total"),]

tableCC.os.final <- rbind.data.frame(tableCC.os.summary, tableCC.os.all.types, stringsAsFactors = F)

tableCC.os.cast <- dcast(setDT(tableCC.os.final)
                      ,formula = BuildingType + Ownership.Type ~ CK_Building_ID
                      ,value.var = c("w.percent","w.SE","count","n", "N", "EB"))

tableCC.os.table <- data.frame("BuildingType"    = tableCC.os.cast$BuildingType
                            ,"Ownership.Type" = tableCC.os.cast$Ownership.Type
                            ,"Percent_SCL.GenPop"   = tableCC.os.cast$`w.percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableCC.os.cast$`w.SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableCC.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableCC.os.cast$`w.percent_SCL LI`
                            ,"SE_SCL.LI"            = tableCC.os.cast$`w.SE_SCL LI`
                            ,"n_SCL.LI"             = tableCC.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableCC.os.cast$`w.percent_SCL EH`
                            ,"SE_SCL.EH"            = tableCC.os.cast$`w.SE_SCL EH`
                            ,"n_SCL.EH"             = tableCC.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableCC.os.cast$`w.percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableCC.os.cast$`w.SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableCC.os.cast$`n_2017 RBSA PS`
                            ,"EB_SCL.GenPop"        = tableCC.os.cast$`EB_SCL GenPop`
                            ,"EB_SCL.LI"            = tableCC.os.cast$`EB_SCL LI`
                            ,"EB_SCL.EH"            = tableCC.os.cast$`EB_SCL EH`
                            ,"EB_2017.RBSA.PS"      = tableCC.os.cast$`EB_2017 RBSA PS`
)

levels(tableCC.os.table$Ownership.Type)
rowOrder <- c("Own / buying"
              ,"Rent"
              ,"Occupy without rent"
              ,"All Types")
tableCC.os.table <- tableCC.os.table %>% mutate(Ownership.Type = factor(Ownership.Type, levels = rowOrder)) %>% arrange(Ownership.Type)  
tableCC.os.table <- data.frame(tableCC.os.table)

tableCC.os.table.SF <- tableCC.os.table[which(tableCC.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableCC.os.table) %notin% c("BuildingType"))]

exportTable(tableCC.os.table.SF, "SF", "Table CC", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# unweighted Analysis
#######################
tableCC.os.summary <- proportions_two_groups_unweighted(CustomerLevelData = tableCC.os.data
                                                     ,valueVariable = "Ind"
                                                     ,columnVariable = "CK_Building_ID"
                                                     ,rowVariable = "Ownership.Type"
                                                     ,aggregateColumnName = "Remove")
tableCC.os.summary <- tableCC.os.summary[which(tableCC.os.summary$Ownership.Type != "Total"),]
tableCC.os.summary <- tableCC.os.summary[which(tableCC.os.summary$CK_Building_ID != "Remove"),]

tableCC.os.all.types <- proportions_one_group(CustomerLevelData = tableCC.os.data
                                           ,valueVariable = "Ind"
                                           ,groupingVariable = "CK_Building_ID"
                                           ,total.name = "All Types"
                                           ,columnName = "Ownership.Type"
                                           ,weighted = FALSE
                                           ,two.prop.total = TRUE)
tableCC.os.all.types <- tableCC.os.all.types[which(tableCC.os.all.types$CK_Building_ID != "Total"),]

tableCC.os.final <- rbind.data.frame(tableCC.os.summary, tableCC.os.all.types, stringsAsFactors = F)

tableCC.os.cast <- dcast(setDT(tableCC.os.final)
                      ,formula = BuildingType + Ownership.Type ~ CK_Building_ID
                      ,value.var = c("Percent","SE","Count","n"))

tableCC.os.table <- data.frame("BuildingType"    = tableCC.os.cast$BuildingType
                            ,"Ownership.Type" = tableCC.os.cast$Ownership.Type
                            ,"Percent_SCL.GenPop"   = tableCC.os.cast$`Percent_SCL GenPop`
                            ,"SE_SCL.GenPop"        = tableCC.os.cast$`SE_SCL GenPop`
                            ,"n_SCL.GenPop"         = tableCC.os.cast$`n_SCL GenPop`
                            ,"Percent_SCL.LI"       = tableCC.os.cast$`Percent_SCL LI`
                            ,"SE_SCL.LI"            = tableCC.os.cast$`SE_SCL LI`
                            ,"n_SCL.LI"             = tableCC.os.cast$`n_SCL LI`
                            ,"Percent_SCL.EH"       = tableCC.os.cast$`Percent_SCL EH`
                            ,"SE_SCL.EH"            = tableCC.os.cast$`SE_SCL EH`
                            ,"n_SCL.EH"             = tableCC.os.cast$`n_SCL EH`
                            ,"Percent_2017.RBSA.PS" = tableCC.os.cast$`Percent_2017 RBSA PS`
                            ,"SE_2017.RBSA.PS"      = tableCC.os.cast$`SE_2017 RBSA PS`
                            ,"n_2017.RBSA.PS"       = tableCC.os.cast$`n_2017 RBSA PS`
)

levels(tableCC.os.table$Ownership.Type)
rowOrder <- c("Own / buying"
              ,"Rent"
              ,"Occupy without rent"
              ,"All Types")
tableCC.os.table <- tableCC.os.table %>% mutate(Ownership.Type = factor(Ownership.Type, levels = rowOrder)) %>% arrange(Ownership.Type)  
tableCC.os.table <- data.frame(tableCC.os.table)

tableCC.os.table.SF <- tableCC.os.table[which(tableCC.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableCC.os.table) %notin% c("BuildingType"))]

exportTable(tableCC.os.table.SF, "SF", "Table CC", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table NN: Percentage of homes with CFLs by CK_Building_ID
#############################################################################################
tableNN.os.dat <- left_join(scl.dat, lighting.dat.CFL)
tableNN.os.dat$Ind <- 0
tableNN.os.dat$Ind[which(tableNN.os.dat$Lamp.Category == "Compact Fluorescent")] <- 1

tableNN.os.sum <- summarise(group_by(tableNN.os.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))
unique(tableNN.os.sum$Ind)

tableNN.os.merge <- left_join(scl.dat, tableNN.os.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableNN.os.data <- weightedData(tableNN.os.merge[-which(colnames(tableNN.os.merge) %in% c("Ind"))])
tableNN.os.data <- left_join(tableNN.os.data, unique(tableNN.os.merge[which(colnames(tableNN.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
tableNN.os.data$count <- 1
tableNN.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableNN.os.table <- proportions_one_group(CustomerLevelData = tableNN.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableNN.os.table <- tableNN.os.table[which(tableNN.os.table$CK_Building_ID %notin% c("Remove", "Total")),]
tableNN.os.table.SF <- tableNN.os.table[which(tableNN.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableNN.os.table) %notin% c("BuildingType"))]

exportTable(tableNN.os.table.SF, "SF", "Table NN", weighted = TRUE, osIndicator = "SCL", OS = T)

#######################
# unweighted Analysis
#######################
tableNN.os.table <- proportions_one_group(CustomerLevelData = tableNN.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableNN.os.table <- tableNN.os.table[which(tableNN.os.table$CK_Building_ID %notin% c("Remove", "Total")),]
tableNN.os.table.SF <- tableNN.os.table[which(tableNN.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableNN.os.table) %notin% c("BuildingType"))]

exportTable(tableNN.os.table.SF, "SF", "Table NN", weighted = FALSE, osIndicator = "SCL", OS = T)



#############################################################################################
# Table OO: Percentage of homes with LEDs by CK_Building_ID
#############################################################################################
tableOO.os.dat <- left_join(scl.dat, lighting.dat.LED)
tableOO.os.dat$Ind <- 0
tableOO.os.dat$Ind[which(tableOO.os.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableOO.os.sum <- summarise(group_by(tableOO.os.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableOO.os.merge <- left_join(scl.dat, tableOO.os.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableOO.os.data <- weightedData(tableOO.os.merge[-which(colnames(tableOO.os.merge) %in% c("Ind"))])
tableOO.os.data <- left_join(tableOO.os.data, unique(tableOO.os.merge[which(colnames(tableOO.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
tableOO.os.data$count <- 1
tableOO.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableOO.os.table <- proportions_one_group(CustomerLevelData = tableOO.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableOO.os.table <- tableOO.os.table[which(tableOO.os.table$CK_Building_ID %notin% c("Remove", "Total")),]

tableOO.os.table.SF <- tableOO.os.table[which(tableOO.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableOO.os.table) %notin% c("BuildingType"))]

exportTable(tableOO.os.table.SF, "SF", "Table OO", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# unweighted Analysis
#######################
tableOO.os.table <- proportions_one_group(CustomerLevelData = tableOO.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableOO.os.table <- tableOO.os.table[which(tableOO.os.table$CK_Building_ID %notin% c("Remove", "Total")),]

tableOO.os.table.SF <- tableOO.os.table[which(tableOO.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableOO.os.table) %notin% c("BuildingType"))]

exportTable(tableOO.os.table.SF, "SF", "Table OO", weighted = FALSE, osIndicator = "SCL", OS = T)




#############################################################################################
# Table WW: Percentage of homes with LEDs by CK_Building_ID
#############################################################################################
tableWW.os.dat <- left_join(scl.dat, lighting.dat)
tableWW.os.dat$Ind <- 0
tableWW.os.dat$Ind[grep("grow",tableWW.os.dat$Clean.Room,ignore.case = T)] <- 1

tableWW.os.sum <- summarise(group_by(tableWW.os.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableWW.os.merge <- left_join(scl.dat, tableWW.os.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableWW.os.data <- weightedData(tableWW.os.merge[-which(colnames(tableWW.os.merge) %in% c("Ind"))])
tableWW.os.data <- left_join(tableWW.os.data, unique(tableWW.os.merge[which(colnames(tableWW.os.merge) %in% c("CK_Cadmus_ID"
                                                                                           ,"Ind"))]))
tableWW.os.data$count <- 1
tableWW.os.data$Count <- 1

#######################
# Weighted Analysis
#######################
tableWW.os.table <- proportions_one_group(CustomerLevelData = tableWW.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = TRUE)
tableWW.os.table <- tableWW.os.table[which(tableWW.os.table$CK_Building_ID %notin% c("Remove", "Total")),]

tableWW.os.table.SF <- tableWW.os.table[which(tableWW.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableWW.os.table) %notin% c("BuildingType"))]

exportTable(tableWW.os.table.SF, "SF", "Table WW", weighted = TRUE, osIndicator = "SCL", OS = T)


#######################
# unweighted Analysis
#######################
tableWW.os.table <- proportions_one_group(CustomerLevelData = tableWW.os.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "CK_Building_ID"
                                       ,total.name = "Remove"
                                       ,weighted = FALSE)
tableWW.os.table <- tableWW.os.table[which(tableWW.os.table$CK_Building_ID %notin% c("Remove", "Total")),]

tableWW.os.table.SF <- tableWW.os.table[which(tableWW.os.table$BuildingType == "Single Family")
                                  ,which(colnames(tableWW.os.table) %notin% c("BuildingType"))]

exportTable(tableWW.os.table.SF, "SF", "Table WW", weighted = FALSE, osIndicator = "SCL", OS = T)