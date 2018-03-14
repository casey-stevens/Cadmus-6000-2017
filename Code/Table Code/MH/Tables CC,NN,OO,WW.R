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
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
rbsa.dat.site <- rbsa.dat[grep("site", rbsa.dat$CK_Building_ID, ignore.case = T),]
length(unique(rbsa.dat$CK_Cadmus_ID))

#Read in data for analysis
# lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
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
# survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")
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


###########################################################
# Multifamily
###########################################################
tableNN.MF.dat <- left_join(rbsa.dat, lighting.dat.CFL)
tableNN.MF.dat$Ind <- 0
tableNN.MF.dat$Ind[which(tableNN.MF.dat$Lamp.Category == "Compact Fluorescent")] <- 1

tableNN.MF.sum <- summarise(group_by(tableNN.MF.dat, CK_Cadmus_ID)
                            ,Ind = sum(unique((Ind))))

tableNN.MF.merge <- left_join(rbsa.dat, tableNN.MF.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableNN.MF.data <- weightedData(tableNN.MF.merge[-which(colnames(tableNN.MF.merge) %in% c("Ind"))])
tableNN.MF.data <- left_join(tableNN.MF.data, tableNN.MF.merge[which(colnames(tableNN.MF.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Ind"))])
tableNN.MF.data$count <- 1
tableNN.MF.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableNN.MF.table <- proportions_one_group(CustomerLevelData = tableNN.MF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = TRUE)
tableNN.MF.table.MF <- tableNN.MF.table[which(tableNN.MF.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableNN.MF.table) %notin% c("BuildingType"))]

exportTable(tableNN.MF.table.MF, "MF", "Table NN", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableNN.MF.table <- proportions_one_group(CustomerLevelData = tableNN.MF.data
                                       ,valueVariable = "Ind"
                                       ,groupingVariable = "HomeType"
                                       ,total.name = "All Sizes"
                                       ,weighted = FALSE)
tableNN.MF.table.MF <- tableNN.MF.table[which(tableNN.MF.table$BuildingType == "Multifamily")
                                  ,which(colnames(tableNN.MF.table) %notin% c("BuildingType"))]

exportTable(tableNN.MF.table.MF, "MF", "Table NN", weighted = FALSE)






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


###########################################################
# Multifamily
###########################################################
tableOO.MF.dat <- left_join(rbsa.dat, lighting.dat.LED)
tableOO.MF.dat$Ind <- 0
tableOO.MF.dat$Ind[which(tableOO.MF.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableOO.MF.sum <- summarise(group_by(tableOO.MF.dat, CK_Cadmus_ID)
                            ,Ind = sum(unique((Ind))))

tableOO.MF.merge <- left_join(rbsa.dat, tableOO.MF.sum)

################################################
# Adding pop and sample sizes for weights
################################################
tableOO.MF.data <- weightedData(tableOO.MF.merge[-which(colnames(tableOO.MF.merge) %in% c("Ind"))])
tableOO.MF.data <- left_join(tableOO.MF.data, tableOO.MF.merge[which(colnames(tableOO.MF.merge) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Ind"))])
tableOO.MF.data$count <- 1
tableOO.MF.data$Count <- 1
#######################
# Weighted Analysis
#######################
tableOO.MF.table <- proportions_one_group(CustomerLevelData = tableOO.MF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Sizes"
                                          ,weighted = TRUE)
tableOO.MF.table.MF <- tableOO.MF.table[which(tableOO.MF.table$BuildingType == "Multifamily")
                                        ,which(colnames(tableOO.MF.table) %notin% c("BuildingType"))]

exportTable(tableOO.MF.table.MF, "MF", "Table OO", weighted = TRUE)

#######################
# unweighted Analysis
#######################
tableOO.MF.table <- proportions_one_group(CustomerLevelData = tableOO.MF.data
                                          ,valueVariable = "Ind"
                                          ,groupingVariable = "HomeType"
                                          ,total.name = "All Sizes"
                                          ,weighted = FALSE)
tableOO.MF.table.MF <- tableOO.MF.table[which(tableOO.MF.table$BuildingType == "Multifamily")
                                        ,which(colnames(tableOO.MF.table) %notin% c("BuildingType"))]

exportTable(tableOO.MF.table.MF, "MF", "Table OO", weighted = FALSE)








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
