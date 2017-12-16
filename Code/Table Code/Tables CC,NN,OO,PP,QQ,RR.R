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
                                                               ,"Clean.Room"))]
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
merge.dat2 <- merge.dat1[which(!(is.na(merge.dat1$Ownership.Type))),]
length(unique(merge.dat2$CK_Cadmus_ID))


# rooms.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
# rooms.dat$CK_Cadmus_ID <- trimws(toupper(rooms.dat$CK_Cadmus_ID))
# rooms.dat <- rooms.dat[which(colnames(rooms.dat) %in% c("CK_Cadmus_ID"
#                                                         ,"Clean.Type"))]
# rooms.dat$count <- 1
# rooms.dat$Clean.Type[which(rooms.dat$Clean.Type %in% c("Attic"
#                                                              ,"Basement"
#                                                              ,"Crawlspace"
#                                                              ,"Crawl Space"
#                                                              ,"Mechanical"
#                                                              ,"Grow Room"))] <- "Other"
# rooms.sum <- summarise(group_by(rooms.dat, CK_Cadmus_ID, Clean.Type)
#                        ,Room.Count = sum((count)))
# rooms.cast <- dcast(setDT(rooms.sum)
#                     ,formula = CK_Cadmus_ID ~ Clean.Type,sum
#                     ,value.var = "Room.Count")
# rooms.melt <- melt(rooms.cast, id.vars = "CK_Cadmus_ID")
# names(rooms.melt) <- c("CK_Cadmus_ID", "Clean.Type", "Room.Count")


#############################################################################################
# Table CC: Percentage of homes with LEDs by State and Owndership Type
#############################################################################################
tableCC.dat <- left_join(merge.dat2, lighting.dat.LED)
tableCC.dat$Ind <- 0
tableCC.dat$Ind[which(tableCC.dat$Lamp.Category == "Light Emitting Diode")] <- 1

tableCC.sum <- summarise(group_by(tableCC.dat, CK_Cadmus_ID)
                         ,Ind = sum(unique((Ind))))

tableCC.merge <- left_join(merge.dat2, tableCC.sum)

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
                      ,value.var = c("w.percent","w.SE","count","n", "N"))

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

exportTable(tableCC.table.SF, "SF", "Table CC", weighted = TRUE)
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

exportTable(tableCC.table.SF, "SF", "Table CC", weighted = FALSE)
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

exportTable(tableNN.table.SF, "SF", "Table NN", weighted = TRUE)
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

exportTable(tableNN.table.SF, "SF", "Table NN", weighted = FALSE)
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

exportTable(tableOO.table.SF, "SF", "Table OO", weighted = TRUE)
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

exportTable(tableOO.table.SF, "SF", "Table OO", weighted = FALSE)
exportTable(tableOO.table.MH, "MH", "Table OO", weighted = FALSE)




# #############################################################################################
# # Table PP: Percentage of homes with CFLs by Room Type
# #############################################################################################
# tablePP.dat <- left_join(rbsa.dat, lighting.dat.CFL)
# tablePP.dat$Ind <- 0
# tablePP.dat$Ind[which(tablePP.dat$Lamp.Category == "Compact Fluorescent")] <- 1
# Count <- 1
# 
# tablePP.dat1 <- tablePP.dat[which(!is.na(tablePP.dat$Clean.Room)),]
# tablePP.dat1$Clean.Room[which(tablePP.dat1$Clean.Room %in% c("Attic"
#                                                            ,"Basement"
#                                                            ,"Crawlspace"
#                                                            ,"Crawl Space"
#                                                            ,"Mechanical"
#                                                            ,"Grow Room"))] <- "Other"
# 
# tablePP.cast <- dcast(setDT(tablePP.dat1)
#                       ,formula = CK_Cadmus_ID ~ Clean.Room,sum
#                       ,value.var = "Ind")
# tablePP.cast <- data.frame(tablePP.cast, stringsAsFactors = F)
# 
# tablePP.melt <- melt(tablePP.cast, id.vars = "CK_Cadmus_ID")
# names(tablePP.melt) <- c("CK_Cadmus_ID", "Clean.Type", "Ind")
# 
# 
# tablePP.melt$Ind[which(tablePP.melt$Ind > 0)] <- 1
# unique(tablePP.melt$Ind)
# 
# 
# tablePP.sum.bulbs <- summarise(group_by(tablePP.melt, CK_Cadmus_ID, Clean.Type)
#                          ,Ind = sum(unique((Ind))))
# 
# unique(tablePP.sum.bulbs$Clean.Type)
# tablePP.merge <- left_join(rooms.melt, tablePP.sum.bulbs, c("CK_Cadmus_ID","Clean.Type"))
# tablePP.merge <- left_join(rbsa.dat,tablePP.merge)
# 
# tablePP.merge$Ind[which(is.na(tablePP.merge$Ind))] <- 0
# names(tablePP.merge)
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tablePP.data <- weightedData(tablePP.merge[-which(colnames(tablePP.merge) %in% c("Ind"
#                                                                                  ,"Clean.Type"
#                                                                                  ,"Room.Count"))])
# tablePP.data <- left_join(tablePP.data, tablePP.merge[which(colnames(tablePP.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"
#                                                                                            ,"Clean.Type"
#                                                                                            ,"Room.Count"))])
# tablePP.data <- tablePP.data[which(!is.na(tablePP.data$Clean.Type)),]
# tablePP.data$count <- 1
# # tablePP.data$Count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# tablePP.table <- proportions_one_group(CustomerLevelData = tablePP.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "Clean.Type"
#                                        ,total.name = "All Room Types"
#                                        ,weighted = TRUE)
# tablePP.table.SF <- tablePP.table[which(tablePP.table$BuildingType == "Single Family")
#                                   ,which(colnames(tablePP.table) %notin% c("BuildingType"))]
# tablePP.table.MH <- tablePP.table[which(tablePP.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tablePP.table) %notin% c("BuildingType"))]
# 
# exportTable(tablePP.table.SF, "SF", "Table PP", weighted = TRUE)
# exportTable(tablePP.table.MH, "MH", "Table PP", weighted = TRUE)
# 
# 
# #######################
# # unweighted Analysis
# #######################
# tablePP.table <- proportions_one_group(CustomerLevelData = tablePP.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# tablePP.table.SF <- tablePP.table[which(tablePP.table$BuildingType == "Single Family")
#                                   ,which(colnames(tablePP.table) %notin% c("BuildingType"))]
# tablePP.table.MH <- tablePP.table[which(tablePP.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tablePP.table) %notin% c("BuildingType"))]
# 
# exportTable(tablePP.table.SF, "SF", "Table PP", weighted = FALSE)
# exportTable(tablePP.table.MH, "MH", "Table PP", weighted = FALSE)
# 
# 
# 
# #############################################################################################
# # Table QQ: Percentage of homes with LEDs by state
# #############################################################################################
# tableQQ.dat <- left_join(rbsa.dat, lighting.dat.LED)
# tableQQ.dat$Ind <- 0
# tableQQ.dat$Ind[which(tableQQ.dat$Lamp.Category == "Light Emitting Diode")] <- 1
# 
# tableQQ.sum <- summarise(group_by(tableQQ.dat, CK_Cadmus_ID)
#                          ,Ind = sum(unique((Ind))))
# 
# tableQQ.merge <- left_join(rbsa.dat, tableQQ.sum)
# 
# ################################################
# # Adding pop and sample sizes for weights
# ################################################
# tableQQ.data <- weightedData(tableQQ.merge[-which(colnames(tableQQ.merge) %in% c("Ind"))])
# tableQQ.data <- left_join(tableQQ.data, tableQQ.merge[which(colnames(tableQQ.merge) %in% c("CK_Cadmus_ID"
#                                                                                            ,"Ind"))])
# tableQQ.data$count <- 1
# tableQQ.data$Count <- 1
# 
# #######################
# # Weighted Analysis
# #######################
# tableQQ.table <- proportions_one_group(CustomerLevelData = tableQQ.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = TRUE)
# tableQQ.table.SF <- tableQQ.table[which(tableQQ.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableQQ.table) %notin% c("BuildingType"))]
# tableQQ.table.MH <- tableQQ.table[which(tableQQ.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableQQ.table) %notin% c("BuildingType"))]
# 
# exportTable(tableQQ.table.SF, "SF", "Table QQ", weighted = TRUE)
# exportTable(tableQQ.table.MH, "MH", "Table QQ", weighted = TRUE)
# 
# 
# #######################
# # unweighted Analysis
# #######################
# tableQQ.table <- proportions_one_group(CustomerLevelData = tableQQ.data
#                                        ,valueVariable = "Ind"
#                                        ,groupingVariable = "State"
#                                        ,total.name = "Region"
#                                        ,weighted = FALSE)
# tableQQ.table.SF <- tableQQ.table[which(tableQQ.table$BuildingType == "Single Family")
#                                   ,which(colnames(tableQQ.table) %notin% c("BuildingType"))]
# tableQQ.table.MH <- tableQQ.table[which(tableQQ.table$BuildingType == "Manufactured")
#                                   ,which(colnames(tableQQ.table) %notin% c("BuildingType"))]
# 
# exportTable(tableQQ.table.SF, "SF", "Table QQ", weighted = FALSE)
# exportTable(tableQQ.table.MH, "MH", "Table QQ", weighted = FALSE)
# 
