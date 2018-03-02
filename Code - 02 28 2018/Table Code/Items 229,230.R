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
rbsa.dat.site <- rbsa.dat[grep("site",rbsa.dat$CK_Building_ID, ignore.case = T),]
rbsa.dat.bldg <- rbsa.dat[grep("bldg",rbsa.dat$CK_Building_ID, ignore.case = T),]

#Read in data for analysis
envelope.dat <- read.xlsx(envelope.export)
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))



#############################################################################################
#Item 229: DISTRIBUTION OF STRUCTURAL SYSTEM TYPES BY BUILDING SIZE (MF Table 21)
#############################################################################################
#subset to columns needed for analysis
item229.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"))]
item229.dat$count <- 1

item229.dat1 <- left_join(rbsa.dat.site, item229.dat, by = "CK_Cadmus_ID")

#Subset to Multifamily
item229.dat2 <- item229.dat1[grep("(3 or fewer floors)", item229.dat1$BuildingTypeXX),]

item229.dat3 <- item229.dat2[which(!(item229.dat2$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame %in% c("Unknown", NA, "N/A", "-- Datapoint not asked for --", "Datapoint not asked for", 0))),]
unique(item229.dat3$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame)
item229.dat3$ENV_Construction_CONSTRUCTION_ConstructionType[which(item229.dat3$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame == "Wood")] <- "Framing"
unique(item229.dat3$ENV_Construction_CONSTRUCTION_ConstructionType)

item229.dat4 <- unique(item229.dat3)
which(duplicated(item229.dat4$CK_Cadmus_ID))

item229.dat4$Structural.System <- paste(item229.dat4$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame, item229.dat4$ENV_Construction_CONSTRUCTION_ConstructionType, sep = " ")
item229.dat4$count <- 1


################################################
# Adding pop and sample sizes for weights
################################################
item229.data <- weightedData(item229.dat4[-which(colnames(item229.dat4) %in% c("ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"
                                                                               ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                               ,"Structural.System"
                                                                               ,"count"))])
item229.data <- left_join(item229.data, item229.dat4[which(colnames(item229.dat4) %in% c("CK_Cadmus_ID"
                                                                                         ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"
                                                                                         ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                                         ,"Structural.System"
                                                                                         ,"count"))])
item229.data$Ind <- 1
item229.data$Count <- 1
colnames(item229.data)

#######################
# Weighted Analysis
#######################
item229.summary <- proportionRowsAndColumns1(CustomerLevelData = item229.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'HomeType'
                                             ,rowVariable = 'Structural.System'
                                             ,aggregateColumnName = "Remove")
item229.summary <- item229.summary[which(item229.summary$HomeType != "Remove"),]
item229.summary <- item229.summary[which(item229.summary$Structural.System != "Total"),]
item229.summary$HomeType[which(item229.summary$HomeType == "Apartment Building (3 or fewer floors)")] <- "Low-Rise (1-3)"

mid.rise <- data.frame("BuildingType" = "Multifamily"
                       ,"HomeType" = "Mid-Rise (4-6)"
                       ,"Structural.System" = NA
                       ,"w.percent" = NA
                       ,"w.SE" = NA
                       ,"count" = NA
                       ,"N" = NA
                       ,"n" = NA
                       ,"EB" = NA)
high.rise <- data.frame("BuildingType" = "Multifamily"
                       ,"HomeType" = "High-Rise (7+)"
                       ,"Structural.System" = NA
                       ,"w.percent" = NA
                       ,"w.SE" = NA
                       ,"count" = NA
                       ,"N" = NA
                       ,"n" = NA
                       ,"EB" = NA)

item229.final <- rbind.data.frame(item229.summary, mid.rise, high.rise, stringsAsFactors = F)
item229.cast <- dcast(setDT(item229.final)
                      ,formula = BuildingType + HomeType ~ Structural.System
                      ,value.var = c("w.percent", "w.SE", "count", "n", "N","EB"))

item229.table <- data.frame("BuildingType"             = item229.cast$BuildingType
                            ,"HomeType"                = item229.cast$HomeType
                            ,"Rigid.Frame.Concrete"    = NA#
                            ,"Rigid.Frame.Concrete.SE" = NA#
                            ,"Rigid.Frame.Steel"       = NA#
                            ,"Rigid.Frame.Steel.SE"    = NA#
                            ,"Steel.Framing"           = NA#
                            ,"Steel.Framing.SE"        = NA#
                            ,"Wood.Framing"            = item229.cast$`w.percent_Wood Framing`
                            ,"Wood.Framing.SE"         = item229.cast$`w.SE_Wood Framing`
                            ,"Wood.Framing.n"          = item229.cast$`n_Wood Framing`
                            ,"Brick.Solid.Masonry"     = NA#item229.cast$`w.percent_Brick Solid masonry`
                            ,"Brick.Solid.Masonry.SE"  = NA#item229.cast$`w.SE_Brick Solid masonry`
                            ,"Wood.Framing.EB"         = item229.cast$`EB_Wood Framing`
)
levels(item229.table$HomeType)
rowOrder <- c("Low-Rise (1-3)"
              ,"Mid-Rise (4-6)"
              ,"High-Rise (7+)")
item229.table <- item229.table %>% mutate(HomeType = factor(HomeType, levels = rowOrder)) %>% arrange(HomeType)  
item229.table <- data.frame(item229.table[which(colnames(item229.table) %notin% c("BuildingType"))])
exportTable(item229.table, "MF", "Table 21", weighted = TRUE)

#######################
# unweighted Analysis
#######################
item229.summary <- proportions_two_groups_unweighted(CustomerLevelData = item229.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'HomeType'
                                             ,rowVariable = 'Structural.System'
                                             ,aggregateColumnName = "Remove")
item229.summary <- item229.summary[which(item229.summary$HomeType != "Remove"),]
item229.summary <- item229.summary[which(item229.summary$Structural.System != "Total"),]
item229.summary$HomeType[which(item229.summary$HomeType == "Apartment Building (3 or fewer floors)")] <- "Low-Rise (1-3)"

mid.rise <- data.frame("BuildingType" = "Multifamily"
                       ,"HomeType" = "Mid-Rise (4-6)"
                       ,"Structural.System" = NA
                       ,"Count" = NA
                       ,"n" = NA
                       ,"Percent" = NA
                       ,"SE" = NA)
high.rise <- data.frame("BuildingType" = "Multifamily"
                        ,"HomeType" = "High-Rise (7+)"
                        ,"Structural.System" = NA
                        ,"Count" = NA
                        ,"n" = NA
                        ,"Percent" = NA
                        ,"SE" = NA)

item229.final <- rbind.data.frame(item229.summary, mid.rise, high.rise, stringsAsFactors = F)
item229.cast <- dcast(setDT(item229.final)
                      ,formula = BuildingType + HomeType ~ Structural.System
                      ,value.var = c("Percent", "SE", "Count", "n"))

item229.table <- data.frame("BuildingType"             = item229.cast$BuildingType
                            ,"HomeType"                = item229.cast$HomeType
                            ,"Rigid.Frame.Concrete"    = NA#
                            ,"Rigid.Frame.Concrete.SE" = NA#
                            ,"Rigid.Frame.Steel"       = NA#
                            ,"Rigid.Frame.Steel.SE"    = NA#
                            ,"Steel.Framing"           = NA#
                            ,"Steel.Framing.SE"        = NA#
                            ,"Wood.Framing"            = item229.cast$`Percent_Wood Framing`
                            ,"Wood.Framing.SE"         = item229.cast$`SE_Wood Framing`
                            ,"Wood.Framing.n"          = item229.cast$`n_Wood Framing`
                            ,"Brick.Solid.Masonry"     = NA#item229.cast$`Percent_Brick Solid masonry`
                            ,"Brick.Solid.Masonry.SE"  = NA#item229.cast$`SE_Brick Solid masonry`
)
levels(item229.table$HomeType)
rowOrder <- c("Low-Rise (1-3)"
              ,"Mid-Rise (4-6)"
              ,"High-Rise (7+)")
item229.table <- item229.table %>% mutate(HomeType = factor(HomeType, levels = rowOrder)) %>% arrange(HomeType)  
item229.table <- data.frame(item229.table[which(colnames(item229.table) %notin% c("BuildingType"))])
exportTable(item229.table, "MF", "Table 21", weighted = FALSE)






#############################################################################################
#Item 230: DISTRIBUTION OF WALL AREA BY STRUCTURAL SYSTEM AND WALL TYPE IN RIGID FRAME BUILDINGS (MF Table 22)
#############################################################################################
#subset to columns needed for analysis
item230.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"))]
item230.dat$count <- 1

item230.dat0 <- item230.dat[which(item230.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item230.dat1 <- left_join(rbsa.dat, item230.dat0, by = "CK_Cadmus_ID")

#Subset to Multifamily
item230.dat2 <- item230.dat1[grep("Multifamily", item230.dat1$BuildingType),]

item230.dat3 <- item230.dat2[which(item230.dat2$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame %notin% c("N/A",NA,0,"Unknown") & item230.dat2$ENV_Construction_CONSTRUCTION_ConstructionType %notin% c("N/A",NA,0,"Unknown")),]

item230.dat3$Wall.Type.X <- item230.dat3$ENV_Construction_BLDG_STRUCTURE_TypeOfFrame
item230.dat3$Structural.System <- item230.dat3$ENV_Construction_CONSTRUCTION_ConstructionType

################################################
# Adding pop and sample sizes for weights
################################################
item230.data <- weightedData(item230.dat3[-which(colnames(item230.dat3) %in% c("ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"
                                                                               ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                               ,"count"
                                                                               ,"Wall.Type.X"
                                                                               ,"Structural.System"))])
item230.data <- left_join(item230.data, item230.dat3[which(colnames(item230.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"ENV_Construction_BLDG_STRUCTURE_TypeOfFrame"
                                                                                         ,"ENV_Construction_CONSTRUCTION_ConstructionType"
                                                                                         ,"count"
                                                                                         ,"Wall.Type.X"
                                                                                         ,"Structural.System"))])
item230.data$Ind <- 1
item230.data$Count <- 1
colnames(item230.data)

#######################
# Weighted Analysis
#######################
item230.summary <- proportionRowsAndColumns1(CustomerLevelData = item230.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Structural.System'
                                             ,rowVariable = 'Wall.Type.X'
                                             ,aggregateColumnName = "All Systems")
item230.summary <- item230.summary[which(item230.summary$Structural.System != "All Systems"),]

item230.all.systems <- proportions_one_group(CustomerLevelData = item230.data
                                           ,valueVariable = 'Ind'
                                           ,groupingVariable = "Wall.Type.X"
                                           ,total.name = "All Systems"
                                           ,columnName = "Structural.System"
                                           ,weighted = TRUE
                                           ,two.prop.total = TRUE)

item230.final <- rbind.data.frame(item230.summary,item230.all.systems,stringsAsFactors = F)

item230.cast <- dcast(setDT(item230.final)
                      ,formula = BuildingType + Structural.System ~ Wall.Type.X
                      ,value.var = c("w.percent","w.SE","count","n","N","EB"))
item230.cast$n <- item230.cast$n_Total
item230.cast <- data.frame(item230.cast)
item230.table <- item230.cast[-grep("Total|buildingtype",names(item230.cast),ignore.case = T)]

exportTable(item230.table, "MF","Table 22",weighted = TRUE)

#######################
# Weighted Analysis
#######################
item230.summary <- proportions_two_groups_unweighted(CustomerLevelData = item230.data
                                             ,valueVariable = 'Ind'
                                             ,columnVariable = 'Structural.System'
                                             ,rowVariable = 'Wall.Type.X'
                                             ,aggregateColumnName = "All Systems")
item230.summary <- item230.summary[which(item230.summary$Structural.System != "All Systems"),]

item230.all.systems <- proportions_one_group(CustomerLevelData = item230.data
                                             ,valueVariable = 'Ind'
                                             ,groupingVariable = "Wall.Type.X"
                                             ,total.name = "All Systems"
                                             ,columnName = "Structural.System"
                                             ,weighted = FALSE
                                             ,two.prop.total = TRUE)

item230.final <- rbind.data.frame(item230.summary,item230.all.systems,stringsAsFactors = F)

item230.cast <- dcast(setDT(item230.final)
                      ,formula = BuildingType + Structural.System ~ Wall.Type.X
                      ,value.var = c("Percent","SE","Count","n"))
item230.cast$n <- item230.cast$n_Total
item230.cast <- data.frame(item230.cast)
item230.table <- item230.cast[-grep("Total|buildingtype",names(item230.cast),ignore.case = T)]

exportTable(item230.table, "MF","Table 22",weighted = FALSE)
