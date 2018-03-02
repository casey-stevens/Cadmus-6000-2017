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
rbsa.dat.MF <- rbsa.dat[which(rbsa.dat$BuildingType == "Multifamily"),]

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))





#############################################################################################
#Item 284: IN-UNIT DUCT CHARACTERISTICS (MF Table 76)
#############################################################################################

item284.dat <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                    ,"Generic"
                                                                    ,"System.Type"
                                                                    ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                    ,"Duct.Insulation.Condition"
                                                                    ,"Duct.Plenum.Insulation.Thickness.1"
                                                                    ,"Duct.Plenum.Insulation.Thickness.2"
                                                                    ,"Duct.Plenum.Insulation.Thickness.3"
                                                                    ,"Duct.Plenum.Insulation.Type.1"
                                                                    ,"Duct.Plenum.Insulation.Type.2"
                                                                    ,"Duct.Plenum.Insulation.Type.3"
                                                                    ,"Duct.Runs.Insulation.Type.1"
                                                                    ,"Duct.Runs.Insulation.Type.2"
                                                                    ,"Duct.Runs.Insulation.Type.3"
                                                                    ,""))]

item284.dat1 <- unique(item284.dat[which(item284.dat$Generic == "Ducting"),])

item284.dat2 <- left_join(rbsa.dat.MF, item284.dat1,  by = "CK_Cadmus_ID")

#####################################################
# For Units with Duct Systems
#####################################################
item284.dat2$Ind <- 0
item284.dat2$Ind[which(item284.dat2$Generic == "Ducting")] <- 1

######################################
#Pop and Sample Sizes for weights
######################################
item284.data <- weightedData(item284.dat2[which(colnames(item284.dat2) %notin% c("Generic"
                                                                                 ,"System.Type"
                                                                                 ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                 ,"Duct.Insulation.Condition"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                 ,"Duct.Plenum.Insulation.Type.1"
                                                                                 ,"Duct.Plenum.Insulation.Type.2"
                                                                                 ,"Duct.Plenum.Insulation.Type.3"
                                                                                 ,"Duct.Runs.Insulation.Type.1"
                                                                                 ,"Duct.Runs.Insulation.Type.2"
                                                                                 ,"Duct.Runs.Insulation.Type.3"
                                                                                 ,"Ind"))])

item284.data <- left_join(item284.data, item284.dat2[which(colnames(item284.dat2) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"System.Type"
                                                                                         ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                         ,"Duct.Insulation.Condition"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                         ,"Duct.Plenum.Insulation.Type.1"
                                                                                         ,"Duct.Plenum.Insulation.Type.2"
                                                                                         ,"Duct.Plenum.Insulation.Type.3"
                                                                                         ,"Duct.Runs.Insulation.Type.1"
                                                                                         ,"Duct.Runs.Insulation.Type.2"
                                                                                         ,"Duct.Runs.Insulation.Type.3"
                                                                                         ,"Ind"))])
item284.data$count <- 1
item284.data$Count <- 1
######################################
# weighted analysis
######################################
item284.units.with.ducts <- proportions_one_group(CustomerLevelData = item284.data
                                                  ,valueVariable = 'Ind'
                                                  ,groupingVariable = "BuildingType"
                                                  ,total.name = "Remove")

######################################
# unweighted analysis
######################################
item284.units.with.ducts.unw <- proportions_one_group(CustomerLevelData = item284.data
                                                      ,valueVariable = 'Ind'
                                                      ,groupingVariable = "BuildingType"
                                                      ,total.name = "Remove"
                                                      ,weighted = FALSE)




#####################################################
# For Percentage of Ducts in Unconditioned Space
#####################################################
item284.dat2$Percentage.of.Supply.Ducts.in.Unconditioned.Space <- 1 - as.numeric(as.character(item284.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space)) / 100
item284.dat.percent <- item284.dat2[which(!is.na(item284.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space)),]

######################################
#Pop and Sample Sizes for weights
######################################
item284.data <- weightedData(item284.dat.percent[which(colnames(item284.dat.percent) %notin% c("Generic"
                                                                                               ,"System.Type"
                                                                                               ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                               ,"Duct.Insulation.Condition"
                                                                                               ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                               ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                               ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                               ,"Duct.Plenum.Insulation.Type.1"
                                                                                               ,"Duct.Plenum.Insulation.Type.2"
                                                                                               ,"Duct.Plenum.Insulation.Type.3"
                                                                                               ,"Duct.Runs.Insulation.Type.1"
                                                                                               ,"Duct.Runs.Insulation.Type.2"
                                                                                               ,"Duct.Runs.Insulation.Type.3"
                                                                                               ,"Percentage.of.Supply.Ducts.in.Unconditioned.Space"))])

item284.data <- left_join(item284.data, item284.dat.percent[which(colnames(item284.dat.percent) %in% c("CK_Cadmus_ID"
                                                                                                       ,"Generic"
                                                                                                       ,"System.Type"
                                                                                                       ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                                       ,"Duct.Insulation.Condition"
                                                                                                       ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                                       ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                                       ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                                       ,"Duct.Plenum.Insulation.Type.1"
                                                                                                       ,"Duct.Plenum.Insulation.Type.2"
                                                                                                       ,"Duct.Plenum.Insulation.Type.3"
                                                                                                       ,"Duct.Runs.Insulation.Type.1"
                                                                                                       ,"Duct.Runs.Insulation.Type.2"
                                                                                                       ,"Duct.Runs.Insulation.Type.3"
                                                                                                       ,"Percentage.of.Supply.Ducts.in.Unconditioned.Space"))])
item284.data$count <- 1

######################################
# weighted analysis
######################################
item284.percent.ducting <- mean_one_group(CustomerLevelData = item284.data
                                          ,valueVariable = 'Percentage.of.Supply.Ducts.in.Unconditioned.Space'
                                          ,byVariable = "BuildingType"
                                          ,aggregateRow = "Remove")

######################################
# unweighted analysis
######################################
item284.percent.ducting.unw <- mean_one_group_unweighted(CustomerLevelData = item284.data
                                                         ,valueVariable = 'Percentage.of.Supply.Ducts.in.Unconditioned.Space'
                                                         ,byVariable = "BuildingType"
                                                         ,aggregateRow = "Remove")





#####################################################
# For Duct Insulation
#####################################################
item284.dat3 <- item284.dat2[which(item284.dat2$Generic == "Ducting"),]
item284.dat3 <- item284.dat3[which(item284.dat3$System.Type %notin% c("Ducting Unknown","All Metal")),]

item284.dat3$RValue <- "None"
item284.dat3$RValue[grep("R4|R6|R8|Unknown",item284.dat3$Duct.Runs.Insulation.Type.1)] <- item284.dat3$Duct.Runs.Insulation.Type.1[grep("R4|R6|R8|Unknown",item284.dat3$Duct.Runs.Insulation.Type.1)]
unique(item284.dat3$RValue)
item284.dat3$RValue <- gsub("R","",item284.dat3$RValue)
item284.dat3$RValueBins <- "Duct Insulation: None"
item284.dat3$RValueBins[grep("R4",item284.dat3$Duct.Runs.Insulation.Type.1)] <- "Duct Insulation: R1-R4"
item284.dat3$RValueBins[grep("R6|R8",item284.dat3$Duct.Runs.Insulation.Type.1)] <- "Duct Insulation: R5+"
item284.dat3$RValueBins[grep("Unknown",item284.dat3$Duct.Runs.Insulation.Type.1)] <- "Duct Insulation: Unknown"


######################################
#Pop and Sample Sizes for weights
######################################
item284.data <- weightedData(item284.dat3[which(colnames(item284.dat3) %notin% c("Generic"
                                                                                 ,"System.Type"
                                                                                 ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                 ,"Duct.Insulation.Condition"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                 ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                 ,"Duct.Plenum.Insulation.Type.1"
                                                                                 ,"Duct.Plenum.Insulation.Type.2"
                                                                                 ,"Duct.Plenum.Insulation.Type.3"
                                                                                 ,"Duct.Runs.Insulation.Type.1"
                                                                                 ,"Duct.Runs.Insulation.Type.2"
                                                                                 ,"Duct.Runs.Insulation.Type.3"
                                                                                 ,"Percentage.of.Supply.Ducts.in.Unconditioned.Space"
                                                                                 ,"Ind"
                                                                                 ,"RValue"
                                                                                 ,"RValueBins"))])

item284.data <- left_join(item284.data, item284.dat3[which(colnames(item284.dat3) %in% c("CK_Cadmus_ID"
                                                                                         ,"Generic"
                                                                                         ,"System.Type"
                                                                                         ,"Percentage.of.Supply.Ducts.in.Conditioned.Space"
                                                                                         ,"Duct.Insulation.Condition"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.1"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.2"
                                                                                         ,"Duct.Plenum.Insulation.Thickness.3"
                                                                                         ,"Duct.Plenum.Insulation.Type.1"
                                                                                         ,"Duct.Plenum.Insulation.Type.2"
                                                                                         ,"Duct.Plenum.Insulation.Type.3"
                                                                                         ,"Duct.Runs.Insulation.Type.1"
                                                                                         ,"Duct.Runs.Insulation.Type.2"
                                                                                         ,"Duct.Runs.Insulation.Type.3"
                                                                                         ,"Percentage.of.Supply.Ducts.in.Unconditioned.Space"
                                                                                         ,"Ind"
                                                                                         ,"RValue"
                                                                                         ,"RValueBins"))])
item284.data$count <- 1

######################################
# weighted analysis
######################################
item284.insulation <- proportions_one_group(CustomerLevelData = item284.data
                                            ,valueVariable = 'count'
                                            ,groupingVariable = 'RValueBins'
                                            ,total.name = "Remove")

######################################
# unweighted analysis
######################################
item284.insulation.unw <- proportions_one_group(CustomerLevelData = item284.data
                                                ,valueVariable = 'count'
                                                ,groupingVariable = 'RValueBins'
                                                ,total.name = "Remove"
                                                ,weighted = FALSE)






######################################
# COMBINE WEIGHTED RESULTS
######################################
item284.units.with.ducts$Category <- "Units with Duct Systems"
item284.units.with.ducts <- item284.units.with.ducts[which(names(item284.units.with.ducts) %in% c("Category","BuidingType","w.percent","w.SE","n","EB"))]

item284.percent.ducting <- item284.percent.ducting[which(names(item284.percent.ducting) %in% c("Category","BuidingType","Mean","SE","n","EB"))]
names(item284.percent.ducting)[which(names(item284.percent.ducting) %in% c("Mean","SE"))] <- c("w.percent","w.SE")

names(item284.insulation)[which(names(item284.insulation) == "RValueBins")] <- "Category"
item284.insulation <- item284.insulation[which(item284.insulation$Category != "Total"),]
item284.insulation <- item284.insulation[which(names(item284.insulation) %in% c("Category","BuidingType","w.percent","w.SE","n","EB"))]


item284.weighted <- rbind.data.frame(item284.units.with.ducts
                                     ,item284.percent.ducting
                                     ,item284.insulation)

item284.table <- data.frame("Category" = item284.weighted$Category
                            ,"Percent" = item284.weighted$w.percent
                            ,"SE" = item284.weighted$w.SE
                            ,"n" = item284.weighted$n
                            ,"EB" = item284.weighted$EB)

exportTable(item284.table,"MF","Table 76",weighted = TRUE)

######################################
# COMBINE UNWEIGHTED RESULTS
######################################
item284.units.with.ducts.unw$Category <- "Units with Duct Systems"
item284.units.with.ducts.unw <- item284.units.with.ducts.unw[which(names(item284.units.with.ducts.unw) %in% c("Category","BuildingType","Percent","SE","n"))]

item284.percent.ducting.unw <- item284.percent.ducting.unw[which(names(item284.percent.ducting.unw) %in% c("Category","BuildingType","Mean","SE","n"))]
names(item284.percent.ducting.unw)[which(names(item284.percent.ducting.unw) == "Mean")] <- "Percent"

names(item284.insulation.unw)[which(names(item284.insulation.unw) == "RValueBins")] <- "Category"
item284.insulation.unw <- item284.insulation.unw[which(names(item284.insulation.unw) %in% c("Category","BuildingType","Percent","SE","n"))]

item284.unweighted <- rbind.data.frame(item284.units.with.ducts.unw
                                     ,item284.percent.ducting.unw
                                     ,item284.insulation.unw)

item284.table.unw <- data.frame("Category" = item284.unweighted$Category
                            ,"Percent" = item284.unweighted$Percent
                            ,"SE" = item284.unweighted$SE
                            ,"n" = item284.unweighted$n)

exportTable(item284.table.unw,"MF","Table 76",weighted = FALSE)
