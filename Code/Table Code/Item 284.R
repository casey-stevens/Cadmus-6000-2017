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
# For Percentage of Ducts in Unconditioned Space
#####################################################
item284.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space <- as.numeric(as.character(item284.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space)) / 100
item284.dat.percent <- item284.dat2[which(!is.na(item284.dat2$Percentage.of.Supply.Ducts.in.Conditioned.Space)),]
item284.dat.percent <- item284.dat.percent[grep("site",item284.dat.percent$CK_Building_ID,ignore.case = T),]

mean(item284.dat.percent$Percentage.of.Supply.Ducts.in.Conditioned.Space)
