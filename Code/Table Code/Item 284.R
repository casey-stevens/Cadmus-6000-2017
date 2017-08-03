#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

##  Clear variables
# rm(list=ls())

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

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

item284.dat2 <- left_join(item284.dat1, rbsa.dat, by = "CK_Cadmus_ID")

