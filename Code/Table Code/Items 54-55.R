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
# Mechanical
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))
#subset to columns needed for analysis
mechanical.dat1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                        ,"Generic"
                                                                        ,"Heating.Fuel"
                                                                        ,"Heating.Efficiency.-.High"
                                                                        ,"Component.1.Year.of.Manufacture"
                                                                        ,"HSPF"))]
#fix capitalization error
mechanical.dat1$Heating.Fuel[which(mechanical.dat1$Heating.Fuel == "Natural gas")] <- "Natural Gas"
#remove any irrelevant equipment vintages (datapoint not asked for)
mechanical.dat2 <- mechanical.dat1[which(mechanical.dat1$Component.1.Year.of.Manufacture != "-- Datapoint not asked for --"),]
#remove any NA equipment vintages
mechanical.dat3 <- mechanical.dat2[which(!(is.na(mechanical.dat2$Component.1.Year.of.Manufacture))),]
