#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #565

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Read in data for analysis
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))



#############################################################################################
# Item 163: DISTRIBUTION OF FLOOR INSULATION, ELECTRICALLY HEATED HOMES (SF table B-8)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item163.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item163.dat.11 <- item163.dat.1[which(item163.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item163.dat.12 <- item163.dat.11[which(item163.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item163.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item163.dat.12$Primary.Heating.System)
item163.dat.12$count <- 1

item163.dat.13 <- unique(item163.dat.12[which(item163.dat.12$Heating.Fuel == "Electric"),])

item163.sum1 <- summarise(group_by(item163.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item163.sum1$Count <- 1
which(duplicated(item163.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item163.sum1$Heating.Fuel)

item163.mechanical <- item163.sum1






#############################################################################################
# Similar to Item 23
#############################################################################################
#
#For Envelope information
#
#############################################################################################
item163.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_FoundationType"
                                                                ,""))]

item163.dat0 <- left_join(item163.dat, item163.mechanical, by = "CK_Cadmus_ID")


item163.dat0.1 <- left_join(rbsa.dat, item163.dat0, by = "CK_Cadmus_ID")
item163.dat1 <- item163.dat0.1[which(item163.dat0.1$Heating.Fuel == "Electric"),]
length(unique(item163.dat1$CK_Cadmus_ID))#326

#subset to only single family homes
item163.dat2 <- item163.dat1[which(item163.dat1$BuildingType == "Single Family"),]
