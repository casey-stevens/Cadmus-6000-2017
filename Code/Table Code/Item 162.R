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
# Item 162: PERCENTAGE OF ELECTRICALLY HEATED HOMES WITH FLOOR AREA OVER CRAWLSPACE BY STATE (SF table B-7)
#############################################################################################
#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item162.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item162.dat.11 <- item162.dat.1[which(item162.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item162.dat.12 <- item162.dat.11[which(item162.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item162.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item162.dat.12$Primary.Heating.System)
item162.dat.12$count <- 1

item162.dat.13 <- unique(item162.dat.12[which(item162.dat.12$Heating.Fuel == "Electric"),])

item162.sum1 <- summarise(group_by(item162.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item162.sum1$Count <- 1
which(duplicated(item162.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item162.sum1$Heating.Fuel)

item162.mechanical <- item162.sum1






#############################################################################################
# Similar to Item 22 -- Not completed yet
#############################################################################################
#
#For Envelope information
#
#############################################################################################
item162.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                ,"ENV_Construction_BLDG_STRUCTURE_FoundationType"
                                                                ,""))]

item162.dat0 <- left_join(item162.dat, item162.mechanical, by = "CK_Cadmus_ID")


item162.dat0.1 <- left_join(rbsa.dat, item162.dat0, by = "CK_Cadmus_ID")
item162.dat1 <- item162.dat0.1[which(item162.dat0.1$Heating.Fuel == "Electric"),]
length(unique(item162.dat1$CK_Cadmus_ID))#326

#subset to only single family homes
item162.dat2 <- item162.dat1[which(item162.dat1$BuildingType == "Single Family"),]




