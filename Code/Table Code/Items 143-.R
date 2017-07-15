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
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
#clean cadmus IDs
sites.interview.dat$CK_Cadmus_ID <- trimws(toupper(sites.interview.dat$CK_Cadmus_ID))





#############################################################################################
#Item :  (SF table , MH table )
#############################################################################################
#subset to columns needed for analysis
item139.dat <- unique(sites.interview.dat[which(colnames(sites.interview.dat) %in% c("CK_Cadmus_ID"
                                                                                     ,"INTRVW_CUST_RES_ConservationEffortsConversation_OnYourOwn_LastFewYears_Y_N"
                                                                                     ,""))])

colnames(item139.dat) <- c("CK_Cadmus_ID", "Reporting")
#remove any repeat header rows from exporting
item139.dat0 <- item139.dat[which(item139.dat$CK_Cadmus_ID != "CK_CADMUS_ID"),]

item139.dat0.5 <- item139.dat0[which(!(is.na(item139.dat0$Reporting))),]
item139.dat1   <- item139.dat0.5[which(item139.dat0.5$Reporting != "Unknown"),]


item139.dat2 <- left_join(item139.dat1, rbsa.dat, by = "CK_Cadmus_ID")
item139.dat2$count <- 1
