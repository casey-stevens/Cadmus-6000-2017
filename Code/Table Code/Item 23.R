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
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

###################################################################################################################
#
#
# This will be used for item 23
#
#
###################################################################################################################


#############################################################################################
# Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (SF table 30) (also need MH)
#############################################################################################
#subset envelope data to necessary columns
prep.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Floor.Type"
                                                               , "Floor.Sub-Type"
                                                               , "Floor.Area"
                                                               , "Floor.Insulated?"
                                                               , "Floor.Insulation.Type.1"
                                                               , "Floor.Insulation.Thickness.1"
                                                               , "Floor.Insulation.Condition.1"
                                                               , "Floor.Insulation.Type.2"                                                  
                                                               , "Floor.Insulation.Thickness.2"
                                                               , "Floor.Insulation.Condition.2"
                                                               , "Floor.Insulation.Type.3"
                                                               , "Floor.Insulation.Thickness.3"
                                                               , "Floor.Insulation.Condition.3"
                                                               , "Slab.Insulated?"
                                                               , "Slab.Insulation.Type.1"
                                                               , "Slab.Insulation.Thickness.1"
                                                               , "Slab.Insulation.Condition.1"
                                                               , "Slab.Insulation.Type.2"                                                  
                                                               , "Slab.Insulation.Thickness.2"
                                                               , "Slab.Insulation.Condition.2"
                                                               , "Slab.Insulation.Type.3"
                                                               , "Slab.Insulation.Thickness.3"
                                                               , "Slab.Insulation.Condition.3"
))]

prep.dat0 <- prep.dat[which(prep.dat$`Floor.Insulated?` %in% c("Yes", "No")),]
unique(prep.dat0$`Floor.Insulated?`)

prep.dat0$`Slab.Insulated?`[which(prep.dat0$`Slab.Insulated?` != "Yes")] <- "No" ###treat anything not Yes as No

prep.dat1.1 <- prep.dat0[which(prep.dat0$Floor.Insulation.Thickness.1 != "Unknown"),]
prep.dat1.2 <- prep.dat1.1[-which(prep.dat1.1$Slab.Insulation.Thickness.1 == "Unknown"),]

#review types
unique(prep.dat1.2$Floor.Insulation.Type.1)
unique(prep.dat1.2$Floor.Insulation.Type.2)
unique(prep.dat1.2$Floor.Insulation.Type.3) #nothing in this column
unique(prep.dat1.2$Slab.Insulation.Type.1)
unique(prep.dat1.2$Slab.Insulation.Type.2) #nothing in this column
unique(prep.dat1.2$Slab.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(prep.dat1.2$Floor.Insulation.Thickness.1)
unique(prep.dat1.2$Floor.Insulation.Thickness.2)
unique(prep.dat1.2$Floor.Insulation.Thickness.3)
unique(prep.dat1.2$Slab.Insulation.Thickness.1)
unique(prep.dat1.2$Slab.Insulation.Thickness.2)
unique(prep.dat1.2$Slab.Insulation.Thickness.3)

#review conditions
unique(prep.dat1.2$Floor.Insulation.Condition.1)
unique(prep.dat1.2$Floor.Insulation.Condition.2)
unique(prep.dat1.2$Floor.Insulation.Condition.3)
unique(prep.dat1.2$Slab.Insulation.Condition.1)
unique(prep.dat1.2$Slab.Insulation.Condition.2)
unique(prep.dat1.2$Slab.Insulation.Condition.3)

#assign new dataset
prep.dat3 <- prep.dat1.2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(prep.dat3)){
  prep.dat3[,i] <- ifelse(prep.dat3[,i] == "-- Datapoint not asked for --", NA, prep.dat3[,i])
}

#cleaning for wall.cavity
prep.dat3$Floor.Insulation.Thickness.1[which(prep.dat3$Floor.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Floor.Insulation.Thickness.1[which(is.na(prep.dat3$Floor.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Floor.Insulation.Thickness.2[which(prep.dat3$Floor.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Floor.Insulation.Thickness.2[which(prep.dat3$Floor.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Floor.Insulation.Thickness.2[which(is.na(prep.dat3$Floor.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Floor.Insulation.Thickness.3[which(prep.dat3$Floor.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
prep.dat3$Floor.Insulation.Thickness.3[which(prep.dat3$Floor.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Floor.Insulation.Thickness.3[which(is.na(prep.dat3$Floor.Insulation.Thickness.3))] <- "N/A N/A"
unique(prep.dat3$Floor.Insulation.Thickness.1)
unique(prep.dat3$Floor.Insulation.Thickness.2)
unique(prep.dat3$Floor.Insulation.Thickness.3)

#cleaning for wall exterior
prep.dat3$Slab.Insulation.Thickness.1[which(prep.dat3$Slab.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
prep.dat3$Slab.Insulation.Thickness.1[which(is.na(prep.dat3$Slab.Insulation.Thickness.1))] <- "N/A N/A"
prep.dat3$Slab.Insulation.Thickness.2[which(prep.dat3$Slab.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
prep.dat3$Slab.Insulation.Thickness.2[which(is.na(prep.dat3$Slab.Insulation.Thickness.2))] <- "N/A N/A"
prep.dat3$Slab.Insulation.Thickness.3[which(prep.dat3$Slab.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
prep.dat3$Slab.Insulation.Thickness.3[which(is.na(prep.dat3$Slab.Insulation.Thickness.3))] <- "N/A N/A"
unique(prep.dat3$Slab.Insulation.Thickness.1)
unique(prep.dat3$Slab.Insulation.Thickness.2)
unique(prep.dat3$Slab.Insulation.Thickness.3)


#Clean Condition unknown values
prep.dat3$Floor.Insulation.Condition.1[which(prep.dat3$Floor.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Floor.Insulation.Condition.2[which(prep.dat3$Floor.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Floor.Insulation.Condition.3[which(prep.dat3$Floor.Insulation.Condition.3 == "Unknown")] <- "100%"
prep.dat3$Slab.Insulation.Condition.1[which(prep.dat3$Slab.Insulation.Condition.1 == "Unknown")] <- "100%"
prep.dat3$Slab.Insulation.Condition.2[which(prep.dat3$Slab.Insulation.Condition.2 == "Unknown")] <- "100%"
prep.dat3$Slab.Insulation.Condition.3[which(prep.dat3$Slab.Insulation.Condition.3 == "Unknown")] <- "100%"

prep.dat3$Floor.Insulation.Condition.1[which(is.na(prep.dat3$Floor.Insulation.Condition.1) & prep.dat3$Floor.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Floor.Insulation.Condition.2[which(is.na(prep.dat3$Floor.Insulation.Condition.2) & prep.dat3$Floor.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Floor.Insulation.Condition.3[which(is.na(prep.dat3$Floor.Insulation.Condition.3) & prep.dat3$Floor.Insulation.Thickness.3 != "N/A N/A")] <- "100%"
prep.dat3$Slab.Insulation.Condition.1[which(is.na(prep.dat3$Slab.Insulation.Condition.1) & prep.dat3$Slab.Insulation.Thickness.1 != "N/A N/A")] <- "100%"
prep.dat3$Slab.Insulation.Condition.2[which(is.na(prep.dat3$Slab.Insulation.Condition.2) & prep.dat3$Slab.Insulation.Thickness.2 != "N/A N/A")] <- "100%"
prep.dat3$Slab.Insulation.Condition.3[which(is.na(prep.dat3$Slab.Insulation.Condition.3) & prep.dat3$Slab.Insulation.Thickness.3 != "N/A N/A")] <- "100%"

prep.dat3$Floor.Insulation.Condition.1[which(is.na(prep.dat3$Floor.Insulation.Condition.1) & prep.dat3$Floor.Insulation.Thickness.1 == "N/A N/A")] <- "0%"
prep.dat3$Floor.Insulation.Condition.2[which(is.na(prep.dat3$Floor.Insulation.Condition.2) & prep.dat3$Floor.Insulation.Thickness.2 == "N/A N/A")] <- "0%"
prep.dat3$Floor.Insulation.Condition.3[which(is.na(prep.dat3$Floor.Insulation.Condition.3) & prep.dat3$Floor.Insulation.Thickness.3 == "N/A N/A")] <- "0%"
prep.dat3$Slab.Insulation.Condition.1[which(is.na(prep.dat3$Slab.Insulation.Condition.1) & prep.dat3$Slab.Insulation.Thickness.1 == "N/A N/A")] <- "0%"
prep.dat3$Slab.Insulation.Condition.2[which(is.na(prep.dat3$Slab.Insulation.Condition.2) & prep.dat3$Slab.Insulation.Thickness.2 == "N/A N/A")] <- "0%"
prep.dat3$Slab.Insulation.Condition.3[which(is.na(prep.dat3$Slab.Insulation.Condition.3) & prep.dat3$Slab.Insulation.Thickness.3 == "N/A N/A")] <- "0%"

prep.dat3$Floor.Insulation.Condition.1[which(prep.dat3$`Floor.Insulated?` == "No")] <- "0%"
prep.dat3$Slab.Insulation.Condition.1[which(prep.dat3$`Floor.Insulated?` == "No")] <- "0%"
unique(prep.dat3$Floor.Insulation.Condition.1)
unique(prep.dat3$Slab.Insulation.Condition.1)

#remove percent signs and make numeric
prep.dat3$Floor.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Floor.Insulation.Condition.1)
prep.dat3$Floor.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Floor.Insulation.Condition.1))
prep.dat3$Floor.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Floor.Insulation.Condition.2)
prep.dat3$Floor.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Floor.Insulation.Condition.2))
prep.dat3$Floor.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Floor.Insulation.Condition.3)
prep.dat3$Floor.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Floor.Insulation.Condition.3))

prep.dat3$Slab.Insulation.Condition.1 <- gsub("%", "", prep.dat3$Slab.Insulation.Condition.1)
prep.dat3$Slab.Insulation.Condition.1 <- as.numeric(as.character(prep.dat3$Slab.Insulation.Condition.1))
prep.dat3$Slab.Insulation.Condition.2 <- gsub("%", "", prep.dat3$Slab.Insulation.Condition.2)
prep.dat3$Slab.Insulation.Condition.2 <- as.numeric(as.character(prep.dat3$Slab.Insulation.Condition.2))
prep.dat3$Slab.Insulation.Condition.3 <- gsub("%", "", prep.dat3$Slab.Insulation.Condition.3)
prep.dat3$Slab.Insulation.Condition.3 <- as.numeric(as.character(prep.dat3$Slab.Insulation.Condition.3))


# add new ID variable for merging -- don't know if we need this
prep.dat3$count <- 1
prep.dat3$TMP_ID <- cumsum(prep.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(prep.dat3$Floor.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(prep.dat3$Floor.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(prep.dat3$Floor.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)

clean.insul1.0 <- unlist(strsplit(prep.dat3$Slab.Insulation.Thickness.1, " "))
clean.insul1.00 <- as.data.frame(matrix(clean.insul1.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul1.00)
dim(clean.insul1.2)

clean.insul2.0 <- unlist(strsplit(prep.dat3$Slab.Insulation.Thickness.2, " "))
clean.insul2.00 <- as.data.frame(matrix(clean.insul2.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul2.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul2.00)
dim(clean.insul2.2)

clean.insul3.0 <- unlist(strsplit(prep.dat3$Slab.Insulation.Thickness.3, " "))
clean.insul3.00 <- as.data.frame(matrix(clean.insul3.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.2 <- cbind.data.frame("CK_Cadmus_ID" = prep.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = prep.dat3$TMP_ID
                                   , clean.insul3.00)
dim(clean.insul3.2)

clean.insul.join1 <- left_join(clean.insul1.1,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join3 <- left_join(clean.insul.join2, clean.insul1.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join4 <- left_join(clean.insul.join3, clean.insul2.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join5 <- left_join(clean.insul.join4, clean.insul3.2, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join5) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"floor.inches1"
                                 ,"Remove.1"
                                 ,"floor.inches2"
                                 ,"Remove.2"
                                 ,"floor.inches3"
                                 ,"Remove.3"
                                 ,"slab.inches1"
                                 ,"Remove.1"
                                 ,"slab.inches2"
                                 ,"Remove.2"
                                 ,"slab.inches3"
                                 ,"Remove.3")

clean.thickness.data <- clean.insul.join5[-grep("Remove", colnames(clean.insul.join5))]

###########################
# End cleaning step
###########################

#make into dataframe
prep.dat4 <- as.data.frame(left_join(prep.dat3, clean.thickness.data, by = c("CK_Cadmus_ID", "TMP_ID"))
                           , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# make numeric
prep.dat4$floor.inches1   <- as.numeric(as.character(prep.dat4$floor.inches1)) # warning here is OK
prep.dat4$floor.inches2   <- as.numeric(as.character(prep.dat4$floor.inches2)) # warning here is OK
prep.dat4$floor.inches3   <- as.numeric(as.character(prep.dat4$floor.inches3)) # warning here is OK
prep.dat4$slab.inches1 <- as.numeric(as.character(prep.dat4$slab.inches1)) # warning here is OK
prep.dat4$slab.inches2 <- as.numeric(as.character(prep.dat4$slab.inches2)) # warning here is OK
prep.dat4$slab.inches3 <- as.numeric(as.character(prep.dat4$slab.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in grep(".inches", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}

#update column names
prep.dat4$floor.rvalues1 <- prep.dat4$Floor.Insulation.Type.1
prep.dat4$floor.rvalues2 <- prep.dat4$Floor.Insulation.Type.2
prep.dat4$floor.rvalues3 <- prep.dat4$Floor.Insulation.Type.3
prep.dat4$slab.rvalues1 <- prep.dat4$Slab.Insulation.Type.1
prep.dat4$slab.rvalues2 <- prep.dat4$Slab.Insulation.Type.2
prep.dat4$slab.rvalues3 <- prep.dat4$Slab.Insulation.Type.3


unique(prep.dat4$floor.rvalues1)
#fix names that are not in R value table
prep.dat4$floor.rvalues1[which(prep.dat4$floor.rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
prep.dat4$floor.rvalues2[which(prep.dat4$floor.rvalues2 == "N/A")]                              <- NA

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  prep.dat4$floor.rvalues1[which(prep.dat4$floor.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$floor.rvalues2[which(prep.dat4$floor.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$floor.rvalues3[which(prep.dat4$floor.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues1[which(prep.dat4$slab.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues2[which(prep.dat4$slab.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  prep.dat4$slab.rvalues3[which(prep.dat4$slab.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

prep.dat4$floor.rvalues1[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$floor.rvalues2[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$floor.rvalues3[which(prep.dat4$`Floor.Insulated?` == "No")] <- 0
prep.dat4$slab.rvalues1[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0
prep.dat4$slab.rvalues2[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0
prep.dat4$slab.rvalues3[which(prep.dat4$`Slab.Insulated?`   == "No")] <- 0
prep.dat4$floor.inches1[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$floor.inches2[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$floor.inches3[which(prep.dat4$`Floor.Insulated?`  == "No")] <- 0
prep.dat4$slab.inches1[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0
prep.dat4$slab.inches2[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0
prep.dat4$slab.inches3[which(prep.dat4$`Slab.Insulated?`    == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- ifelse(is.na(prep.dat4[,i]), 0, prep.dat4[,i])
}


#make all inches and rvalue columns numeric
for(i in grep(".inches|.rvalues", colnames(prep.dat4))){
  prep.dat4[,i] <- as.numeric(as.character(prep.dat4[,i]))
}

prep.dat4.5 <- prep.dat4


#create total.r.value column
prep.dat4.5$total.r.val <- NA


#check uniques -- None should be NA
unique(prep.dat4.5$floor.rvalues1)
unique(prep.dat4.5$floor.rvalues2)
unique(prep.dat4.5$floor.rvalues3)
unique(prep.dat4.5$slab.rvalues1)
unique(prep.dat4.5$slab.rvalues2)
unique(prep.dat4.5$slab.rvalues3)

# clean up condition information
prep.condition.sub1 <- prep.dat4.5[which(prep.dat4.5$Floor.Insulation.Condition.1 %notin% c(100, NA)),]
prep.condition.sub1$Floor.Insulation.Condition.1 <- 100 - prep.condition.sub1$Floor.Insulation.Condition.1
prep.condition.sub1$total.r.val <- NA

prep.dat5 <- rbind.data.frame(prep.dat4.5
                              ,prep.condition.sub1
                              , stringsAsFactors = F)

###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################


#calculate the weighted r value
na.ind <- which(is.na(prep.dat5$total.r.val))
prep.dat5$total.r.val[na.ind] <- (prep.dat5$floor.rvalues1[na.ind] * prep.dat5$floor.inches1[na.ind]) +  
  (prep.dat5$floor.rvalues2[na.ind] * prep.dat5$floor.inches2[na.ind]) +  
  (prep.dat5$floor.rvalues3[na.ind] * prep.dat5$floor.inches3[na.ind]) + 
  (prep.dat5$slab.rvalues1[na.ind] * prep.dat5$slab.inches1[na.ind]) + 
  (prep.dat5$slab.rvalues2[na.ind] * prep.dat5$slab.inches2[na.ind]) + 
  (prep.dat5$slab.rvalues3[na.ind] * prep.dat5$slab.inches3[na.ind])

#check -- NOTE -- NONE SHOULD BE NA
unique(prep.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
prep.dat5$uvalue <- 1 / (1 + prep.dat5$total.r.val)
unique(prep.dat5$uvalue)

#make area numeric
prep.dat5$uvalue    <- as.numeric(as.character(prep.dat5$uvalue))
prep.dat5$Floor.Area <- as.numeric(as.character(prep.dat5$Floor.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(prep.dat5, CK_Cadmus_ID, Floor.Type)
                       ,aveUval = sum(Floor.Area * Floor.Insulation.Condition.1 * uvalue) / sum(Floor.Area * Floor.Insulation.Condition.1)
)

#back-calculate the weight r values
weightedU$aveRval <- (1 / as.numeric(as.character(weightedU$aveUval))) - 1
unique(weightedU$aveRval)

# get unique cadmus IDs and building types for this subset of data
Floor.unique <- unique(prep.dat5[which(colnames(prep.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

# merge on ID and building types to weighted uFactor and rValue data
prep.dat6 <- left_join(weightedU, Floor.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
prep.dat7 <- left_join(prep.dat6, rbsa.dat)
prep.dat7$aveUval[which(is.na(prep.dat7$aveUval))] <- 0
prep.dat7$aveRval[which(is.na(prep.dat7$aveRval))] <- 0

###################################################################################################################
#
#
# End Prep
#
#
###################################################################################################################
## Note: For this table, you must run up to item10.dat7 for the cleaned data
item23.dat <- prep.dat7

item23.dat1 <- item23.dat[which(!(is.na(item23.dat$HomeYearBuilt_bins3))),]

#Bin R values -- SF only
item23.dat1$rvalue.bins.SF <- "Unknown"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval == 0)] <- "None"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >  0  & item23.dat1$aveRval  < 4)]   <- "R1.R3"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 4  & item23.dat1$aveRval  < 11)]  <- "R4.R10"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 11 & item23.dat1$aveRval  < 16)]  <- "R11.R15"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 16 & item23.dat1$aveRval  < 23)]  <- "R16.R22"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 23 & item23.dat1$aveRval  < 28)]  <- "R23.R27"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 28 & item23.dat1$aveRval  < 36)]  <- "R28.R35"
item23.dat1$rvalue.bins.SF[which(item23.dat1$aveRval >= 36)] <- "RGT36"
unique(item23.dat1$rvalue.bins.SF)

#Bin R values -- MH only
item23.dat1$rvalue.bins.MH <- "Unknown"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval > 0  & item23.dat1$aveRval < 9)]    <- "R0.R8"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 9 & item23.dat1$aveRval < 15)]   <- "R9.R14"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 15 & item23.dat1$aveRval < 22)]  <- "R15.R21"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 22 & item23.dat1$aveRval < 31)]  <- "R22.R30"
item23.dat1$rvalue.bins.MH[which(item23.dat1$aveRval >= 31 & item23.dat1$aveRval < 41)]  <- "R31.R40"
unique(item23.dat1$rvalue.bins.MH)


############################################################################################################
# Apply weights
############################################################################################################
item23.dat1$count <- 1
colnames(item23.dat1)

item23.merge <- left_join(rbsa.dat, item23.dat1)
item23.merge <- item23.merge[which(!is.na(item23.merge$count)),]

item23.data <- weightedData(unique(item23.merge[which(colnames(item23.merge) %notin% c("Wall.Type"
                                                                                     ,"aveUval"
                                                                                     ,"aveRval"
                                                                                     ,"rvalue.bins.SF"
                                                                                     ,"rvalue.bins.MH"
                                                                                     ,"count"
                                                                                     ,"Floor.Type"))]))
item23.data <- left_join(item23.data, item23.merge[which(colnames(item23.merge) %in% c("CK_Cadmus_ID"
                                                                                       ,"Wall.Type"
                                                                                       ,"aveUval"
                                                                                       ,"aveRval"
                                                                                       ,"rvalue.bins.SF"
                                                                                       ,"rvalue.bins.MH"
                                                                                       ,"count"
                                                                                       ,"Floor.Type"))])
############################################################################################################
# Summarise - Single Family
############################################################################################################
#summarise by housing vintage
item23.by.vinage <- proportionRowsAndColumns1(item23.data
                                              , valueVariable       = 'count'
                                              , columnVariable      = 'HomeYearBuilt_bins3'
                                              , rowVariable         = 'rvalue.bins.SF'
                                              , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item23.across.vintages <- proportions_one_group(item23.data
                                                , valueVariable    = 'count'
                                                , groupingVariable = 'rvalue.bins.SF'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3')

# row bind summaries
item23.final <- rbind.data.frame(item23.by.vinage
                                 , item23.across.vintages
                                 , stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item23.final <- item23.final[which(item23.final$HomeYearBuilt_bins3 != "Remove"),]



item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 + n + N ~ rvalue.bins.SF,
                     value.var = c("w.percent", "w.SE", "count"))

item23.table <- data.frame("BuildingType"     = item23.cast$BuildingType
                           ,"Housing.Vintage" = item23.cast$HomeYearBuilt_bins3
                           ,"Percent.None"    = item23.cast$w.percent_None
                           ,"SE.None"         = item23.cast$w.SE_None
                           ,"n.None"          = item23.cast$count_None
                           ,"Percent.R1.R3"   = NA #item23.cast$w.percent_R1.R3  
                           ,"SE.R1.R3"        = NA #item23.cast$w.SE_R1.R3
                           ,"n.R1.R3"         = NA #item23.cast$count_R1.R3
                           ,"Percent.R4.R10"  = item23.cast$w.percent_R4.R10  
                           ,"SE.R4.R10"       = item23.cast$w.SE_R4.R10
                           ,"n.R4.R10"        = item23.cast$count_R4.R10
                           ,"Percent.R11.R15" = item23.cast$w.percent_R11.R15
                           ,"SE.R11.R15"      = item23.cast$w.SE_R11.R15
                           ,"n.R11.R15"       = item23.cast$count_R11.R15
                           ,"Percent.R16.R22" = item23.cast$w.percent_R16.R22
                           ,"SE.R16.R22"      = item23.cast$w.SE_R16.R22
                           ,"n.R16.R22"       = item23.cast$count_R16.R22
                           ,"Percent.R23.R27" = item23.cast$w.percent_R23.R27
                           ,"SE.R23.R27"      = item23.cast$w.SE_R23.R27
                           ,"n.R23.R27"       = item23.cast$count_R23.R27
                           ,"Percent.R28.R35" = item23.cast$w.percent_R28.R35
                           ,"SE.R28.R35"      = item23.cast$w.SE_R28.R35
                           ,"n.R28.R35"       = item23.cast$count_R28.R35
                           ,"Percent.RGT36"   = item23.cast$w.percent_RGT36
                           ,"SE.RGT36"        = item23.cast$w.SE_RGT36
                           ,"n.RGT36"         = item23.cast$count_RGT36
                           ,"SampleSize"      = item23.cast$count_Total)


item23.table.SF <- item23.table[which(item23.table$BuildingType == "Single Family"),-1]

#export table to correct workbook using exporting function
exportTable(item23.table.SF, "SF", "Table 30")


############################################################################################################
# Summarise - Manufactured
############################################################################################################
#summarise by housing vintage
item23.by.vinage <- proportionRowsAndColumns1(item23.data
                                              , valueVariable       = 'count'
                                              , columnVariable      = 'HomeYearBuilt_bins3'
                                              , rowVariable         = 'rvalue.bins.MH'
                                              , aggregateColumnName = "Remove"
)
# summarise for all housing vintages
item23.across.vintages <- proportions_one_group(item23.data
                                                , valueVariable    = 'count'
                                                , groupingVariable = 'rvalue.bins.MH'
                                                , total.name       = 'All Housing Vintages'
                                                , columnName       = 'HomeYearBuilt_bins3')

# row bind summaries
item23.final <- rbind.data.frame(item23.by.vinage
                                 , item23.across.vintages
                                 , stringsAsFactors = F)
# remove incorrect all housing vintage rows (labelled "Remove")
item23.final <- item23.final[which(item23.final$HomeYearBuilt_bins3 != "Remove"),]



item23.cast <- dcast(setDT(item23.final),
                     formula   = BuildingType +  HomeYearBuilt_bins3 + n + N ~ rvalue.bins.MH,
                     value.var = c("w.percent", "w.SE", "count"))

item23.table <- data.frame("BuildingType"     = item23.cast$BuildingType
                           ,"Housing.Vintage" = item23.cast$HomeYearBuilt_bins3
                           ,"Percent.R0.R8"   = item23.cast$w.percent_R0.R8  
                           ,"SE.R0.R8"        = item23.cast$w.SE_R0.R8
                           ,"n.R0.R8"         = item23.cast$count_R0.R8
                           ,"Percent.R9.R14"  = item23.cast$w.percent_R9.R14  
                           ,"SE.R9.R14"       = item23.cast$w.SE_R9.R14
                           ,"n.R9.R14"        = item23.cast$count_R9.R14
                           ,"Percent.R15.R21" = item23.cast$w.percent_R15.R21
                           ,"SE.R15.R21"      = item23.cast$w.SE_R15.R21
                           ,"n.R15.R21"       = item23.cast$count_R15.R21
                           ,"Percent.R22.R30" = item23.cast$w.percent_R22.R30
                           ,"SE.R22.R30"      = item23.cast$w.SE_R22.R30
                           ,"n.R22.R30"       = item23.cast$count_R22.R30
                           ,"Percent.R31.R40" = item23.cast$w.percent_R31.R40
                           ,"SE.R31.R40"      = item23.cast$w.SE_R31.R40
                           ,"n.R31.R40"       = item23.cast$count_R31.R40
                           ,"SampleSize"      = item23.cast$count_Total)


item23.table.MH <- item23.table[which(item23.table$BuildingType == "Manufactured"),-1]

#export table to correct workbook using exporting function
exportTable(item23.table.SF, "SF", "Table 30")




# item23.SF <- item23.dat8[which(item23.dat8$BuildingType == "Single Family"),]
# 
# #Bin R values -- SF only
# item23.SF$rvalue.bins <- "Unknown"
# item23.SF$rvalue.bins[which(item23.SF$aveRval == 0)] <- "R0"
# item23.SF$rvalue.bins[which(item23.SF$aveRval > 0   & item23.SF$aveRval < 4) ]  <- "R1.R3"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 4  & item23.SF$aveRval < 11)]  <- "R4.R10"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 11 & item23.SF$aveRval < 16)]  <- "R11.R15"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 16 & item23.SF$aveRval < 23)]  <- "R16.R22"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 23 & item23.SF$aveRval < 28)]  <- "R23.R27"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 28 & item23.SF$aveRval < 38)]  <- "R28.R37"
# item23.SF$rvalue.bins[which(item23.SF$aveRval >= 38)] <- "RGT38"
# unique(item23.SF$rvalue.bins)
# 
# ##cast data
# item23.SF$count <- 1
# item23.SF.cast <- dcast(setDT(item23.SF),
#                          formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
#                          value.var = 'count')
# colnames(item23.SF.cast)
# 
# #summarize --Single Family only
# item23.SF.sum <- summarise(group_by(item23.SF.cast, BuildingType, HomeYearBuilt_bins4)
#                         ,SampleSize      = length(unique(CK_Cadmus_ID))
#                         ,r0.percent      = sum(R0) / SampleSize ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
#                         ,r0.se           = sd(R0) / sqrt(SampleSize)
#                         ,r1.r3.percent  = 0#sum(R1.R3)  / SampleSize
#                         ,r1.r3.se       = 0#sd(R1.R3) / sqrt(SampleSize)
#                         ,r4.r10.percent  = sum(R4.R10)  / SampleSize
#                         ,r4.r10.se       = sd(R4.R10) / sqrt(SampleSize)
#                         ,r11.r15.percent = sum(R11.R15) / SampleSize
#                         ,r11.r15.se      = sd(R11.R15) / sqrt(SampleSize)
#                         ,r16.r22.percent = sum(R16.R22) / SampleSize
#                         ,r16.r22.se      = sd(R16.R22) / sqrt(SampleSize)
#                         ,r23.r27.percent = sum(R23.R27) / SampleSize
#                         ,r23.r27.se      = sd(R23.R27) / sqrt(SampleSize)
#                         ,r28.r37.percent = sum(R28.R37) / SampleSize
#                         ,r28.r37.se      = sd(R28.R37) / sqrt(SampleSize)
#                         ,rGT38.percent   = sum(RGT38) / SampleSize
#                         ,rGT38.se        = sd(RGT38) / sqrt(SampleSize)
# )
# 
# item23.SF.sum.allVintages <- summarise(group_by(item23.SF.cast, BuildingType)
#                                     ,HomeYearBuilt_bins4 = "All Vintages"
#                                     ,SampleSize      = length(unique(CK_Cadmus_ID))
#                                     ,r0.percent      = sum(R0) / SampleSize ## note for two houses, there were two ceilings recorded for two sites where one ceiling was not insulated, and one was insulated. Look into automating this.
#                                     ,r0.se           = sd(R0) / sqrt(SampleSize)
#                                     ,r1.r3.percent  = 0#sum(R1.R3)  / SampleSize
#                                     ,r1.r3.se       = 0#sd(R1.R3) / sqrt(SampleSize)
#                                     ,r4.r10.percent  = sum(R4.R10)  / SampleSize
#                                     ,r4.r10.se       = sd(R4.R10) / sqrt(SampleSize)
#                                     ,r11.r15.percent = sum(R11.R15) / SampleSize
#                                     ,r11.r15.se      = sd(R11.R15) / sqrt(SampleSize)
#                                     ,r16.r22.percent = sum(R16.R22) / SampleSize
#                                     ,r16.r22.se      = sd(R16.R22) / sqrt(SampleSize)
#                                     ,r23.r27.percent = sum(R23.R27) / SampleSize
#                                     ,r23.r27.se      = sd(R23.R27) / sqrt(SampleSize)
#                                     ,r28.r37.percent = sum(R28.R37) / SampleSize
#                                     ,r28.r37.se      = sd(R28.R37) / sqrt(SampleSize)
#                                     ,rGT38.percent   = sum(RGT38) / SampleSize
#                                     ,rGT38.se        = sd(RGT38) / sqrt(SampleSize)
# )
# 
# #join all insulation levels onto rvalue summary
# item23.SF.final <- rbind.data.frame(item23.SF.sum, item23.SF.sum.allVintages
#                                  , stringsAsFactors = F)
# 
# item23.SF.table <- data.frame("BuildingType" = item23.SF.final$BuildingType
#                               ,"Housing.Vintage" = item23.SF.final$HomeYearBuilt_bins4
#                               ,"Percent_R0" = item23.SF.final$r0.percent
#                               ,"SE_R0" = item23.SF.final$r0.se
#                               ,"Percent_R1_R3" = item23.SF.final$r1.r3.percent
#                               ,"SE_R1_R3" = item23.SF.final$r1.r3.se
#                               ,"Percent_R4_R10" = item23.SF.final$r4.r10.percent
#                               ,"SE_R4_R10" = item23.SF.final$r4.r10.se
#                               ,"Percent_R11_R15" = item23.SF.final$r11.r15.percent
#                               ,"SE_R11_R15" = item23.SF.final$r11.r15.se
#                               ,"Percent_R16_R22" = item23.SF.final$r16.r22.percent
#                               ,"SE_R16_R22" = item23.SF.final$r16.r22.se
#                               ,"Percent_R23_R27" = item23.SF.final$r23.r27.percent
#                               ,"SE_R23_R27" = item23.SF.final$r23.r27.se
#                               ,"Percent_R28_R37" = item23.SF.final$r28.r37.percent
#                               ,"SE_R28_R37" = item23.SF.final$r28.r37.se
#                               ,"Percent_RGT38" = item23.SF.final$rGT38.percent
#                               ,"SE_RGT38" = item23.SF.final$rGT38.se
#                               ,"SampleSize" = item23.SF.final$SampleSize)
# item23.SF.table1 <- item23.SF.table[which(!(is.na(item23.SF.table$Housing.Vintage))),]
# 
# 
# 
# 
# #############################################################################################
# #############################################################################################
# # Item 23: DISTRIBUTION OF FLOOR INSULATION BY HOME VINTAGE (MH table 18)
# #############################################################################################
# #############################################################################################
# item23.MH <- item23.dat8[which(item23.dat8$BuildingType == "Manufactured"),]
# 
# #Bin R values -- MH only
# item23.MH$rvalue.bins <- "Unknown"
# item23.MH$rvalue.bins[which(item23.MH$aveRval > 0  & item23.MH$aveRval < 9)]  <- "R0.R8"
# item23.MH$rvalue.bins[which(item23.MH$aveRval >= 9 & item23.MH$aveRval < 15)]  <- "R9.R14"
# item23.MH$rvalue.bins[which(item23.MH$aveRval >= 15 & item23.MH$aveRval < 22)]  <- "R15.R21"
# item23.MH$rvalue.bins[which(item23.MH$aveRval >= 22 & item23.MH$aveRval < 31)]  <- "R22.R30"
# item23.MH$rvalue.bins[which(item23.MH$aveRval >= 31 & item23.MH$aveRval < 41)]  <- "R31.R40"
# unique(item23.MH$rvalue.bins)
# 
# ##cast data
# item23.MH$count <- 1
# item23.MH.cast <- dcast(setDT(item23.MH),
#                             formula   = CK_Cadmus_ID + BuildingType +  HomeYearBuilt_bins4 ~ rvalue.bins, sum,
#                             value.var = 'count')
# head(item23.MH.cast)
# 
# 
# # ## Single Family ## ## Note that there are only 2 MF-Low and 1 MF-High sites, not providing info
# # MH.item23.MH.dat <- subset(item23.MH.dat.cast, item23.MH.dat.cast$BuildingType == "Single Family")
# 
# #summarize --SF
# item23.MH.sum <- summarise(group_by(item23.MH.cast, BuildingType, HomeYearBuilt_bins4)
#                            ,sampleSize      = length(unique(CK_Cadmus_ID))
#                            ,sampleSizeNoNA  = length(unique(CK_Cadmus_ID)) - sum(`Unknown`)
#                            ,r0.r8.percent  = 0#sum(R0.R8)  / sampleSizeNoNA
#                            ,r0.r8.se       = 0#sd(R0.R8) / sqrt(sampleSizeNoNA)
#                            ,r9.r14.percent  = sum(R9.R14)  / sampleSizeNoNA
#                            ,r9.r14.se       = sd(R9.R14) / sqrt(sampleSizeNoNA)
#                            ,r15.r21.percent = sum(R15.R21) / sampleSizeNoNA
#                            ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSizeNoNA)
#                            ,r22.r30.percent = sum(R22.R30) / sampleSizeNoNA
#                            ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSizeNoNA)
#                            ,r31.r40.percent = sum(R31.R40) / sampleSizeNoNA
#                            ,r31.r40.se      = sd(R31.R40) / sqrt(sampleSizeNoNA)
# )
# 
# item23.MH.sum.allVintages <- summarise(group_by(item23.MH.cast, BuildingType)
#                                        ,HomeYearBuilt_bins4 = "All Vintages"
#                                        ,sampleSize      = sum(length(unique(CK_Cadmus_ID)))
#                                        ,sampleSizeNoNA  = sum(length(unique(CK_Cadmus_ID))) - sum(`Unknown`)
#                                        ,r0.r8.percent  = 0#sum(R0.R8)  / sampleSizeNoNA
#                                        ,r0.r8.se       = 0#sd(R0.R8) / sqrt(sampleSizeNoNA)
#                                        ,r9.r14.percent  = sum(R9.R14)  / sampleSizeNoNA
#                                        ,r9.r14.se       = sd(R9.R14) / sqrt(sampleSizeNoNA)
#                                        ,r15.r21.percent = sum(R15.R21) / sampleSizeNoNA
#                                        ,r15.r21.se      = sd(R15.R21) / sqrt(sampleSizeNoNA)
#                                        ,r22.r30.percent = sum(R22.R30) / sampleSizeNoNA
#                                        ,r22.r30.se      = sd(R22.R30) / sqrt(sampleSizeNoNA)
#                                        ,r31.r40.percent = sum(R31.R40) / sampleSizeNoNA
#                                        ,r31.r40.se      = sd(R31.R40) / sqrt(sampleSizeNoNA)
# )
# 
# #join all insulation levels onto rvalue summary
# item23.MH.final <- rbind.data.frame(item23.MH.sum, item23.MH.sum.allVintages
#                                     , stringsAsFactors = F)
# 
# 
# item23.MH.table <- data.frame("BuildingType"     = item23.MH.final$BuildingType
#                               ,"Housing.Vintage" = item23.MH.final$HomeYearBuilt_bins4
#                               ,"Percent_R0_R8"   = item23.MH.final$r0.r8.percent
#                               ,"SE_R0_R8"        = item23.MH.final$r0.r8.se
#                               ,"Percent_R9_R14"  = item23.MH.final$r9.r14.percent
#                               ,"SE_R9_R14"       = item23.MH.final$r9.r14.se
#                               ,"Percent_R15_R21" = item23.MH.final$r15.r21.percent
#                               ,"SE_R15_R21"      = item23.MH.final$r15.r21.percent
#                               ,"Percent_R22_R30" = item23.MH.final$r22.r30.percent
#                               ,"SE_R22_R30"      = item23.MH.final$r22.r30.se
#                               ,"Percent_R31_R40" = item23.MH.final$r31.r40.percent
#                               ,"SE_R31_R40"      = item23.MH.final$r31.r40.se
#                               ,"SampleSize"      = item23.MH.final$sampleSizeNoNA)
# item23.MH.table1 <- item23.MH.table[which(!(is.na(item23.MH.table$Housing.Vintage))),]
