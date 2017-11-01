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




#############################################################################################
#
# PREP FOR ITEMS 26, 30, 31
#
#############################################################################################
#subset envelope data to necessary columns
ceiling.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                , "Category"
                                                                , "Ceiling.Type"
                                                                , "Ceiling.Sub-Type"
                                                                , "Ceiling.Area"
                                                                , "Ceiling.Insulated?"
                                                                , "Ceiling.Insulation.Type.1"
                                                                , "Ceiling.Insulation.Thickness.1"
                                                                , "Ceiling.Insulation.Type.2"                                                  
                                                                , "Ceiling.Insulation.Thickness.2"
                                                                , "Ceiling.Insulation.Type.3"
                                                                , "Ceiling.Insulation.Thickness.3"))]
length(unique(ceiling.dat$CK_Cadmus_ID))#584 - missing 15 sites
unique(ceiling.dat$Ceiling.Type)

ceiling.dat0 <- ceiling.dat[which(ceiling.dat$`Ceiling.Insulated?` %in% c("Yes", "No")),]
ceiling.dat1.0 <- ceiling.dat0[which(!(is.na(ceiling.dat0$Ceiling.Area))),]
ceiling.dat1.1 <- ceiling.dat1.0[which(ceiling.dat1.0$Ceiling.Insulation.Thickness.1 != "Unknown"),]

#subset to only wall information
ceiling.dat1 <- ceiling.dat1.1[which(ceiling.dat1.1$Category == "Ceiling"),]

#merge analysis data with cleaned RBSA data
ceiling.dat2 <- left_join(rbsa.dat, ceiling.dat1, by = "CK_Cadmus_ID")

#subset to only single family sites
ceiling.dat2.5 <- ceiling.dat2[which(ceiling.dat2$BuildingType == "Single Family"),]

length(unique(ceiling.dat2.5$CK_Cadmus_ID))#349
unique(ceiling.dat2.5$`Ceiling.Insulated?`)

ceiling.dat3 <- ceiling.dat2.5

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "Unknown")] <- "Unknown Unknown"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(is.na(ceiling.dat3$Ceiling.Insulation.Thickness.1))] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.1[which(ceiling.dat3$Ceiling.Insulation.Thickness.1 == "20 or more inches")] <- "20 inches"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(is.na(ceiling.dat3$Ceiling.Insulation.Thickness.2))] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.2[which(ceiling.dat3$Ceiling.Insulation.Thickness.2 == "-- Datapoint not asked for --")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.3[which(ceiling.dat3$Ceiling.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
ceiling.dat3$Ceiling.Insulation.Thickness.3[which(ceiling.dat3$Ceiling.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.3[which(is.na(ceiling.dat3$Ceiling.Insulation.Thickness.3))] <- "N/A N/A"
ceiling.dat3$Ceiling.Insulation.Thickness.3[which(ceiling.dat3$Ceiling.Insulation.Thickness.3 == "-- Datapoint not asked for --")] <- "N/A N/A"

# add new ID variable for merging -- don't know if we need this
ceiling.dat3$count <- 1
ceiling.dat3$TMP_ID <- cumsum(ceiling.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(ceiling.dat3$Ceiling.Insulation.Thickness.1, " "))
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = ceiling.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = ceiling.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(ceiling.dat3$Ceiling.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = ceiling.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = ceiling.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(ceiling.dat3$Ceiling.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = ceiling.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = ceiling.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)

clean.insul1 <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul  <- left_join(clean.insul1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))

###########################
# End cleaning step
###########################

#make into dataframe
ceiling.dat4 <- as.data.frame(left_join(ceiling.dat3, clean.insul, by = c("CK_Cadmus_ID", "TMP_ID"))
                              , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# rename columns
ceiling.dat4$inches1 <- as.numeric(as.character(ceiling.dat4$V1.x)) # warning here is OK
ceiling.dat4$inches2 <- as.numeric(as.character(ceiling.dat4$V1.y)) # warning here is OK
ceiling.dat4$inches3 <- as.numeric(as.character(ceiling.dat4$V1)) # warning here is OK

ceiling.dat4$rvalues1 <- ceiling.dat4$Ceiling.Insulation.Type.1
ceiling.dat4$rvalues2 <- ceiling.dat4$Ceiling.Insulation.Type.2
ceiling.dat4$rvalues3 <- ceiling.dat4$Ceiling.Insulation.Type.3

#check uniques
unique(ceiling.dat4$rvalues1)
unique(ceiling.dat4$rvalues2)
unique(ceiling.dat4$rvalues3)

#fix names that are not in R value table
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 %in% c("Unknown fiberglass", "Other"))] <- "Unknown"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 %in% c("Extruded polystyrene (blue)", "Expanded polystyrene foam board (white)"))]      <- "Extruded polystyrene foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "N/A")] <- NA
###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  ceiling.dat4$rvalues3[which(ceiling.dat4$rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}
###########################
# End Replace Step
###########################

ceiling.dat5 <- ceiling.dat4
length(unique(ceiling.dat5$CK_Cadmus_ID)) #349 -- check with line 66

###########################
# Cleaning step (NA to zero)
###########################

ceiling.dat5$rvalues1[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$rvalues2[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$rvalues3[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches1[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches2[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches3[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in 49:54){
  ceiling.dat5[,i] <- ifelse(is.na(ceiling.dat5[,i]), 0, ceiling.dat5[,i])
}


#make all inches and rvalue columns numeric
for(i in 49:54){
  ceiling.dat5[,i] <- as.numeric(as.character(ceiling.dat5[,i]))
}

ceiling.dat6 <- ceiling.dat5[which(!(is.na(ceiling.dat5$`Ceiling.Insulated?`))),]
#check uniques -- None should be NA
unique(ceiling.dat6$rvalues1)
unique(ceiling.dat6$rvalues2)
unique(ceiling.dat6$rvalues3)

length(unique(ceiling.dat6$CK_Cadmus_ID)) # 238 without missing ceiling insulation info
###########################
# End Cleaning step
###########################


# r values multiplied by inches
ceiling.dat6$total.r.val <- (as.numeric(as.character(ceiling.dat6$rvalues1)) * ceiling.dat6$inches1) + (as.numeric(as.character(ceiling.dat6$rvalues2)) * ceiling.dat6$inches2)
unique(ceiling.dat6$total.r.val)

#caluclate u factors = inverse of Rvalue
ceiling.dat6$ufactor <- 1 / as.numeric(as.character(ceiling.dat6$total.r.val))

# replace inf with 0
ceiling.dat6$ufactor[which(ceiling.dat6$ufactor == "Inf")] <- 0

#make area numeric
ceiling.dat6$ufactor <- as.numeric(as.character(ceiling.dat6$ufactor))
ceiling.dat6$Ceiling.Area <- as.numeric(as.character(ceiling.dat6$Ceiling.Area))

#weight the u factor per home
weightedU <- summarise(group_by(ceiling.dat6, CK_Cadmus_ID, Ceiling.Type)
                       ,aveUval = sum(Ceiling.Area * as.numeric(as.character(ufactor))) / sum(Ceiling.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

Ceiling.unique <- unique(ceiling.dat6[which(colnames(ceiling.dat6) %in% c("CK_Cadmus_ID","BuildingType"))])

ceiling.dat7 <- left_join(weightedU, Ceiling.unique, by = "CK_Cadmus_ID")
length(unique(ceiling.dat7$CK_Cadmus_ID)) #238

ceiling.dat8 <- left_join(ceiling.dat7, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))
length(unique(ceiling.dat8$CK_Cadmus_ID)) #238

#############################################################################################
#############################################################################################
# END PREP
#############################################################################################
#############################################################################################




















############################################################################################################
## Item 26
############################################################################################################
item26.dat <- ceiling.dat8[which(ceiling.dat8$Ceiling.Type == "Attic"),]


#Bin R values -- SF only
item26.dat$rvalue.bins <- "Unknown"
item26.dat$rvalue.bins[which(item26.dat$aveRval == 0)] <- "R0"
item26.dat$rvalue.bins[which(item26.dat$aveRval > 0  & item26.dat$aveRval < 11)]  <- "R1.R10"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 11 & item26.dat$aveRval < 16)]  <- "R11.R15"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 16 & item26.dat$aveRval < 21)]  <- "R16.R20"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 21 & item26.dat$aveRval < 26)]  <- "R21.R25"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 26 & item26.dat$aveRval < 31)]  <- "R26.R30"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 31 & item26.dat$aveRval < 41)]  <- "R31.R40"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 41 & item26.dat$aveRval < 51)]  <- "R41.R50"
item26.dat$rvalue.bins[which(item26.dat$aveRval >= 51)] <- "RGT50"
unique(item26.dat$rvalue.bins)

item26.dat0 <- item26.dat[which(item26.dat$rvalue.bins != "Unknown"),]
item26.dat0$count <- 1

#summarise by rvalue bins
item26.sum1 <- summarise(group_by(item26.dat0, BuildingType, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item26.sum2 <- summarise(group_by(item26.dat0, BuildingType)
                         ,rvalue.bins = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item26.merge <- rbind.data.frame(item26.sum1,item26.sum2, stringsAsFactors = F)

item26.tot.count <- item26.sum2[which(colnames(item26.sum2) %in% c("BuildingType"
                                                                   ,"Count"))]
colnames(item26.tot.count) <- c("BuildingType","Total.Count")

item26.final <- left_join(item26.merge, item26.tot.count, by = "BuildingType")

item26.final$Percent <- item26.final$Count / item26.final$Total.Count
item26.final$SE <- sqrt(item26.final$Percent * (1 - item26.final$Percent) / item26.final$SampleSize)


item26.table <- data.frame("BuildingType" = item26.final$BuildingType
                            ,"R Values" = item26.final$rvalue.bins
                            ,"Percent" = item26.final$Percent
                            ,"SE" = item26.final$SE
                            ,"SampleSize" = item26.final$SampleSize)







############################################################################################################
## Item 30
############################################################################################################
item30.dat <- ceiling.dat8[which(ceiling.dat8$Ceiling.Type == "Sloped / Vaulted (no attic)"),]


#Bin R values -- SF only
item30.dat$rvalue.bins <- "Unknown"
item30.dat$rvalue.bins[which(item30.dat$aveRval == 0)] <- "R0"
item30.dat$rvalue.bins[which(item30.dat$aveRval > 0  & item30.dat$aveRval < 11)]  <- "R1.R10"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 11 & item30.dat$aveRval < 16)]  <- "R11.R15"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 16 & item30.dat$aveRval < 21)]  <- "R16.R20"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 21 & item30.dat$aveRval < 26)]  <- "R21.R25"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 26 & item30.dat$aveRval < 31)]  <- "R26.R30"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 31 & item30.dat$aveRval < 41)]  <- "R31.R40"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 41 & item30.dat$aveRval < 51)]  <- "R41.R50"
item30.dat$rvalue.bins[which(item30.dat$aveRval >= 51)] <- "RGT50"
unique(item30.dat$rvalue.bins)

item30.dat0 <- item30.dat[which(item30.dat$rvalue.bins != "Unknown"),]
item30.dat0$count <- 1

#summarise by rvalue bins
item30.sum1 <- summarise(group_by(item30.dat0, BuildingType, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item30.sum2 <- summarise(group_by(item30.dat0, BuildingType)
                         ,rvalue.bins = "Total"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item30.merge <- rbind.data.frame(item30.sum1,item30.sum2, stringsAsFactors = F)

item30.tot.count <- item30.sum2[which(colnames(item30.sum2) %in% c("BuildingType"
                                                                   ,"Count"))]
colnames(item30.tot.count) <- c("BuildingType","Total.Count")

item30.final <- left_join(item30.merge, item30.tot.count, by = "BuildingType")

item30.final$Percent <- item30.final$Count / item30.final$Total.Count
item30.final$SE <- sqrt(item30.final$Percent * (1 - item30.final$Percent) / item30.final$SampleSize)


item30.table <- data.frame("BuildingType" = item30.final$BuildingType
                           ,"R Values" = item30.final$rvalue.bins
                           ,"Percent" = item30.final$Percent
                           ,"SE" = item30.final$SE
                           ,"SampleSize" = item30.final$SampleSize)










############################################################################################################
## Item 31
############################################################################################################
item31.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Ceiling.Type"
                                                               , "Ceiling.Sub-Type"
                                                               , "Ceiling.Area"
                                                               , "Ceiling.Insulated?"
                                                               , "Ceiling.Insulation.Type.1"
                                                               , "Ceiling.Insulation.Thickness.1"
                                                               , "Ceiling.Insulation.Condition.1"                                                 
                                                               , "Ceiling.Insulation.Type.2"                                                      
                                                               , "Ceiling.Insulation.Thickness.2"                                                 
                                                               , "Ceiling.Insulation.Condition.2"                                                 
                                                               , "Ceiling.Insulation.Type.3"                                                      
                                                               , "Ceiling.Insulation.Thickness.3"))]
item31.dat1 <- item31.dat[which(item31.dat$Ceiling.Type == "Roof Deck"),]
length(unique(item31.dat1$CK_Cadmus_ID)) #only 18

item31.dat1$`Ceiling.Insulated?`[which(is.na(item31.dat1$`Ceiling.Insulated?`))] <- "No"
item31.dat1$count <- 1

item31.dat2 <- item31.dat1[which(item31.dat1$`Ceiling.Insulated?` == "Yes"),]

item31.sum1 <- summarise(group_by(item31.dat2, Ceiling.Insulation.Thickness.1)
                         ,Count = sum(count)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item31.sum2 <- summarise(group_by(item31.dat2)
                         ,Ceiling.Insulation.Thickness.1 = "Total"
                         ,Count = sum(count)
                         ,SampleSize = length(unique(CK_Cadmus_ID)))

item31.final <- rbind.data.frame(item31.sum1, item31.sum2, stringsAsFactors = F)

item31.final$Total.Count <- item31.sum2$Count
item31.final$Denom.SampleSize <- item31.sum2$SampleSize

item31.final$Percent <- item31.final$Count / item31.final$Total.Count
item31.final$SE <- sqrt(item31.final$Percent * (1 - item31.final$Percent) / item31.final$Denom.SampleSize)

item31.table <- data.frame("Insulation.Level" = item31.final$Ceiling.Insulation.Thickness.1
                           ,"Percent" = item31.final$Percent
                           ,"SE" = item31.final$SE
                           ,"SampleSize" = item31.final$SampleSize)
