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
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))

#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]



#############################################################################################
# PREP insulation data
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
                                                                , "Ceiling.Insulation.Thickness.3"
))]

#subset to only wall information
ceiling.dat1 <- ceiling.dat[which(ceiling.dat$Category == "Ceiling"),]

unique(ceiling.dat1$Ceiling.Insulation.Type.1)
unique(ceiling.dat1$Ceiling.Insulation.Type.2)
unique(ceiling.dat1$Ceiling.Insulation.Type.3)

#merge analysis data with cleaned RBSA data
ceiling.dat2 <- left_join(rbsa.dat, ceiling.dat1, by = "CK_Cadmus_ID")

#remove items have the "-- datapoint not asked for --"
ceiling.dat3 <- ceiling.dat2[which(ceiling.dat2$`Ceiling.Insulated?` %in% c("Yes", "No")),]
length(unique(ceiling.dat3$CK_Cadmus_ID))#262
unique(ceiling.dat3$`Ceiling.Insulated?`)


###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################
unique(ceiling.dat3$Ceiling.Insulation.Thickness.3)
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


# add new ID variable for merging
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

clean.insul.merge <- left_join(clean.insul1.1, clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul       <- left_join(clean.insul.merge, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))

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
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Unknown fiberglass")]               <- "Unknown"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Extruded polystyrene foam board (pink or blue)")]  <- "Extruded polystyrene foam board"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Other")]                            <- "Unknown"
ceiling.dat4$rvalues1[which(ceiling.dat4$rvalues1 == "Polyurethane foam board (black)")]  <- "Polyurethane foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "Expanded polystyrene foam board (white)")]      <- "Expanded polystyrene foam board"
ceiling.dat4$rvalues2[which(ceiling.dat4$rvalues2 == "N/A")]                              <- NA
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
length(unique(ceiling.dat5$CK_Cadmus_ID)) #262 -- check with line 66

###########################
# Cleaning step (No insulation to zero)
###########################
ceiling.dat5$rvalues1[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$rvalues2[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$rvalues3[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches1[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches2[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0
ceiling.dat5$inches3[which(ceiling.dat5$`Ceiling.Insulated?` == "No")] <- 0

###########################
# End Cleaning step
###########################
ceiling.dat5$rvalues1 <- as.numeric(as.character(ceiling.dat5$rvalues1))
ceiling.dat5$rvalues2 <- as.numeric(as.character(ceiling.dat5$rvalues2))
ceiling.dat5$rvalues3 <- as.numeric(as.character(ceiling.dat5$rvalues3))


R2_R3_NA_ind <- which(is.na(ceiling.dat5$rvalues2) & is.na(ceiling.dat5$rvalues3))
Only_R2_NA_ind <- which(is.na(ceiling.dat5$rvalues2) & (!(is.na(ceiling.dat5$rvalues3))))
Only_R3_NA_ind <- which((!(is.na(ceiling.dat5$rvalues2))) & (is.na(ceiling.dat5$rvalues3)))

########################
# Calculate R values and U values
########################
#create total.r.value column
item176.dat5$total.r.val <- NA

#calculate the weighted r value where 
for (i in R2_R3_NA_ind){
  ceiling.dat5$total.r.val[i] <- (ceiling.dat5$rvalues1[i] * ceiling.dat5$inches1[i])
}
#calculate the weighted r value where 
for (i in Only_R2_NA_ind){
  ceiling.dat5$total.r.val[i] <- (ceiling.dat5$rvalues1[i] * ceiling.dat5$inches1[i]) +  
    (ceiling.dat5$rvalues2[i] * ceiling.dat5$inches2[i])
}
#calculate the weighted r value where 
for (i in Only_R3_NA_ind){
  ceiling.dat5$total.r.val[i] <- (ceiling.dat5$rvalues1[i] * ceiling.dat5$inches1[i]) +  
    (ceiling.dat5$rvalues3[i] * ceiling.dat5$inches3[i])
}

#calculate the weighted r value where wall cavity insulation type is NA
ceiling.dat5$total.r.val[which(is.na(ceiling.dat5$total.r.val))] <- 
  (ceiling.dat5$rvalues1[which(is.na(ceiling.dat5$total.r.val))] *
     ceiling.dat5$inches1[which(is.na(ceiling.dat5$total.r.val))])

#check -- NOTE -- NONE SHOULD BE NA -- NA occurs for these items because of unknown insulation thicknesses
unique(ceiling.dat5$total.r.val)


ceiling.dat6 <- ceiling.dat5[which(!(is.na(ceiling.dat5$total.r.val))),]

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

############################################################################################################
## END PREP: ITEMS 177 AND 178 DIFFER AFTER THIS POINT
############################################################################################################










#############################################################################################
#Item 177: DISTRIBUTION OF CEILING INSULATION (MH TABLE 20)
#############################################################################################
#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(ceiling.dat6[which(colnames(ceiling.dat6) %in% c("CK_Cadmus_ID","BuildingType"))])

item177.dat <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item177.dat1 <- left_join(item177.dat, rbsa.dat, by = c("CK_Cadmus_ID","BuildingType"))

item177.dat2 <- item177.dat1[which(!(is.na(item177.dat1$aveUval))),]


#Bin R values -- SF only
item177.dat2$rvalue.bins <- "Unknown"
item177.dat2$rvalue.bins[which(item177.dat2$aveRval >= 0  & item177.dat2$aveRval < 9) ]  <- "R0.R8"
item177.dat2$rvalue.bins[which(item177.dat2$aveRval >= 9  & item177.dat2$aveRval < 15)]  <- "R9.R14"
item177.dat2$rvalue.bins[which(item177.dat2$aveRval >= 15 & item177.dat2$aveRval < 22)]  <- "R15.R21"
item177.dat2$rvalue.bins[which(item177.dat2$aveRval >= 22 & item177.dat2$aveRval < 31)]  <- "R22.R30"
item177.dat2$rvalue.bins[which(item177.dat2$aveRval >= 31 & item177.dat2$aveRval < 41)]  <- "R31.R40"
unique(item177.dat2$rvalue.bins)

item177.dat2$count <- 1

item177.dat2 <- item177.dat2[which(item177.dat2$rvalue.bins != "Unknown"),]

#summarise by wall frame types
#summarise by r value bins
item177.sum1 <- summarise(group_by(item177.dat2, BuildingType, HomeYearBuilt_bins, rvalue.bins)
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across r value bins
item177.sum2 <- summarise(group_by(item177.dat2, BuildingType, HomeYearBuilt_bins)
                         ,rvalue.bins = "All Insulation Levels"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))


#summarise across wall frame types
#summarise by r value bins
item177.sum3 <- summarise(group_by(item177.dat2, BuildingType, rvalue.bins)
                         , HomeYearBuilt_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))
#summarise across r value bins
item177.sum4 <- summarise(group_by(item177.dat2, BuildingType)
                         ,rvalue.bins = "All Insulation Levels"
                         , HomeYearBuilt_bins = "All Vintages"
                         ,SampleSize = length(unique(CK_Cadmus_ID))
                         ,Count = sum(count))

item177.merge <- rbind.data.frame(item177.sum1,item177.sum2,item177.sum3,item177.sum4,stringsAsFactors = F)

item177.tot.counts <- rbind.data.frame(item177.sum2, item177.sum4, stringsAsFactors = F)
item177.tot.counts <- item177.tot.counts[which(colnames(item177.tot.counts) %in% c("BuildingType"
                                                                                , "HomeYearBuilt_bins"
                                                                                , "Count"))]

item177.merge1 <- left_join(item177.merge, item177.tot.counts, by = c("BuildingType", "HomeYearBuilt_bins"))
colnames(item177.merge1) <- c("BuildingType", "HomeYearBuilt_bins", "rvalue.bins", "SampleSize", "Count", "TotalCount")

building.type <- rbind.data.frame("Manufactured"
                                  , "Multifamily - High Rise"
                                  , "Multifamily - Low Rise"
                                  , "Single Family"
                                  , stringsAsFactors = F)

for(i in 1:4){
  item177.merge1$TotalCount[which(item177.merge1$BuildingType == building.type[i,] & item177.merge1$rvalue.bins == "All Insulation Levels")] <-
    item177.merge1$TotalCount[which(item177.merge1$BuildingType == building.type[i,] & item177.merge1$rvalue.bins == "All Insulation Levels" & item177.merge1$HomeYearBuilt_bins == "All Vintages")]
}

item177.final <- item177.merge1

item177.final$Percent <- item177.final$Count / item177.final$TotalCount
item177.final$SE <- sqrt(item177.final$Percent * (1 - item177.final$Percent) / item177.final$SampleSize)

##cast data
item177.table <- dcast(setDT(item177.final),
                      formula   = BuildingType +  HomeYearBuilt_bins ~ rvalue.bins,
                      value.var = c("Percent", "SE", "SampleSize"))

#join all insulation levels onto rvalue summary
item177.table1 <- data.frame("BuildingType" = item177.table$BuildingType
                            ,"Housing.Vintage" = item177.table$HomeYearBuilt_bins
                            ,"Percent.R0.R8" = item177.table$Percent_R0.R8
                            ,"SE.R0.R8" = item177.table$SE_R0.R8
                            ,"Percent.R15.R21" = item177.table$Percent_R15.R21
                            ,"SE.R15.R21" = item177.table$SE_R15.R21
                            ,"Percent.R22.R30" = item177.table$Percent_R22.R30
                            ,"SE.R22.R30" = item177.table$SE_R22.R30
                            ,"Percent.R31.R40" = item177.table$Percent_R31.R40
                            ,"SE.R31.R40" = item177.table$SE_R31.R40
                            ,"Percent_All Insulation Levels" = item177.table$`Percent_All Insulation Levels`
                            ,"SE_All Insulation Levels"   = item177.table$`SE_All Insulation Levels`
                            ,"SampleSize" = item177.table$`SampleSize_All Insulation Levels`)

item177.table2 <- item177.table1[which(item177.table1$BuildingType == "Manufactured"),]















#############################################################################################
#Item 178: DISTRIBUTION OF CEILING U-VALUE BY STATE (MH TABLE 21)
#############################################################################################

wall.unique <- unique(ceiling.dat6[which(colnames(ceiling.dat6) %in% c("CK_Cadmus_ID","BuildingType"))])

item178.dat <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge weighted u values onto cleaned RBSA data
item178.dat1 <- left_join(item178.dat, rbsa.dat, by = c("CK_Cadmus_ID", "BuildingType"))

item178.dat2 <- item178.dat1[which(!(is.na(item178.dat1$aveUval))),]

#summarise by state
item178.state <- summarise(group_by(item178.dat2, BuildingType, State)
                           ,SampleSize = length(unique(CK_Cadmus_ID))
                           ,Mean = mean(aveUval)
                           ,SE = sd(aveUval) / sqrt(SampleSize))
# summarise by region
item178.region <- summarise(group_by(item178.dat2, BuildingType)
                            ,State = "Region"
                            ,SampleSize = length(unique(CK_Cadmus_ID))
                            ,Mean = mean(aveUval)
                            ,SE = sd(aveUval) / sqrt(SampleSize))
#merge together
item178.final <- rbind.data.frame(item178.state, item178.region, stringsAsFactors = F)

item178.table <- data.frame("BuildingType" = item178.final$BuildingType
                            ,"State" = item178.final$State
                            ,"Mean" = item178.final$Mean
                            ,"SE" = item178.final$SE
                            ,"SampleSize" = item178.final$SampleSize)

item178.table1 <- item178.table[which(item178.table$BuildingType == "Manufactured"),]
