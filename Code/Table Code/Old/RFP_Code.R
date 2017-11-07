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

#read in BPA site IDs for analysis
BPA.dat.init <- read.xlsx(xlsxFile = file.path("C:/Users/Casey.Stevens/Documents/RBSA/RBSA Analysis/Table Code/HomeDataBPA.xlsx"))
BPA.dat <- BPA.dat.init[which(colnames(BPA.dat.init) == "CK_Cadmus_ID")]


neea.dat <- left_join(BPA.dat, rbsa.dat, by = "CK_Cadmus_ID")

########################################################
# ITEMS
# 1. Square footage
# 2. Number of above ground floors
# 3. Window type
# 4. Attic insulation (r value or type and thickness)
# 5. Wall insulation (r value or type and thickness)
########################################################

########################################################
# Square footage (envelope.dat)
########################################################
#Read in data for analysis
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
#clean cadmus IDs
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))


sqft.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                             , "ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt"))]

#merge
sqft.dat1 <- left_join(neea.dat, sqft.dat, by = "CK_Cadmus_ID")
length(unique(sqft.dat1$CK_Cadmus_ID)) #22

#remove NAs
sqft.dat2 <- sqft.dat1[which(!(is.na(sqft.dat1$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))),]
length(unique(sqft.dat2$CK_Cadmus_ID)) #21

#make conditioned area as.numeric
sqft.dat2$ConditionedArea <- as.numeric(as.character(sqft.dat2$ENV_Construction_BLDG_STRUCTURE_BldgLevel_Area_SqFt))

#summarise by region
sqft.region <- summarise(group_by(sqft.dat2, CK_Cadmus_ID)
                              ,siteAreaConditioned = sum(ConditionedArea)
)

# sqft.final <- summarise(group_by(sqft.region1)
#                                ,AveAreaConditioned = mean(siteAreaConditioned)
#                                ,SEAreaConditioned  = sd(siteAreaConditioned) / sqrt(length(unique(CK_Cadmus_ID)))
#                                ,SampleSize         = length(unique(CK_Cadmus_ID))
# )






########################################################
# Number of above ground floors (need.dat)
########################################################
numFloors.dat <- neea.dat[which(colnames(neea.dat) %in% c("CK_Cadmus_ID", "BuildingHeight"))]

numFloors.final <- summarise(numFloors.dat
                             ,Metric = "Number of Above Ground Floors"
                             ,Mean = mean(BuildingHeight)
                             ,SampleSize= length(unique(CK_Cadmus_ID)))





########################################################
# Window type (windows.dat)
########################################################
#Read in data for analysis
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))

windows.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]
windows.dat1 <- left_join(neea.dat, windows.dat, by = "CK_Cadmus_ID")
length(unique(windows.dat1$CK_Cadmus_ID)) #22

#subset to only windows
windows.dat2 <- windows.dat1[which(windows.dat1$Type == "Window"),]

#clean up frame/body type
unique(windows.dat2$`Frame./.Body.Type`)
windows.dat2$Frame.Type <- trimws(windows.dat2$`Frame./.Body.Type`)
windows.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass", windows.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass"
windows.dat2$Frame.Type[grep("Metal|Aluminum", windows.dat2$Frame.Type)] <- "Metal"
windows.dat2$Frame.Type[grep("N/A", windows.dat2$Frame.Type)] <- "Unknown"
unique(windows.dat2$Frame.Type)

#clean up glazing types
windows.dat2$Glazing <- trimws(windows.dat2$Glazing.Type)
windows.dat2$Glazing[grep("Single", windows.dat2$Glazing)] <- "Single Glazed"
windows.dat2$Glazing[grep("Double", windows.dat2$Glazing)] <- "Double Glazed"
windows.dat2$Glazing[grep("Triple", windows.dat2$Glazing)] <- "Triple Glazed"
windows.dat2$Glazing[which(!(windows.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(windows.dat2$Glazing)

windows.dat2$Framing.Categories <- paste(windows.dat2$Frame.Type, windows.dat2$Glazing, sep = " ")

windows.dat2$count <- 1


#summarise by framing categories across states
windows.tmp1 <- summarise(group_by(windows.dat2, CK_Cadmus_ID, Framing.Categories)
                          ,SampleSize = length(unique(CK_Cadmus_ID))
                          , Count = sum(count))
windows.tmp2 <- summarise(group_by(windows.dat2, CK_Cadmus_ID)
                          , denom.SampleSize = length(unique(CK_Cadmus_ID))
                          , TotalCount = sum(count))

windows.final <- left_join(windows.tmp1, windows.tmp2, by = "CK_Cadmus_ID")

windows.final$Percent <- windows.final$Count / windows.final$TotalCount
windows.final$SE      <- sqrt(windows.final$Percent * (1 - windows.final$Percent) / windows.final$denom.SampleSize)

##################################### Table Format ###################################

windows.table <- data.frame("CK_Cadmus_ID" = windows.final$CK_Cadmus_ID
                            ,"Framing.Categories" = windows.final$Framing.Categories
                            ,"Percent" = windows.final$Percent
                            ,"SampleSize" = windows.final$SampleSize)











########################################################
# Attic insulation (r value or type and thickness) (envelope.day)
########################################################
#Bring in R-value table
rvals <- read.xlsx(xlsxFile = file.path(filepathCleaningDocs, "R value table.xlsx"), sheet = 1)
rvals <- rvals[-nrow(rvals),-ncol(rvals)]

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

ceiling.dat0 <- ceiling.dat[which(ceiling.dat$`Ceiling.Insulated?` %in% c("Yes", "No")),]
ceiling.dat1.0 <- ceiling.dat0[which(!(is.na(ceiling.dat0$Ceiling.Area))),]
ceiling.dat1.1 <- ceiling.dat1.0[which(ceiling.dat1.0$Ceiling.Insulation.Thickness.1 != "Unknown"),]

#subset to only wall information
ceiling.dat1 <- ceiling.dat1.1[which(ceiling.dat1.1$Category == "Ceiling"),]

#merge analysis data with cleaned RBSA data
ceiling.dat2 <- left_join(neea.dat, ceiling.dat1, by = "CK_Cadmus_ID")

#subset to only single family sites
ceiling.dat2.5 <- ceiling.dat2[which(ceiling.dat2$BuildingType == "Single Family"),]

length(unique(ceiling.dat2.5$CK_Cadmus_ID))#254
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
length(unique(ceiling.dat5$CK_Cadmus_ID)) #254 -- check with line 66

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
for(i in 30:35){
  ceiling.dat5[,i] <- ifelse(is.na(ceiling.dat5[,i]), 0, ceiling.dat5[,i])
}


#make all inches and rvalue columns numeric
for(i in 30:35){
  ceiling.dat5[,i] <- as.numeric(as.character(ceiling.dat5[,i]))
}

ceiling.dat6 <- ceiling.dat5[which(!(is.na(ceiling.dat5$`Ceiling.Insulated?`))),]
#check uniques -- None should be NA
unique(ceiling.dat6$rvalues1)
unique(ceiling.dat6$rvalues2)
unique(ceiling.dat6$rvalues3)

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
length(unique(ceiling.dat7$CK_Cadmus_ID))

ceiling.dat8 <- left_join(ceiling.dat7, neea.dat, by = c("CK_Cadmus_ID", "BuildingType"))
length(unique(ceiling.dat8$CK_Cadmus_ID))

#############################################################################################
# END PREP
#############################################################################################
# #############################################################################################
# # ANALYSIS
# #############################################################################################
# 
# attic.dat <- ceiling.dat8[which(ceiling.dat8$Ceiling.Type == "Attic"),]
# 
# 
# #Bin R values -- SF only
# attic.dat$rvalue.bins <- "Unknown"
# attic.dat$rvalue.bins[which(attic.dat$aveRval == 0)] <- "R0"
# attic.dat$rvalue.bins[which(attic.dat$aveRval > 0  & attic.dat$aveRval < 11)]  <- "R1.R10"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 11 & attic.dat$aveRval < 16)]  <- "R11.R15"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 16 & attic.dat$aveRval < 21)]  <- "R16.R20"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 21 & attic.dat$aveRval < 26)]  <- "R21.R25"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 26 & attic.dat$aveRval < 31)]  <- "R26.R30"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 31 & attic.dat$aveRval < 41)]  <- "R31.R40"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 41 & attic.dat$aveRval < 51)]  <- "R41.R50"
# attic.dat$rvalue.bins[which(attic.dat$aveRval >= 51)] <- "RGT50"
# unique(attic.dat$rvalue.bins)
# 
# attic.dat0 <- attic.dat[which(attic.dat$rvalue.bins != "Unknown"),]
# attic.dat0$count <- 1
# 
# #summarise by rvalue bins
# attic.sum1 <- summarise(group_by(attic.dat0, rvalue.bins)
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# attic.sum2 <- summarise(group_by(attic.dat0)
#                          ,rvalue.bins = "Total"
#                          ,SampleSize = length(unique(CK_Cadmus_ID))
#                          ,Count = sum(count))
# 
# attic.merge <- rbind.data.frame(attic.sum1,attic.sum2, stringsAsFactors = F)
# 
# attic.tot.count <- attic.sum2[which(colnames(attic.sum2) %in% c("Count"))]
# colnames(attic.tot.count) <- c("Total.Count")
# 
# attic.final <- cbind.data.frame(attic.merge, attic.tot.count)
# 
# attic.final$Percent <- attic.final$Count / attic.final$Total.Count
# attic.final$SE <- sqrt(attic.final$Percent * (1 - attic.final$Percent) / attic.final$SampleSize)
# 
# 
# attic.table <- data.frame("R Values" = attic.final$rvalue.bins
#                           ,"Percent" = attic.final$Percent
#                           ,"SE" = attic.final$SE
#                           ,"SampleSize" = attic.final$SampleSize)
# 









########################################################
# Wall insulation (r value or type and thickness) (envelope.dat)
########################################################
#subset envelope data to necessary columns
wall.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                               , "Category"
                                                               , "Wall.Type"
                                                               , "Wall.Area"
                                                               , "Wall.Framing.Size"
                                                               , "Wall.Cavity.Insulated?"
                                                               , "Wall.Cavity.Insulation.Type.1"
                                                               , "Wall.Cavity.Insulation.Thickness.1"
                                                               , "Wall.Cavity.Insulation.Condition.1"
                                                               , "Wall.Cavity.Insulation.Type.2"                                                  
                                                               , "Wall.Cavity.Insulation.Thickness.2"
                                                               , "Wall.Cavity.Insulation.Condition.2"
                                                               , "Wall.Cavity.Insulation.Type.3"                                                  
                                                               , "Wall.Cavity.Insulation.Thickness.3"
                                                               , "Wall.Cavity.Insulation.Condition.3"
                                                               , "Wall.Exterior.Insulated?"
                                                               , "Wall.Exterior.Insulation.Type.1"
                                                               , "Wall.Exterior.Insulation.Thickness.1"
                                                               , "Wall.Exterior.Insulation.Condition.1"
                                                               , "Wall.Exterior.Insulation.Type.2"                                                  
                                                               , "Wall.Exterior.Insulation.Thickness.2"
                                                               , "Wall.Exterior.Insulation.Condition.2"
                                                               , "Wall.Exterior.Insulation.Type.3"                                                  
                                                               , "Wall.Exterior.Insulation.Thickness.3"
                                                               , "Wall.Exterior.Insulation.Condition.3"))]
wall.dat0 <- wall.dat[which(wall.dat$`Wall.Cavity.Insulated?` %in% c("Yes", "No")),]
wall.dat0$`Wall.Exterior.Insulated?`[which(wall.dat0$`Wall.Exterior.Insulated?` != "Yes")] <- "No" ###treat anything not Yes as No
wall.dat1.0 <- wall.dat0[which(!(is.na(wall.dat0$Wall.Area))),]
wall.dat1.1 <- wall.dat1.0[which(wall.dat1.0$Wall.Cavity.Insulation.Thickness.1 != "Unknown"),]
wall.dat1.2 <- wall.dat1.1[-which(wall.dat1.1$Wall.Exterior.Insulation.Thickness.1 == "Unknown"),]

#review types
unique(wall.dat1.2$Wall.Cavity.Insulation.Type.1)
unique(wall.dat1.2$Wall.Cavity.Insulation.Type.2)
unique(wall.dat1.2$Wall.Cavity.Insulation.Type.3) #nothing in this column
unique(wall.dat1.2$Wall.Exterior.Insulation.Type.1)
unique(wall.dat1.2$Wall.Exterior.Insulation.Type.2) #nothing in this column
unique(wall.dat1.2$Wall.Exterior.Insulation.Type.3) #nothing in this column

#review insulation thicknesses
unique(wall.dat1.2$Wall.Cavity.Insulation.Thickness.1)
unique(wall.dat1.2$Wall.Cavity.Insulation.Thickness.2)
unique(wall.dat1.2$Wall.Cavity.Insulation.Thickness.3)
unique(wall.dat1.2$Wall.Exterior.Insulation.Thickness.1)
unique(wall.dat1.2$Wall.Exterior.Insulation.Thickness.2)
unique(wall.dat1.2$Wall.Exterior.Insulation.Thickness.3)

#review conditions
unique(wall.dat1.2$Wall.Cavity.Insulation.Condition.1)
unique(wall.dat1.2$Wall.Cavity.Insulation.Condition.2)
unique(wall.dat1.2$Wall.Cavity.Insulation.Condition.3)
unique(wall.dat1.2$Wall.Exterior.Insulation.Condition.1)
unique(wall.dat1.2$Wall.Exterior.Insulation.Condition.2)
unique(wall.dat1.2$Wall.Exterior.Insulation.Condition.3)

#Clean Condition unknown values
wall.dat1.2$Wall.Cavity.Insulation.Condition.1[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
wall.dat1.2$Wall.Cavity.Insulation.Condition.2[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
wall.dat1.2$Wall.Cavity.Insulation.Condition.3[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"
wall.dat1.2$Wall.Cavity.Insulation.Condition.1[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.1 == "Unknown")] <- "100%"
wall.dat1.2$Wall.Cavity.Insulation.Condition.2[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.2 == "Unknown")] <- "100%"
wall.dat1.2$Wall.Cavity.Insulation.Condition.3[which(wall.dat1.2$Wall.Cavity.Insulation.Condition.3 == "Unknown")] <- "100%"

#remove unneccesary wall types
wall.dat2 <- wall.dat1.2[which(!(wall.dat1.2$Wall.Type %in% c("Masonry","Masonry (Basement)","Log","Adiabatic"))),]

#create "Alternative" category
wall.dat2$Wall.Type[which(wall.dat2$Wall.Type %in% c("Knee Wall", "Framed Alternative Framed Wall", "Framed ", "Other"))] <- "Alternative"
length(unique(wall.dat2$CK_Cadmus_ID))#473
unique(wall.dat2$Wall.Type)

#assign new dataset
wall.dat3 <- wall.dat2

###########################
# Cleaning Step: Set up unknown and N/A insulation thickness information in order to separate the # from the word "inches" in R
###########################

for(i in 1:ncol(wall.dat3)){
  wall.dat3[,i] <- ifelse(wall.dat3[,i] == "-- Datapoint not asked for --", NA, wall.dat3[,i])
}

#cleaning for wall.cavity
wall.dat3$Wall.Cavity.Insulation.Thickness.1[which(wall.dat3$Wall.Cavity.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Cavity.Insulation.Thickness.1[which(is.na(wall.dat3$Wall.Cavity.Insulation.Thickness.1))] <- "N/A N/A"
wall.dat3$Wall.Cavity.Insulation.Thickness.2[which(wall.dat3$Wall.Cavity.Insulation.Thickness.2 == "Unknown")] <- "Unknown Unknown"
wall.dat3$Wall.Cavity.Insulation.Thickness.2[which(wall.dat3$Wall.Cavity.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Cavity.Insulation.Thickness.2[which(is.na(wall.dat3$Wall.Cavity.Insulation.Thickness.2))] <- "N/A N/A"
wall.dat3$Wall.Cavity.Insulation.Thickness.3[which(wall.dat3$Wall.Cavity.Insulation.Thickness.3 == "Unknown")] <- "Unknown Unknown"
wall.dat3$Wall.Cavity.Insulation.Thickness.3[which(wall.dat3$Wall.Cavity.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Cavity.Insulation.Thickness.3[which(is.na(wall.dat3$Wall.Cavity.Insulation.Thickness.3))] <- "N/A N/A"
unique(wall.dat3$Wall.Cavity.Insulation.Thickness.1)
unique(wall.dat3$Wall.Cavity.Insulation.Thickness.2)
unique(wall.dat3$Wall.Cavity.Insulation.Thickness.3)

#cleaning for wall exterior
wall.dat3$Wall.Exterior.Insulation.Thickness.1[which(wall.dat3$Wall.Exterior.Insulation.Thickness.1 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Exterior.Insulation.Thickness.1[which(is.na(wall.dat3$Wall.Exterior.Insulation.Thickness.1))] <- "N/A N/A"
wall.dat3$Wall.Exterior.Insulation.Thickness.2[which(wall.dat3$Wall.Exterior.Insulation.Thickness.2 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Exterior.Insulation.Thickness.2[which(is.na(wall.dat3$Wall.Exterior.Insulation.Thickness.2))] <- "N/A N/A"
wall.dat3$Wall.Exterior.Insulation.Thickness.3[which(wall.dat3$Wall.Exterior.Insulation.Thickness.3 == "N/A")] <- "N/A N/A"
wall.dat3$Wall.Exterior.Insulation.Thickness.3[which(is.na(wall.dat3$Wall.Exterior.Insulation.Thickness.3))] <- "N/A N/A"
unique(wall.dat3$Wall.Exterior.Insulation.Thickness.1)
unique(wall.dat3$Wall.Exterior.Insulation.Thickness.2)
unique(wall.dat3$Wall.Exterior.Insulation.Thickness.3)

# add new ID variable for merging -- don't know if we need this
wall.dat3$count <- 1
wall.dat3$TMP_ID <- cumsum(wall.dat3$count)

## r-values ##
clean.insul1 <- unlist(strsplit(wall.dat3$Wall.Cavity.Insulation.Thickness.1, " "))
clean.insul2 <- as.data.frame(matrix(clean.insul1, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.1 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , clean.insul2)
dim(clean.insul1.1)

clean.insul2 <- unlist(strsplit(wall.dat3$Wall.Cavity.Insulation.Thickness.2, " "))
clean.insul2.1 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul2, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul2.1)

clean.insul3 <- unlist(strsplit(wall.dat3$Wall.Cavity.Insulation.Thickness.3, " "))
clean.insul3.1 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , as.data.frame(matrix(clean.insul3, ncol = 2, byrow = T)
                                                   , stringsAsFactors = F))
dim(clean.insul3.1)

clean.insul1.0 <- unlist(strsplit(wall.dat3$Wall.Exterior.Insulation.Thickness.1, " "))
clean.insul1.00 <- as.data.frame(matrix(clean.insul1.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul1.2 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , clean.insul1.00)
dim(clean.insul1.2)

clean.insul2.0 <- unlist(strsplit(wall.dat3$Wall.Exterior.Insulation.Thickness.2, " "))
clean.insul2.00 <- as.data.frame(matrix(clean.insul2.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul2.2 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , clean.insul2.00)
dim(clean.insul2.2)

clean.insul3.0 <- unlist(strsplit(wall.dat3$Wall.Exterior.Insulation.Thickness.3, " "))
clean.insul3.00 <- as.data.frame(matrix(clean.insul3.0, ncol = 2, byrow = T), stringsAsFactors = F)
clean.insul3.2 <- cbind.data.frame("CK_Cadmus_ID" = wall.dat3$CK_Cadmus_ID
                                   , "TMP_ID" = wall.dat3$TMP_ID
                                   , clean.insul3.00)
dim(clean.insul3.2)

clean.insul.join1 <- left_join(clean.insul1.1,    clean.insul2.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join2 <- left_join(clean.insul.join1, clean.insul3.1, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join3 <- left_join(clean.insul.join2, clean.insul1.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join4 <- left_join(clean.insul.join3, clean.insul2.2, by = c("CK_Cadmus_ID", "TMP_ID"))
clean.insul.join5 <- left_join(clean.insul.join4, clean.insul3.2, by = c("CK_Cadmus_ID", "TMP_ID"))

colnames(clean.insul.join5) <- c("CK_Cadmus_ID"
                                 ,"TMP_ID"
                                 ,"cavity.inches1"
                                 ,"Remove.1"
                                 ,"cavity.inches2"
                                 ,"Remove.2"
                                 ,"cavity.inches3"
                                 ,"Remove.3"
                                 ,"exterior.inches1"
                                 ,"Remove.1"
                                 ,"exterior.inches2"
                                 ,"Remove.2"
                                 ,"exterior.inches3"
                                 ,"Remove.3")

clean.thickness.data <- clean.insul.join5[-grep("Remove", colnames(clean.insul.join5))]

###########################
# End cleaning step
###########################

#make into dataframe
wall.dat4 <- as.data.frame(left_join(wall.dat3, clean.thickness.data, by = c("CK_Cadmus_ID", "TMP_ID"))
                             , stringsAsFactors = F) 
# warning here is OK

###########################
# Cleaning inches and rvalue information
###########################
# make numeric
wall.dat4$cavity.inches1   <- as.numeric(as.character(wall.dat4$cavity.inches1)) # warning here is OK
wall.dat4$cavity.inches2   <- as.numeric(as.character(wall.dat4$cavity.inches2)) # warning here is OK
wall.dat4$cavity.inches3   <- as.numeric(as.character(wall.dat4$cavity.inches3)) # warning here is OK
wall.dat4$exterior.inches1 <- as.numeric(as.character(wall.dat4$exterior.inches1)) # warning here is OK
wall.dat4$exterior.inches2 <- as.numeric(as.character(wall.dat4$exterior.inches2)) # warning here is OK
wall.dat4$exterior.inches3 <- as.numeric(as.character(wall.dat4$exterior.inches3)) # warning here is OK

#replace any inches that are NA with zeros
for(i in 28:33){
  wall.dat4[,i] <- ifelse(is.na(wall.dat4[,i]), 0, wall.dat4[,i])
}

#update column names
wall.dat4$cavity.rvalues1 <- wall.dat4$Wall.Cavity.Insulation.Type.1
wall.dat4$cavity.rvalues2 <- wall.dat4$Wall.Cavity.Insulation.Type.2
wall.dat4$cavity.rvalues3 <- wall.dat4$Wall.Cavity.Insulation.Type.3
wall.dat4$exterior.rvalues1 <- wall.dat4$Wall.Exterior.Insulation.Type.1
wall.dat4$exterior.rvalues2 <- wall.dat4$Wall.Exterior.Insulation.Type.2
wall.dat4$exterior.rvalues3 <- wall.dat4$Wall.Exterior.Insulation.Type.3

#fix names that are not in R value table
wall.dat4$cavity.rvalues1[which(wall.dat4$cavity.rvalues1 == "Fiberglass or mineral wool batts")] <- "Mineral wool batts"
wall.dat4$cavity.rvalues1[which(wall.dat4$cavity.rvalues1 == "Unknown fiberglass")]               <- "Unknown"
wall.dat4$cavity.rvalues1[which(wall.dat4$cavity.rvalues1 == "-- Datapoint not asked for --")]    <- NA
wall.dat4$cavity.rvalues1[which(wall.dat4$cavity.rvalues1 == "None")]                             <- NA
wall.dat4$cavity.rvalues2[which(wall.dat4$cavity.rvalues2 == "Extruded polystyrene (blue)")]      <- "Extruded polystyrene foam board"
wall.dat4$cavity.rvalues2[which(wall.dat4$cavity.rvalues2 == "N/A")]                              <- NA
wall.dat4$cavity.rvalues2[which(wall.dat4$cavity.rvalues2 == "-- Datapoint not asked for --")]    <- NA
wall.dat4$exterior.rvalues1[which(wall.dat4$exterior.rvalues1 == "-- Datapoint not asked for --")]    <- NA
wall.dat4$exterior.rvalues1[which(wall.dat4$exterior.rvalues1 == "Extruded polystyrene foam board (pink or blue)")] <- "Extruded polystyrene foam board"
wall.dat4$exterior.rvalues1[which(wall.dat4$exterior.rvalues1 == "Expanded polystyrene foam board (white)")] <- "Expanded polystyrene foam board"

###########################
# End cleaning step
###########################


###########################
# Replace R-value Names with Values from R value table
###########################
for (i in 1:length(rvals$Type.of.Insulation)){
  wall.dat4$cavity.rvalues1[which(wall.dat4$cavity.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  wall.dat4$cavity.rvalues2[which(wall.dat4$cavity.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  wall.dat4$cavity.rvalues3[which(wall.dat4$cavity.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  wall.dat4$exterior.rvalues1[which(wall.dat4$exterior.rvalues1 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  wall.dat4$exterior.rvalues2[which(wall.dat4$exterior.rvalues2 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
  wall.dat4$exterior.rvalues3[which(wall.dat4$exterior.rvalues3 == rvals$Type.of.Insulation[i])] <- rvals$`Avg..R-Value.Per.Inch`[i]
}

###########################
# End Replace Step
###########################

wall.dat4$cavity.rvalues1[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$cavity.rvalues2[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$cavity.rvalues3[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$exterior.rvalues1[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
wall.dat4$exterior.rvalues2[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
wall.dat4$exterior.rvalues3[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
wall.dat4$cavity.inches1[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$cavity.inches2[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$cavity.inches3[which(wall.dat4$`Wall.Cavity.Insulated?` == "No")] <- 0
wall.dat4$exterior.inches1[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
wall.dat4$exterior.inches2[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0
wall.dat4$exterior.inches3[which(wall.dat4$`Wall.Exterior.Insulated?` == "No")] <- 0

#replace any inches and rvalues that are NA with zeros
for(i in 28:39){
  wall.dat4[,i] <- ifelse(is.na(wall.dat4[,i]), 0, wall.dat4[,i])
}


#make all inches and rvalue columns numeric
for(i in 28:39){
  wall.dat4[,i] <- as.numeric(as.character(wall.dat4[,i]))
}

wall.dat5 <- wall.dat4


#check uniques -- None should be NA
unique(wall.dat5$cavity.rvalues1)
unique(wall.dat5$cavity.rvalues2)
unique(wall.dat5$cavity.rvalues3)
unique(wall.dat5$exterior.rvalues1)
unique(wall.dat5$exterior.rvalues2)
unique(wall.dat5$exterior.rvalues3)



###########################
# Analysis: Calculate weighted R values by site, convert to U values
###########################

#create total.r.value column
wall.dat5$total.r.val <- NA

#calculate the weighted r value
wall.dat5$total.r.val <- (wall.dat5$cavity.rvalues1 * wall.dat5$cavity.inches1) +  
  (wall.dat5$cavity.rvalues2 * wall.dat5$cavity.inches2) + 
  (wall.dat5$exterior.rvalues1 * wall.dat5$exterior.inches3)

#check -- NOTE -- NONE SHOULD BE NA
unique(wall.dat5$total.r.val)

#caluclate u factors = inverse of Rvalue
wall.dat5$uvalue <- 1 / wall.dat5$total.r.val
unique(wall.dat5$uvalue)

# replace inf with 0
wall.dat5$uvalue[which(wall.dat5$uvalue == "Inf")] <- 0

#make area numeric
wall.dat5$uvalue <- as.numeric(as.character(wall.dat5$uvalue))
wall.dat5$Wall.Area <- as.numeric(as.character(wall.dat5$Wall.Area))

#weight the u factor per home -- where weights are the wall area within home
weightedU <- summarise(group_by(wall.dat5, CK_Cadmus_ID, Wall.Type)
                       ,aveUval = sum(Wall.Area * uvalue) / sum(Wall.Area)
)

#back-calculate the weight r values
weightedU$aveRval <- 1 / as.numeric(as.character(weightedU$aveUval))
unique(weightedU$aveRval)
weightedU$aveRval[which(weightedU$aveRval == "Inf")] <- 0

wall.unique <- unique(wall.dat5[which(colnames(wall.dat5) %in% c("CK_Cadmus_ID","BuildingType"))])

wall.dat6 <- left_join(weightedU, wall.unique, by = "CK_Cadmus_ID")

#merge analysis data with cleaned RBSA data
wall.dat7 <- left_join(neea.dat, wall.dat6, by = "CK_Cadmus_ID")





