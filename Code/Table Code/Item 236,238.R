#############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          06/13/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

# Read in clean RBSA data
rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData, paste("clean.rbsa.data", rundate, ".xlsx", sep = "")))
length(unique(rbsa.dat$CK_Cadmus_ID)) #601

#read in Envelope data for MF table
envelope.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, envelope.export))
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))
envelope.dat1 <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID"
                                                                  ,"CK_SiteID"
                                                                  ,"Ceiling.Type"
                                                                  ,"Ceiling.Area"
                                                                  ,"Floor.Type"
                                                                  ,"Floor.Area"
                                                                  ,"Floor.Sub-Type",
                                                                  "Type.of.Area.Below"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(envelope.dat2$CK_Cadmus_ID)) #601
envelope.dat3 <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

unique(envelope.dat3$Ceiling.Type)
envelope.dat3$CeilingType <- ""
envelope.dat3$CeilingType[grep("Attic",envelope.dat3$Ceiling.Type,ignore.case = T)] <- "Attic"
envelope.dat3$CeilingType[grep("Vaulted",envelope.dat3$Ceiling.Type,ignore.case = T)] <- "Vaulted"
envelope.dat3$CeilingType[grep("Roof Deck",envelope.dat3$Ceiling.Type,ignore.case = T)] <- "Roof Deck"
envelope.dat3$CeilingType[grep("Sloped",envelope.dat3$Ceiling.Type,ignore.case = T)] <- "Vaulted"
envelope.dat3$CeilingType[which(!(envelope.dat3$CeilingType %in% c("Attic", "Vaulted","Roof Deck")))] <- "Other"

unique(envelope.dat3$Floor.Type)
envelope.dat3$FloorType <- ""
envelope.dat3$FloorType[grep("Slab",envelope.dat3$Floor.Type,ignore.case = T)] <- "Slab"
envelope.dat3$FloorType[grep("Crawlspace",envelope.dat3$Floor.Type,ignore.case = T)] <- "Crawlspace"
envelope.dat3$FloorType[grep("Basement",envelope.dat3$Floor.Type,ignore.case = T)] <- "Basement"
envelope.dat3$FloorType[grep("Floor over other area",envelope.dat3$Floor.Type,ignore.case = T)] <- "Framed"
envelope.dat3$FloorType[grep("Cantilever",envelope.dat3$Floor.Type,ignore.case = T)] <- "Cantilever"

envelope.dat3$SubFloorType <- "Unknown"
unique(envelope.dat3$`Floor.Sub-Type`)
envelope.dat3$SubFloorType[grep("framed",envelope.dat3$FloorType,ignore.case = T)] <- 
  envelope.dat3$Type.of.Area.Below[grep("framed",envelope.dat3$FloorType,ignore.case = T)]
envelope.dat3$SubFloorType[grep("conditioned",envelope.dat3$`Floor.Sub-Type`,ignore.case = T)] <- "conditioned"
envelope.dat3$SubFloorType[grep("unconditioned",envelope.dat3$`Floor.Sub-Type`,ignore.case = T)] <- "unconditioned"
envelope.dat3$SubFloorType[grep("slab",envelope.dat3$`Floor.Sub-Type`,ignore.case = T)] <- "slab"

envelope.dat3$FloorType <- paste(envelope.dat3$FloorType, envelope.dat3$SubFloorType)
#############################################################################################
#Item 236: Table 30
#############################################################################################

item236.dat <- unique(envelope.dat3[,c("CK_Cadmus_ID",
                                       "CK_SiteID",
                                       "Ceiling.Type",
                                       "Ceiling.Area",
                                       "BuildingTypeXX",
                                       "CeilingType")])
item236.dat1 <- item236.dat[which(item236.dat$Ceiling.Area > 0),] 

item236.sum1 <- summarise(group_by(item236.dat1, BuildingTypeXX,CeilingType)
                          ,CeilingAreaByWallType = sum(Ceiling.Area))

item236.sum2 <- summarise(group_by(item236.dat1, BuildingTypeXX)
                          ,CeilingAreaTotal = sum(Ceiling.Area)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item236.sum3 <- summarise(group_by(item236.dat1,CeilingType)
                          ,BuildingTypeXX = "All Sizes"
                          ,CeilingAreaByWallType = sum(Ceiling.Area))

item236.sum4 <- summarise(group_by(item236.dat1)
                          ,BuildingTypeXX = "All Sizes"
                          ,CeilingAreaTotal = sum(Ceiling.Area)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item236.Type <- rbind.data.frame(item236.sum1,
                                     item236.sum3,
                                     stringsAsFactors = F)
item236.Totals <- rbind.data.frame(item236.sum2,
                                   item236.sum4,
                                   stringsAsFactors = F)

item236.all <- left_join(item236.Type,
                         item236.Totals,
                         by = c("BuildingTypeXX"))
item236.all$Percent <- item236.all$CeilingAreaByWallType/item236.all$CeilingAreaTotal
item236.all$SE      <- sqrt(item236.all$Percent * (1 - item236.all$Percent) / item236.all$SampleSize)

item236.table <- dcast(setDT(item236.all)
                       , formula = BuildingTypeXX + SampleSize ~ CeilingType
                       , value.var = c("Percent", "SE"))
item236.final <- data.frame( "Building Type" = item236.table$BuildingTypeXX
                             ,"Attic" = item236.table$Percent_Attic
                             ,"Attic SE" = item236.table$SE_Attic
                             ,"Vaulted" = item236.table$Percent_Vaulted
                             ,"Vaulted SE" = item236.table$SE_Vaulted
                             ,"Other" = item236.table$Percent_Other
                             ,"Other SE" = item236.table$SE_Other
                             ,"SampleSize" = item236.table$SampleSize)
#############################################################################################
#Item 238: Table 28
#############################################################################################

item238.dat <- unique(envelope.dat3[,c("CK_Cadmus_ID",
                                       "CK_SiteID",
                                       "BuildingTypeXX",
                                       "FloorType",
                                       "SubFloorType",
                                       "Floor.Area")])
item238.dat1 <- item238.dat[which(item238.dat$Floor.Area > 0),] 

item238.dat1$Count <- 1
item238.sum1 <- summarise(group_by(item238.dat1, BuildingTypeXX,FloorType)
                          ,FloorAreaByWallType = sum(Floor.Area)
                          ,n = 0)

item238.sum2 <- summarise(group_by(item238.dat1, BuildingTypeXX)
                          ,FloorAreaTotal = sum(Floor.Area)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item238.sum3 <- summarise(group_by(item238.dat1,FloorType)
                          ,BuildingTypeXX = "All Sizes"
                          ,FloorAreaByWallType = sum(Floor.Area)
                          ,n = sum(Count))
n_total <- item238.sum3[,c("FloorType","n")]
item238.sum4 <- summarise(group_by(item238.dat1)
                          ,BuildingTypeXX = "All Sizes"
                          ,FloorAreaTotal = sum(Floor.Area)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))
                                
item238.Type <- rbind.data.frame(item238.sum1,
                                 item238.sum3,
                                 stringsAsFactors = F)
item238.Type2 <- left_join(item238.Type,n_total,by="FloorType")
item238.Totals <- rbind.data.frame(item238.sum2,
                                   item238.sum4,
                                   stringsAsFactors = F)

item238.all <- left_join(item238.Type2,
                         item238.Totals,
                         by = c("BuildingTypeXX"))                           
item238.all$Percent <- item238.all$FloorAreaByWallType/item238.all$FloorAreaTotal
item238.all$SE      <- sqrt(item238.all$Percent * (1 - item238.all$Percent) / item238.all$SampleSize)
item238.all$n <- item238.all$n.y
item238.table <- dcast(setDT(item238.all)
                       , formula = FloorType + n ~ BuildingTypeXX
                       , value.var = c("Percent", "SE"))

item238.final <- data.frame( "Building Type" = item238.table$FloorType
                             ,"Low Rise (Less than 3)" = item238.table$`Percent_Apartment Building (3 or fewer floors)`
                             ,"Low Rise (Less than 3) SE" = item238.table$`SE_Apartment Building (3 or fewer floors)`
                             ,"Mid-Rise (4-6)" = item238.table$`Percent_Apartment Building (4 to 6 floors)`
                             ,"Mid-Rise (4-6) SE" = item238.table$`SE_Apartment Building (4 to 6 floors)`
                             ,"High-Rise(7+)" = item238.table$`Percent_Apartment Building (More than 6 floors)`
                             ,"High-Rise(7+) SE" = item238.table$`SE_Apartment Building (More than 6 floors)`
                             ,"n" = item238.table$n)
