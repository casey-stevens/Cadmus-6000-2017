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
                                                                  ,"Wall.Area"
                                                                  ,"Wall.Type"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(envelope.dat2$CK_Cadmus_ID)) #601
envelope.dat3 <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

#############################################################################################
#Item 234: Table 26
#############################################################################################

item234.dat <- envelope.dat3[which(!is.na(envelope.dat3$Wall.Area)),]

item234.sum1 <- summarise(group_by(item234.dat, BuildingTypeXX,Wall.Type)
                                           ,WallAreaByWallType = sum(Wall.Area))

item234.sum2 <- summarise(group_by(item234.dat, BuildingTypeXX)
                                                 ,WallAreaTotal = sum(Wall.Area)
                                                 ,SampleSize = length(unique(CK_Cadmus_ID)))

item234.sum3 <- summarise(group_by(item234.dat,Wall.Type)
                          ,BuildingTypeXX = "All Sizes"
                          ,WallAreaByWallType = sum(Wall.Area))

item234.sum4 <- summarise(group_by(item234.dat)
                          ,BuildingTypeXX = "All Sizes"
                          ,WallAreaTotal = sum(Wall.Area)
                          ,SampleSize = length(unique(CK_Cadmus_ID)))


item234.WallType <- rbind.data.frame(item234.sum1,
                                     item234.sum3,
                                     stringsAsFactors = F)
item234.Totals <- rbind.data.frame(item234.sum2,
                                   item234.sum4,
                                   stringsAsFactors = F)

item234.all <- left_join(item234.WallType,
                         item234.Totals,
                         by = c("BuildingTypeXX"))
item234.all$Percent <- item234.all$WallAreaByWallType/item234.all$WallAreaTotal
item234.all$SE      <- sqrt(item234.all$Percent * (1 - item234.all$Percent) / item234.all$SampleSize)

item234.table <- dcast(setDT(item234.all)
                          , formula = BuildingTypeXX + SampleSize ~ Wall.Type
                          , value.var = c("Percent", "SE"))


