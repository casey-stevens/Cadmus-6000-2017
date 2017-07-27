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
                                                                  ,"Wall.Type"
                                                                  ,"Wall.Framing.Material"))]

envelope.dat2 <- left_join(rbsa.dat, envelope.dat1, by = "CK_Cadmus_ID")
length(unique(envelope.dat2$CK_Cadmus_ID)) #601
envelope.dat3 <- envelope.dat2[grep("Multifamily", envelope.dat2$BuildingType),]

#############################################################################################
#Item 234: Table 26
#############################################################################################

item234.dat <- envelope.dat3[which(!is.na(envelope.dat3$Wall.Area)),]

item234.dat$WallType <- ""
item234.dat$WallType[grep("masonry",item234.dat$Wall.Type,ignore.case = T)] <- "Masonry"
item234.dat$WallType[grep("framed",item234.dat$Wall.Type,ignore.case = T)] <- "Framed"
item234.dat$WallType[which(!(item234.dat$WallType %in% c("Masonry", "Framed")))] <- "Other"
item234.dat$WallType[which(item234.dat$WallType == "Framed")] <- item234.dat$Wall.Framing.Material[which(item234.dat$WallType == "Framed")]

item234.dat1 <- item234.dat[which(item234.dat$WallType != "Unknown"),]

item234.sum1 <- summarise(group_by(item234.dat1, BuildingTypeXX,WallType)
                                           ,WallAreaByWallType = sum(Wall.Area))

item234.sum2 <- summarise(group_by(item234.dat1, BuildingTypeXX)
                                                 ,WallAreaTotal = sum(Wall.Area)
                                                 ,SampleSize = length(unique(CK_Cadmus_ID)))

item234.sum3 <- summarise(group_by(item234.dat1,WallType)
                          ,BuildingTypeXX = "All Sizes"
                          ,WallAreaByWallType = sum(Wall.Area))

item234.sum4 <- summarise(group_by(item234.dat1)
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
                          , formula = BuildingTypeXX + SampleSize ~ WallType
                          , value.var = c("Percent", "SE"))
item234.final <- data.frame( "Building Type" = item234.table$BuildingTypeXX
                             ,"Masonry" = item234.table$Percent_Masonry
                             ,"Masonry SE" = item234.table$SE_Masonry
                             ,"Wood" = item234.table$Percent_Wood
                             ,"Wood SE" = item234.table$SE_Wood
                             ,"Other" = item234.table$Percent_Other
                             ,"Other SE" = item234.table$SE_Other
                             ,"SampleSize" = item234.table$SampleSize)

