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
mechanical.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, mechanical.export))
#clean cadmus IDs
mechanical.dat$CK_Cadmus_ID <- trimws(toupper(mechanical.dat$CK_Cadmus_ID))


#Read in data for analysis
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
windows.doors.dat$CK_Cadmus_ID <- trimws(toupper(windows.doors.dat$CK_Cadmus_ID))



#############################################################################################
#
#For Mechanical information
#
#############################################################################################
#subset to columns needed for analysis
item166.dat.1 <- mechanical.dat[which(colnames(mechanical.dat) %in% c("CK_Cadmus_ID"
                                                                      ,"Generic"
                                                                      ,"Primary.Heating.System"
                                                                      ,"Heating.Fuel"
                                                                      ,""
                                                                      ,""))]

#remove any repeat header rows from exporting
item166.dat.11 <- item166.dat.1[which(item166.dat.1$CK_Cadmus_ID != "CK_CADMUS_ID"),]

#Keep only Yes and No in primary heating system indicator
item166.dat.12 <- item166.dat.11[which(item166.dat.11$Primary.Heating.System == "Yes"),]
length(unique(item166.dat.12$CK_Cadmus_ID)) #576 out of 601
#check uniques
unique(item166.dat.12$Primary.Heating.System)
item166.dat.12$count <- 1

item166.dat.13 <- unique(item166.dat.12[which(item166.dat.12$Heating.Fuel == "Electric"),])

item166.sum1 <- summarise(group_by(item166.dat.13, CK_Cadmus_ID, Heating.Fuel)
                          ,Count = sum(count))
item166.sum1$Count <- 1
which(duplicated(item166.sum1$CK_Cadmus_ID)) #none are duplicated!
unique(item166.sum1$Heating.Fuel)

item166.mechanical <- item166.sum1






#############################################################################################
# 
#Windows Data
# 
#############################################################################################
item166.dat <- windows.doors.dat[which(colnames(windows.doors.dat) %in% c("CK_Cadmus_ID"
                                                                         ,"Type"
                                                                         ,"Sub-Type"
                                                                         ,"Area"
                                                                         ,"Quantity"
                                                                         ,"Frame./.Body.Type"
                                                                         ,"Glazing.Type"))]

item166.dat0 <- left_join(item166.dat, item166.mechanical, by = "CK_Cadmus_ID")
item166.dat0.1 <- item166.dat0[which(item166.dat0$Heating.Fuel == "Electric"),]


item166.dat1 <- left_join(item166.dat0.1, rbsa.dat, by = "CK_Cadmus_ID")
length(unique(item166.dat1$CK_Cadmus_ID)) #316

#subset to only windows
item166.dat2 <- item166.dat1[which(item166.dat1$Type == "Window"),]

#clean up frame/body type
unique(item166.dat2$`Frame./.Body.Type`)
item166.dat2$Frame.Type <- trimws(item166.dat2$`Frame./.Body.Type`)
item166.dat2$Frame.Type[grep("Wood|Vinyl|Fiberglass", item166.dat2$Frame.Type)] <- "Wood/Vinyl/Fiberglass"
item166.dat2$Frame.Type[grep("Metal|Aluminum", item166.dat2$Frame.Type)] <- "Metal"
item166.dat2$Frame.Type[grep("N/A", item166.dat2$Frame.Type)] <- "Unknown"
unique(item166.dat2$Frame.Type)

#clean up glazing types
item166.dat2$Glazing <- trimws(item166.dat2$Glazing.Type)
item166.dat2$Glazing[grep("Single", item166.dat2$Glazing)] <- "Single Glazed"
item166.dat2$Glazing[grep("Double", item166.dat2$Glazing)] <- "Double Glazed"
item166.dat2$Glazing[grep("Triple", item166.dat2$Glazing)] <- "Triple Glazed"
item166.dat2$Glazing[which(!(item166.dat2$Glazing %in% c("Single Glazed", "Double Glazed", "Triple Glazed")))] <- "Unknown"
unique(item166.dat2$Glazing)

item166.dat2$Framing.Categories <- paste(item166.dat2$Frame.Type, item166.dat2$Glazing, sep = " ")

item166.dat2$count <- 1


#####################################Analysis - needs cleaning & work ###################################33
# summarise by state and framing categories
item166.tmp1 <- summarise(group_by(item166.dat2, BuildingType, State, Framing.Categories)
                         , SampleSize = length(unique(CK_Cadmus_ID))
                         , Count = sum(count))
# summarise by state across framing categories
item166.tmp2 <- summarise(group_by(item166.dat2, BuildingType, State)
                         , Framing.Categories = "All Window Types"
                         , SampleSize = length(unique(CK_Cadmus_ID))
                         , Count = sum(count))
#summarise by framing categories across states
item166.tmp3 <- summarise(group_by(item166.dat2, BuildingType, Framing.Categories)
                         , State = "Region"
                         , SampleSize = length(unique(CK_Cadmus_ID))
                         , Count = sum(count))
item166.tmp4 <- summarise(group_by(item166.dat2, BuildingType)
                         , State = "Region"
                         , Framing.Categories = "All Window Types"
                         , SampleSize = length(unique(CK_Cadmus_ID))
                         , Count = sum(count))

item166.merge1 <- rbind.data.frame(item166.tmp1, item166.tmp2, item166.tmp3, item166.tmp4, stringsAsFactors = F)

#obtain total counts
item166.tot.counts <- rbind.data.frame(item166.tmp2, item166.tmp4, stringsAsFactors = F)

item166.final <- left_join(item166.merge1, item166.tot.counts, by = c("BuildingType","State"))
colnames(item166.final) <- c("BuildingType"
                            ,"State"
                            ,"Framing.Categories"
                            ,"SampleSize"
                            ,"Count"
                            ,"Remove"
                            ,"Remove"
                            ,"TotalCount")

item166.final$Percent <- item166.final$Count / item166.final$TotalCount
item166.final$SE      <- sqrt(item166.final$Percent * (1 - item166.final$Percent) / item166.final$SampleSize)

##################################### Table Format ###################################33
detach(package:reshape2)
library(data.table)
item166.table <- dcast(setDT(item166.final)
                      , formula = BuildingType + Framing.Categories ~ State
                      , value.var = c("Percent", "SE", "SampleSize"))

item166.table2 <- data.frame("BuildingType" = item166.table$BuildingType
                            ,"Framing.Categories" = item166.table$Framing.Categories
                            ,"Percent_MT" = item166.table$Percent_MT
                            ,"SE_MT" = item166.table$SE_MT
                            ,"Percent_WA" = item166.table$Percent_WA
                            ,"SE_WA" = item166.table$SE_WA
                            ,"Percent_Region" = item166.table$Percent_Region
                            ,"SE_Region" = item166.table$SE_Region
                            ,"SampleSize" = item166.table$SampleSize_Region)

item166.table3 <- item166.table2[which(item166.table2$BuildingType %in% c("Single Family")),]
