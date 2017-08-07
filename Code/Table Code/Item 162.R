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
                                                                ,"Foundation"
                                                                ,""))]

item162.dat0 <- left_join(item162.dat, item162.mechanical, by = "CK_Cadmus_ID")


item162.dat1 <- unique(item162.dat0[grep("crawl|Crawl", item162.dat0$Foundation),])

item162.dat2 <- left_join(rbsa.dat, item162.dat1, by = "CK_Cadmus_ID")

item162.dat2$count <- 1
item162.dat2$FloorOverCrawl <- 0
item162.dat2$FloorOverCrawl[which(item162.dat2$Foundation == "Crawlspace")] <- 1

#SUBSET TO ONLY SINGLE FAMILY
item162.dat3 <- item162.dat2[which(item162.dat2$BuildingType == "Single Family"),]

#subset to only electrically heated homes
item162.dat4 <- item162.dat3[which(item162.dat3$Heating.Fuel == "Electric"),]

item162.state <- summarise(group_by(item162.dat4, State)
                          ,Percent = sum(FloorOverCrawl) / sum(count)
                          ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                          ,SampleSize = length(unique(CK_Cadmus_ID)))

item162.region <- summarise(group_by(item162.dat4)
                           ,State = "Region"
                           ,Percent = sum(FloorOverCrawl) / sum(count)
                           ,SE = sqrt(Percent * (1 - Percent) / length(unique(CK_Cadmus_ID)))
                           ,SampleSize = length(unique(CK_Cadmus_ID)))

item162.final <- rbind.data.frame(item162.state, item162.region, stringsAsFactors = F)




