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
envelope.dat$CK_Cadmus_ID <- trimws(toupper(envelope.dat$CK_Cadmus_ID))





#############################################################################################
# Item 24: PERCENTAGE OF CRAWLSPACES WITH INSULATED WALLS BY STATE (SF table 31)
#############################################################################################
item24.dat <- envelope.dat[grep("CK_Cadmus_ID|Crawlspace", colnames(envelope.dat))]

item24.dat1 <- left_join(rbsa.dat, item24.dat, by = "CK_Cadmus_ID")
length(unique(item24.dat1$CK_Cadmus_ID)) #565

item24.dat2 <- item24.dat1[which(item24.dat1$`Crawlspace.Walls.Insulated?` %in% c("Yes", "No")),]
length(unique(item24.dat2$CK_Cadmus_ID)) #183

item24.dat2$count <- 1
item24.dat2$crawl.ins.ind <- 0
item24.dat2$crawl.ins.ind[which(item24.dat2$`Crawlspace.Walls.Insulated?` == "Yes")] <- 1

item24.final <- summarise(group_by(item24.dat2, BuildingType, State)
                          ,InsulatedCount = sum(crawl.ins.ind)
                          ,SampleSize     = sum(count)
                          ,Percent        = InsulatedCount / SampleSize
                          ,SE             = sqrt(Percent * (1 - Percent) / SampleSize)
)


item24.table <- data.frame("BuildingType" = item24.final$BuildingType
                           ,"State" = item24.final$State
                           ,"Percent" = item24.final$Percent
                           ,"SE" = item24.final$SE
                           ,"SampleSize" = item24.final$SampleSize)









#############################################################################################
# Item 25: PERCENTAGE OF HOMES WITH ATTICS BY STATE (SF table 32)
#############################################################################################
item25.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item25.dat0 <- left_join(rbsa.dat, item25.dat, by = "CK_Cadmus_ID")

item25.dat00 <- item25.dat0[which(item25.dat0$BuildingType == "Single Family"),]

item25.dat1 <- item25.dat00[which(item25.dat00$Ceiling.Type == "Attic"),]


item25.cnt <- summarise(group_by(item25.dat1, BuildingType, State)
                        , InsulatedCount = length(unique(CK_Cadmus_ID))
)

item25.SS <- summarise(group_by(item25.dat00, BuildingType, State)
                       , SampleSize = length(unique(CK_Cadmus_ID))
)

item25.final <- left_join(item25.cnt, item25.SS, by = c("BuildingType", "State"))
item25.final$Percent <- item25.final$InsulatedCount / item25.final$SampleSize
item25.final$SE      <- sqrt(item25.final$Percent * (1 - item25.final$Percent) / item25.final$SampleSize)

item25.table <- data.frame("BuildingType" = item25.final$BuildingType
                           ,"State" = item25.final$State
                           ,"Percent" = item25.final$Percent
                           ,"SE" = item25.final$SE
                           ,"SampleSize" = item25.final$SampleSize)






#############################################################################################
# Item 28: PERCENTAGE OF HOMES WITH VAULT CEILINGS BY STATE (SF table 35)
#############################################################################################
item28.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item28.dat0 <- left_join(rbsa.dat, item28.dat, by = "CK_Cadmus_ID")

item28.dat00 <- item28.dat0[which(item28.dat0$BuildingType == "Single Family"),]

item28.dat1 <- item28.dat00[which(item28.dat00$Ceiling.Type == "Sloped / Vaulted (no attic)"),]


item28.cnt <- summarise(group_by(item28.dat1, BuildingType, State)
                        , InsulatedCount = length(unique(CK_Cadmus_ID))
)

item28.SS <- summarise(group_by(item28.dat00, BuildingType, State)
                       , SampleSize = length(unique(CK_Cadmus_ID))
)

item28.final <- left_join(item28.cnt, item28.SS, by = c("BuildingType", "State"))
item28.final$Percent <- item28.final$InsulatedCount / item28.final$SampleSize
item28.final$SE      <- sqrt(item28.final$Percent * (1 - item28.final$Percent) / item28.final$SampleSize)

item28.table <- data.frame("BuildingType" = item28.final$BuildingType
                           ,"State" = item28.final$State
                           ,"Percent" = item28.final$Percent
                           ,"SE" = item28.final$SE
                           ,"SampleSize" = item28.final$SampleSize)






#############################################################################################
# Item 29: PERCENTAGE OF HOMES WITH ROOF DECK CEILINGS BY STATE (SF table 36)
#############################################################################################
item29.dat <- envelope.dat[which(colnames(envelope.dat) %in% c("CK_Cadmus_ID", "Ceiling.Type"))]

item29.dat0 <- left_join(rbsa.dat, item29.dat, by = "CK_Cadmus_ID")

item29.dat00 <- item29.dat0[which(item29.dat0$BuildingType == "Single Family"),]

item29.dat1 <- item29.dat00[which(item29.dat00$Ceiling.Type == "Roof Deck"),]


item29.cnt <- summarise(group_by(item29.dat1, BuildingType, State)
                        , InsulatedCount = length(unique(CK_Cadmus_ID))
)

item29.SS <- summarise(group_by(item29.dat00, BuildingType, State)
                       , SampleSize = length(unique(CK_Cadmus_ID))
)

item29.final <- left_join(item29.cnt, item29.SS, by = c("BuildingType", "State"))
item29.final$Percent <- item29.final$InsulatedCount / item29.final$SampleSize
item29.final$SE      <- sqrt(item29.final$Percent * (1 - item29.final$Percent) / item29.final$SampleSize)


item29.table <- data.frame("BuildingType" = item29.final$BuildingType
                           ,"State" = item29.final$State
                           ,"Percent" = item29.final$Percent
                           ,"SE" = item29.final$SE
                           ,"SampleSize" = item29.final$SampleSize)
