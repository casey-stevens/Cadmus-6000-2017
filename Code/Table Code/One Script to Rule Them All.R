#############################################################################################
##  Title:            RBSA Analysis - script to source all scripts                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          03/13/2018
##  Updated:                                            
##  Billing Code(s):  
#############################################################################################
source("Code/Table Code/Step 1-Clean Data.R")
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')

#############################################################################################
# Read in all datasets
#############################################################################################
one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
mechanical.dat <- read.xlsx(mechanical.export)
envelope.dat <- read.xlsx(envelope.export)
room.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat <- room.dat





#############################################################################################
# Source all SF scripts
#############################################################################################
source("Code/Table Code/Items 1,2,6.R")
source("Code/Table Code/Items 3-5.R")
source("Code/Table Code/Items 7-9.R")
# source("Code/Table Code/Items 10-18,175,235,Table AP.R") #having issues - insulation
source("Code/Table Code/Items 19-22.R")
# source("Code/Table Code/Items 23,176,239.R") #insulaion
source("Code/Table Code/Items 24,25,28,29.R")
