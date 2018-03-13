#############################################################################################
##  Title:            RBSA Analysis - script to source all scripts                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          03/13/2018
##  Updated:                                            
##  Billing Code(s):  
#############################################################################################


#############################################################################################
# Read in all datasets
#############################################################################################
one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
mechanical.dat <- rad.xlsx(mechanical.export)
download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
envelope.dat <- rad.xlsx(envelope.export)
room.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))






#############################################################################################
# Source all SF scripts
#############################################################################################
source("Code/Table Code/Items 1,2,6.R")
source("Code/Table Code/Items 3-5.R")