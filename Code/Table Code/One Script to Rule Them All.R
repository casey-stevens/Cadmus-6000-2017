#############################################################################################
##  Title:            RBSA Analysis - script to source all scripts                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          03/13/2018
##  Updated:                                            
##  Billing Code(s):  
#############################################################################################
# source("Code/Table Code/Step 1-Clean Data.R")
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')

#############################################################################################
# Read in all datasets
#############################################################################################
one.line.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.export), sheet = "Site One Line Summary", startRow = 2)
one.line.bldg.dat  <- read.xlsx(xlsxFile = file.path(filepathRawData, one.line.bldg.export), sheet = "Building One Line Summary", startRow = 3)
mechanical.dat <- read.xlsx(mechanical.export)
envelope.dat <- read.xlsx(envelope.export)
room.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, rooms.export))
rooms.dat <- room.dat
windows.doors.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))
lighting.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, lighting.export), startRow = 2)
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
sites.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.interview.export))
water.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, water.export))
survey.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, survey.export), sheet = "Labeled and Translated")





#############################################################################################
# Source all SF scripts
#############################################################################################
source("Code/Table Code/Items 1,2,6.R")
source("Code/Table Code/Items 3-5.R")
source("Code/Table Code/Items 7-9.R")
source("Code/Table Code/Items 10-18,175,235,Table AP.R") #having issues - insulation
source("Code/Table Code/Items 19-22.R")
source("Code/Table Code/Items 23,176,239.R") #insulaion
source("Code/Table Code/Items 24,25,28,29.R")
source("Code/Table Code/Items 26,30,31,177,178,237.R") #insulaion
source("Code/Table Code/Items 32-34.R")
source("Code/Table Code/Item 35.R")
source("Code/Table Code/Items 36,37,182.R")
source("Code/Table Code/Items 38-42.R")
source("Code/Table Code/Items 43-46.R")
source("Code/Table Code/Items 47-49.R")
source("Code/Table Code/Items 50-53.R")
source("Code/Table Code/Items 54,55,190.R")
source("Code/Table Code/Items 56-58.R")
source("Code/Table Code/Items 59,60.R")
source("Code/Table Code/Items 66-67.R")
source("Code/Table Code/Items 68-70, Table AE.R")
source("Code/Table Code/Items 71-76,Tables XX,YY,AF.R")
source("Code/Table Code/Items 77-79, Tables ZZ,AG,AH.R")
#appliances
source("Code/Table Code/Items 80,81, Tables AB,AC.R")
source("Code/Table Code/Items 82-85.R")

#not ran yet
source("Code/Table Code/Items 86-88.R")
source("Code/Table Code/Items 89,91,93, Table AJ.R")
source("Code/Table Code/Items 90,92.R")
source("Code/Table Code/Items 94,95.R")
source("Code/Table Code/Items 96-98, Tables SS,AI.R")
source("Code/Table Code/Items 99-101.R")
source("Code/Table Code/Items 102-105.R")
source("Code/Table Code/Item 106, Tables FF,AM,AR,AS,AT.R")
source("Code/Table Code/Items 107-110.R")
source("Code/Table Code/Item 111.R")
source("Code/Table Code/Items 112-114.R")
source("Code/Table Code/Items 115-120.R")
source("Code/Table Code/Items 121-123.R")
source("Code/Table Code/Items 124-127, Table AD.R")
source("Code/Table Code/Items 128-131.R")
source("Code/Table Code/Items 132-134, Table AU.R")
source("Code/Table Code/Items 135-138.R")
source("Code/Table Code/Items 139-142.R")
source("Code/Table Code/Items 143, 144, 146, 147, 148, 150.R")
source("Code/Table Code/Items 145 and 149.R")
source("Code/Table Code/Items 151-153.R")
source("Code/Table Code/Items 154, 155.R")
source("Code/Table Code/Item 156.R")
source("Code/Table Code/Item 157.R")
source("Code/Table Code/Item 158.R")
source("Code/Table Code/Item 159.R")
source("Code/Table Code/Item 160.R")
source("Code/Table Code/Item 161.R")
source("Code/Table Code/Item 162.R")
source("Code/Table Code/Item 163.R")
source("Code/Table Code/Items 164,165.R")
source("Code/Table Code/Item 166.R")
source("Code/Table Code/Items 167,168.R")
source("Code/Table Code/Item 169.R")
source("Code/Table Code/Item 170,171.R")
source("Code/Table Code/Item 172.R")


#not yet updated in script
source("Code/Table Code/Table AA, Table BB.R")
source("Code/Table Code/Table AK.R")
source("Code/Table Code/Table AL.R")
source("Code/Table Code/Table AQ.R")
source("Code/Table Code/Table AV.R")
source("Code/Table Code/Table B-18.R")
source("Code/Table Code/Tables CC,NN,OO,WW.R")
source("Code/Table Code/Tables DD,EE,GG,HH,II,KK,LL,MM.R")
source("Code/Table Code/Tables RR,TT,UU,VV.R")