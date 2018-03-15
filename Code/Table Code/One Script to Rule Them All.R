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

# Source codes
source("Code/Table Code/SourceCode.R")
source("Code/Table Code/Weighting Implementation Functions.R")
source("Code/Sample Weighting/Weights.R")
source("Code/Table Code/Export Function.R")

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
sites.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, sites.export))
windows.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, windows.export))




#############################################################################################
# Source all SF scripts
#############################################################################################
run <- try(source("Code/Table Code/SF/Items 1,2,6.R"))
run <- try(source("Code/Table Code/SF/Items 3-5.R"))
run <- try(source("Code/Table Code/SF/Items 7-9.R"))
run <- try(source("Code/Table Code/SF/Items 10-18,175,235,Table AP.R")) #insulation
run <- try(source("Code/Table Code/SF/Items 19-22.R"))
run <- try(source("Code/Table Code/SF/Items 23,176,239.R")) #insulaion
run <- try(source("Code/Table Code/SF/Items 24,25,28,29.R"))
run <- try(source("Code/Table Code/SF/Items 26,30,31,177,178,237.R")) #insulaion
run <- try(source("Code/Table Code/SF/Items 32-34.R"))
run <- try(source("Code/Table Code/SF/Item 35.R"))
run <- try(source("Code/Table Code/SF/Items 36,37,182.R"))
run <- try(source("Code/Table Code/SF/Items 38-42.R"))
run <- try(source("Code/Table Code/SF/Items 43-46.R"))
run <- try(source("Code/Table Code/SF/Items 47-49.R"))
run <- try(source("Code/Table Code/SF/Items 50-53.R"))
run <- try(source("Code/Table Code/SF/Items 54,55,190.R"))
run <- try(source("Code/Table Code/SF/Items 56-58.R"))
run <- try(source("Code/Table Code/SF/Items 59,60.R"))
run <- try(source("Code/Table Code/SF/Items 66-67.R"))
run <- try(source("Code/Table Code/SF/Items 68-70, Table AE.R"))
run <- try(source("Code/Table Code/SF/Items 71-76,Tables XX,YY,AF.R"))
run <- try(source("Code/Table Code/SF/Items 77-79, Tables ZZ,AG,AH.R"))
run <- try(source("Code/Table Code/SF/Items 80,81, Tables AB,AC.R"))
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
run <- try(source("Code/Table Code/SF/Items 82-85.R"))
run <- try(source("Code/Table Code/SF/Items 86-88.R"))
run <- try(source("Code/Table Code/SF/Items 89,91,93, Table AJ.R"))
run <- try(source("Code/Table Code/SF/Items 90,92.R"))
run <- try(source("Code/Table Code/SF/Items 94,95.R"))
run <- try(source("Code/Table Code/SF/Items 96-98, Tables SS,AI.R"))
run <- try(source("Code/Table Code/SF/Items 99-101.R"))
run <- try(source("Code/Table Code/SF/Items 102-105.R"))
run <- try(source("Code/Table Code/SF/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/SF/Items 107-110.R"))
run <- try(source("Code/Table Code/SF/Item 111.R"))
run <- try(source("Code/Table Code/SF/Items 112-114.R"))
run <- try(source("Code/Table Code/SF/Items 115-120.R"))
run <- try(source("Code/Table Code/SF/Items 121-123.R"))
run <- try(source("Code/Table Code/SF/Items 124-127, Table AD.R"))
run <- try(source("Code/Table Code/SF/Items 128-131.R"))
run <- try(source("Code/Table Code/SF/Items 132-134, Table AU.R"))
run <- try(source("Code/Table Code/SF/Items 135-138.R"))
run <- try(source("Code/Table Code/SF/Items 143, 144, 146, 147, 148, 150.R"))
run <- try(source("Code/Table Code/SF/Items 145 and 149.R"))
run <- try(source("Code/Table Code/SF/Items 151-153.R"))
run <- try(source("Code/Table Code/SF/Items 154, 155.R"))
run <- try(source("Code/Table Code/SF/Items 139-142.R"))
run <- try(source("Code/Table Code/SF/Item 156.R"))
run <- try(source("Code/Table Code/SF/Item 157.R"))
run <- try(source("Code/Table Code/SF/Item 158.R"))
run <- try(source("Code/Table Code/SF/Item 159.R"))
run <- try(source("Code/Table Code/SF/Item 160.R"))
run <- try(source("Code/Table Code/SF/Item 161.R"))
run <- try(source("Code/Table Code/SF/Item 162.R"))
run <- try(source("Code/Table Code/SF/Item 163.R"))
run <- try(source("Code/Table Code/SF/Items 164,165.R"))
run <- try(source("Code/Table Code/SF/Item 166.R"))
run <- try(source("Code/Table Code/SF/Items 167,168.R"))
run <- try(source("Code/Table Code/SF/Item 169.R"))
run <- try(source("Code/Table Code/SF/Items 170,171.R"))
run <- try(source("Code/Table Code/SF/Item 172.R"))
run <- try(source("Code/Table Code/SF/Table AA, Table BB.R"))
run <- try(source("Code/Table Code/SF/Table AK.R"))
run <- try(source("Code/Table Code/SF/Table AL.R"))
run <- try(source("Code/Table Code/SF/Table AV.R"))
run <- try(source("Code/Table Code/SF/Table B-18.R"))
run <- try(source("Code/Table Code/SF/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/SF/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/SF/Tables RR,TT,UU,VV.R"))




#############################################################################################
# Source all MH scripts
#############################################################################################
run <- try(source("Code/Table Code/MH/Items 1,2.R"))
run <- try(source("Code/Table Code/MH/Items 4,5.R"))
run <- try(source("Code/Table Code/MH/Items 7-9.R"))
run <- try(source("Code/Table Code/MH/Items 12,175,Table AP.R")) #insulation
run <- try(source("Code/Table Code/MH/Items 23,176.R")) #insulaion
run <- try(source("Code/Table Code/MH/Items 177,178.R")) #insulaion
run <- try(source("Code/Table Code/MH/Items 36,37,182.R"))
run <- try(source("Code/Table Code/MH/Items 38-42.R"))
run <- try(source("Code/Table Code/MH/Items 43-46.R"))
run <- try(source("Code/Table Code/MH/Items 47-49.R"))
run <- try(source("Code/Table Code/MH/Items 50-53.R"))
run <- try(source("Code/Table Code/MH/Items 54,55,190.R"))
run <- try(source("Code/Table Code/MH/Items 56-58.R"))
run <- try(source("Code/Table Code/MH/Items 66-67.R"))
run <- try(source("Code/Table Code/MH/Items 68-70, Table AE.R"))
run <- try(source("Code/Table Code/MH/Items 71-76,Tables XX,YY,AF.R"))
run <- try(source("Code/Table Code/MH/Items 77-79, Tables ZZ,AG,AH.R"))
run <- try(source("Code/Table Code/MH/Items 80,81, Tables AB,AC.R"))
run <- try(source("Code/Table Code/MH/Items 82-85.R"))
run <- try(source("Code/Table Code/MH/Items 86-88.R"))
run <- try(source("Code/Table Code/MH/Items 89,91,93, Table AJ.R")) #no errors unless otherwise specfied after this scripts
run <- try(source("Code/Table Code/MH/Items 90,92.R"))
run <- try(source("Code/Table Code/MH/Items 94,95.R"))
run <- try(source("Code/Table Code/MH/Items 96-98, Tables SS,AI.R")) #Error, sheet SS does not exist
run <- try(source("Code/Table Code/MH/Item 99.R"))
run <- try(source("Code/Table Code/MH/Item 105.R"))
run <- try(source("Code/Table Code/MH/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/MH/Items 107-110.R"))
run <- try(source("Code/Table Code/MH/Item 111.R"))
run <- try(source("Code/Table Code/MH/Items 112-114.R"))
run <- try(source("Code/Table Code/MH/Items 115-120.R"))
run <- try(source("Code/Table Code/MH/Items 121-123.R"))
run <- try(source("Code/Table Code/MH/Items 124-127, Table AD.R"))
run <- try(source("Code/Table Code/MH/Items 128-131.R"))
run <- try(source("Code/Table Code/MH/Items 132-134, Table AU.R"))
run <- try(source("Code/Table Code/MH/Items 135-138.R"))
run <- try(source("Code/Table Code/MH/Items 139-142.R"))
run <- try(source("Code/Table Code/MH/Items 143, 144, 146, 147, 148, 150.R"))
run <- try(source("Code/Table Code/MH/Items 145 and 149.R"))
run <- try(source("Code/Table Code/MH/Items 151-153.R"))
run <- try(source("Code/Table Code/MH/Items 154, 155.R"))
run <- try(source("Code/Table Code/MH/Item 173.R"))
run <- try(source("Code/Table Code/MH/Items 177,178.R"))
run <- try(source("Code/Table Code/MH/Item 179.R"))
run <- try(source("Code/Table Code/MH/Item 180.R"))
run <- try(source("Code/Table Code/MH/Item 192.R"))
run <- try(source("Code/Table Code/MH/Items 202-203.R"))
run <- try(source("Code/Table Code/MH/Table AA, Table BB.R"))
run <- try(source("Code/Table Code/MH/Table AK.R")) #wrong total n for weighted
run <- try(source("Code/Table Code/MH/Table AL.R"))
run <- try(source("Code/Table Code/MH/Table AV.R"))
run <- try(source("Code/Table Code/MH/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/MH/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/MH/Tables RR,TT,UU,VV.R"))
