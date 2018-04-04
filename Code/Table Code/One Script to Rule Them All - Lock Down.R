#############################################################################################
##  Title:            RBSA Analysis - script to source all scripts                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          03/13/2018
##  Updated:                                            
##  Billing Code(s):  
#############################################################################################
# source("Code/Table Code/Step 1-Clean Data - Lock Down.R")
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
buildings.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.export))
buildings.interview.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, buildings.interview.export))



#############################################################################################
# Source all SF scripts
#############################################################################################
run <- try(source("Code/Table Code/SF - Lock Down/Items 1,2,6.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 3-5.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 7-9.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 10-18,175,235,Table AP.R")) #insulation
run <- try(source("Code/Table Code/SF - Lock Down/Items 19-22.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 23,176,239.R")) #insulaion
run <- try(source("Code/Table Code/SF - Lock Down/Items 24,25,28,29.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 26,30,31,177,178,237.R")) #insulaion
run <- try(source("Code/Table Code/SF - Lock Down/Items 32-34.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 35.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 36,37,182.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 38-42.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 43-46.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 47-49.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 50-53.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 54,55,190.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 56-58.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 59,60.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 66-67.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 68-70, Table AE.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 71-76,Tables XX,YY,AF.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 77-79, Tables ZZ,AG,AH.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 80,81, Tables AB,AC.R"))
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
run <- try(source("Code/Table Code/SF - Lock Down/Items 82-85.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 86-88.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 89,91,93, Table AJ.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 90,92.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 94,95.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 96-98, Tables SS,AI.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 99-101.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 102-105.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 107-110.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 111.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 112-114.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 115-120.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 121-123.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 124-127, Table AD.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 128-131.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 132-134, Table AU.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 135-138.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 139-142.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 143, 144, 146, 147, 148, 150.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 145 and 149.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 151-153.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 154, 155.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 156.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 157.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 158.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 159.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 160.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 161.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 162.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 163.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 164,165.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 166.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Items 167,168.R")) #Error in read.xlsx.default(xlsxFile = file.path(filepathRawData, meter.export),  : File does not exist.
run <- try(source("Code/Table Code/SF - Lock Down/Item 169.R")) # must need to comment out a read in file
run <- try(source("Code/Table Code/SF - Lock Down/Items 170,171.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Item 172.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Table AA, Table BB.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Table AK.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Table AL.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Table AV.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Table B-18.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/SF - Lock Down/Tables RR,TT,UU,VV.R"))




#############################################################################################
# Source all MH scripts
#############################################################################################
run <- try(source("Code/Table Code/MH - Lock Down/Items 1,2.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 4,5.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 7-9.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 12,175,Table AP.R")) #insulation
run <- try(source("Code/Table Code/MH - Lock Down/Items 23,176.R")) #insulaion
run <- try(source("Code/Table Code/MH - Lock Down/Items 177,178.R")) #insulaion
run <- try(source("Code/Table Code/MH - Lock Down/Items 36,37,182.R")) #heat-loss
run <- try(source("Code/Table Code/MH - Lock Down/Items 38-42.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 43-46.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 47-49.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 50-53.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 54,55,190.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 56-58.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 66-67.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 68-70, Table AE.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 71-76,Tables XX,YY,AF.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 77-79, Tables ZZ,AG,AH.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 80,81, Tables AB,AC.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 82-85.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 86-88.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 89,91,93, Table AJ.R")) #no errors unless otherwise specfied after this scripts
run <- try(source("Code/Table Code/MH - Lock Down/Items 90,92.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 94,95.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 96-98, Tables SS,AI.R")) #Error, sheet SS does not exist
run <- try(source("Code/Table Code/MH - Lock Down/Item 99.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 105.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 107-110.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 111.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 112-114.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 115-120.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 121-123.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 124-127, Table AD.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 128-131.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 132-134, Table AU.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 135-138.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 139-142.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 143, 144, 146, 147, 148, 150.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 145 and 149.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 151-153.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 154, 155.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 173.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 177,178.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 179.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 180.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Item 192.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Items 202-203.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Table AA, Table BB.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Table AK.R")) #wrong total n for weighted
run <- try(source("Code/Table Code/MH - Lock Down/Table AL.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Table AV.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/MH - Lock Down/Tables RR,TT,UU,VV.R"))





#############################################################################################
# Source all MF scripts
#############################################################################################
run <- try(source("Code/Table Code/MF - Lock Down/Items 212,216.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 213,217,226.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 214,215,222,231,240,241,242.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 218,219,223.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 220.R")) #16 tables
run <- try(source("Code/Table Code/MF - Lock Down/Item 224.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 225.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 227.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 228.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 232,233.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 234.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 235.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 236,238.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 237.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 239.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 243-246,249.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 247.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 248.R")) #16 + 19 = 35 tables
#250,251 don't exist
run <- try(source("Code/Table Code/MF - Lock Down/Items 252,254.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 253.R"))
#255 doesn't exist
run <- try(source("Code/Table Code/MF - Lock Down/Items 256-258.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 259.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 260-263.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 264-267.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 268-271.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 272-274.R")) 
#275 - doesn't exist
run <- try(source("Code/Table Code/MF - Lock Down/Item 276.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 277,278.R")) #35 + 25 = 60 tables
#end BLDG tables

run <- try(source("Code/Table Code/MF - Lock Down/Items 68-70, Table AE.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 80,81, Tables AB,AC.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Table AL - MF.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Table AV.R")) #Error : Sheet 'Table AV' does not exist.
run <- try(source("Code/Table Code/MF - Lock Down/Table AJ.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Tables RR,TT,UU,VV.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 283.R"))
#284 - doesn't exist
run <- try(source("Code/Table Code/MF - Lock Down/Items 285-287.R"))
#288, 289 are completed in a previous code
run <- try(source("Code/Table Code/MF - Lock Down/Items 290-292.R")) #Error in `$<-.data.frame`(`*tmp*`, "tally", value = 1) : replacement has 1 row, data has 0
run <- try(source("Code/Table Code/MF - Lock Down/Items 293-295.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 296,297.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 298.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 299.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Items 300,301.R"))
run <- try(source("Code/Table Code/MF - Lock Down/Item 301A.R")) #This is for Tables HH and II
run <- try(source("Code/Table Code/MF - Lock Down/Items 302-304.R"))
run <- try(source("Code/Table Code/MF - Lock Down/MF EUI Tables.R")) #EUI Tables are BLDG tables
run <- try(source("Code/Table Code/MF - Lock Down/MF Gas EUI Tables.R")) #EUI Tables are BLDG tables
run <- try(source("Code/Table Code/MF - Lock Down/Table AI .R"))
run <- try(source("Code/Table Code/MF - Lock Down/Tables AG,AH.R")) #Error in `$<-.data.frame`(`*tmp*`, "tally", value = 1) : replacement has 1 row, data has 0
run <- try(source("Code/Table Code/MF - Lock Down/Table AD.R"))






#############################################################################################
# Source all MF-PSE scripts
#############################################################################################
run <- try(source("Code/Table Code/MF - PSE/Items 212,216.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 213,217,226.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 214,215,222,231,240,241,242.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 218,219,223.R"))#check in excel
run <- try(source("Code/Table Code/MF - PSE/Item 220.R")) #16 tables
run <- try(source("Code/Table Code/MF - PSE/Item 224.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 225.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 227.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 228.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 232,233.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 234.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 235.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 236,238.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 237.R"))#check in excel
run <- try(source("Code/Table Code/MF - PSE/Item 239.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 243-246,249.R"))#check in excel
run <- try(source("Code/Table Code/MF - PSE/Item 247.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 248.R")) #16 + 19 = 35 tables
#250,251 don't exist
run <- try(source("Code/Table Code/MF - PSE/Items 252,254.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 253.R"))
#255 doesn't exist
run <- try(source("Code/Table Code/MF - PSE/Items 256-258.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 259.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 260-263.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 264-267.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 268-271.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 272-274.R")) 
#275 - doesn't exist
run <- try(source("Code/Table Code/MF - PSE/Item 276.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 277,278.R")) #35 + 25 = 60 tables
#end BLDG tables

run <- try(source("Code/Table Code/MF - PSE/Items 68-70, Table AE.R")) #check in excel
run <- try(source("Code/Table Code/MF - PSE/Items 80,81, Tables AB,AC.R")) #check in excel
run <- try(source("Code/Table Code/MF - PSE/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 89,91,93, Table AJ.R"))
run <- try(source("Code/Table Code/MF - PSE/Table AL - MF.R"))
run <- try(source("Code/Table Code/MF - PSE/Table AJ.R"))
run <- try(source("Code/Table Code/MF - PSE/Table AF.R"))
run <- try(source("Code/Table Code/MF - PSE/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/MF - PSE/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/MF - PSE/Tables RR,TT,UU,VV.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 283.R"))
#284 - doesn't exist
run <- try(source("Code/Table Code/MF - PSE/Items 285-287.R"))
#288, 289 are completed in a previous code
run <- try(source("Code/Table Code/MF - PSE/Items 290-292.R")) #Error in `$<-.data.frame`(`*tmp*`, "tally", value = 1) : replacement has 1 row, data has 0
run <- try(source("Code/Table Code/MF - PSE/Items 293-295.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 296,297.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 298.R"))
run <- try(source("Code/Table Code/MF - PSE/Item 299.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 300,301.R"))
run <- try(source("Code/Table Code/MF - PSE/Items 302-304.R"))
run <- try(source("Code/Table Code/MF - PSE/MF EUI Tables.R")) #EUI Tables are BLDG tables
run <- try(source("Code/Table Code/MF - PSE/MF Gas EUI Tables.R")) #EUI Tables are BLDG tables
run <- try(source("Code/Table Code/MF - PSE/Table AI .R"))
run <- try(source("Code/Table Code/MF - PSE/Tables AG,AH.R")) #check in excel
run <- try(source("Code/Table Code/MF - PSE/Table AD.R"))












#############################################################################################
# Source all SF OVERSAMPLE scripts
#############################################################################################
run <- try(source("Code/Table Code/SF - OS/Items 1,2,6.R"))
run <- try(source("Code/Table Code/SF - OS/Items 3-5.R"))
run <- try(source("Code/Table Code/SF - OS/Items 7-9.R"))
run <- try(source("Code/Table Code/SF - OS/Items 10-18,175,235,Table AP.R")) #insulation
run <- try(source("Code/Table Code/SF - OS/Items 19-22.R"))
run <- try(source("Code/Table Code/SF - OS/Items 23,176,239.R")) #insulaion
run <- try(source("Code/Table Code/SF - OS/Items 24,25,28,29.R"))
run <- try(source("Code/Table Code/SF - OS/Items 26,30,31,177,178,237.R")) #insulaion
run <- try(source("Code/Table Code/SF - OS/Items 32-34.R"))
run <- try(source("Code/Table Code/SF - OS/Item 35.R"))
run <- try(source("Code/Table Code/SF - OS/Items 36,37,182.R"))
run <- try(source("Code/Table Code/SF - OS/Items 38-42.R")) #Error in nrow(item.group.rowFinal) : object 'item.group.rowFinal' not found
run <- try(source("Code/Table Code/SF - OS/Items 43-46.R"))
run <- try(source("Code/Table Code/SF - OS/Items 47-49.R"))
run <- try(source("Code/Table Code/SF - OS/Items 50-53.R"))
run <- try(source("Code/Table Code/SF - OS/Items 54,55,190.R"))
run <- try(source("Code/Table Code/SF - OS/Items 56-58.R"))
run <- try(source("Code/Table Code/SF - OS/Items 59,60.R"))
run <- try(source("Code/Table Code/SF - OS/Items 66-67.R"))
run <- try(source("Code/Table Code/SF - OS/Items 68-70, Table AE.R")) #Error : Sheet 'Table AE' does not exist.
run <- try(source("Code/Table Code/SF - OS/Items 71-76,Tables XX,YY,AF.R"))
run <- try(source("Code/Table Code/SF - OS/Items 77-79, Tables ZZ,AG,AH.R")) #Error in eval(expr, envir, enclos) : object 'tableAH.os.cast' not found
run <- try(source("Code/Table Code/SF - OS/Items 80,81, Tables AB,AC.R"))
appliances.dat <- read.xlsx(xlsxFile = file.path(filepathRawData, appliances.export))
run <- try(source("Code/Table Code/SF - OS/Items 82-85.R")) #Error in eval(expr, envir, enclos) : object 'item.os.cast' not found
run <- try(source("Code/Table Code/SF - OS/Items 86-88.R"))
run <- try(source("Code/Table Code/SF - OS/Items 89,91,93, Table AJ.R"))
run <- try(source("Code/Table Code/SF - OS/Items 90,92.R"))
run <- try(source("Code/Table Code/SF - OS/Items 94,95.R"))
run <- try(source("Code/Table Code/SF - OS/Items 96-98, Tables SS,AI.R"))
run <- try(source("Code/Table Code/SF - OS/Items 99-101.R"))
run <- try(source("Code/Table Code/SF - OS/Items 102-105.R"))
run <- try(source("Code/Table Code/SF - OS/Item 106, Tables FF,AM,AR,AS,AT.R"))
run <- try(source("Code/Table Code/SF - OS/Items 107-110.R"))
run <- try(source("Code/Table Code/SF - OS/Item 111.R"))
run <- try(source("Code/Table Code/SF - OS/Items 112-114.R"))
run <- try(source("Code/Table Code/SF - OS/Items 115-120.R"))
run <- try(source("Code/Table Code/SF - OS/Items 121-123.R"))
run <- try(source("Code/Table Code/SF - OS/Items 124-127, Table AD.R"))
run <- try(source("Code/Table Code/SF - OS/Items 128-131.R"))
run <- try(source("Code/Table Code/SF - OS/Items 132-134, Table AU.R"))
run <- try(source("Code/Table Code/SF - OS/Items 135-138.R"))
run <- try(source("Code/Table Code/SF - OS/Items 139-142.R"))
run <- try(source("Code/Table Code/SF - OS/Items 143, 144, 146, 147, 148, 150.R"))
run <- try(source("Code/Table Code/SF - OS/Items 145 and 149.R"))
run <- try(source("Code/Table Code/SF - OS/Items 151-153.R"))
run <- try(source("Code/Table Code/SF - OS/Items 154, 155.R"))
run <- try(source("Code/Table Code/SF - OS/Table AA, Table BB.R"))
run <- try(source("Code/Table Code/SF - OS/Table AK.R"))
# run <- try(source("Code/Table Code/SF - OS/Table AL.R"))
run <- try(source("Code/Table Code/SF - OS/Table AV.R"))
run <- try(source("Code/Table Code/SF - OS/Tables CC,NN,OO,WW.R"))
run <- try(source("Code/Table Code/SF - OS/Tables DD,EE,GG,HH,II,KK,LL,MM.R"))
run <- try(source("Code/Table Code/SF - OS/Tables RR,TT,UU,VV.R"))
