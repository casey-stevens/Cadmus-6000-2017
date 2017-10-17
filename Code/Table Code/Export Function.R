###############################################################################################
##  Title:            RBSA Analysis                      
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          10/17/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################

#################################################################################
# Function: exportTable
# Used For: Exporting the specific tables to the previous RBSA Tables in excel
#           Allows for exporting to the correct workbooks / tabs
#################################################################################


exportTable <- function(buildingTypeData, buildingTypeIndicator, tableName) {
  library(openxlsx)
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip")
  workbook.export <- loadWorkbook(file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY.xlsx", sep = ""), sep="/"))

  writeData(workbook.export, sheet = tableName, x = buildingTypeData, startRow = 20)
  
  saveWorkbook(workbook.export, file = paste(outputFolder, paste("Tables in Excel - ",buildingTypeIndicator," - COPY.xlsx", sep = ""), sep="/"), overwrite = T)
  
}
