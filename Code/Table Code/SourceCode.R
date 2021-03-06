#############################################################################################
##  Title:            RBSA Analysis - Source File Paths                     
##  Author:           Casey Stevens, Cadmus Group               
##  Created:          02/27/2017
##  Updated:                                             
##  Billing Code(s):  
#############################################################################################




##  Include packages
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(stringr)
library(data.table)


################################################################################
# SET FILEPATHS for folders and file names of:
# - raw input data
# - clean input data
# - output data
################################################################################
rootpath              <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/FileMaker Data"
billingRootpath       <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Analysis/Billing analysis"
analysisFolder        <- file.path(rootpath, "$Clean Data", "2017.10.30")
# rootpath              <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Management/Data to NEEA/Delivery Data"
# billingRootpath       <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Management/2018 NEEA Followup/Data to NEEA/Delivery Data"
# analysisFolder        <- "//projects.cadmusgroup.com@SSL/DavWWWRoot/sites/6000-P14/Shared Documents/Management/2018 NEEA Followup/Data to NEEA/Delivery Data"
# filepathRawData       <- file.path(analysisFolder,"Data for PSE")
filepathRawData       <- file.path(analysisFolder)
filepathCleanData     <- file.path(rootpath, "Analysis Documents", "Clean Data")
filepathCleaningDocs  <- file.path(rootpath, "Analysis Documents")
filepathWeightingDocs <- file.path(rootpath, "Analysis Documents", "Weight Source")
outputFolder          <- file.path(rootpath, "Tables from Previous RBSA Report")
filepathBillingData   <- file.path(billingRootpath)

file.exists(rootpath)

stopifnot(all(file.exists(rootpath, analysisFolder, filepathRawData, filepathCleanData, filepathCleaningDocs, outputFolder, filepathBillingData)))

#File Names
appliances.export          <- "Appliances.xlsx"
buildings.interview.export <- "Buildings Interview.xlsx"
buildings.export           <- "Buildings.xlsx"
envelope.export            <- "Envelope.xlsx"
lighting.export            <- "Lighting.xlsx"
mechanical.export          <- "Mechanical.xlsx"
rooms.export               <- "Rooms.xlsx"
sites.interview.export     <- "Sites Interview.xlsx"
sites.export               <- "Sites.xlsx"
water.export               <- "Water_FlowFactors.xlsx"
windows.export             <- "Windows.xlsx"
survey.export              <- "Participant Survey.xlsx"
one.line.export            <- "One Line Summary.xlsm"
one.line.bldg.export       <- "One Line Summary - BLDG.xlsm"
billing.data               <- "RBSA Usages Compiled Final_2018-01-16.xlsx"
stopifnot(all(file.exists(file.path(filepathRawData, water.export))))



# os.ind <- "scl"
# export.ind <- "SCL"
# subset.ind <- "SCL GenPop"

# os.ind <- "snopud"
# export.ind <- "SnoPUD"
# subset.ind <- "SnoPUD"

# # Read in clean RBSA data
# os.ind <- "rbsa"
# rbsa.dat <- read.xlsx(xlsxFile = file.path(filepathCleanData ,paste("clean.",os.ind,".data", rundate, ".xlsx", sep = "")))


# For Mechanical
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Mechanical.xlsx', mechanical.export, mode = 'wb')

# For Envelope
# download.file('https://projects.cadmusgroup.com/sites/6000-P14/Shared Documents/Analysis/FileMaker Data/$Clean Data/2017.10.30/Envelope.xlsx', envelope.export, mode = 'wb')
