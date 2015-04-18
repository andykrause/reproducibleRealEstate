##########################################################################################
#                                                                                        #
#   This code creates the initial raw sales file to use in analyzing 2014                #
#   King County Residential Sales data                                                   #
#                                                                                        #
##########################################################################################

### Preliminary Commands -----------------------------------------------------------------

 ## Load Libraries

  library(RODBC)
  library(RSQLite)
  library(DBI)
  library(stringr)
  library(maptools)
  library(plyr)

 ## Set directories
  
  codePath <- 'D:/code/R/Research/RRR'
  dataPath <- 'D:/Data/Research/RRR'
  
 ## Read in Functions and helper files  
  
  source(file.path(codePath, '/basicConversionTools.R'))
  source(file.path(codePath, '/kingDataDevelop.R'))
  source(file.path(codePath, '/kingBuildSales.R'))

 ## Set Parameters
 
  # Does the raw data need to be converted?  Change to FALSE after this has been finished
  convertData <- FALSE

  # What year of sales are you working with (Leave at 2014 for now)
  studyYear <- 2014


### Converting data from .csv to .db -----------------------------------------------------

  if(convertData){
  
 ## Convert Assessor's Characteristic Data
  
    convertCSVtoSQLite(dataPathCurrent = dataPath,
                       dataPathNew = dataPath,
                       newFileName = 'KingData2014.db',
                       fileNames=c('Extr_Parcel.csv', 'Extr_ResBldg.csv'),
                       tableNames = c(paste0('Parcel', studyYear),
                                      paste0('ResBldg', studyYear2014),
                       overWrite=TRUE) 

 ## Convert Assessed Value History
 
    convertCSVtoSQLite(dataPathCurrent=dataPath,
                       dataPathNew = dataPath,
                       newFileName = 'KingValueHistory.db',
                       fileNames=c('EXTR_ValueHistory_V.csv'),
                       tableNames = c('ValueHistory'),
                       overWrite=TRUE)

 ##  Isolate the study year Assessed Value File 
 
    kngBuildAssdVal(avYears=studyYear+1,  
                    assdValDB = file.path(dataPath, 'kingvaluehistory.db'),
                    overWrite=TRUE)


 ## Convert Sales File

    convertCSVtoSQLite(dataPathCurrent=dataPath,
                       dataPathNew = dataPath,
                       newFileName = 'KingSales.db',
                       fileNames=c('EXTR_RPSale.csv'),
                       tableNames = c('AllSales'),
                       overWrite=TRUE)

  }

### Initial cleaning and combining -------------------------------------------------------

 ## Remove sales with labels indicating likely non-arms length transactions
 
  kngSCleanSales(saleYears = studyYear, 
                 transLimit = 10,
                 salesDB = file.path(dataPath, 'kingsales.db'),
                 trimList=list(SaleReason=2:19,
                               SaleInstrument=c(0, 1, 4:28),
                               SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 25, 27,
                                                         31:33, 37, 39, 43, 46, 48, 49,
                                                         50:53, 59, 61, 63, 64, 66),
                                                  " ")),
                 overWrite=TRUE,
                 verbose=FALSE 
  )

 ## Add Use category and limit to residential only sales

  kngSLabelSales(saleYears=studyYear, 
                 salesDB=file.path(dataPath, 'kingsales.db'),
                 overWrite=TRUE,
                 verbose=FALSE)

 ## Remove multiple parcel sales

  kngSConfirmLabels(salesDB=file.path(dataPath, 'kingsales.db'),
                    latestYear=studyYear,
                    verbose=TRUE,
                    overWrite=TRUE)

 ## Add Parcel and Residential Building Information to the sales

  kngSSplitAttachSales(salesDB=file.path(dataPath, 'kingsales.db'),
                       dataDir=dataPath,
                       verbose=TRUE,
                       overWrite=TRUE)

 ## Add Assessed Values

  xSales <-  kngSAttachAssdValues(salesDB=file.path(dataPath, 'kingsales.db'),
                                  dataDir=dataPath,
                                  dataYear=studyYear,
                                  verbose=TRUE,
                                  overWrite=TRUE)

 ## Add Lat/Long to data

  xSales <- kngSAttachXYs(xSales,
                          latlongFile = file.path(dataPath, 'parcelpoints2014.shp'),
                          verbose=FALSE)

### Write out data -----------------------------------------------------------------------

  write.csv(xSales,
            file=file.path(dataPath, 'cleansales.csv'),
            row.names=FALSE)




