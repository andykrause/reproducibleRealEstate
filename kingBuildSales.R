##########################################################################################
#
#  Function dealing with building and preparing property transaction in 
#    King County, WA
#
##########################################################################################

### (SUB) Create Function to count total transactions ------------------------------------

kngSBuildTransCount <- function(xSales,                    # Sales dataFrame
                                transLimit                 # Maximum allowed
                                ){
  
  # Compute trans number
  pinxTable <- as.data.frame(table(xSales$pinx))
  colnames(pinxTable) <- c("pinx", "tnTotal")
  
  # Merge back to sales
  xSales$tnTotal <- pinxTable$tnTotal[match(xSales$pinx, pinxTable$pinx)] 
  
  # Order Properly and remove those with too many transactions
  xSales <- xSales[order(xSales$pinx, 
                         xSales$salesYear,
                         xSales$ExciseTaxNbr), ]  
  
  xSales <- xSales[xSales$tnTotal < transLimit, ]
  
  # Assign a transNumber
  xSales$transNbr <- 1
  for(i in 2:max(xSales$tnTotal)){ 
    idX <- which(xSales$tnTotal == i)
    idL <- length(idX) / i
    repNbrs <- rep(1:i, idL)
    xSales$transNbr[idX] <- repNbrs
  }
  
  return(xSales)
}

### (SUB) Function that adds Unique IDs for sales ----------------------------------------

kngSBuildSaleUIDs <- function(xSales                       # Sales dataframes
                              ){
  
  # Add Unique IDs for each Record and Each Sales
  years <- rownames(table(xSales$salesYear))
  xSales$RecID <- ' '
  xSales$SaleID <- ' '
  
  # Loop through the years
  for(i in 1:length(years)){
    idX <- which(xSales$salesYear == as.numeric(years[i]))
    xSales$RecID[idX] <- paste0(years[i], '..', 1:length(idX))
    xSales$SaleID[idX] <- paste0(years[i], '..', 
                                 as.numeric(as.factor(xSales$ExciseTaxNbr[idX])))
  }
  
  # Reconfigure file so that IDs are in the beginning
  cNbr <- ncol(xSales)
  xSales <- xSales[ ,c(1 ,cNbr - 1, cNbr, 3:(cNbr - 2))]
  return(xSales)
}

### (MAIN) Function that cleans up raw sales data ----------------------------------------

kngSCleanSales <- function(saleYears = c(1997, 2014),      # Sales years to use
                           transLimit = 10,                # Max number of sales per prop
                           salesDB = 'd:/data/wa/king/assessor/kingsales.db',
                           trimList=list(SaleReason=2:19,  
                                        SaleInstrument=c(0, 1, 4:28),
                                        SaleWarning=paste0(" ", c(1:2, 5:9, 11:14,
                                                                  18:23, 25, 27,
                                                                  31:33, 37, 39,
                                                                  43, 46, 48, 49,
                                                                  50:53, 59, 61,
                                                                  63, 64, 66),
                                                           " ")),
                           overWrite=TRUE,                 # Overwrite existing files
                           verbose=FALSE                   # Give progress
                           ){
  
  # libraries
  require(RODBC)
  require(RSQLite)
  require(stringr)
  
  # read in Sales File
  if(verbose) cat('Reading in raw sales\n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  rawSales <- dbReadTable(salesConn, 'AllSales')
  
  # base clean
  if(verbose) cat('Removing sales with missing PIN or Price\n')
  cleanSales <- rawSales[rawSales$Major > 0, ]
  cleanSales <- cleanSales[cleanSales$SalePrice > 0, ]
  
  # build sales data
  if(verbose) cat('Building sales date\n')
  cleanSales$docDate <- paste(substr(cleanSales$DocumentDate, 4, 5)
                              ,substr(cleanSales$DocumentDate, 1, 2),
                              substr(cleanSales$DocumentDate, 7, 10), sep="")
  cleanSales$salesDate <- as.POSIXct(strptime(cleanSales$docDate, "%d%m%Y"))
  cleanSales$salesYear <- as.numeric(format(cleanSales$salesDate, "%Y"))
  cleanSales <- cleanSales[!is.na(cleanSales$salesDate), ]
  
  # eliminate Transactions prior to Sales Year Limit
  if(verbose) cat('Limiting to selected time frame\n')
  if(length(saleYears) == 1) saleYears <- c(saleYears, saleYears)
  cleanSales <- cleanSales[cleanSales$salesYear >= saleYears[1] & 
                             cleanSales$salesYear <= saleYears[2], ]
  
  # add PINX (Parcel Identification Number)
  if(verbose) cat('Adding PINx\n')
  cleanSales <- kngBuildPinx(cleanSales)
  
  # add transaction count and limit by paramter
  if(verbose) cat('Adding Trans count\n')
  cleanSales <- kngSBuildTransCount(cleanSales, transLimit=transLimit)
  
  # add MultiParcel sale designation
  if(verbose) cat('Labeling Multiple parcel sales\n')
  cleanSales <- idDup(cleanSales, 'ExciseTaxNbr', newField = 'multiParcel',
                      iddType='labelNonUnique', binNonUq=TRUE)
  
  # Add unique IDs
  if(verbose) cat('Adding Unique IDs\n')
  trimSales <- kngSBuildSaleUIDs(cleanSales)
  
  # Trim sales by Insturment, reason and warning
  if(verbose) cat('Removing bad Reasons Instruments and Warnings\n')
  
  # Fix the "Warning" Field.  Add a leading/trailing space for the grep()
  trimSales$SaleWarning <- paste(" ", trimSales$SaleWarning, " ", sep="")
  
  for(tL in 1:length(trimList)){
    trimSales <- trimByField(trimSales, names(trimList)[tL],
                             trimList = unlist(trimList[tL]))
  }
  
  # Write out
  if(verbose) cat('Writing out data \n')
  tExists <- dbExistsTable(salesConn, 'trimmedSales')
  
  if(overWrite & tExists) {
    dbRemoveTable(salesConn, 'trimmedSales')
    if(verbose) cat(paste0('    Removing existing table: trimmedSales\n'))
  }
  dbWriteTable(salesConn, 'trimmedSales', trimSales, row.names=FALSE)
  
  # Close
  dbDisconnect(salesConn)
}

### Function to read in king data for a given year ---------------------------------------

kngSReadData <- function(dataYear,                         # Current year
                         verbose=FALSE                     # Show progress
                         ){
  
  # Connect to database
  dbName <- paste0('d:/data/wa/king/assessor/king', dataYear, '.db')
  dyConn <- dbConnect(RSQLite::SQLite(), dbname=dbName)
  if(verbose) cat ('\n Connecting to ', dbName, '\n')
  
  # Parcel Data    
  if(verbose) cat ('\n Reading in Parcel', dataYear, ' data \n')
  assign(paste0("parcel", dataYear), 
         kngBuildPinx(dbGetQuery(dyConn, 
                                 paste0('SELECT Major, Minor, PresentUse FROM ',
                                        'Parcel', dataYear))),
         envir=.GlobalEnv)
  
  # Resbldg Data
  if(verbose) cat ('\n Reading in ResBldg', dataYear, ' data \n')
  assign(paste0("resbldg", dataYear), 
         kngBuildPinx(dbGetQuery(dyConn, 
                                 paste0('SELECT Major, Minor, BldgNbr FROM ',
                                        'ResBldg', dataYear))),
         envir=.GlobalEnv)
  
  # Close
  dbDisconnect(dyConn) 
}

### Function to add present use to sale --------------------------------------------------

kngSAddPresentUse <- function(ySales,                      # Set of yearly sales
                              yParcel,                     # Parcel data from year
                              ySuffix                      # Year to use
                              ){
  
  # All Props
  ySales[paste0('parcel', ySuffix)] <- yParcel$PresentUse[match(ySales$pinx,
                                                                yParcel$pinx)]
  return(ySales)
}

### Function to add record type to a set of sales ----------------------------------------

kngSAddRecordType <- function(ySales,                       # Set of yearly sales
                              yTable,                       # Record Type table to test
                              fieldName,                    # Name of new field to apply
                              condoComp=FALSE               # Is this condoComp?
                              ){
  
  # Set blanks
  ySales[fieldName] <- 0
  
  # Find matches
  if(!condoComp){
    idM <- !is.na(match(ySales$pinx, yTable$pinx))
  } else {
    idM <- !is.na(match(ySales$pinc, yTable$pinc))
  }
  
  # Convert to 1 if match
  ySales[idM, fieldName] <- 1
  
  # Return data
  return(ySales)
}

### Function to place label on sales based on record type situation ----------------------

kngSLabelRecordType <- function(ySales,                     # Set of yearly sales
                                ySuffix                     # Suffix type to add (_1,0,1) 
                                ){
  
  ySales[paste0('Rt', ySuffix)] <- 'V'
  ySales[is.na(ySales[paste0('parcel', ySuffix)]), paste0('Rt', ySuffix)] <- 'X'
  ySales[ySales[paste0('C', ySuffix)] == 1, paste0('Rt', ySuffix)] <- 'C'
  ySales[ySales[paste0('A', ySuffix)] == 1, paste0('Rt', ySuffix)] <- 'A'
  ySales[ySales[paste0('K', ySuffix)] == 1, paste0('Rt', ySuffix)] <- 'K'
  ySales[ySales[paste0('R', ySuffix)] == 1, paste0('Rt', ySuffix)] <- 'R'
  
  return(ySales)  
}

### Function that applies labels to king sales -------------------------------------------

kngSLabelSales <- function(saleYears=1999:2014,             # Sale years to use
                           salesDB='d:/data/wa/king/assessor/kingsales.db',
                           overWrite=TRUE,                  # Should overwrite? 
                           verbose=FALSE                    # See progress?
                           ){
  
  require(plyr)
  
  # Set null values
  labeledSales <- list()
  oldYears <- NULL
  
  # Read in sales Data
  if(verbose) cat('Reading in sales data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  pSales <- dbReadTable(salesConn, 'trimmedSales')
  dbDisconnect(salesConn)
  
  # Read in Characteristics Data
  if(verbose) cat(paste0('Working on sales in ', iYears, ' \n'))
  kngSReadData(saleYears, verbose=verbose)

  # Add Present Uses
  ySales <- kngSAddPresentUse(ySales=pSales, 
                              yParcel=get(paste0('parcel', saleYears)),
                              ySuffix='0')
  
  # Add all other record Types
  ySales <- kngSAddRecordType(ySales, yTable=get(paste0('resbldg', saleYears)), 
                            fieldName='R0')

  # Remove all of those not residential in use  
  labeledSales <- ySales[ySales$R0 == 1, ]
  
  # Write out
  if(verbose) cat('Writing out data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  tExists <- dbExistsTable(salesConn, 'labeledSales')
  
  if(overWrite & tExists) {
    dbRemoveTable(salesConn, 'labeledSales')
    if(verbose) cat(paste0('    Removing existing table: labeledSales\n'))
  }
  dbWriteTable(salesConn, 'labeledSales', labeledSales, row.names=FALSE)
  
  # Close
  dbDisconnect(salesConn)
  
  #Clean up
  for(delX in c('parcel','resbldg')){
    rm(list=ls(pattern=glob2rx(paste0(delX,"*"))))
  }
  gc()  
  
} #ends function


### Function that confirms King county sale labels ---------------------------------------

kngSConfirmLabels <- function(salesDB='d:/data/wa/king/assessor/kingsales.db',
                              latestYear=2014,              # Last year in data
                              verbose=FALSE,                # Show progress
                              overWrite=TRUE                # Overwrite?
                              ){
  
  ## Init ops
  require(RSQLite)
  require(plyr)
  
  ## Read in sales data
  
  if(verbose) cat('Reading in sales data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  lSales <- dbReadTable(salesConn, 'labeledSales')
  
  ## Initial labeling
  if(verbose) cat('Initial labeling of Sales \n')
  
  # Split by multiparcel-ness
  nmSales <- lSales[lSales$multiParcel == 0, ]
  mSales <- lSales[lSales$multiParcel == 1, ]
  
  ## Add a record type to the sales
  nmSales$recType <- "R"
  
  ## Limit Columns
  allSales <- nmSales[, c('pinx', 'RecID', 'SaleID', 'SalePrice', 'SellerName',
                           'BuyerName', 'PropertyType', 'PrincipalUse', 'recType',
                           'SaleWarning', 'DocumentDate', 'salesYear', 'tnTotal',
                           'transNbr', 'multiParcel')]
  
  ## Write out
  
  if(verbose) cat('Writing out data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  tExists <- dbExistsTable(salesConn, 'recTypeSales')
  
  if(overWrite & tExists) {
    dbRemoveTable(salesConn, 'recTypeSales')
    if(verbose) cat(paste0('    Removing existing table: recTypeSales\n'))
  }
  dbWriteTable(salesConn, 'recTypeSales', allSales, row.names=FALSE)
  
  ## Close
  
  dbDisconnect(salesConn)
  
  ##Clean up
  
  for(delX in c('sales','Sales','Sales1')){
    rm(list=ls(pattern=glob2rx(paste0("*",delX))))
    rm(list=ls(pattern=glob2rx(paste0(delX,"*"))))  
  }
  gc()   
  
}

### Function that splits sales and adds data to them -------------------------------------

kngSSplitAttachSales <- function(salesDB='d:/data/wa/king/assessor/kingsales.db',
                                 dataDir='d:/data/wa/king/assessor',
                                 verbose=FALSE,
                                 overWrite=TRUE){
  ## Init ops
  
  require(RSQLite)
  require(plyr)
  
  ## Read in sales data
  
  if(verbose) cat('Reading in sales data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  ySales <- dbReadTable(salesConn, 'recTypeSales')
  yData <- names(table(ySales$salesYear))
  
  ## Set up null capture lists
  
  rSalesList <- list()
  
  ## Attach Data
  if(verbose) cat('Attaching data for ', yData, ' \n')
    
   # Adding parcel data
  yConn <- dbConnect(RSQLite::SQLite(),
                     paste0(dataDir, '/kingData', yData, '.db'))
  xParc <- dbReadTable(yConn, paste0('Parcel', yData))
  dbDisconnect(yConn)  
  xParc <- kngBuildPinx(xParc)
    
  # Eliminate Dup Fields
  xNames <- c('pinx', setdiff(names(xParc), names(ySales)))
        
  # Merging Parcel data to sales data
  ySales <- merge(ySales, xParc[,xNames], by='pinx')
    
  # Add Use specific data
  resSales <- kngSAttachKingData(ySales, "R", yData)  
  
  ## Write out
  
  if(verbose) cat('Writing out data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  tExists <- dbExistsTable(salesConn, 'ResSales')
  
  if(overWrite & tExists) {
    dbRemoveTable(salesConn, 'ResSales')
    if(verbose) cat(paste0('    Removing existing table: ResSales \n'))
  }
  
  dbWriteTable(salesConn, 'ResSales', resSales, row.names=FALSE)
   
  # Close connection
  dbDisconnect(salesConn)
  
  #Clean up
  for(delX in c('parcel','resbldg')){
    rm(list=ls(pattern=glob2rx(paste0(delX,"*"))))
  }
  gc()
}

### Function to attach King data to king sales -------------------------------------------

kngSAttachKingData <- function(xSales,                        # Sales data frame                          
                               recType,                       # Record Type
                               dataYear,                      # Current data year
                               dataDir='d:/data/wa/king/assessor'  # Data location
                               ){
  
  # Set data location
  yConn <- dbConnect(RSQLite::SQLite(), paste0(dataDir, '/king', dataYear, '.db'))
  
  # Load Resbldg data
  if(recType == 'R'){
    tempData <- kngBuildPinx(dbReadTable(yConn, paste0('resbldg', dataYear)))
    nbrBldgs <- as.data.frame(table(tempData$pinx))
    tempData$nbrBldgs <- nbrBldgs$Freq[match(tempData$pinx, nbrBldgs$Var1)]
    tempData <- tempData[tempData$BldgNbr == 1, ]
  }
  
  # Identify and remove duplicate field names
  xNames <- c('pinx', setdiff(names(tempData), names(xSales)))
  
  # Attach residential building data
  xSales <- merge(xSales, tempData[,xNames], by='pinx')
  
  # Disconnect
  dbDisconnect(yConn)
  
  return(xSales)
}

### Function to attach assessed values ---------------------------------------------------

kngSAttachAssdValues <- function(salesDB,
                                 dataDir,
                                 dataYear,
                                 verbose=FALSE,
                                 overWrite=TRUE){
  
  ## Load in Sales
  
  if(verbose) cat('Reading in sales data \n')
  salesConn <- dbConnect(RSQLite::SQLite(), salesDB)
  lSales <- dbReadTable(salesConn, 'ResSales')
  dbDisconnect(salesConn)
  
  ## Load in Value History
  
  if(verbose) cat('Reading in assessed values \n')
  assdValConn <- dbConnect(RSQLite::SQLite(), file.path(dataDir, 'KingValueHistory.db'))
  assdValues <- dbReadTable(assdValConn, paste0('valuehist', dataYear))
  dbDisconnect(salesConn)
  
  ## Add Assessed Values
  
  lSales$LandVal <- assdValues$LandVal[match(lSales$pinx, assdValues$pinx)]
  lSales$ImpsVal <- assdValues$ImpsVal[match(lSales$pinx, assdValues$pinx)]
  
  ## Remove sales with no values
  
  lSales <- lSales[!is.na(lSales$LandVal), ]
  
  ## Return values
  
  return(lSales)
}  

### Function to attach XY values ---------------------------------------------------------

kngSAttachXYs <- function(sales,
                          latlongFile,
                          verbose=FALSE){
  
  ## Read in data
  
  if(verbose) cat('Reading in XY data \n') 
  xys <- readShapePoints(latlongFile)    
  
  ## Add XY data
  
  sales$X <- xys$X[match(sales$pinx, xys$pinx)]
  sales$Y <- xys$Y[match(sales$pinx, xys$pinx)]
  
  ## Remove sales with no values
  
  sales <- sales[!is.na(sales$X), ]
  
  ## Return values
  
  return(sales)
}
