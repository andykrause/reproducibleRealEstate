
### Builds annual assessed value files ---------------------------------------------------

kngBuildAssdVal <- function(avYears,            # Years to use
                            assdValDB,          # Location of existing .db file
                            verbose=FALSE,      # Do you want progress info
                            overWrite=TRUE      # Overwrite existing tables?
                            ){
  
  ## Set up connection to data
  
  if(verbose) cat('Reading in raw data \n')
  assValConn <- dbConnect(RSQLite::SQLite(), assdValDB)
  avData <- dbGetQuery(assValConn, 
                       paste0('SELECT Major, Minor, TaxYr, ApprLandVal AS LandVal, ',
                              'ApprImpsVal AS ImpsVal FROM ValueHistory ',
                              'WHERE TaxYr >= ', avYears[1], ' AND TaxYr <= ',
                              avYears[length(avYears)], ' AND Reason = "REVALUE"'))
  
  # Write out
  if(verbose) cat('Writing out data \n')
  
  # Loop throug the years
  for(avY in avYears){
    avName <- paste0('ValueHist', avY)
    
    tExists <- dbExistsTable(assValConn, avName)
   
    if(overWrite & tExists) {
      dbRemoveTable(assValConn, avName)
      if(verbose) cat(paste0('    Removing existing table: ', avName, '\n'))
    }
    dbWriteTable(assValConn, avName, kngBuildPinx(avData[avData$TaxYr == avY, ])
                                                  , row.names=FALSE)
    if(verbose) cat('Writing out ', avName, '\n')
    
  }
  
  # Close
  dbDisconnect(assValConn) 
}


### Create funtional Property Identification #s for King Data ----------------------------

kngBuildPinx <- function(X,                     # Dataset to use, including Major, Minor
                         condoComp = FALSE      # Is this a condoComp dataset?
                         ){
  
  # If condoComp fix minor to 0000
  if(condoComp) {
    X <- as.data.frame(X)
    X$Minor <- '0000'
  }
  
  # convert names    
  oldNames <- colnames(X)
  newNames <- tolower(oldNames)
  colnames(X) <- newNames
   
  # ensure value is numeric
  X$major <- as.numeric(X$major)
  if(!condoComp) X$minor <- as.numeric(X$minor)
      
  # remove NA columns
  X <- X[!is.na(X$major),]
  if(!condoComp) X <- X[!is.na(X$minor), ]
  
  # Remove those with invalid values 
  X <- X[X$major < 1000000, ]
  if(!condoComp) X <- X[X$minor < 10000, ]
  
  # Add leading to major
  X$major <- addLeadZeros(X$major, maxChar=6)
  
  # Add leading to minor
  if(!condoComp) X$minor <- addLeadZeros(X$minor, maxChar=4)
  
  # Combine  
  X$pinx <- paste0("..", X$major, X$minor)
  
  # Reorder
  X <- X[ ,c("pinx", newNames)]
  colnames(X)[2:ncol(X)] <- oldNames
  
  # Return X
  return(X)
}

