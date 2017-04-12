
### Convert Microsoft Access into SQLite data --------------------------------------------

convertMSAtoSQLite <- function(dataPathCurrent,             # Path to Access Files
                               dataPathNew=dataPathCurrent, # Path to new files
                               overWrite=FALSE,             # Overwrite existing .db?
                               fileNames=NULL,              # File name(s) of Access files
                               newFileNames=NULL,           # File name(s) of .db file?
                               verbose=FALSE,               # Should debugging be shown
                               writeRowNames=FALSE          # Write row names?
                               ){
  ## Set libraries
  
  require(RSQLite)
  require(RODBC)

  ## Determine File Names to convert
  
  if(!is.null(fileNames)){
    fileList <- fileNames
    if(is.null(newFileNames)) newFileNames <- fileNames
  } else {
    fileList <- list.files(dataPathCurrent)
    fileList <- fileList[grep('.accdb', fileList)]
    newFileNames <- fileList
  }
  
  ## Show error if file names are not same length
  
  if(length(fileList) != length(newFileNames)){
    cat('ERROR: File names not same length')
    return('File names not the same length')
  }  
  
  ## Set up connections, read and write files
  
  for(fL in 1:length(fileList)){
    if(verbose) cat(paste0('Converting file number: ', fL, ' of ', 
                           length(fileList), '\n'))
    
    # Set up connections
    
    msaConn <- odbcConnectAccess2007(paste0(dataPathCurrent, '/', 
                                            fileList[fL]))
    if(verbose) cat("  Converting from MS Access file: ",
                    paste0(dataPathCurrent, '/', fileList[fL], '\n'))
    
    slConn <- dbConnect(RSQLite::SQLite(),
                        dbname=paste0(dataPathNew, '/', 
                                      gsub('.accdb', '.db', newFileNames[fL])))
    if(verbose) cat("  Converting to SQLite file: ",
                    paste0(dataPathNew, '/', 
                           gsub('.accdb', '.db', newFileNames[fL]), '\n'))
    
    # Read in Tables
    msaTables <- sqlTables(msaConn)
    msaTables <- msaTables[msaTables$TABLE_TYPE != 'SYSTEM TABLE', ]
    
    # Write Tables
    for(tL in 1:nrow(msaTables)){
      xTable <- sqlQuery(msaConn, 
                         paste0('SELECT * FROM ',
                                msaTables$TABLE_NAME[tL]))
      
      tExists <- dbExistsTable(slConn, msaTables$TABLE_NAME[tL])
      
      if(overWrite & tExists) {
        dbRemoveTable(slConn, msaTables$TABLE_NAME[tL])
        if(verbose) cat(paste0("    Removing existing table: ",
                                   msaTables$TABLE_NAME[tL], '\n'))
      }
      dbWriteTable(slConn, msaTables$TABLE_NAME[tL], xTable, 
                   row.names=writeRowNames)
      if(verbose) cat(paste0("    Writing table: ", msaTables$TABLE_NAME[tL],
                             '\n'))
    }
    
    # Close
    odbcClose(msaConn)
    dbDisconnect(slConn)
  }
    
    # Return List of tables written
  
return(fileList)
  
}

### Convert CSV into SQLite data ---------------------------------------------------------

convertCSVtoSQLite <- function(dataPathCurrent,             # Path to .csv files
                               dataPathNew=dataPathCurrent, # Path to new .db file
                               newFileName,                 # Name of new .db file
                               fileNames=NULL,              # Names of .csv to convert
                               tableNames=NULL,             # Names of new tables in .db
                               overWrite=FALSE,             # Overwrite existing tables?
                               verbose=FALSE,               # Show debugging
                               writeRowNames=FALSE          # Write out row names?
){
  ## Set libraries
  
  require(RSQLite)
  require(RODBC)
  
  ## Determine File Names to convert
  
  if(!is.null(fileNames)){
    fileList <- fileNames
    if(is.null(tableNames)) tableNames <- gsub('.csv', '', fileNames)
  } else {
    fileList <- list.files(dataPathCurrent)
    fileList <- fileList[grep('.csv', fileList)]
    if(is.null(tableNames)) tableNames <- gsub('.csv', '', fileList)
  }
  
  ## Check for proper length in file name lists
  
  if(length(fileList) != length(tableNames)){
    cat('ERROR: File names and table names not same length')
    return('File and table names not the same length')
  }

  ## Set up connections

  slConn <- dbConnect(RSQLite::SQLite(),
                      dbname=paste0(dataPathNew, '/', newFileName))
  if(verbose) cat("  Converting to SQLite file: ",
                  paste0(dataPathNew, '/', newFileName), '\n')
  
  ## Read in and convert each file
  
  for(fL in 1:length(fileList)){
    if(verbose) cat(paste0('Converting file number: ', fL, ' of ', 
                           length(fileList), '\n'))
    
    # Read in Tables
    xTable <- read.csv(paste0(dataPathCurrent, '/', fileList[fL]))
      
    # Check to see if it exists
    tExists <- dbExistsTable(slConn, tableNames[fL])
    
    # Overwrite if exists
    if(overWrite & tExists) {
        dbRemoveTable(slConn, tableNames[fL])
        if(verbose) cat(paste0("    Removing existing table: ",
                               tableNames[fL], '\n'))
    }
    
    # Write if doesn't exist
    dbWriteTable(slConn, tableNames[fL], xTable, 
                   row.names=writeRowNames)
    if(verbose) cat(paste0("    Writing table: ", tableNames[fL],
                             '\n'))
  }
    
  # Close
  dbDisconnect(slConn)

  # Return List of tables written
  
  return(fileList)  
}

### Function for adding the leading zeros ------------------------------------------------

addLeadZeros <- function(xNbr,           # Numbers to add 0s to
                         maxChar = 6     # Desired total length
                         )
{
  
  # Count the number of missing zeros
  
  missZero <- maxChar - nchar(xNbr)
  
  # Add missing zeros
  
  xNbr <- paste0(unlist(as.character(lapply(missZero, leadZeroBuilder))),
                 xNbr)
  return(xNbr)
}

### Subordinate function that creates set of leading 0s ----------------------------------

leadZeroBuilder <- function(x)
{
  if(length(x) == 0){
    return(0)
  } else {
    gsub('x', '0', paste0(unlist(rep("x", x)), collapse=''))
  }
}

### Function to trim a dataset based on a field with multiple values ---------------------

trimByField <- function(xData,          # Dataset to be trimmed 
                        field,          # Field to trim by
                        trimList,       # List of potential field values to trim by
                        inclusive=FALSE # Are trimList value removed (or kept as valid)
                        ){
  
  ## Identify those in list
  
  if(!inclusive){
    xData$valid <- 1
    
    for(i in 1:length(trimList)){
      xData$valid <- ifelse(xData[field] == trimList[i], 0 , xData$valid)
    }
  } else {
    xData$valid <- 0
    
    for(i in 1:length(trimList)){
      xData$valid <- ifelse(xData[field] == trimList[i], 1 , xData$valid)
    }
    
  }
  
  ## Trim data
  
  xData <- xData[xData$valid == 1, ]
  xData$valid <- NULL
  
  # Return data
  
  return(xData)
}

### Function to identify duplicate values and treat a variety of ways --------------------

idDup <- function(xData,                # Data frame to analyze
                  field,                # Field in which to search for duplicates
                  iddType='toRemove',   # Manner to handle dupicate
                  newField='newField',  # Name of new field (if using 'labelNonUnique)
                  binNonUq=FALSE        # SHould unique be given as 1/0 (vs. counts)
                  ){
  
  ## If removing duplicates
  
  if(iddType == 'toRemove'){
    xData <- xData[!duplicated(xData[field]), ]
  }
  
  # #If Labeling just the duplicated rows
  
  if(iddType == 'labelDup'){
    xData[ ,ncol(xData) + 1] <- duplicated(xData[field])
    colnames(xData)[ncol(xData)] <- newField   
  }
  
  ## If labeling all records that are not unique in the field
  
  if(iddType == 'labelNonUnique'){
    cntTable <- as.data.frame(table(xData[field]))
    xData[ ,ncol(xData) + 1] <- cntTable$Freq[match(xData[, field],
                                                    cntTable$Var1)]
    colnames(xData)[ncol(xData)] <- newField   
    if(binNonUq){
      xData[ ,ncol(xData)] <- ifelse(xData[ ,ncol(xData)] > 1, 1, 0)  
    }
  }
  
  return(xData)
}

