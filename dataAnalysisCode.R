'#########################################################################################
#                                                                                        #                                         
#       Analyzing King County Sales Data                                                 #
#                                                                                        #   
#       by Anonymous Real Estate Author                                                  #
#                                                                                        # 
#       Code for Data Analysis                                                           #                                                                                                      ###  
#                                                                                        #
#########################################################################################'

### Load Libraries, Helper Files and Set Global Parameters -------------------------------

 ## Load Libraries

  library(maptools)
  library(spdep)
  library(sp)
  library(rgeos)
  library(spgwr)
  library(spam)
  library(colorRamps)
  library(RColorBrewer)
  library(ggmap)
  library(reshape)
  library(ggplot2)
  library(gridExtra)

 ## Set location parameter

  #locDir <- 'c:/dropbox/dropbox/' 
  codePath <- 'D:/Code/R/Research/RRR'
  dataPath <- 'D:/Data/Research/RRR' 
  #exportPlots <- FALSE

 ## Source Files
  
  source(file.path(codePath, 'spatEconTools.R'))
 
 ## Define helper functions

  # Extract pseudo R-squared
  pseudoR2 <- function(mod)
  {
    if(class(mod) == 'sarlm') Y <- mod$y
    if(class(mod) == 'lm') Y <- mod$model[ ,1]
    
    ybar <- sum((Y - mean(Y)) ^ 2)
    yhat <- sum((Y - mod$fitted.values) ^ 2)
    return(1 - (yhat / ybar))
  }

  # Build a summary statistics table for export
  buildSSTable <- function(ssData){
    ssData <- ssData[, apply(ssData, 2, class) == 'numeric']
    SS <- t(apply(ssData, 2, summary))
    return(SS)
  }

### Load in Data -------------------------------------------------------------------------
  
 ##  Load in Prepared sales data
  
  kcSales <- read.csv(file.path(dataPath, 'cleanSales.csv'), header=T)

### Initial data clean -------------------------------------------------------------------

 ## Remove any remaining likely non Single Family Residential 

  # Remove multiple building sales
  kcSales <- subset(kcSales, nbrBldgs == 1)

  # Remove those with a present use code not of 2 or 29 
  kcSales <- subset(kcSales, PresentUse == 2 | PresentUse == 29)

  # Remove those with more than one living unit
  kcSales <- subset(kcSales, NbrLivingUnits == 1)

  # Remove those with an improved assessed value of 0
  kcSales <- subset(kcSales, ImpsVal != 0)

### Data transformation ------------------------------------------------------------------

 ## Control variables

  # Create a sales month variable
  kcSales$Month <- as.numeric(substr(kcSales$DocumentDate,1,2))

  # Create a waterfront Binary variable
  kcSales$WFNT <- ifelse(kcSales$WfntLocation > 0, 1, 0)

  # Sum up all bathrooms
  kcSales$Baths <- (kcSales$BathFullCount + kcSales$Bath3qtrCount * .75 +
                    kcSales$BathHalfCount * .5)  

  # Sum up all fireplaces
  kcSales$Fireplaces <- (kcSales$FpSingleStory + kcSales$FpMultiStory +
                         kcSales$FpFreestanding + kcSales$FpAdditional)
                         
  # Create a binary variable for townhomes
  kcSales$Townhome <- ifelse(kcSales$PresentUse == 29, 1, 0)

 ## Variables of interest (Views) 

  # Has Mountain View
  kcSales$viewMount <- ifelse(rowSums(cbind(kcSales$MtRainier, kcSales$Olympics,
                                            kcSales$Cascades)) > 0, 1, 0)

  # Best Mountain View Rating
  kcSales$viewMountScore <- apply(cbind(kcSales$MtRainier, kcSales$Olympics,
                                        kcSales$Cascades), 1, max)

  # Has Water View
  kcSales$viewWater <- ifelse(rowSums(cbind(kcSales$PugetSound, kcSales$LakeWashington,
                                            kcSales$LakeSammamish)) > 0, 1, 0)

  # Best Water View Rating
  kcSales$viewWaterScore <- apply(cbind(kcSales$PugetSound, kcSales$LakeWashington,
                                        kcSales$LakeSammamish), 1, max)

  # Has Other View
  kcSales$viewOther <- ifelse(rowSums(cbind(kcSales$Territorial, kcSales$SeattleSkyline,
                                            kcSales$SmallLakeRiverCreek, 
                                            kcSales$OtherView)) > 0, 1, 0)

  # Best Water View Rating
  kcSales$viewOtherScore <- apply(cbind(kcSales$Territorial, kcSales$SeattleSkyline,
                                        kcSales$SmallLakeRiverCreek, 
                                        kcSales$OtherView), 1, max)

  # Multiple Views?
  kcSales$viewMult <- ifelse(rowSums(cbind(kcSales$viewMount, kcSales$viewWater,
                                           kcSales$viewOther)) > 0, 1, 0)

  # Total view score across all categories
  kcSales$viewTotalScore <- rowSums(kcSales[,which(names(kcSales) == 'MtRainier'):
                                              which(names(kcSales) == 'OtherView')])

### Create a trimmed set that removes outliers -------------------------------------------

 ## Create trimmed set

  # Remove sales with more than 2 acres of land
  trimSales <- subset(kcSales, SqFtLot < (43560 * 2))

  # Remove sales with more than 8 bedrooms
  trimSales <- subset(trimSales, Bedrooms < 8)

  # Remove sales with more than 8 bedrooms
  trimSales <- subset(trimSales, SqFtTotLiving >= 500 & SqFtTotLiving <= 8000)

### Create a base model ------------------------------------------------------------------

  modBase <- lm(log(SalePrice) ~ as.factor(Month) + SqFtLot + WFNT + BldgGrade + 
                  SqFtTotLiving + Baths + YrBuilt + YrRenovated + Fireplaces + Townhome + 
                  viewMount + viewWater + viewOther, data=trimSales)

 ## Test for spatial autocorrelation

  salesSP <- SpatialPointsDataFrame(coords=cbind(trimSales$X, trimSales$Y),
                                       data=trimSales)

  # Create the Spatial Weights Matrix

  swmAll10 <- createSWM(salesSP, 10, nugget=25)

  # Test for spatial autocorrelation in the error terms

  miAll <- moran.test(modBase$resid, swmAll10, zero.policy=TRUE)

  # Test for the type of spatial dependence

  lmAll <- lm.LMtests(modBase, swmAll10, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))

 ## Specify a Spatial Error Model 

  # Estimate Model

  modSEM <- errorsarlm(as.formula(modBase), data=salesSP@data, swmAll10, method="spam", 
                       zero.policy=TRUE)

  pseudoR2(modSEM)

### Add differntation based on view score

  modBaseSc <- lm(log(SalePrice) ~ as.factor(Month) + SqFtLot + WFNT + BldgGrade + 
                  SqFtTotLiving + Baths +
                  YrBuilt + YrRenovated + Fireplaces + Townhome + 
                  as.factor(viewMountScore) + 
                  as.factor(viewWaterScore) + 
                  as.factor(viewOtherScore), data=trimSales)

 ## Specify a Spatial Error Model 

  # Estimate Model

  modSEMSc <- errorsarlm(as.formula(modBaseSc), data=salesSP@data, swmAll10,
                         method="spam", zero.policy=TRUE)
 
  pseudoR2(modSEMSc)
