
### Function for calculating the Pseudo R2 of a spatial error model ----------------------

calcPseudoR2 <- function(semModel){
  pR2 <- (1 - (as.numeric(semModel$SSE) / 
                 sum((semModel$y - mean(semModel$y)) ^ 2)))
  return(pR2)
}

### Function for building a spatial weights matrix (NOT COMPLETE)

createSWM <- function(data # SpatialPointsDataFrame
                      ,knn  # Number of neighbors desired
                      ,distWeighted = TRUE # Distance Weighted ?
                      ,nugget = 1 # How much to add to identical points
                      ){
  
  ## Create distance weighting function
  
  dwf <- function(x) {1 / ((x + nugget) ^ 2)}
  
  ## Create Neighbor List
  nbList <- knn2nb(knearneigh(data, knn))
  
  ## Create Distances
  if(distWeighted){
    nbDists <- nbdists(nbList, data)    
  }
  
  ## Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                               , style="W",zero.policy=T))    
}
  
### Function for extracting full coefficient table from sarLM object ---------------------

extractSEMCoefs <- function(sarlmObj){
  
  xDF <- data.frame(Estimate = sarlmObj$coefficients,
                    StdErr = sarlmObj$rest.se,
                    zVal = sarlmObj$coefficients/sarlmObj$rest.se,
                    pVal = 2 * pnorm(-abs(sarlmObj$coefficients/sarlmObj$rest.se)))
  xDF
}

