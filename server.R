################################################################################
#                                                                              #
#   Server for Reproducible Real Estate Analysis                               #
#                                                                              #  
################################################################################

# load libraries

library(shiny)
library(xtable)
library(sp)
library(spdep)
library(maptools)

load('RRR.RData')
source('spatEconTools.R')
setAlpha <- function(col,        # vector of colors
                     alpha=1     # single value or vector of transparency alphas
){
  
  # Warning messages
  if(missing(col)) stop("Please provide a vector of colours.")
  if(length(alpha) != 1 & length(alpha) != length(col)){
    stop("Please provide a single alpha value OR a value for each color in the vector")    
  }
  
  # Set function to convert to 0,1 range
  rgb01 <- function(x){col2rgb(x)/255}
  
  # Contruct rgb Matrix
  rgbMatrix <- lapply(col, rgb01)
  
  # Apply alpha appending to all colors
  mapply(appendAlpha, rgbMatrix, alpha=alpha) 
}

### Helper function that appends alpha value to rgb values -------------------------------

appendAlpha <- function(x,       # Color as an rgb object
                        alpha    # Tranparency level 0,1
){
  rgb(x[1], x[2], x[3], alpha=alpha) 
}



shinyServer(function(input, output) {

  
### Reactive function to call UpdateData function ----------------------------------------  
 
  updateData <- reactive({
   
    sales <- subset(kcSales, SalePrice <= as.numeric(input$priceLimits[2]) & 
                             SalePrice >= as.numeric(input$priceLimits[1]))
    
    sales <- subset(sales, SqFtLot <= as.numeric(input$lotSizeLimits[2]) & 
                           SqFtLot >= as.numeric(input$lotSizeLimits[1]))
    
    sales <- subset(sales, SqFtTotLiving <= as.numeric(input$homeSizeLimits[2]) & 
                           SqFtTotLiving >= as.numeric(input$homeSizeLimits[1]))  
    
    sales <- subset(sales, Bedrooms <= as.numeric(input$bedLimits[2]) & 
                           Bedrooms >= as.numeric(input$bedLimits[1]))  
    
    sales
 })

### Building Model Specifications --------------------------------------------------------

  buildModSpec <- reactive({
    
    modSpec <- "log(SalePrice) ~ "
    namesRows <- NULL
    
    if(input$viewMount){
      if(input$byScore){
        modSpec <- paste0(modSpec, " + as.factor(viewMountScore)")
        namesRows <- c(namesRows, paste0("Mountain View, Level ",
                                         names(table(updateData()$viewMountScore))[-1]))  
      } else {
        modSpec <- paste0(modSpec, " + viewMount")
        namesRows <- c(namesRows, 'Mountain View')    
      }
    }
    if(input$viewWater){
      if(input$byScore){
        modSpec <- paste0(modSpec, " + as.factor(viewWaterScore)")
        namesRows <- c(namesRows, paste0("Water View, Level ",
                                         names(table(updateData()$viewWaterScore))[-1]))  
      } else {
        modSpec <- paste0(modSpec, " + viewWater")
        namesRows <- c(namesRows, 'Water View')    
      }
    }  
    if(input$viewOther){
      if(input$byScore){
        modSpec <- paste0(modSpec, " + as.factor(viewOtherScore)")
        namesRows <- c(namesRows, paste0("Other View, Level ",
                                         names(table(updateData()$viewOtherScore))[-1]))  
      } else {
        modSpec <- paste0(modSpec, " + viewOther")
        namesRows <- c(namesRows, 'Other View')    
      }
    }  
    if(input$SqFtTotLiving){
      modSpec <- paste0(modSpec, " + SqFtTotLiving")
      namesRows <- c(namesRows, 'Home Size (SqFt)')      
    }
    if(input$YrBuilt){
      modSpec <- paste0(modSpec, " + YrBuilt")
      namesRows <- c(namesRows, 'Year Built')      
    }
    if(input$YrRenovated){
      modSpec <- paste0(modSpec, " + YrRenovated")
      namesRows <- c(namesRows, 'Year Renovated')      
    }
    if(input$Baths){
      modSpec <- paste0(modSpec, " + Baths")
      namesRows <- c(namesRows, 'Bathrooms')      
    }
    if(input$BldgGrade){
      modSpec <- paste0(modSpec, " + BldgGrade")
      namesRows <- c(namesRows, 'Building Quality')      
    }
    if(input$Fireplaces){
      modSpec <- paste0(modSpec, " + Fireplaces")
      namesRows <- c(namesRows, 'Fireplaces')      
    }
    if(input$Townhome){
      if(length(table(updateData()$Townhome)) > 1){
         modSpec <- paste0(modSpec, " + Townhome")
         namesRows <- c(namesRows, 'Townhome')
      }   
    }
    if(input$SqFtLot){
      modSpec <- paste0(modSpec, " + SqFtLot")
      namesRows <- c(namesRows, 'Lot Size (SqFt)')      
    }
    if(input$WFNT){  modSpec <- paste0(modSpec, " + WFNT")
      modSpec <- paste0(modSpec, " + WFNT")
      namesRows <- c(namesRows, 'Water Frontage')      
    }
    if(input$Month) {
      modSpec <- paste0(modSpec, " + as.factor(Month)")
      months <- c('January', 'February', 'March', 'April', 'May', 'June',
                  'July', 'August', 'September', 'October', 'November', 'December')
      mNames <- as.numeric(names(table(updateData()$Month))[-1])
      namesRows <- c(namesRows, months[mNames])
    }
    return(list(modSpec=modSpec,
                namesRows=namesRows))
  })

###  Estimate Model ------------------------------------------------------------------------

  estimateModel <- eventReactive(input$rerun, {
    res <- updateData()
    modSpec <- buildModSpec()
    
    if(input$spatEcon){
      swm <- buildSWM()
      modObj <- errorsarlm(as.formula(modSpec$modSpec), data=res, listw=swm, method='spam',
                           zero.policy=TRUE)
    } else {
      modObj <- lm(modSpec$modSpec, data=res)
    }
    
    modObj
  })

###  Build SWM OLS Model ------------------------------------------------------------------------

  buildSWM <- eventReactive(input$rerun, {
    
    if(input$spatEcon){
      dataSP <- SpatialPointsDataFrame(coords=cbind(updateData()$X,
                                                  updateData()$Y),
                                     data = updateData())
      swmX <- createSWM(dataSP, knn=input$swmKnn, distWeighted=input$swmType, nugget=25)
      swmX
    }
  })

### Output value table -------------------------------------------------------------------

  output$valTable <- renderTable({  
    createCoefTable()
  })

### Table of Valuation Information ------------------------------------------------------------

  createCoefTable <- eventReactive(input$rerun, {
  rNames <- buildModSpec()$namesRows
  modRes <- try(estimateModel(), silent=T)
  if(class(modRes) == 'try-error'){
    xtable(data.frame(
      message="You have filtered out too many sales, include more sales and try again"))
  } else {
    if(class(modRes) == 'sarlm'){
      valTable <- data.frame(Coef = modRes$coefficients,
                             StErr = modRes$rest.se)
                             
      errorCheck <- try(rownames(valTable) <- c('Constant', rNames), silent=T)      
    } else {
      valTable <- summary(modRes)[[4]]       
      errorCheck <- try(rownames(valTable) <- c('Constant', rNames), silent=T)
      colnames(valTable) <- c("Coef","StErr", "tval", "pval")
    }
    
    if(class(errorCheck) == 'try-error'){
      xtable(data.frame(
        message="You have filtered out too many sales, include more sales and try againv"))    
    } else {
      xtable(valTable, digits=4)      
    }
  }   
})

### Plot View Coefficients    ------------------------------------------------------------

  output$valPlot2 <- renderPlot({
    coefs <- estimateModel()
      par(mar=c(12.1,4.1,4.1,2.1))
      sel <- grep('view', names(coefs$coefficients))
      vNames <- buildModSpec()$namesRows[sel-1]
      vCol <- rep(4, length(vNames))
      vCol[grep('Mount', vNames)] <- 3
      vCol[grep('Other', vNames)] <- 'gray50'
      barplot(100*(exp(coefs$coefficients[sel])-1),
              names.arg=vNames, las=3, col=vCol, border=vCol,
              ylab="% Premium")
   })

### Plot Data Map ------------------------------------------------------------------------

  output$mapP <- renderPlot({
    res <- updateData()
    plot(kcSales$X, kcSales$Y, cex=.2, col='gray75', xaxt='n', yaxt='n',
         xlab="", ylab="")
    points(res$X, res$Y, cex=.2, col=2)
  })

### Output value table -------------------------------------------------------------------

output$diagTable <- renderTable({  
    mod <- estimateModel()
  
  if(class(mod) == 'sarlm'){
    r2 <- calcPseudoR2(mod)
    stderr <- sqrt(mod$s2)
    mape <- mean(abs(mod$residuals))
    lambda <- mod$lambda
    aicM <- AIC(mod)
    diags <- data.frame(Diagnostic=c('R-squared', 'Std Error', 'Mean Absolute Error',
                                     'Lambda', 'AIC'),
                        Value = c(r2, stderr, mape, lambda, aicM))
  } else {
    r2 <- summary(mod)$r.squared
    stderr <- summary(mod)$sigma
    mape <- mean(abs(summary(mod)$residuals))
    fstat <- summary(mod)$fstatistic[1]
    aicM <- AIC(mod)
    diags <- data.frame(Diagnostic=c('R-squared', 'Std Error', 'Mean Absolute Error',
                                     'F-Stat', 'AIC'),
                        Value = c(r2, stderr, mape, fstat, aicM))
  }
  

    xtable(diags, digits=1)                       
})

### Plot Residuals -----------------------------------------------------------------------

  output$resPlot <- renderPlot({
    modOut <- estimateModel()
    res <- updateData()
    under <- modOut$residuals > 0
    over <- modOut$residuals <= 0
    xCol <- rep(4, length(modOut$residuals))
    xCol[over] <- 2
    
    resPer <- abs(modOut$residuals)
    resPer[resPer>1] <- 1

    xCol <- setAlpha(xCol, resPer)
    
    plot(res$X, res$Y, cex=.1, col=xCol, xaxt='n', yaxt='n',
       xlab="", ylab="", main='Plot of Residuals')
    legend('bottomright', c('Over Predict', 'Under Predict', 'Dark=higher error'),
           col=c(2,4,0), pch=16)  
 
})



})