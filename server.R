################################################################################
#                                                                              #
#   Server for Zestimate MD Shiny App.                                         #
#                                                                              #  
################################################################################

# load libraries

library(shiny)
library(xtable)

load('RRR.RData')

shinyServer(function(input, output) {

  
### Reactive function to call ZestimateMD function ----------------------------------------------------  
 
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

###  Estimate OLS Model ------------------------------------------------------------------------

  estimateOLSModel <- reactive({
    res <- updateData()
    modSpec <- buildModSpec()
    modBase <- lm(modSpec$modSpec, data=res)
    modBase    
  })


### Basic Information Table -----------------------------------------------------------

#     output$infoTable <- renderTable({
#       res <- updateData()
#       infoTable <- data.frame(
#            Zpid = res$property$PropertyID,
#            UseCode = res$property$UsecodeTypeIDStandard,
#            FinishedSquareFeet = res$property$FinishedSquareFeet,
#            Bedrooms = res$property$BedroomCnt,
#            Bathrooms = res$property$BathroomCnt,
#            YearBuilt = res$property$BuiltYear,
#            MajorRemodelYear = res$property$MajorRemodelYear,
#            LotSizeSquareFeet = res$property$LotSizeSquareFeet,
#            GrossAssessement = res$property$AssessedGrossValueDollarCnt,
#            MarketAssessement = res$property$MarketValueDollarCnt,
#            Waterfront = res$property$IsWaterfront,
#            SLR = res$property$IsSLR
#       )
#       resX <- data.frame(t(infoTable))
#       colnames(resX) <- "Value"
#       resX$Value <- round(resX$Value, 0)
#       xtable(resX, digits=0)
#     })
#  
#  ### Table of Valuation Information ------------------------------------------------------------
# 
    output$valTable <- renderTable({
      rNames <- buildModSpec()$namesRows
      modRes <- try(estimateOLSModel(), silent=T)
      if(class(modRes) == 'try-error'){
        xtable(data.frame(
          message="You have filtered out too many sales, include more sales and try again"))
      } else {
        valTable <- summary(modRes)[[4]]
        errorCheck <- try(rownames(valTable) <- c('Constant', rNames), silent=T)
        if(class(errorCheck) == 'try-error'){
          xtable(data.frame(
            message="You have filtered out too many sales, include more sales and try again"))    
        } else {
          xtable(valTable, digits=3)      
        }
      }
   })
# #   
# # ### Plot of Valuations over time Information ------------------------------------------------------------
#  

  output$valPlot2 <- renderPlot({
    coefs <- estimateOLSModel()
      par(mar=c(12.1,4.1,4.1,2.1))
      sel <- grep('view', names(coefs$coefficients))
      vNames <- buildModSpec()$namesRows[sel-1]
      vCol <- rep(4, length(vNames))
      vCol[grep('Mount', vNames)] <- 3
      vCol[grep('Other', vNames)] <- 'gray50'
      barplot(100*(exp(coefs$coef[sel])-1),
              names.arg=vNames, las=3, col=vCol, border=vCol,
              ylab="% Premium")
#     } else {
#       sel <- grep('view', names(coefs$coefficients))
#       vNames <- buildModSpec()$namesRows[sel-1]
#       
#       
#       barplot(coefs$coef[sel])
#     }  
   })

  output$mapP <- renderPlot({
    res <- updateData()
    plot(kcSales$X, kcSales$Y, cex=.2, col='gray50', xaxt='n', yaxt='n',
         xlab="", ylab="")
    points(res$X, res$Y, cex=.2, col=2)
  })


#  
# # ### Comp Map --------------------------------------------------------------------------------------------
# # 
#    output$mapPlot <- renderPlot({
#       res <- updateData()
#       rgmPlot(data = list(res$taeReport$Comps, res$zaeReport$Comps, res$property),
#               cols = list(4,3,2),
#               cexs = list(1,1,1.5),
#               pchs = list(17,18,15))
#     }, height=500, width=600)
# 
# # ## Knn Plots -------------------------------------------------------------------------------------------
# # 
# #   output$knnPlot <- renderPlot({
# #     res <- updateData()
# # 
# #     plot(0,0,col=0,xlim=c(-.5,2.5), ylim=c(-.2,1.2),xlab="KNN Measure",ylab="Relative Value",
# #        xaxt='n')
# #     axis(1, at=0:2, labels=c("Zest","Zest/SqFt","Zest/LotSqFt"))
# #     zpS <- c(res$property$ZestPrior, res$property$ZestPriorSmooth15, res$property$ZestPriorSmooth50)
# #     zpS <- (zpS - min(zpS)) / max(zpS)
# #     zpFS <- c(res$property$ZestPriorSqft, res$property$ZestPriorSqftSmooth15, res$property$ZestPriorSqftSmooth50)
# #     zpFS <- (zpFS - min(zpFS)) / max(zpFS)
# #     zpLS <- c(res$property$ZestPriorLotSizeSqft, res$property$ZestPriorLotSizeSqftSmooth15, 
# #             res$property$ZestPriorLotSizeSqftSmooth50)
# #     zpLS <- (zpLS - min(zpLS)) / max(zpLS)
# #   
# #     lines(c(0:2),c(zpS[1],zpFS[1],zpLS[1]),lwd=3,col=2)
# #     lines(c(0:2),c(zpS[2],zpFS[2],zpLS[2]),lwd=3,col=4)
# #     lines(c(0:2),c(zpS[3],zpFS[3],zpLS[3]),lwd=3,col=5)
# #     legend('topleft', c("Subject","KNN15","KNN50"),lwd=2,col=c(2,4,5))
# #   })
# # 
# # ### AppEmu Trans Comp Information           ------------------------------------------------------------
# # 
# output$taeTable <- renderTable({
#   res <- updateData()
#   taeTable <- data.frame(
#     CompNbr = c("Subject",paste0("Comp",1:nrow(res$predAET$comps))),
#     HomeSqFt = c(res$property$FinishedSquareFeet, res$predAET$comps$FinishedSquareFeet),
#     Baths = c(res$property$BathroomCnt, res$predAET$comps$BathroomCnt),
#     Beds = c(res$property$BedroomCnt, res$predAET$comps$BedroomCnt),
#     YrBuilt = c(res$property$BuiltYear, res$predAET$comps$BuiltYear),
#     LotSize = c(res$property$LotSizeSquareFeet, res$predAET$comps$LotSizeSquareFeet),
#     AssdVal = c(res$property$AssessedGrossValueDollarCnt, res$predAET$comps$AssessedGrossValueDollarCnt),
#     OrigEst = c(0, res$predAET$adjs$origEst),
#     AdjAmt = c(0, res$predAET$adjs$adjAmt),
#     AdjEst = c(0, res$predAET$adjs$adjEst),
#     CompWgt = c(0, res$predAET$adjs$wt)    
#   )
#   xtable(taeTable, digits=0, size='tiny')
# })
# # 
# # ### AppEmu Zest Comp Information           ------------------------------------------------------------
# # 
# output$zaeTable <- renderTable({
#   res <- updateData()
#   zaeTable <- data.frame(
#     CompNbr = c("Subject",paste0("Comp",1:nrow(res$predAEZ$comps))),
#     HomeSqFt = c(res$property$FinishedSquareFeet, res$predAEZ$comps$FinishedSquareFeet),
#     Baths = c(res$property$BathroomCnt, res$predAEZ$comps$BathroomCnt),
#     Beds = c(res$property$BedroomCnt, res$predAEZ$comps$BedroomCnt),
#     YrBuilt = c(res$property$BuiltYear, res$predAEZ$comps$BuiltYear),
#     LotSize = c(res$property$LotSizeSquareFeet, res$predAEZ$comps$LotSizeSquareFeet),
#     AssdVal = c(res$property$AssessedGrossValueDollarCnt, res$predAEZ$comps$AssessedGrossValueDollarCnt),
#     OrigEst = c(0, res$predAEZ$adjs$origEst),
#     AdjAmt = c(0, res$predAEZ$adjs$adjAmt),
#     AdjEst = c(0, res$predAEZ$adjs$adjEst),
#     CompWgt = c(0, res$predAEZ$adjs$wt)    
#   )
#   xtable(zaeTable, digits=0, size='tiny')
# })
# 


})