
##-------------------------SDS计算--------------------------
SDSIndex.function = function(factorCoefSeq){
  SMB.var = var(factorCoefSeq$SMBCoef)
  HML.var = var(factorCoefSeq$HMLCoef)
  CMA.var = var(factorCoefSeq$CMACoef)
  SDSIndex = sqrt(sum(SMB.var,HML.var,CMA.var))
  return(SDSIndex)
}

##------------------各基金稳定指标有序序列-------------------
fundStability.function = function(totalFundData){
  SDSIndexSeq = c()
  
  fundcodes = unique(totalFundData$INNERCODE)
  
  for(fundcode in fundcodes[1:19566]){
    fundData = totalFundData[which(totalFundData$INNERCODE==fundcode)]
    print(which(fundcodes == fundcode))
    if(max(fundData$TRADINGDAY)<as.Date("2022-01-01") | length(fundData$TRADINGDAY)<=10){ 
      SDSIndex = 1000
      SDSIndexSeq = append(SDSIndexSeq,SDSIndex)
      next # 截至今年年初无数据，或总数据量在10及以下的，则跳过
    }
    
    factorCoefSeq = factorCoefSeq.function(fundData)
    SDSIndex = SDSIndex.function(factorCoefSeq)
    SDSIndexSeq = append(SDSIndexSeq,SDSIndex)
  }
  
  fundStability = data.frame(fundcodes,SDSIndexSeq)
  fundStability = fundStability[order(fundStability$SDSIndexSeq),]
  return(fundStability)
  
}
