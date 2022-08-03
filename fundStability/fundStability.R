
##-------------------------SDS计算--------------------------
SDSIndex.function = function(factorCoefSeq){
  SML.var = var(factorCoefSeq$SMLCoef)
  HML.var = var(factorCoefSeq$HMLCoef)
  CMA.var = var(factorCoefSeq$CMACoef)
  SDSIndex = sqrt(sum(SML.var,HML.var,CMA.var))
  return(SDSIndex)
}

##------------------各基金稳定指标有序序列-------------------
fundStability.function = function(totalFundData){
  funds = totalFundData$funds
  SDSIndexSeq = c()
  
  for(fundData in totalFundData&datas){
    factorCoefSeq = factorCoefSeq.function(fundData)
    SDSIndex = SDSIndex.function(factorCoefSeq)
    SDSIndexSeq = append(SDSIndexSeq,SDSIndex)
  }
  
  fundStability = data.frame(funds,SDSIndexSeq)
  fundStability = fundStability[order(fundStability$SDSIndexSeq)]
  return(fundStability)
  
}
