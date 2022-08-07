# 计算基金稳定性指标

##-------------------------SDS计算--------------------------
# 定义稳定性指标：SDS指标
# 某基金历史时段内，对各因子参数求方差，方差之和再开根
SDSIndex.function = function(factorCoefSeq){
  SMB.var = var(factorCoefSeq$SMBCoef)
  HML.var = var(factorCoefSeq$HMLCoef)
  CMA.var = var(factorCoefSeq$CMACoef)
  SDSIndex = sqrt(sum(SMB.var,HML.var,CMA.var))
  return(SDSIndex)
}

##------------------各基金稳定指标有序序列-------------------
# 遍历所有基金，计算各基金的稳定性指标
fundStability.function = function(totalFundData){
  SDSIndexSeq = c()
  
  fundcodes = unique(totalFundData$INNERCODE)
  
  # 遍历所有基金
  for(fundcode in fundcodes){
    fundData = totalFundData[which(totalFundData$INNERCODE==fundcode)]
    
    # 截至今年年初无数据，或总数据量在10及以下的，则跳过
    if(max(fundData$TRADINGDAY)<as.Date("2022-01-01") | length(fundData$TRADINGDAY)<=10){ 
      SDSIndex = 1000
      SDSIndexSeq = append(SDSIndexSeq,SDSIndex)
      next 
    }
    
    # 根据基金历史数据fundData，计算SDSIndex
    factorCoefSeq = factorCoefSeq.function(fundData)
    SDSIndex = SDSIndex.function(factorCoefSeq)
    SDSIndexSeq = append(SDSIndexSeq,SDSIndex)
  }
  
  fundStability = data.frame(fundcodes,SDSIndexSeq)
  fundStability = fundStability[order(fundStability$SDSIndexSeq),]
  return(fundStability)
  
}
