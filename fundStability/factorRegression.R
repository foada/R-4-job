#根据fundData(某基金的2012年至今历史数据)
#计算基金历史每个月末的因子暴露，得到因子参数序列factorCoefSeq

##-----------------------月末日期序列----------------------
#取2015年至今，每个月末日期，得到序列endDateSeq=c(endDate1,endDate2,...)
#endDate后续用于分割出回归时段(endDate-3year,endDate)
endDateSeq.function = function(){
  startYMD = as.Date("2015-01-01")
  endYMD = today()
  monthRange = floor((endYMD-startYMD+ddays(1))/dmonths(1))
  endDateSeq = seq(as.Date("2015-02-01"),length=monthRange,by="1 month")-1
  return(endDateSeq)
}

##----------------------日期区间内数据---------------------
#取基金数据中，以endDate为截至日期过去三年的历史数据dataRange
dataRange.function = function(fundData,endDate){
  startDate = as_date(today() - dyears(3))
  dataRange = fundData[which(fundData$tradingDay>startDate & fundData$tradingDay<=endDate)]
  return(dataRange)
}


##------------------------模型拟合-------------------------
#对于dataRange的数据，进行拟合，得到过去三年时段内的β
factorCoefs.function = function(dataRange){
  fit = lm(fundNvd~SML+HML+CMA,data = dataRange)
  coefs = fit$coefficients
  factorCoefs = data.frame(
    SMLCoef = coefs[2],
    HMLCoef = coefs[3],
    CMACoef = coefs[4]
  )
  return(factorCoefs)
}


##----------------------因子参数序列----------------------
#对某基金，根据endDateSeq，计算每个endDate对应的beta
#得到因子参数序列factorCoefSeq
factorCoefSeq.function = function(fundData){
  factorCoefSeq = data.frame(
    SMLCoef = c(),
    HMLCoef = c(),
    CMACoef = c()
  )
  
  endDateSeq = endDateSeq.function()
  for(endDate in endDateSeq){
    dataRange = dataRange.function(fundData,endDate)
    factorCoefs = factorCoefs.function(dataRange)
    factorCoefSeq = rbind(factorCoefSeq,factorCoefs)
  }
  
  return(factorCoefSeq)
}

 
# ##------------------------风格序列------------------------
# #根据因子参数序列，计算该基金在每个endDate时点对应的风格
# styleSeq.function = function(factorCoefSeq){
#   styles = c("规模","价值","成长")
#   styleSeq = c()
#   for(i in 1:nrow(factorCoefSeq)){
#     factorCoefs = factorCoefSeq[i,]
#     styleSeq =append(styleSeq, styles[which.max(factorCoefs)])
#   }
#   return(styleSeq)
# }
# 
# ##------------------------基金风格表------------------------
