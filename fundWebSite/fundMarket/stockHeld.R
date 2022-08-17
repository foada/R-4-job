##------------------------t时刻，基金净值-------------------------
# 寻找时刻t的基金净值
netValue.function = function(endDate,data){
  startDate = endDate-months(6)
  valueRange = data[data$ENDDATE>startDate & data$ENDDATE<=endDate]
  netValue = t(sapply(split(valueRange,valueRange$INNERCODE),
                      function(a){a[which.max(a$ENDDATE)]}))
  return(netValue)
}

##----------------------t时刻，单基金持股仓位---------------------
# 计算t时刻，某基金股票仓位β
# data为截至t时刻，基金一个月数据，回归得到β
betaOptim.function = function(data){
  if(nrow(data)<=5) return(NA)
  y = data$NVRDAILYGROWTHRATE
  x = data$CHANGEPCT
  
  # 最优化，得到β
  a = (t(x)%*%x)[1]
  b = (-2 * (t(x) %*% y))[1]
  f = function(beta) a*beta^2 + b*beta
  betaOptim = optimize(f,lower=0,upper=1)$minimum
  
  return(betaOptim)
}

# 计算t时刻，各基金的持股比例，构成序列betaSeq
betaSeq.function = function(dataRange){
  # 按基金代码分类，每个基金求β，得到betaSeq
  betaSeq = sapply(split(dataRange,by="INNERCODE"),betaOptim.function)
  betaSeq = cbind(as.data.frame(betaSeq),names(betaSeq))
  if(nrow(betaSeq)==0) return(NA)
  colnames(betaSeq) = c("BETA","INNERCODE")
  row.names(betaSeq) = 1:nrow(betaSeq)
  return(betaSeq)
}

##-----------------------基金市场股票仓位--------------------------
#计算t时刻市场上，所有基金持有股票仓位，stockHeld
stockHeld.function = function(dataRange,netValue){
  if(nrow(dataRange)==0 | nrow(netValue)<=1) return(NA)
  
  betaSeq = betaSeq.function(dataRange)
  if(nrow(betaSeq)==0) return(NA)
  
  # 整理数据
  betaWeightSeq = merge(betaSeq,netValue,by="INNERCODE",all=F)
  betaWeightSeq$NV = as.numeric(betaWeightSeq$NV)
  betaWeightSeq = betaWeightSeq[!is.na(betaWeightSeq$BETA),]
  
  # 求基金市场股票持仓
  stockHeld = sum(betaWeightSeq$BETA * betaWeightSeq$NV)/sum(betaWeightSeq$NV)
  return(stockHeld)
}


# 计算各时刻，市场上，基金持有股票仓位，stockHeldSeq
stockHeldSeq.function = function(totalData,fundValue){
  # 给出计算时点seq of t
  endDateSeq = endDateSeq.function(as.Date("2015-01-01"),today())
  
  # 对每个t，给出【t-3months,t】的基金数据dataRange
  # 所有dataRange 构成 dataRangeSeq
  dataRangeSeq = lapply(endDateSeq,dataRange.function,data=totalData)
  # 同理，对每个t，给出基金净值
  netValueSeq = lapply(endDateSeq,netValue.function,data=fundValue)
  
  # 得到计算每个t的β，得到时间序列stockHeldSeq
  stockHeldSeq = mapply(stockHeld.function,dataRangeSeq,netValueSeq)
  stockHeldSeq = data.frame(date = endDateSeq,position = stockHeldSeq)
  stockHeldSeq = stockHeldSeq[!is.na(stockHeldSeq$position),]
  return(stockHeldSeq)
}

##-----------------------------绘图----------------------------
stockHeldPlot.function = function(stockHeldSeq){
  p = ggplot(stockHeldSeq, mapping = aes(x=date, y=position))
  p + geom_line() + geom_point() + labs(x="date", y="position")
}