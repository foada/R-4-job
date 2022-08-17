dataPrepare.function = function(path,type){
  # 读取数据
  files = list.files(path)
  for(file in files){
    filename = strsplit(file,split = ".",fixed = T)[[1]][1]
    assign(filename,read_feather(file.path(path,file)))
  } 
  
  # 中证全指的内部代码
  code = "000985"
  indexInnerCode = indexCode[indexCode$SECUCODE==code]$INNERCODE
  # 中证全指的历史收益数据
  indexData = indexNvd[indexNvd$INDEXCODE==indexInnerCode]
  
  # 所有混合型基金数据
  # type = "混合型"
  fundData = merge(fundNvd,fundType[fundType$FUNDTYPE==type],by="INNERCODE",all=F)
  
  # 按日期对应indexData与fundData
  rawTotalData = merge(indexData,fundData,by="TRADINGDAY",all=F)
  totalData = rawTotalData[!(is.na(rawTotalData$CHANGEPCT) | is.na(rawTotalData$NVRDAILYGROWTHRATE))]
  totalData$INDEXCODE = c()
  totalData$FUNDTYPE = c()
  
  #基金净值数据
  fundValue = fundValue[!is.na(fundValue$NV)]
  
  return(list(totalData,fundValue))
}

##
#生成endDate序列
endDateSeq.function = function(startDate,endDate){
  monthRange = floor((endDate-startDate+ddays(1))/dmonths(1))+1
  endDateSeq = seq(startDate,length=monthRange,by="1 month")-1
  return(endDateSeq)
}

# 给出endDate截止，前3个月的数据dataRange，用于后续回归
dataRange.function = function(endDate,data){
  startDate = endDate-months(3)
  dataRange = data[which(data$TRADINGDAY>startDate & data$TRADINGDAY<=endDate),]
  return(dataRange)
}
