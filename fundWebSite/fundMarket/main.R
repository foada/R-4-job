main.function = function(){
  # 设置环境函数
  library(lubridate)
  source("E:/MyFiles/Programming/R/fundWebSite/dataHandle.R")
  path = "E:/MyFiles/Programming/R/fundWebSite/fundMarket"
  setwd(path)
  files = c("dataPrepare.R","stockHeld.R")
  for(f in files) source(f)
  
  # 数据预处理
  dataPath = "E:/MyFiles/Programming/R/fundWebSite/fundMarket/Data"
  tmp = dataPrepare.function(path=dataPath,type="混合型")
  totalData = tmp[[1]]
  fundValue = tmp[[2]]
  
  # 计算并绘图
  stockHeldSeq = stockHeldSeq.function(totalData,fundValue)
  stockHeldPlot.function(stockHeldSeq)

}
