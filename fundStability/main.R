##----------------------数据格式介绍------------------------
# totalFundData = data.frame()
# 合并fund_nvd.feather 与 factors.feather中的数据得到
# 
# fundData = data.frame(
#   INNERCODE = c()
#   TRADINGDAY = c(ymd_1,ymd_2,...),
#   UNITNVRESTORED = c(),
#   SMB = c(),
#   HML = c(),
#   CMA = c()
# )


##------------------------main函数--------------------------
main.function = function(){
  # 设置环境函数
  path = "E:/MyFiles/Programming/R/fundStability"
  setwd(path)
  files = c("dataHandle.R","factorRegression.R","fundStability.R")
  for(f in files) source(f)
  
  # 读取数据
  rawFundData = read_feather(file.path(path,"fund_nvd.feather"), key = "INNERCODE")
  rawFactorData = read_feather(file.path(path,"factors.feather"), key = "ENDDATE")
  colnames(rawFactorData)[1] = "TRADINGDAY"
  totalFundData = merge(rawFundData,rawFactorData,by="TRADINGDAY",all.rawFundData="TRUE")
  
  # 生成基金稳定性指标
  fundStability = fundStability.function(totalFundData)
  xlsx::write.xlsx(fundStability,"fundStability.xlsx")
  
}
