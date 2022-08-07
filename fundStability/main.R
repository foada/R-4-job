##----------------------数据格式介绍------------------------
# totalFundData = data.frame(
#   funds = c(fundcode_1,fundcode_2,...,fundcode_n),
#   datas = c(fundData_1,fundData_2,...,fundData_n)
# )
# 
# fundData = data.frame(
#   tradingDay = c(ymd_1,ymd_2,...),
#   fundNvd = c(),
#   SMB = c(),
#   HML = c(),
#   CMA = c()
# )


##-----------------------数据预处理-------------------------
#从feather文件中读取数据，处理得到totalFundData格式的数据

dataPrepare.function = function(rawFundData,rawFactorData){
  
  
  fundcodes = rawData$INNERCODE
  fundDatas = c()
  for(fundcode in fundcodes){
    
    fundDatas = append(fundDatas,fundData)
  }
  
  totalFundData = data.frame(funds=fundcodes,datas=fundDatas)
  return(totalFundData)
}


##------------------------main函数--------------------------
main.function = function(){
  path = "E:/MyFiles/Programming/R/fundStability"
  setwd(path)
  files = c("dataHandle.R","factorRegression.R","fundStability.R")
  for(f in files) source(f)
  
  rawFundData = read_feather(file.path(path,"fund_nvd.feather"), key = "INNERCODE")
  rawFactorData = read_feather(file.path(path,"factors.feather"), key = "ENDDATE")
  colnames(rawFactorData)[1] = "TRADINGDAY"
  totalFundData = merge(rawFundData,rawFactorData,by="TRADINGDAY",all.rawFundData="TRUE")
  
  fundStability = fundStability.function(totalFundData)
  xlsx::write.xlsx(fundStability,"fundStability.xlsx")
  
}
