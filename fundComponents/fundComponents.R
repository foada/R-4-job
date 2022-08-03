## 基金持股行业比例计算

# rptDate.function确定报告日期
# 若在最近4个报告日期读取内容均为空，则返回空，否则返回最近的非空报告日期
rptDate.function = function(fundcode){
  # 确定最近四个报告日期
  # probDates = c("0331","0630","0930","1231")
  # currentYM = as.numeric(unlist(strsplit(as.character(Sys.Date()),split = "-"))[1:2])
  # i = (currentYM[2]-1)%/%3
  # if(i>0){
  #   probRptDates = append(paste(currentYM[1]-1,probDates[(i+1):4],sep=""),
  #                     paste(currentYM[1],probDates[1:i],sep=""))
  # }else if(i==0) {
  #   probRptDates = paste(currentYM[1]-1,probDates[1:4],sep="")
  # }
  
  # 确定最近的非空报告日期，或返回空
  for(rptDate in c("20220630")){ #probRptDates[4:1]
    # 从最近的报告日期开始，读取第1重仓股的数据
    stCode = w.wsd(fundcode,"prt_topstockcode,prt_heavilyheldstocktonav",rptDate,
              rptDate,paste("order=",as.character(1)))$Data["PRT_TOPSTOCKCODE"]
    # 若非空则返回报告日期
    if(!is.na(stCode)){return(rptDate)}
  }
  # 最近四个报告日期读取内容全为空，则返回空
  return(NaN)
}
#---------------------------------------------------------------------


singleProp.function = function(fundcode,rptDate){
  
  # 生成所有股票代码及持有市值
  stData = w.wset('allfundhelddetail',paste('rptdate=',rptDate,';windcode=',fundcode,sep=""))$Data
  stCodes = stData$stock_code
  stInduProp = stData$proportiontonetvalue
  stIndustry = c() # 前N大重仓股票对应的行业
  
  for(i in 1:length(stCodes)){
    # 由股票代码得到对应行业
    indust = w.wsd(stCodes[i],"industry_sw_2021",rptDate,rptDate,
                   "industryType=1")$Data["INDUSTRY_SW_2021"]
    if(is.na(indust)){
      cat("\t行业不明：",paste(stCodes[i],stData$stock_name[i],stInduProp[i]),"\n")
    } 
    # 将第i个股票对应行业写入stIndustry向量
    stIndustry = append(stIndustry,as.character(indust))
  }
  
  # 将行业与对应持有市值写入singleProp
  singleProp = data.frame(
    industry = stIndustry,
    proportion = stInduProp
  )
  return(singleProp)
}
#---------------------------------------------------------------------

# industryProp.function
# totalProp为所有基金的singleProp合并得到，即所有基金所有重仓股的各所属行业及对应市值
# 计算totalProp各行业的占比并返回
industryProp.function = function(totalProp){
  # 按行业分组，求和各行业对应持有市值比例加权和
  industryProp = aggregate(totalProp$proportion,by=list(totalProp$industry),FUN=sum)
  # 计算各行业对应市值比例
  industryProp$x = industryProp$x / sum(industryProp$x)
  names(industryProp) = c("行业","持有比例")
  return(industryProp)
}
#---------------------------------------------------------------------


# proportions.function
# main；给定基金代码列表，计算总的行业持有占比
proportions.function = function(fundcodes,fundWeights){
  library(WindR)
  w.start()
  
  totalProp = data.frame(
    industry = c(),
    proportion = c()
  )
  
  for(i in 1:length(fundcodes)){
    fundcode = fundcodes[i]
    rptDate = rptDate.function(fundcode)
    cat(paste("\n",fundcode,fundWeights[i],"\n"))
    if(is.na(rptDate)) {
      cat("\t未能读取该基金数据，已跳过\n")
      next
    }  # 若报告日期返回为空，则跳过该基金
    
    singleProp = singleProp.function(fundcode,rptDate)
    singleProp$proportion = singleProp$proportion*fundWeights[i]
    totalProp = rbind(totalProp,singleProp)
  }
  
  industryProp = industryProp.function(totalProp)
  
  return(industryProp)
}
#---------------------------------------------------------------------


# 调用示例
globalIndust <<- c()
globalProp <<- c()
# fundcodes = c("000242.OF","010790.OF")
# fundWeights = c(0.0681061098482082,0.0746139840434839)
# load("E:/MyFiles/Programming/R/fundComponents/fundCodeWeight.RData")
# fundcodes = fundCodeWeight$fundcodes
# fundWeights = fundCodeWeight$fundWeights
file = "E:/MyFiles/Programming/R/fundComponents/财富成长基金占比0729.xlsx"
data = xlsx::read.xlsx(file,1)
fundcodes = paste(data$基金代码,".OF",sep="")[2:20]
fundWeights = data$持仓比例[2:20]

industryProp = proportions.function(fundcodes,fundWeights)

industryProp = industryProp[order(-industryProp$持有比例),]
rownames(industryProp) = NULL
file = "E:/MyFiles/Programming/R/fundComponents/tenthsComponents0729.xlsx"
xlsx::write.xlsx(industryProp,file)

#---------------------------------------------------------------------

# 生成股票持有明细情况
# df = w.wset('allfundhelddetail',paste('rptdate=20211231;windcode=',fundcodes[1],sep=""))$Data
# df$CODE = fundcodes[1]
# 
# for(i in 2:length(fundcodes)){
#   tempdf = w.wset('allfundhelddetail',paste('rptdate=20211231;windcode=',fundcodes[i],sep=""))$Data
#   if(length(tempdf$CODE)==0){
#     print(fundcodes[i])
#     next
#   }
#   tempdf$CODE = fundcodes[i]
#   df = rbind(df,tempdf)
# }

##---------------------------------------------------------------------
# ## singleProp.function
# # 给定fundcode，得到对应前N大重仓股的所属行业及对应持仓市值
# # 返回由行业与持仓市值构成的dataframe
# singleProp.function = function(fundcode,rptDate){
#   N = 10 # 取前N大重仓股
#   stCodes = c()
#   stNames = c()
#   stIndustry = c() # 前N大重仓股票对应的行业
#   stInduProp = c() # 前N大重仓股票对应的持有市值
#   
#   # 生成第i大重仓股的行业及持有市值
#   for(i in 1:N){
#     # 读取第i大重仓股的股票代码及持有市值
#     stData = w.wsd(fundcode,"prt_topstockcode,prt_heavilyheldstocktonav",
#                    rptDate,rptDate,paste("order=",as.character(i)))$Data
#     
#     # 得到标准的股票代码及名称
#     stCode = as.character(stData["PRT_TOPSTOCKCODE"])
#     if(nchar(stCode)==4){
#       stCode = paste(stCode,".HK",sep="")
#     }else if (startsWith(stCode,"6")){
#       stCode = paste(stCode,".SH",sep="")
#     }else{
#       stCode = paste(stCode,".SZ",sep="")
#     }
#     stName = as.character(w.wsd(stCode,"sec_name",rptDate,rptDate)$Data["SEC_NAME"])
#     
#     # 由股票代码得到对应行业
#     indust = w.wsd(stCode,"industry_sw_2021",rptDate,rptDate,
#                    "industryType=1")$Data["INDUSTRY_SW_2021"]
#     prop = stData["PRT_HEAVILYHELDSTOCKTONAV"]
#     print(paste(stCode,stName,indust,prop))
#     
#     
#     stCodes = append(stCodes,stCode)
#     stNames = append(stNames,stName)
#     
#     # 将第i大重仓股对应行业写入stIndustry向量
#     stIndustry = append(stIndustry,as.character(indust))
#     # 将第i大重仓股对应持有市值占比写入stInduProp向量
#     stInduProp = append(stInduProp,as.numeric(stData["PRT_HEAVILYHELDSTOCKTONAV"]))
#   }
#   
#   # 将行业与对应持有市值写入singleProp
#   singleProp = data.frame(
#     fundcode = rep(fundcode,10),
#     stCodes = stCodes,
#     stNames = stNames,
#     industry = stIndustry,
#     proportion = stInduProp
#   )
#   return(singleProp)
# }
# 
# 
# totalProp = data.frame(
#   fundcode = c(),
#   stCodes = c(),
#   stNames = c(),
#   industry = c(),
#   proportion = c()
# )
# 
# for(i in 1:length(fundcodes)){
#   fundcode = fundcodes[i]
#   rptDate = rptDate.function(fundcode)
#   cat(paste("\n",fundcode,fundWeights[i],"\n"))
#   if(is.na(rptDate)) {
#     cat("\t未能读取该基金数据，已跳过\n")
#     next
#   }  # 若报告日期返回为空，则跳过该基金
#   
#   singleProp = singleProp.function(fundcode,rptDate)
#   singleProp$proportion = singleProp$proportion*fundWeights[i]
#   totalProp = rbind(totalProp,singleProp)
# }
# 
